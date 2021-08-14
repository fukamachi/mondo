(defpackage #:mondo/debugger
  (:use #:cl)
  (:import-from #:mondo/readline
                #:read-input
                #:default
                #:defkeymap
                #:with-keymap)
  (:import-from #:mondo/swank/protocol
                #:swank-invoke-nth-restart
                #:debug-activate
                #:debug-activate-thread
                #:debug-activate-restarts
                #:debug-activate-level
                #:debug-activate-condition
                #:debug-return
                #:debug-return-level
                #:ignore-event
                #:swank-listener-eval
                #:swank-throw-to-toplevel)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:integer-string-p
                #:string-space-trim)
  (:export #:process-debugger-mode
           #:print-error-information))
(in-package #:mondo/debugger)

(defun debug-prompt (level)
  (format nil "debugger [~A] " level))

(defun print-error-information (condition restarts)
  (format t "~&~{~A~%~}" (remove-if #'null condition))
  (format t "~2&Restarts:~%")
  (loop for i from 0
        for (name description) in restarts
        do (format t "~&  ~D: [~A] ~A~%" i name description))
  (format t "~2&  Ctrl-t: show backtraces~%"))

(defun swank-invoke-restart (event num connection)
  (let ((restarts (debug-activate-restarts event)))
    (unless (<= 0 num (1- (length restarts)))
      (log :warn "Invalid restart number: ~A" num)
      (return-from swank-invoke-restart))
    (swank-invoke-nth-restart (debug-activate-thread event)
                              (debug-activate-level event)
                              num
                              connection)))

(defun swank-invoke-restart-by-name (event name connection)
  (let ((restarts (debug-activate-restarts event)))
    (let ((restart-num (position name restarts :key #'first :test #'string=)))
      (if restart-num
          (swank-invoke-restart event restart-num connection)
          (progn
            (log :warn "No restarts named '~A'" name)
            nil)))))

(defun swank-debugger-abort (event connection)
  (swank-invoke-restart-by-name event
                                (if (eql (debug-activate-level event) 1)
                                    "*ABORT"
                                    "ABORT")
                                connection))

(defvar *current-event*)

(defun show-backtrace (&rest args)
  (declare (ignore args))
  (with-slots (condition frames) *current-event*
    (uiop:run-program
      (format nil "echo ~S | less"
              (with-output-to-string (*standard-output*)
                (format t "~&~{~A~%~}" (remove-if #'null condition))
                (format t "~2&Backtrace:~%")
                (loop for (num frame) in frames
                      do (format t "~&~A: ~A~%" num frame))))
      :ignore-error-status t
      :input :interactive
      :output :interactive)))

(defkeymap (debugger :base default)
  ("\\C-t" #'show-backtrace))

(defun process-debugger-mode (connection event)
  (with-keymap (debugger)
    (let ((*current-event* event))
      (handler-case
          (let ((level (debug-activate-level event))
                (condition (debug-activate-condition event))
                (restarts (debug-activate-restarts event)))
            (print-error-information condition restarts)
            (handler-bind ((debug-activate
                             (lambda (event)
                               (unless (< level (debug-activate-level event))
                                 (funcall #'ignore-event event))))
                           (debug-return
                             (lambda (event)
                               (when (= level (debug-return-level event))
                                 (return-from process-debugger-mode)))))
              (loop
                (let* ((input (read-input (debug-prompt level)))
                       (input (and input
                                   (string-space-trim input))))
                  (multiple-value-bind (result success)
                      (if input
                          (cond
                            ((integer-string-p input)
                             (swank-invoke-restart event
                                                   (parse-integer input)
                                                   connection))
                            ((or (string= input "a")
                                 (string= input "abort"))
                             (swank-debugger-abort event connection))
                            (t
                             (swank-listener-eval input connection
                                                  :thread (debug-activate-thread event))))
                          (swank-debugger-abort event connection))
                    (declare (ignore result))
                    (unless success  ;; when aborted
                      (return)))))))
        (debug-activate (event)
          (process-debugger-mode connection event))
        #+sbcl
        (sb-sys:interactive-interrupt ()
          (swank-throw-to-toplevel (debug-activate-thread event) connection))))))
