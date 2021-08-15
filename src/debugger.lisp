(defpackage #:mondo/debugger
  (:use #:cl)
  (:import-from #:mondo/readline
                #:read-input
                #:default
                #:defkeymap
                #:with-keymap)
  (:import-from #:mondo/swank/protocol
                #:swank-invoke-nth-restart
                #:debug-restarts
                #:debug-thread
                #:debug-level
                #:debug-condition
                #:debug-frames
                #:debug-activate
                #:debug-activate-thread
                #:debug-activate-level
                #:debug-return
                #:debug-return-thread
                #:debug-return-level
                #:ignore-event
                #:swank-listener-eval
                #:swank-throw-to-toplevel)
  (:shadowing-import-from #:mondo/swank/protocol
                          #:debug)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:integer-string-p
                #:string-space-trim)
  (:import-from #:alexandria
                #:when-let
                #:with-gensyms
                #:once-only)
  (:export #:process-debugger-mode
           #:print-error-information
           #:with-debugger))
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
  (let ((restarts (debug-restarts event)))
    (unless (<= 0 num (1- (length restarts)))
      (log :warn "Invalid restart number: ~A" num)
      (return-from swank-invoke-restart))
    (swank-invoke-nth-restart (debug-thread event)
                              (debug-level event)
                              num
                              connection)))

(defun swank-invoke-restart-by-name (event name connection)
  (let ((restarts (debug-restarts event)))
    (let ((restart-num (position name restarts :key #'first :test #'string=)))
      (if restart-num
          (swank-invoke-restart event restart-num connection)
          (progn
            (log :warn "No restarts named '~A'" name)
            nil)))))

(defun swank-debugger-abort (event connection)
  (swank-invoke-restart-by-name event
                                (if (eql (debug-level event) 1)
                                    "*ABORT"
                                    "ABORT")
                                connection))

(defvar *current-event*)

(defun show-backtrace (&rest args)
  (declare (ignore args))
  (let* ((condition (debug-condition *current-event*))
         (frames (debug-frames *current-event*))
         (content
           (with-output-to-string (*standard-output*)
             (format t "~&~{~A~%~}" (remove-if #'null condition))
             (format t "~2&Backtrace:~%")
             (loop for (num frame) in frames
                   do (format t "~&~A: ~A~%" num frame)))))
    (handler-case
        (uiop:run-program
          (format nil "echo ~S | less" content)
          :input :interactive
          :output :interactive)
      (uiop:subprocess-error ()
        (format t "~%~A" content)
        (rl:on-new-line nil)))))

(defkeymap (debugger :base default)
  ("\\C-t" #'show-backtrace))

(defmacro with-debugger ((connection) &body body)
  (with-gensyms (event debug-info debug-event in-debugger)
    (once-only (connection)
      `(let ((,debug-info (make-hash-table :test 'equal)))
         (handler-case
             (handler-bind ((debug
                              (lambda (,event)
                                (setf (gethash (cons (debug-thread ,event)
                                                     (debug-level ,event))
                                               ,debug-info)
                                      ,event)
                                (ignore-event ,event))))
               ,@body)
           (debug-activate (,event)
             (when-let (,debug-event (gethash (cons (debug-activate-thread ,event)
                                                    (debug-activate-level ,event))
                                              ,debug-info))
               (block ,in-debugger
                 (handler-bind ((debug-return
                                  (lambda (,event)
                                    (when (and (= (debug-level ,debug-event)
                                                  (debug-return-level ,event))
                                               (= (debug-thread ,debug-event)
                                                  (debug-return-thread ,event)))
                                      (return-from ,in-debugger)))))
                   (process-debugger-mode ,connection
                                          ,debug-event)))))
           (debug-return ()))))))

(defun process-debugger-mode (connection event)
  (with-keymap (debugger)
    (let ((*current-event* event))
      (handler-case
          (with-debugger (connection)
            (let ((level (debug-level event))
                  (condition (debug-condition event))
                  (restarts (debug-restarts event)))
              (print-error-information condition restarts)
              (handler-bind ((debug-activate
                               (lambda (event)
                                 (unless (< level (debug-activate-level event))
                                   (funcall #'ignore-event event)))))
                (loop
                  (let* ((input (read-input (debug-prompt level)
                                            :no-history t))
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
                                                    :thread (debug-thread event))))
                            (swank-debugger-abort event connection))
                      (declare (ignore result))
                      (unless success  ;; when aborted
                        (return))))))))
        #+sbcl
        (sb-sys:interactive-interrupt ()
          (swank-throw-to-toplevel (debug-thread event) connection))))))
