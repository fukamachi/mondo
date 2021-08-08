(defpackage #:mondo/debugger
  (:use #:cl)
  (:import-from #:mondo/readline
                #:read-input
                #:default
                #:defkeymap
                #:with-keymap)
  (:import-from #:mondo/swank/protocol
                #:invoke-debugger-restart
                #:swank-eval
                #:exit-debugger)
  (:import-from #:mondo/swank/client
                #:wait-for-response-of-call
                #:response)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:integer-string-p
                #:string-space-trim)
  (:export #:process-debugger-mode
           #:mondo-debugger
           #:invoke-mondo-debugger
           #:debugger-level
           #:print-error-information))
(in-package #:mondo/debugger)

(define-condition mondo-debugger ()
  ((level :initarg :level
          :reader debugger-level)
   (condition :initarg :condition
              :reader debugger-condition)
   (restarts :initarg :restarts
             :reader debugger-restarts)
   (frames :initarg :frames
           :reader debugger-frames)))

(defun debug-prompt (level)
  (format nil "debugger [~A] " level))

(defun print-error-information (condition restarts)
  (format t "~&~{~A~%~}" (remove-if #'null condition))
  (format t "~2&Restarts:~%")
  (loop for i from 0
        for (name description) in restarts
        do (format t "~&  ~D: [~A] ~A~%" i name description))
  (format t "~2&  Ctrl-t: show backtraces~%"))

(defun invoke-swank-restart (connection error num)
  (with-slots (restarts) error
    (unless (<= 0 num (1- (length restarts)))
      (log :warn "Invalid restart number: ~A" num)
      (return-from invoke-swank-restart))
    (invoke-debugger-restart connection
                             (debugger-level error)
                             num)))

(defun invoke-swank-restart-by-name (connection error name)
  (with-slots (restarts) error
    (let ((restart-num (position name restarts :key #'first :test #'string=)))
      (if restart-num
          (invoke-swank-restart connection error restart-num)
          (progn
            (log :warn "No restarts named '~A'" name)
            nil)))))

(defun request-abort (connection error)
  (invoke-swank-restart-by-name connection
                                error
                                (if (eql (debugger-level error) 1)
                                    "*ABORT"
                                    "ABORT")))

(defvar *current-error*)

(defun show-backtrace (&rest args)
  (declare (ignore args))
  (with-slots (condition frames) *current-error*
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

(defun process-debugger-mode (connection error)
  (with-keymap (debugger)
    (let ((*current-error* error))
      (handler-case
          (let ((level (debugger-level error)))
            (with-slots (condition restarts) error
              (print-error-information condition restarts))
            (handler-bind ((mondo-debugger
                             (lambda (debugger)
                               (unless (< level (debugger-level debugger))
                                 (let ((restart (find-restart 'ignore debugger)))
                                   (when restart
                                     (invoke-restart restart)))))))
              (loop
                (let* ((input (read-input (debug-prompt level)))
                       (input (and input
                                   (string-space-trim input)))
                       (call-id
                         (if input
                             (cond
                               ((integer-string-p input)
                                (invoke-swank-restart connection
                                                      error
                                                      (parse-integer input)))
                               ((or (string= input "a")
                                    (string= input "abort"))
                                (request-abort connection error))
                               (t
                                (swank-eval connection input)))
                             (request-abort connection error))))
                  (when call-id
                    (wait-for-response-of-call connection
                                               call-id)
                    (destructuring-bind (status value)
                        (response connection call-id)
                      (declare (ignore value))
                      (when (eq status :abort)
                        (return))))))))
        (mondo-debugger (debugger)
          (process-debugger-mode connection debugger))
        #+sbcl
        (sb-sys:interactive-interrupt ()
          (let ((call-id (exit-debugger connection)))
            (wait-for-response-of-call connection call-id)))))))

(defun invoke-mondo-debugger (connection debugger-error)
  (restart-case
      (error debugger-error)
    (start-debugger-mode ()
      (process-debugger-mode connection debugger-error))
    (ignore ()
      nil)))
