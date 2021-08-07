(defpackage #:mondo/debugger
  (:use #:cl)
  (:import-from #:mondo/readline
                #:read-input)
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
                #:starts-with)
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

(defun print-error-information (condition restarts frames)
  (format t "~&~{~A~%~}" (remove-if #'null condition))
  (format t "~2&Backtrace:~%")
  (loop for (num frame) in frames
        do (format t "~&~A: ~A~%" num frame))
  (format t "~2&Restarts:~%")
  (loop for i from 0
        for (name description) in restarts
        do (format t "~&  ~D: [~A] ~A~%" i name description)))

(defun invoke-restarts-by-name (connection error name)
  (with-slots (restarts) error
    (let ((restart-num (position name restarts :key #'first :test #'string=)))
      (if restart-num
          (invoke-debugger-restart connection
                                   (debugger-level error)
                                   restart-num)
          (progn
            (log :warn "No restarts named '~A'" name)
            nil)))))

(defun request-abort (connection error)
  (invoke-restarts-by-name connection
                           error
                           (if (eql (debugger-level error) 1)
                               "*ABORT"
                               "ABORT")))

(defun process-debugger-mode (connection error)
  (handler-case
      (let ((level (debugger-level error)))
        (with-slots (condition restarts frames) error
          (print-error-information condition restarts frames))
        (handler-bind ((mondo-debugger
                         (lambda (debugger)
                           (unless (< level (debugger-level debugger))
                             (let ((restart (find-restart 'ignore debugger)))
                               (when restart
                                 (invoke-restart restart)))))))
          (loop
            (let* ((input (read-input (debug-prompt level)))
                   (call-id
                     (if input
                         (if (starts-with "restart" input)
                             (invoke-debugger-restart connection
                                                      level
                                                      ;; TODO: Check if the input is an integer
                                                      (parse-integer (subseq input 7)))
                             (swank-eval connection input))
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
        (wait-for-response-of-call connection call-id)))))

(defun invoke-mondo-debugger (connection debugger-error)
  (restart-case
      (error debugger-error)
    (start-debugger-mode ()
      (process-debugger-mode connection debugger-error))
    (ignore ()
      nil)))
