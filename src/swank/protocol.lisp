(defpackage #:mondo/swank/protocol
  (:use #:cl)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/debugger
                #:*debugger-level*)
  (:import-from #:mondo/utils
                #:find-shortest-nickname)
  (:import-from #:swank-protocol
                #:connection
                #:connection-thread
                #:connection-package
                #:request-listener-eval
                #:request-swank-require
                #:request-create-repl
                #:request-throw-to-toplevel
                #:request-invoke-restart
                #:emacs-rex
                #:message-waiting-p
                #:read-all-messages
                #:read-message)
  (:export #:swank-eval
           #:swank-rex
           #:swank-complete
           #:initialize-swank-repl
           #:invoke-debugger-restart
           #:exit-debugger
           #:process-messages
           #:debugger
           #:debugger-level
           #:debugger-restarts))
(in-package #:mondo/swank/protocol)

(defun wait-for-messages (connection)
  (loop until (message-waiting-p connection)
        do (sleep 0.01)))

(define-condition debugger ()
  ((restarts :initarg :restarts
             :accessor debugger-restarts)
   (level :initarg :level
          :accessor debugger-level)))

(defun process-messages (connection)
  (wait-for-messages connection)
  (let (debugger)
    (loop for message = (read-message connection)
          when message
          do (log :debug "Swank message: ~S" message)
          (case (first message)
            (:write-string (write-string (second message)))
            (:new-package
             (setf (connection-package connection)
                   (find-shortest-nickname (rest message))))
            ((:new-features :indentation-update))
            (:return
             (let ((result (second message)))
               (case (first result)
                 ((:ok :abort)
                  (return (values (second result) (first result))))
                 (otherwise
                   (log :error "Unexpected result: ~S" result)
                   (return)))))
            (:debug
             (destructuring-bind (tid level condition restarts frames continuations)
                 (rest message)
               (declare (ignore tid continuations))
               (format t "~&~{~A~%~}" (remove-if #'null condition))
               (format t "~2&Backtrace:~%")
               (loop for (num frame) in frames
                     do (format t "~&~A: ~A~%" num frame))
               (format t "~2&Restarts:~%")
               (loop for i from 0
                     for (name description) in restarts
                     do (format t "~&  ~D: [~A] ~A~%" i name description))
               (setf *debugger-level* level)
               (setf debugger (make-condition 'debugger :restarts restarts :level level))))
            (:debug-activate (error debugger))
            (:debug-return
             (destructuring-bind (tid level continuations)
                 (rest message)
               (declare (ignore tid continuations))
               (setf *debugger-level* (1- level))))
            (otherwise
              (log :error "Unknown message: ~S" message))))))

(defun swank-eval (connection form-string)
  (check-type connection connection)
  (check-type form-string string)
  (request-listener-eval connection form-string)
  (process-messages connection))

(defun swank-rex (connection form)
  (let ((thread-id (connection-thread connection)))
    (setf (connection-thread connection) t)
    (unwind-protect
        (progn
          (swank-protocol:emacs-rex connection form)
          (process-messages connection))
      (setf (connection-thread connection) thread-id))))

(defun swank-complete (connection prefix &optional (package-name (connection-package connection)))
  (swank-rex connection `(swank:simple-completions ,prefix ',package-name)))

(defun invoke-debugger-restart (connection level restart-num)
  (request-invoke-restart connection level restart-num)
  (process-messages connection))

(defun exit-debugger (connection)
  (request-throw-to-toplevel connection)
  (process-messages connection))

(defun initialize-swank-repl (connection)
  (unless (eql (connection-thread connection) 1)
    (request-swank-require connection '(swank-repl))
    (process-messages connection)
    (request-create-repl connection)
    (process-messages connection)))
