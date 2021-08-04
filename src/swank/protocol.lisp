(defpackage #:mondo/swank/protocol
  (:use #:cl)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:find-shortest-nickname)
  (:import-from #:swank-protocol
                #:connection
                #:connection-thread
                #:connection-package
                #:request-listener-eval
                #:request-swank-require
                #:request-create-repl
                #:emacs-rex
                #:message-waiting-p
                #:read-all-messages
                #:read-message)
  (:export #:swank-eval
           #:initialize-swank-repl))
(in-package #:mondo/swank/protocol)

(defun wait-for-messages (connection)
  (loop until (message-waiting-p connection)
        do (sleep 0.01)))

(defun process-messages (connection)
  (wait-for-messages connection)
  (loop for message = (read-message connection)
        when message
        do (log :debug "Swank message: ~S" message)
           (case (first message)
             (:write-string (format t "~&~A" (second message)))
             (:new-package
              (setf (connection-package connection)
                    (find-shortest-nickname (rest message))))
             ((:new-features :indentation-update))
             (:return
              (let ((result (second message)))
                (if (eq (first result) :ok)
                    (return (second result))
                    (progn
                      (log :error "Result is not ok: ~S" result)
                      (return)))))
             (otherwise
               (log :error "Unknown message: ~S" message)))))

(defun swank-eval (connection form-string)
  (check-type connection connection)
  (check-type form-string string)
  (request-listener-eval connection form-string)
  (process-messages connection))

(defun initialize-swank-repl (connection)
  (unless (eql (connection-thread connection) 1)
    (request-swank-require connection '(swank-repl))
    (process-messages connection)
    (request-create-repl connection)
    (setf (connection-thread connection) t)
    (process-messages connection)))
