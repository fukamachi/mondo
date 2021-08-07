(defpackage #:mondo/swank/protocol
  (:use #:cl)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:swank-protocol
                #:connection
                #:connection-thread
                #:connection-package
                #:connection-request-count
                #:request-listener-eval
                #:request-swank-require
                #:request-create-repl
                #:request-throw-to-toplevel
                #:request-invoke-restart
                #:send-message-string
                #:emacs-rex
                #:read-message)
  (:export #:swank-eval
           #:swank-rex
           #:swank-complete
           #:swank-arglist
           #:swank-interrupt
           #:invoke-debugger-restart
           #:exit-debugger))
(in-package #:mondo/swank/protocol)

(defun swank-eval (connection form-string)
  (check-type connection connection)
  (check-type form-string string)
  (request-listener-eval connection form-string)
  (connection-request-count connection))

(defun swank-rex (connection form)
  (let ((thread-id (connection-thread connection)))
    (setf (connection-thread connection) t)
    (unwind-protect
        (progn
          (swank-protocol:emacs-rex connection form)
          (connection-request-count connection))
      (setf (connection-thread connection) thread-id))))

(defun swank-complete (connection prefix &optional (package-name (connection-package connection)))
  (swank-rex connection `(swank:simple-completions ,prefix ',package-name)))

(defun swank-arglist (connection symbol-name &optional (package-name (connection-package connection)))
  (swank-rex connection `(swank:operator-arglist ,symbol-name ',package-name)))

(defun swank-send (connection form)
  (send-message-string connection
                       (let ((*print-case* :downcase))
                         (prin1-to-string form)))
  (values))

(defun swank-interrupt (connection &optional (thread :repl-thread))
  (swank-send connection `(:emacs-interrupt ,thread)))

(defun invoke-debugger-restart (connection level restart-num)
  (request-invoke-restart connection level restart-num)
  (connection-request-count connection))

(defun exit-debugger (connection)
  (request-throw-to-toplevel connection)
  (connection-request-count connection))
