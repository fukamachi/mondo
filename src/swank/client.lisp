(defpackage #:mondo/swank/client
  (:use #:cl)
  (:import-from #:mondo/swank/server
                #:swank-server
                #:swank-server-port)
  (:import-from #:swank-protocol
                #:make-connection
                #:connection-package
                #:connect)
  (:export #:connect-to-swank-server))
(in-package #:mondo/swank/client)

(defun connect-to-swank-server (swank-server)
  (check-type swank-server swank-server)
  (let ((conn (make-connection "127.0.0.1" (swank-server-port swank-server))))
    (setf (connection-package conn) "CL-USER")
    (connect conn)
    conn))
