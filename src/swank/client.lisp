(defpackage #:mondo/swank/client
  (:use #:cl)
  (:import-from #:mondo/swank/server
                #:swank-server
                #:swank-server-host
                #:swank-server-port)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:swank-protocol
                #:make-connection
                #:connection-package
                #:connect)
  (:import-from #:mondo/logger
                #:*log-level*
                #:+debug+)
  (:export #:connect-to-swank-server))
(in-package #:mondo/swank/client)

(defun connect-to-swank-server (swank-server)
  (check-type swank-server swank-server)
  (let* ((host (swank-server-host swank-server))
         (port (swank-server-port swank-server))
         (conn (make-connection host port
                                :logp (eql *log-level* +debug+))))
    (setf (connection-package conn) "CL-USER")
    (log :debug "Connecting to the swank server on ~A:~A" host port)
    (handler-case
        (connect conn)
      (usocket:connection-refused-error ()
        (log :error "Failed to connect to the swank server on ~A:~A" host port)
        (uiop:quit -1)))
    (log :debug "Connected")
    conn))
