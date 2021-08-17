(defpackage #:mondo/swank/client
  (:use #:cl)
  (:import-from #:mondo/swank/server
                #:swank-server
                #:swank-server-host
                #:swank-server-port)
  (:import-from #:mondo/swank/connection
                #:make-connection
                #:open-connection
                #:connection-process-thread)
  (:import-from #:mondo/swank/protocol
                #:make-dispatch-event-function
                #:receive-message)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:make-thread)
  (:import-from #:bordeaux-threads)
  (:export #:connect-to-swank-server
           #:start-processing))
(in-package #:mondo/swank/client)

(defun make-process-thread (connection process-fn)
  (make-thread
    (lambda ()
      (loop
        (let ((message (receive-message connection)))
          (when message
            (funcall process-fn message)))))
    :name "mondo swank message processor"))

(defun connect-to-swank-server (swank-server)
  (check-type swank-server swank-server)
  (let* ((host (swank-server-host swank-server))
         (port (swank-server-port swank-server))
         (conn (make-connection :host host
                                :port port)))
    (log :debug "Connecting to the swank server on ~A:~A" host port)
    (handler-case
        (open-connection conn)
      (usocket:connection-refused-error ()
        (log :error "Failed to connect to the swank server on ~A:~A" host port)
        (uiop:quit -1)))
    (log :debug "Connected")
    conn))

(defun start-processing (connection)
  (log :debug "Start processing")
  (setf (connection-process-thread connection)
        (make-process-thread connection
                             (make-dispatch-event-function connection))))
