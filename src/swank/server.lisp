(defpackage #:mondo/swank/server
  (:use #:cl)
  (:import-from #:usocket)
  (:export #:create-swank-server
           #:swank-server
           #:swank-server-process
           #:swank-server-port))
(in-package #:mondo/swank/server)

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (progn
                         (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address t))
                         t)
           (usocket:address-in-use-error () nil)
           (usocket:socket-error (e)
             (warn "USOCKET:SOCKET-ERROR: ~A" e)
             nil))
      (when socket
        (usocket:socket-close socket)
        t))))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))

(defstruct swank-server
  process
  port)

(defun server-running-p (port)
  (handler-case (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                  (usocket:socket-close socket)
                  t)
    (usocket:connection-refused-error () nil)
    (usocket:connection-reset-error () nil)))

(defun create-swank-server (&key (port (random-port)))
  (prog1
      (make-swank-server
        :process (uiop:launch-program
                   `("ros" "-s" "swank" "-e" ,(format nil "(swank:create-server :port ~D :dont-close t)" port) "run")
                   :input :stream
                   :output nil
                   :error t)
        :port port)
    (loop do (sleep 0.1)
          until (server-running-p port))))
