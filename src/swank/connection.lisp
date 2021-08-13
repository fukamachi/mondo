(defpackage #:mondo/swank/connection
  (:use #:cl)
  (:import-from #:usocket
                #:stream-usocket
                #:socket-connect
                #:socket-close)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-recursive-lock-held
                #:destroy-thread
                #:join-thread)
  (:export #:connection
           #:connection-host
           #:connection-port
           #:connection-socket
           #:connection-package
           #:connection-process-thread
           #:connection-lock
           #:connection-continuation-counter
           #:connection-rex-continuations
           #:make-connection
           #:open-connection
           #:close-connection))
(in-package #:mondo/swank/connection)

(defstruct connection
  (host "127.0.0.1" :type string)
  (port 4005 :type integer)
  (socket nil :type (or usocket:stream-usocket null))
  (package "CL-USER" :type string)
  (process-thread nil)
  (lock (bt:make-recursive-lock "mondo connection lock"))
  (continuation-counter 0 :type integer)
  (rex-continuations '()))

(defun open-connection (connection)
  (let ((socket (usocket:socket-connect (connection-host connection)
                                        (connection-port connection)
                                        :element-type '(unsigned-byte 8))))
    (setf (connection-socket connection) socket)
    connection))

(defun close-connection (connection)
  (with-slots (socket process-thread lock)
      connection
    (bt:with-recursive-lock-held (lock)
      (when socket
        (usocket:socket-close socket))
      (when process-thread
        (bt:destroy-thread process-thread)
        (bt:join-thread process-thread)
        (setf process-thread nil))))
  (values))
