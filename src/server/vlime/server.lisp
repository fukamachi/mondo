(defpackage #:mondo/server/vlime/server
  (:use #:cl
        #:mondo/server)
  (:import-from #:mondo/server/vlime/protocol
                #:read-message
                #:write-message)
  (:import-from #:mondo/swank
                #:swank-send
                #:swank-rex
                #:event-message)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:octets-to-string
                #:make-thread)
  (:import-from #:usocket)
  (:import-from #:bordeaux-threads)
  (:import-from #:alexandria
                #:destructuring-case)
  (:export #:vlime))
(in-package #:mondo/server/vlime/server)

(defclass vlime (server)
  ((socket :accessor vlime-server-socket)
   (lock :initform (bt:make-lock "vlime server lock")
         :accessor vlime-server-lock)
   (main-thread :initform nil)
   (clients :initform nil
            :reader vlime-server-clients)))

(defstruct client
  socket
  thread
  (lock (bt:make-lock "vlime client lock")))

(defun write-message-to-client (message client)
  (#+sbcl sb-sys:without-interrupts
   #-sbcl progn
   (bt:with-lock-held ((client-lock client))
     (write-message
       message
       (usocket:socket-stream (client-socket client))))))

(defmethod start-server ((server vlime))
  (let ((host (server-host server))
        (port (server-port server)))
    (let ((server-socket
            (usocket:socket-listen host port
                                   :reuse-address t
                                   :backlog 128
                                   :element-type '(unsigned-byte 8))))
      (setf (vlime-server-socket server) server-socket))
    server)

  (with-slots (lock main-thread) server
    (bt:with-lock-held (lock)
      (setf main-thread
            (make-thread
              (lambda ()
                (main-thread-loop server))
              :name "vlime server main thread"))))

  (values))

(defun main-thread-loop (server)
  (let ((server-socket (vlime-server-socket server)))
    (loop
      (let ((client-socket (usocket:socket-accept server-socket)))
        (log :info "New client: ~A" client-socket)
        (bt:with-lock-held ((vlime-server-lock server))
          (let ((client (make-client :socket client-socket)))
            (setf (client-thread client)
                  (make-thread
                    (lambda ()
                      (client-thread-loop client (server-swank-connection server)))
                    :name "vlime client loop thread"))
            (push client (slot-value server 'clients))))))))

(defmethod stop-server ((server vlime))
  (with-slots (lock main-thread clients socket) server
    (when socket
      (bt:with-lock-held (lock)
        (bt:destroy-thread main-thread)
        (dolist (client clients)
          (with-slots (lock thread socket) client
            (bt:with-lock-held (lock)
              (bt:destroy-thread thread)
              (usocket:socket-close socket)
              (setf socket nil
                    thread nil))))
        (when (bt:thread-alive-p main-thread)
          (ignore-errors (bt:join-thread main-thread)))
        (usocket:socket-close socket)
        (setf socket nil
              main-thread nil
              clients nil)))))

(defun client-thread-loop (client swank-connection)
  (let* ((client-socket (client-socket client))
         (stream (usocket:socket-stream client-socket)))
    (loop
      (let ((request (read-message stream)))
        (destructuring-case request
          ((:emacs-rex form package thread id)
           (multiple-value-bind (retval success)
               (swank-rex form swank-connection
                          :package package
                          :thread thread)
             (write-message-to-client
               `(:return (,(if success
                               :ok
                               :abort)
                           ,retval)
                 ,id)
               client)))
          ((t &rest args)
           (declare (ignore args))
           (bt:with-lock-held ((client-lock client))
             (swank-send request swank-connection))))))))

(defmethod receive-event ((server vlime) event)
  (let ((clients (slot-value server 'clients)))
    (dolist (client clients)
      (bt:interrupt-thread (client-thread client)
                           #'write-message-to-client
                           (event-message event)
                           client))))
