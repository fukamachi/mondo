(defpackage #:mondo/server/vlime/server
  (:use #:cl
        #:mondo/server)
  (:import-from #:mondo/server/vlime/protocol
                #:read-message
                #:write-message)
  (:import-from #:mondo/swank
                #:swank-send
                #:swank-rex)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:octets-to-string)
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
   (client-threads :initform nil)))

(defmethod start-server ((server vlime))
  (let ((host (server-host server))
        (port (server-port server)))
    (let ((server-socket
            (usocket:socket-listen host port
                                   :reuse-address t
                                   :backlog 128
                                   :element-type '(unsigned-byte 8))))
      (setf (vlime-server-socket server) server-socket)))

  (with-slots (lock main-thread) server
    (bt:with-lock-held (lock)
      (setf main-thread
            (bt:make-thread
              (lambda ()
                (main-thread-loop server))
              :initial-bindings
              `((*standard-output* . ,*standard-output*)
                (*error-output* . ,*error-output*))
              :name "vlime server main thread"))))

  (values))

(defun main-thread-loop (server)
  (let ((server-socket (vlime-server-socket server)))
    (loop
      (let ((client-socket (usocket:socket-accept server-socket)))
        (log :info "New client: ~A" client-socket)
        (bt:with-lock-held ((vlime-server-lock server))
          (push
            (bt:make-thread
              (lambda ()
                (client-thread-loop client-socket (server-swank-connection server)))
              :initial-bindings
              `((*standard-output* . ,*standard-output*)
                (*error-output* . ,*error-output*))
              :name "vlime client loop thread")
            (slot-value server 'client-threads)))))))

(defmethod stop-server ((server vlime))
  (with-slots (lock main-thread client-threads socket) server
    (when socket
      (bt:with-lock-held (lock)
        (mapc #'bt:destroy-thread
              (cons main-thread client-threads))
        (when (bt:thread-alive-p main-thread)
          (ignore-errors (bt:join-thread main-thread)))
        (usocket:socket-close socket)
        (setf socket nil
              main-thread nil
              client-threads nil)))))

(defun client-thread-loop (client-socket swank-connection)
  (let ((stream (usocket:socket-stream client-socket)))
    (loop
      (let ((request (read-message stream)))
        (destructuring-case request
          ((:emacs-rex form package thread id)
           (multiple-value-bind (retval success)
               (swank-rex form swank-connection
                          :package package
                          :thread thread)
             (write-message
               `(:return (,(if success
                               :ok
                               :abort)
                           ,retval)
                 ,id)
               (usocket:socket-stream client-socket))))
          ((t &rest args)
           (declare (ignore args))
           (swank-send request swank-connection)))))))
