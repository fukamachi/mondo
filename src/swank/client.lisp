(defpackage #:mondo/swank/client
  (:use #:cl)
  (:import-from #:mondo/swank/server
                #:swank-server
                #:swank-server-host
                #:swank-server-port)
  (:shadowing-import-from #:mondo/logger
                          #:log
                          #:*log-level*
                          #:+debug+)
  (:import-from #:swank-protocol
                #:connection
                #:connect
                #:read-message)
  (:import-from #:bordeaux-threads)
  (:export #:connect-to-swank-server
           #:start-processing
           #:response
           #:mondo-connection
           #:connection-prompt
           #:wait-for-response
           #:wait-for-response-of-call
           #:notify-message))
(in-package #:mondo/swank/client)

(defclass mondo-connection (swank-protocol:connection)
  ((prompt :initform nil
           :accessor connection-prompt)
   (process-thread :initform nil
                   :accessor connection-process-thread)
   (response-map :initform (make-hash-table :test 'eql))
   (response-lock :initform (bt:make-lock "mondo response lock"))
   (wait-condvar :initform (bt:make-condition-variable))
   (wait-condlock :initform (bt:make-recursive-lock "mondo response wait lock"))))

(defun wait-for-response (connection &key timeout)
  (with-slots (wait-condvar wait-condlock)
      connection
    (bt:with-recursive-lock-held (wait-condlock)
      (apply #'bt:condition-wait wait-condvar wait-condlock
             (when timeout
               (list :timeout timeout))))))

(defun response (connection call-id)
  (with-slots (response-map response-lock)
      connection
    (bt:with-lock-held (response-lock)
      (multiple-value-prog1 (gethash call-id response-map)
        (remhash call-id response-map)))))

(defun (setf response) (value connection call-id)
  (with-slots (response-map response-lock)
      connection
    (bt:with-lock-held (response-lock)
      (setf (gethash call-id response-map) value))))

(defun wait-for-response-of-call (connection call-id &key timeout)
  (loop
    (with-slots (response-map response-lock)
        connection
      (bt:with-lock-held (response-lock)
        (when (gethash call-id response-map)
          (return))))
    (apply #'wait-for-response connection
           (when timeout
             (list :timeout timeout)))))

(defun notify-message (connection)
  (with-slots (wait-condvar wait-condlock)
      connection
    (bt:with-recursive-lock-held (wait-condlock)
      (bt:condition-notify wait-condvar))))

(defun make-process-thread (connection process-fn)
  (bt:make-thread
    (lambda ()
      (loop
        (let ((message (read-message connection)))
          (when message
            (funcall process-fn message)))))
    :initial-bindings `((*standard-output* . ,*standard-output*)
                        (*error-output* . ,*error-output*))
    :name "mondo swank message processor"))

(defun connect-to-swank-server (swank-server)
  (check-type swank-server swank-server)
  (let* ((host (swank-server-host swank-server))
         (port (swank-server-port swank-server))
         (conn (make-instance 'mondo-connection
                              :hostname host
                              :port port
                              :logp (eql *log-level* +debug+))))
    (log :debug "Connecting to the swank server on ~A:~A" host port)
    (handler-case
        (connect conn)
      (usocket:connection-refused-error ()
        (log :error "Failed to connect to the swank server on ~A:~A" host port)
        (uiop:quit -1)))
    (log :debug "Connected")
    conn))

(defun start-processing (connection process-fn)
  (log :debug "Start processing")
  (setf (connection-process-thread connection)
        (make-process-thread connection process-fn)))
