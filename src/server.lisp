(defpackage #:mondo/server
  (:use #:cl)
  (:import-from #:mondo/swank
                #:event)
  (:import-from #:mondo/utils
                #:random-port)
  (:export #:server
           #:server-host
           #:server-port
           #:server-swank-connection
           #:make-server
           #:start-server
           #:stop-server
           #:server-not-found
           #:start-mondo-server))
(in-package #:mondo/server)

(defclass server ()
  ((host :initarg :host
         :initform #(127 0 0 1)
         :accessor server-host)
   (port :initarg :port
         :initform (random-port)
         :accessor server-port)
   (swank-connection :initarg :swank-connection
                     :accessor server-swank-connection)))

(defun make-server (class-name &rest initargs)
  (apply #'make-instance class-name initargs))

(defgeneric start-server (object))

(defmethod start-server :after (object)
  (format t "~&Server created: (~A ~A)~%"
          (server-host object)
          (server-port object)))

(defgeneric stop-server (object))

(define-condition server-not-found (error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Server '~A' not found" (slot-value condition 'name)))))

(defun load-server-package (server-name)
  (let ((package-name
          (intern (string-upcase (format nil "mondo/server/~A/server" server-name))
                  :keyword)))
    (unless (find-package package-name)
      (handler-case
          (let ((*standard-output* (make-broadcast-stream))
                (*error-output* (make-broadcast-stream)))
            #+quicklisp
            (ql:quickload package-name :silent t)
            #-quicklisp
            (asdf:load-system package-name))
        ((or #+quicklisp ql:system-not-found
             asdf:missing-component) ()
          (error 'server-not-found
                 :name server-name))))

    (unless (find-package package-name)
      (error 'server-not-found :name server-name))

    (let ((class-name (intern (string-upcase server-name) package-name)))
      (unless (find-class class-name nil)
        (error 'server-not-found :name server-name))
      class-name)))

(defun start-mondo-server (server-name &rest initargs)
  (let* ((class-name (load-server-package server-name))
         (server
           (apply #'make-server class-name initargs)))
    (start-server server)))
