(defpackage #:mondo/swank/server
  (:use #:cl)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:random-port)
  (:import-from #:usocket)
  (:export #:create-swank-server
           #:swank-server
           #:swank-server-process
           #:swank-server-host
           #:swank-server-port
           #:make-swank-server))
(in-package #:mondo/swank/server)

(defstruct swank-server
  process
  host
  port)

(defun server-running-p (port)
  (handler-case (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                  (usocket:socket-close socket)
                  t)
    (usocket:connection-refused-error () nil)
    (usocket:connection-reset-error () nil)))

(defun normalize-quicklisp-path (path)
  (etypecase path
    (pathname
      (uiop:escape-shell-command
        (uiop:native-namestring (uiop:ensure-directory-pathname path))))
    (string
      (normalize-quicklisp-path (pathname path)))))

(defun create-swank-server (&key lisp source-registry quicklisp port)
  (let ((lisp (or lisp "sbcl-bin"))
        (port (or port (random-port))))
    (log :debug "Starting a swank server on ~A at port=~A" lisp port)
    (let ((process (handler-case
                       (uiop:launch-program
                         (format nil "~@[QUICKLISP_HOME=~A ~]~A"
                                 (and quicklisp
                                      (normalize-quicklisp-path quicklisp))
                                 (uiop:escape-shell-command
                                   `("ros" "-L" ,lisp
                                           ,@(when source-registry
                                               `("-S" ,source-registry))
                                           "-s" "swank"
                                           "-e" ,(format nil "(swank:create-server :port ~D :dont-close t)" port) "run")))
                         :input :stream
                         :output nil
                         :error-output :stream)
                     (error (e)
                       (log :error "Failed to start a swank server: ~A" e)
                       (uiop:quit -1)))))
      (prog1
          (make-swank-server :process process
                             :host "127.0.0.1"
                             :port port)
        (log :debug "Waiting for the server to be ready")
        (loop repeat 300
              do (sleep 0.1)
              when (server-running-p port)
              do (return)
              unless (uiop:process-alive-p process)
              do (log :error "Failed to start a swank server: ~A"
                      (string-right-trim '(#\Newline #\Return)
                                         (uiop:slurp-stream-string (uiop:process-info-error-output process))))
              (uiop:quit -1)
              finally
              (progn
                (log :error "Took too long for the server to start. Timeout.")
                (uiop:quit -1)))))))
