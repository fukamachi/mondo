(defpackage #:mondo/swank/protocol
  (:use #:cl)
  (:shadow #:debug)
  (:import-from #:mondo/swank/connection
                #:connection
                #:connection-package
                #:connection-lock
                #:connection-socket
                #:connection-continuation-counter
                #:connection-rex-continuations)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:find-shortest-nickname
                #:string-to-octets
                #:octets-to-string)
  (:import-from #:swank)
  (:import-from #:alexandria
                #:when-let
                #:ensure-list
                #:destructuring-case
                #:destructuring-ecase)
  (:export #:send-message
           #:receive-message
           #:swank-send-async
           #:swank-send
           #:swank-rex-async
           #:swank-rex

           #:make-dispatch-event-function
           #:event
           #:event-message
           #:debug
           #:debug-activate
           #:debug-return
           #:indentation-update
           #:new-features
           #:ignore-event
           #:repl-action
           #:repl-action-thread
           #:read-string
           #:abort-action
           #:invoke-repl-action

           #:swank-require
           #:swank-create-repl
           #:swank-init-presentations
           #:swank-connection-info
           #:swank-listener-eval
           #:swank-invoke-nth-restart
           #:swank-interrupt
           #:swank-complete
           #:swank-arglist
           #:swank-throw-to-toplevel))
(in-package #:mondo/swank/protocol)

(eval-when (:compile-toplevel :load-toplevel)
  (swank:swank-require '(swank-repl swank-presentations swank-c-p-c)))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octets (&optional length)
  `(simple-array (unsigned-byte 8) (,(or length '*))))

(defun encode-length (n)
  (format nil "~6,'0,X" n))

(defun decode-length (length-string)
  (parse-integer length-string :radix 16))

(define-condition swank-network-error (error) ())

(defvar *io-package*
  (let ((package (make-package :mondo-swank-protocol-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defun write-data-to-socket (data socket)
  (check-type data octets)
  (check-type socket usocket:stream-usocket)
  (let* ((body-length (1+ (length data)))
         (length-payload (string-to-octets (encode-length body-length)))
         (payload (make-array (+ 6 body-length) :element-type 'octet)))
    (replace payload length-payload)
    (replace payload data :start1 6)
    (setf (aref payload (1- (length payload))) (char-code #\Newline))
    (let ((stream (usocket:socket-stream socket)))
      (handler-bind
          ((error
             (lambda (e)
               (declare (ignore e))
               (error 'swank-network-error))))
        (write-sequence payload stream)
        (force-output stream)
        t))))

(defun send-message (sexp connection)
  (check-type connection connection)
  (bt:with-recursive-lock-held ((connection-lock connection))
    (let ((sexp-string (with-standard-io-syntax
                         (let ((*package* *io-package*)
                               (*print-case* :downcase))
                           (prin1-to-string sexp)))))
      (log :debug "Sending: ~A" sexp-string)
      (write-data-to-socket (string-to-octets sexp-string)
                            (connection-socket connection)))))

(defun read-n-bytes-from-socket (socket n-bytes)
  (let ((stream (usocket:socket-stream socket))
        (data (make-array n-bytes :element-type 'octet))
        (read-bytes 0))
    (loop
      (let ((bytes (handler-case
                       (read-sequence data stream :start read-bytes)
                     (error ()
                       (return)))))
        (incf read-bytes bytes)
        (when (= read-bytes n-bytes)
          (return data))
        (usocket:wait-for-input socket)))))

(defun read-message-length (socket)
  (when-let (length-data (read-n-bytes-from-socket socket 6))
    (decode-length (octets-to-string length-data))))

(defun read-data-from-socket (socket)
  (check-type socket usocket:stream-usocket)
  (when-let (body-length (read-message-length socket))
    (read-n-bytes-from-socket socket body-length)))

(defun receive-message (connection)
  (when-let (data (read-data-from-socket (connection-socket connection)))
    (let ((message-string (octets-to-string data)))
      (with-standard-io-syntax
        (let ((*package* *io-package*)
              (*print-case* :downcase))
          (handler-bind ((error
                           (lambda (e)
                             (when-let ((restart (find-restart 'unintern e)))
                               (invoke-restart restart))
                             (log :error "Failed to read a string message: ~S~%  ~A" message-string e))))
            (let ((message (read-from-string message-string)))
              (log :debug "Received: ~S" message)
              message)))))))

(defun swank-send (message connection)
  (bt:with-recursive-lock-held ((connection-lock connection))
    (send-message message connection)))

(defun swank-rex-async (form connection &key (package "COMMON-LISP-USER") continuation (thread 't))
  (let ((message
          (bt:with-recursive-lock-held ((connection-lock connection))
            (let ((id (incf (connection-continuation-counter connection))))
              (when continuation
                (push
                  (cons id continuation)
                  (connection-rex-continuations connection)))
              `(:emacs-rex ,form ,package ,thread ,id)))))
    (swank-send message connection)))

(defun swank-rex (form connection &rest args &key package thread)
  (declare (ignore package thread))
  (let ((condvar (bt:make-condition-variable))
        (condlock (bt:make-lock))
        raw-message
        result-ready)
    (apply #'swank-rex-async form connection
           :continuation
           (lambda (message)
             (bt:with-lock-held (condlock)
               (setf raw-message message
                     result-ready t)
               (bt:condition-notify condvar)))
           args)
    (bt:with-lock-held (condlock)
      (loop until result-ready
            do (bt:condition-wait condvar condlock)))
    (destructuring-ecase raw-message
      ((:ok value)
       (values value t raw-message))
      ((:abort condition)
       (values condition nil raw-message)))))

(define-condition event ()
  ((message :initarg :message
            :reader event-message)))

(define-condition debug (event)
  ((thread :initarg :thread
           :reader debug-thread)
   (level :initarg :level
          :reader debug-level)
   (condition :initarg :condition
              :reader debug-condition)
   (restarts :initarg :restarts
             :reader debug-restarts)
   (frames :initarg :frames
           :reader debug-frames)
   (continuations :initarg :continuations
                  :reader debug-continuations)))

(define-condition debug-activate (event)
  ((thread :initarg :thread
           :reader debug-activate-thread)
   (level :initarg :level
          :reader debug-activate-level)
   (select :initarg :select
           :reader debug-activate-select)))

(define-condition debug-return (event)
  ((thread :initarg :thread
           :reader debug-return-thread)
   (level :initarg :level
          :reader debug-return-level)
   (continuations :initarg :continuations
                  :reader debug-return-continuations)))

(define-condition new-features (event)
  ((features :initarg :features
             :reader new-features-features)))

(define-condition indentation-update (event)
  ((info :initarg :info
         :reader indentation-update-info)))

(defun invoke-event (event)
  (restart-case
      (error event)
    (ignore-event () nil)))

(defun ignore-event (event)
  (let ((restart (find-restart 'ignore-event event)))
    (when restart
      (invoke-restart restart))))

(define-condition repl-action ()
  ((function :initarg :function
             :reader repl-action-function)
   (thread :initarg :thread
           :reader repl-action-thread)
   (message :initarg :message
            :reader repl-action-message)))

(define-condition read-string (repl-action)
  ((tag :initarg :tag
        :reader read-string-tag)))

(define-condition abort-action (repl-action)
  ((tag :initarg :tag
        :reader abort-action-tag))
  (:default-initargs
   :function (lambda ())))

(defun invoke-repl-action (action)
  (funcall (repl-action-function action)))

(defun invoke-event-in-main-thread (event main-thread)
  (bt:interrupt-thread main-thread #'invoke-event event))

(defun make-dispatch-event-function (connection &optional main-thread)
  (let ((main-thread (or main-thread (bt:current-thread))))
    (lambda (event)
      (destructuring-case event
        ((:return value id)
         (bt:with-recursive-lock-held ((connection-lock connection))
           (when-let (rex-cont (assoc id (connection-rex-continuations connection)))
             (setf (connection-rex-continuations connection)
                   (remove rex-cont (connection-rex-continuations connection)))
             (funcall (cdr rex-cont) value))))
        ((:write-string output &optional target)
         (declare (ignore target))
         (write-string output)
         (force-output))
        ((:read-string thread tag)
         (let ((function (lambda ()
                           (multiple-value-bind (string missing-new-line)
                               (loop
                                 (handler-case
                                     (return (read-line))
                                   (end-of-file ())))
                             (swank-send `(:emacs-return-string ,thread ,tag
                                           ,(if missing-new-line
                                                string
                                                (format nil "~A~%" string)))
                                         connection)))))
           (invoke-event-in-main-thread (make-condition 'read-string
                                                        :function function
                                                        :thread thread
                                                        :tag tag
                                                        :message event)
                                        main-thread)))
        ((:read-aborted thread tag)
         (invoke-event-in-main-thread (make-condition 'abort-action
                                                      :thread thread
                                                      :tag tag
                                                      :message event)
                                      main-thread))
        ((:new-package package-name &rest nicknames)
         (setf (connection-package connection)
               (find-shortest-nickname (cons package-name nicknames))))
        ((:new-features features)
         (invoke-event-in-main-thread (make-condition 'new-features
                                                      :features features
                                                      :message event)
                                      main-thread))
        ((:indentation-update info)
         (invoke-event-in-main-thread (make-condition 'indentation-update
                                                      :info info
                                                      :message event)
                                      main-thread))
        ((:presentation-start id &optional target)
         (declare (ignore id target)))
        ((:presentation-end id &optional target)
         (declare (ignore id target)))
        ((:debug thread level condition restarts frames continuations)
         (invoke-event-in-main-thread (make-condition 'debug
                                                      :thread thread
                                                      :level level
                                                      :condition condition
                                                      :restarts restarts
                                                      :frames frames
                                                      :continuations continuations
                                                      :message event)
                                      main-thread))
        ((:debug-activate thread level &optional select)
         (invoke-event-in-main-thread (make-condition 'debug-activate
                                                      :thread thread
                                                      :level level
                                                      :select select
                                                      :message event)
                                      main-thread))
        ((:debug-return thread level continuations)
         (invoke-event-in-main-thread (make-condition 'debug-return
                                                      :thread thread
                                                      :level level
                                                      :continuations continuations
                                                      :message event)
                                      main-thread))
        ((:ping thread tag)
         (swank-send `(:emacs-pong ,thread ,tag) connection))
        ((t &rest rest)
         (declare (ignore rest))
         (log :error "Unknown event received: ~S" event))))))

(defun swank-require (modules connection)
  (swank-rex `(swank:swank-require ',(loop for module in (ensure-list modules)
                                           collect (intern (symbol-name module) *io-package*)))
             connection))

(defun swank-create-repl (connection)
  (swank-rex `(swank-repl:create-repl nil :coding-system "utf-8-unix")
             connection))

(defun swank-init-presentations (connection)
  (swank-rex '(swank:init-presentations) connection))

(defun swank-connection-info (connection)
  (swank-rex '(swank:connection-info) connection))

(defun swank-listener-eval (input connection &rest args &key continuation thread)
  (declare (ignore thread))
  (apply (if continuation
             #'swank-rex-async
             #'swank-rex)
         `(swank-repl:listener-eval ,input)
         connection
         :package (connection-package connection)
         args))

(defun swank-invoke-nth-restart (thread level restart-num connection)
  (swank-rex-async `(swank:invoke-nth-restart-for-emacs ,level ,restart-num)
                   connection
                   :thread thread))

(defun swank-interrupt (connection &optional (thread :repl-thread))
  (#+sbcl sb-sys:without-interrupts
   #-sbcl progn
   (send-message `(:emacs-interrupt ,thread)
                 connection)))

(defun swank-complete (prefix connection &optional (package-name (connection-package connection)))
  (swank-rex `(swank:completions ,prefix ',package-name)
             connection))

(defun swank-arglist (symbol-name connection &optional (package-name (connection-package connection)))
  (swank-rex `(swank:operator-arglist ,symbol-name ',package-name)
             connection))

(defun swank-throw-to-toplevel (thread connection)
  (swank-rex '(swank:throw-to-toplevel) connection
             :thread thread))
