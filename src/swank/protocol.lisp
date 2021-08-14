(defpackage #:mondo/swank/protocol
  (:use #:cl)
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
           #:swank-eval-async
           #:swank-eval

           #:make-dispatch-event-function
           #:event
           #:debug-activate
           #:debug-return
           #:ignore-event

           #:swank-require
           #:swank-create-repl
           #:swank-connection-info
           #:swank-listener-eval
           #:swank-invoke-nth-restart
           #:swank-interrupt
           #:swank-complete
           #:swank-arglist
           #:swank-throw-to-toplevel))
(in-package #:mondo/swank/protocol)

(eval-when (:compile-toplevel :load-toplevel)
  (swank:swank-require '(swank-repl)))

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
      (log :debug "Sending: ~S" sexp-string)
      (write-data-to-socket (string-to-octets sexp-string)
                            (connection-socket connection)))))

(defun read-data-from-socket (socket)
  (check-type socket usocket:stream-usocket)
  (let ((stream (usocket:socket-stream socket))
        (length-data (make-array 6 :element-type 'octet)))
    (read-sequence length-data stream)

    (let* ((body-length (decode-length (octets-to-string length-data)))
           (body-data (make-array body-length :element-type 'octet)))
      (read-sequence body-data stream)
      body-data)))

(defun receive-message (connection)
  (let* ((data (read-data-from-socket (connection-socket connection)))
         (message-string (octets-to-string data)))
    (with-standard-io-syntax
      (let ((*package* *io-package*))
        (let ((message (read-from-string message-string)))
          (log :debug "Received: ~S" message)
          message)))))

(defun swank-eval-async (form connection &key (package "COMMON-LISP-USER") continuation (thread 't))
  (bt:with-recursive-lock-held ((connection-lock connection))
    (let ((id (incf (connection-continuation-counter connection))))
      (when continuation
        (push
          (cons id continuation)
          (connection-rex-continuations connection)))
      (send-message `(:emacs-rex ,form ,package ,thread ,id)
                    connection))))

(defun swank-eval (form connection &rest args &key package thread)
  (declare (ignore package thread))
  (let ((condvar (bt:make-condition-variable))
        (condlock (bt:make-lock))
        success
        result
        result-ready)
    (apply #'swank-eval-async form connection
           :continuation
           (lambda (message)
             (bt:with-lock-held (condlock)
               (destructuring-ecase message
                 ((:ok value)
                  (setf success t
                        result value))
                 ((:abort condition)
                  (setf result condition)))
               (setf result-ready t)
               (bt:condition-notify condvar)))
           args)
    (bt:with-lock-held (condlock)
      (loop until result-ready
            do (bt:condition-wait condvar condlock)))
    (values result success)))

(define-condition event () ())

(define-condition debug-activate (event)
  ((thread :initarg :thread
           :reader debug-activate-thread)
   (level :initarg :level
          :reader debug-activate-level)
   (condition :initarg :condition
              :reader debug-activate-condition)
   (restarts :initarg :restarts
             :reader debug-activate-restarts)
   (frames :initarg :frames
           :reader debug-activate-frames)))

(define-condition debug-return (event)
  ((thread :initarg :thread
           :reader debug-return-thread)
   (level :initarg :level
          :reader debug-return-level)))

(defun invoke-event (event)
  (restart-case
      (error event)
    (ignore-event () nil)))

(defun ignore-event (event)
  (let ((restart (find-restart 'ignore-event event)))
    (when restart
      (invoke-restart restart))))

(defun invoke-event-in-main-thread (event main-thread)
  (bt:interrupt-thread main-thread #'invoke-event event))

(defun make-dispatch-event-function (connection &optional main-thread)
  (let ((main-thread (or main-thread (bt:current-thread)))
        (debug-info (make-hash-table :test 'eql)))
    (lambda (event)
      (destructuring-case event
        ((:return value id)
         (bt:with-recursive-lock-held ((connection-lock connection))
           (when-let (rex-cont (assoc id (connection-rex-continuations connection)))
             (setf (connection-rex-continuations connection)
                   (remove rex-cont (connection-rex-continuations connection)))
             (funcall (cdr rex-cont) value))))
        ((:write-string output &optional target)
         (when (eq target :repl-result)
           (fresh-line))
         (write-string output)
         (force-output))
        ((:new-package package-name &rest nicknames)
         (setf (connection-package connection)
               (find-shortest-nickname (cons package-name nicknames))))
        ((:new-features features)
         (declare (ignore features)))
        ((:indentation-update info)
         (declare (ignore info)))
        ((:debug thread level condition restarts frames continuations)
         (declare (ignore continuations))
         (setf (gethash level debug-info)
               (make-condition 'debug-activate
                               :thread thread
                               :level level
                               :condition condition
                               :restarts restarts
                               :frames frames)))
        ((:debug-activate thread level &optional select)
         (declare (ignore thread select))
         (invoke-event-in-main-thread (gethash level debug-info) main-thread))
        ((:debug-return thread level continuations)
         (declare (ignore continuations))
         (invoke-event-in-main-thread (make-condition 'debug-return
                                                      :thread thread
                                                      :level level)
                                      main-thread))
        ((:ping thread tag)
         (send-message `(:emacs-pong ,thread ,tag) connection))
        ((t &rest rest)
         (declare (ignore rest))
         (log :error "Unknown event received: ~S" event))))))

(defun swank-require (modules connection)
  (swank-eval `(swank:swank-require ',(loop for module in (ensure-list modules)
                                            collect (intern (symbol-name module) *io-package*)))
              connection))

(defun swank-create-repl (connection)
  (swank-eval `(swank-repl:create-repl nil :coding-system "utf-8-unix")
              connection))

(defun swank-connection-info (connection)
  (swank-eval '(swank:connection-info) connection))

(defun swank-listener-eval (input connection &rest args &key continuation thread)
  (declare (ignore thread))
  (apply (if continuation
             #'swank-eval-async
             #'swank-eval)
         `(swank-repl:listener-eval ,input)
         connection
         :package (connection-package connection)
         args))

(defun swank-invoke-nth-restart (thread level restart-num connection)
  (swank-eval-async `(swank:invoke-nth-restart-for-emacs ,level ,restart-num)
                    connection
                    :thread thread))

(defun swank-interrupt (connection)
  (#+sbcl sb-sys:without-interrupts
   #-sbcl progn
   (send-message '(:emacs-interrupt :repl-thread)
                 connection)))

(defun swank-complete (prefix connection &optional (package-name (connection-package connection)))
  (swank-eval `(swank:simple-completions ,prefix ',package-name)
              connection))

(defun swank-arglist (symbol-name connection &optional (package-name (connection-package connection)))
  (swank-eval `(swank:operator-arglist ,symbol-name ',package-name)
              connection))

(defun swank-throw-to-toplevel (thread connection)
  (swank-eval '(swank:throw-to-toplevel) connection
              :thread thread))
