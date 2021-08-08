(defpackage #:mondo/process
  (:use #:cl)
  (:import-from #:mondo/swank/client
                #:connection-prompt
                #:response
                #:notify-message)
  (:import-from #:mondo/swank/protocol
                #:swank-send)
  (:import-from #:mondo/debugger
                #:invoke-mondo-debugger
                #:mondo-debugger)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:find-shortest-nickname)
  (:import-from #:swank-protocol
                #:connection-package)
  (:import-from #:bordeaux-threads)
  (:export #:make-process-message-function))
(in-package #:mondo/process)

(defun make-process-message-function (connection &key main-thread)
  (let ((main-thread (or main-thread (bt:current-thread)))
        (debug-info (make-hash-table :test 'eql)))
    (lambda (message)
      (check-type message cons)
      (destructuring-bind (type &rest body)
          message
        (case type
          (:write-string
           (destructuring-bind (output &optional target)
               body
             (when (eq target :repl-result)
               (fresh-line))
             (write-string output)
             (force-output)))
          (:new-package
           (setf (connection-package connection) (first body)
                 (connection-prompt connection) (find-shortest-nickname body)))
          ((:new-features :indentation-update))
          (:return
           (destructuring-bind (value call-id)
               body
             (setf (response connection call-id) value)
             (notify-message connection)))
          (:debug
           (destructuring-bind (tid level condition restarts frames continuations)
               body
             (declare (ignore tid continuations))
             (setf (gethash level debug-info)
                   (make-condition 'mondo-debugger
                                   :level level
                                   :condition condition
                                   :restarts restarts
                                   :frames frames))))
          (:debug-activate
           (destructuring-bind (tid level &optional select)
               body
             (declare (ignore tid select))
             (bt:interrupt-thread main-thread #'invoke-mondo-debugger connection (gethash level debug-info))))
          (:debug-return
           (destructuring-bind (tid level continuations)
               body
             (declare (ignore tid continuations))
             (remhash level debug-info)))
          (:ping
           (destructuring-bind (thread tag)
               body
             (swank-send connection `(:emacs-pong ,thread ,tag))))
          (otherwise
            (log :error "Unknown message: ~S" message)))))))
