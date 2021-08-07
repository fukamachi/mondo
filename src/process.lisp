(defpackage #:mondo/process
  (:use #:cl)
  (:import-from #:mondo/swank/client
                #:response
                #:debug-info
                #:remove-debug-info
                #:notify-message
                #:connection-debug-activated)
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
  (:export #:process-message))
(in-package #:mondo/process)

(defun process-message (main-thread connection message)
  (check-type message cons)
  (destructuring-bind (type &rest body)
      message
    (case type
      (:write-string
       (destructuring-bind (output &optional target)
           body
         (when (eq target :repl-result)
           (fresh-line))
         (write-string output)))
      (:new-package
       (setf (connection-package connection)
             (find-shortest-nickname body)))
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
         (setf (debug-info connection level)
               (make-condition 'mondo-debugger
                               :level level
                               :condition condition
                               :restarts restarts
                               :frames frames))))
      (:debug-activate
       (destructuring-bind (tid level &optional select)
           body
         (declare (ignore tid select))
         (bt:interrupt-thread main-thread #'invoke-mondo-debugger connection (debug-info connection level))
         (setf (connection-debug-activated connection) level)))
      (:debug-return
       (destructuring-bind (tid level continuations)
           body
         (declare (ignore tid continuations))
         (setf (connection-debug-activated connection)
               (if (= level 1)
                   nil
                   (1- level)))
         (remove-debug-info connection level)))
      (:ping
       (destructuring-bind (thread tag)
           body
         (swank-send connection `(:emacs-pong ,thread ,tag))))
      (otherwise
        (log :error "Unknown message: ~S" message)))))
