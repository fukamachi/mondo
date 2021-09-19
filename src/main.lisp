(defpackage #:mondo
  (:nicknames #:mondo/main)
  (:use #:cl)
  (:import-from #:mondo/swank
                #:create-swank-server
                #:make-swank-server
                #:connect-to-swank-server
                #:start-processing
                #:event
                #:ignore-event
                #:indentation-update
                #:indentation-update-info
                #:new-package
                #:new-package-name
                #:new-package-nicknames
                #:connection-package-name
                #:connection-package-nicknames)
  (:import-from #:mondo/repl
                #:run-repl)
  (:import-from #:mondo/server
                #:start-mondo-server
                #:receive-event)
  (:import-from #:mondo/sexp/indent
                #:update-indentation-rules)
  (:import-from #:mondo/logger
                #:with-logging
                #:remove-old-log-files)
  (:import-from #:mondo/utils
                #:add-exit-hook)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:when-let)
  (:export #:main))
(in-package #:mondo)

(defun directory-qlot-directory (directory)
  (let ((dot-qlot-dir
          (merge-pathnames #P".qlot/" directory)))
    (when (uiop:directory-exists-p dot-qlot-dir)
      dot-qlot-dir)))

(defun start-swank-server (&key directory lisp source-registry quicklisp host port)
  (if (or host port)
      (make-swank-server :host (or host "127.0.0.1")
                         :port (or port 4005))
      (create-swank-server :lisp lisp
                           :source-registry (or source-registry
                                                 (and directory
                                                      (uiop:native-namestring directory)))
                           :quicklisp (or quicklisp
                                          (and directory
                                               (directory-qlot-directory directory)))
                           :port port)))

(defmacro with-forward-events ((server &optional (event-names nil event-names-specified)) &body body)
  (with-gensyms (event)
    (once-only (server)
      `(handler-bind ,(loop for name in (if event-names-specified
                                            event-names
                                            '(event)) collect
                            `(,name
                               (lambda (,event)
                                 (when ,server
                                   (receive-event ,server ,event))
                                 (ignore-event ,event))))
         ,@body))))

(defun main (directory &key lisp source-registry quicklisp host port server)
  (with-logging
    (add-exit-hook 'remove-old-log-files)
    (let* ((directory (and directory
                           (or (uiop:directory-exists-p directory)
                               (error "Directory not exist: ~A" directory))))
           (swank-server
             (start-swank-server :directory directory
                                 :lisp lisp
                                 :source-registry source-registry
                                 :quicklisp quicklisp
                                 :host host
                                 :port port))
           (connection (connect-to-swank-server swank-server)))

      (start-processing connection)

      (let ((mondo-server
              (when server
                (start-mondo-server server
                                    :swank-connection connection))))

        (with-forward-events (mondo-server)
          (handler-bind ((indentation-update
                           (lambda (e)
                             (update-indentation-rules (indentation-update-info e))))
                         (new-package
                           (lambda (e)
                             (setf (connection-package-name connection)
                                   (new-package-name e))
                             (setf (connection-package-nicknames connection)
                                   (new-package-nicknames e)))))
            (run-repl directory
                      :connection connection
                      :use-debugger (not mondo-server))))))))
