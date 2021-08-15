(defpackage #:mondo/repl
  (:use #:cl)
  (:import-from #:mondo/readline
                #:read-input
                #:*line-buffer*
                #:defkeymap
                #:use-keymap
                #:default
                #:load-history
                #:dump-history)
  (:import-from #:mondo/sexp/parse
                #:function-at-point)
  (:import-from #:mondo/swank
                #:connection-package
                #:connection-host
                #:connection-port
                #:create-swank-server
                #:make-swank-server
                #:connect-to-swank-server
                #:start-processing
                #:swank-require
                #:swank-create-repl
                #:swank-init-presentations
                #:swank-connection-info
                #:swank-listener-eval
                #:swank-complete
                #:swank-arglist
                #:swank-interrupt
                #:event
                #:debug-activate
                #:debug-return
                #:new-features
                #:indentation-update
                #:ignore-event)
  (:shadowing-import-from #:mondo/swank/protocol
                          #:debug)
  (:import-from #:mondo/debugger
                #:with-debugger)
  (:import-from #:mondo/server
                #:start-mondo-server
                #:receive-event)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:bordeaux-threads)
  (:import-from #:alexandria
                #:destructuring-ecase
                #:with-gensyms
                #:once-only)
  (:export #:run-repl))
(in-package #:mondo/repl)

(defvar *connection*)

(defun initialize-swank-repl (connection)
  (log :debug "Initializing Swank REPL")
  (swank-require '(swank-repl swank-presentations) connection)
  (swank-create-repl connection)
  (swank-init-presentations connection)
  (multiple-value-bind (info success)
      (swank-connection-info connection)
    (when success
      (destructuring-bind (&key pid lisp-implementation package &allow-other-keys)
          info
        (format t "~&~A ~A running at ~A:~A (pid=~A)~%"
                (getf lisp-implementation :type)
                (getf lisp-implementation :version)
                (connection-host connection)
                (connection-port connection)
                pid)
        (destructuring-bind (&key name prompt)
            package
          (declare (ignore name))
          (when (stringp prompt)
            (setf (connection-package connection) prompt)))))))

(defun prompt-string ()
  (format nil "~A> "
          (connection-package *connection*)))

(defun symbol-complete (text start end)
  (declare (ignore start end))
  (let ((func (function-at-point rl:*line-buffer* rl:*point*)))
    (if (or (string= text "")
            (not (equal text func)))
        (multiple-value-bind (result success)
            (swank-arglist func *connection*)
          (if success
              (and result
                   (list "" (color-text :gray result)))
              (progn
                (log :error "Displaying arglist is failed")
                nil)))
        (multiple-value-bind (result success)
            (swank-complete func *connection*)
          (if success
              (destructuring-bind (candidates &optional common)
                  result
                (cons common candidates))
              (progn
                (log :error "Completion failed")
                nil))))))

(defvar *previous-completions* nil)

(defun setup-exit-hook (&key directory)
  (let ((hook-fn (lambda () (dump-history :directory directory))))
    (declare (ignorable hook-fn))
    #+allegro
    (push `(funcall ,hook-fn) sys:*exit-cleanup-forms*)
    #+(or sbcl ccl clisp)
    (push hook-fn
          #+sbcl sb-ext:*exit-hooks*
          #+ccl ccl:*lisp-cleanup-functions*
          #+clisp custom:*fini-hooks*)
    (values)))

(defun directory-qlot-directory (directory)
  (let ((dot-qlot-dir
          (merge-pathnames #P".qlot/" directory)))
    (when (uiop:directory-exists-p dot-qlot-dir)
      dot-qlot-dir)))

(defmacro with-forward-events ((server &optional event-names) &body body)
  (with-gensyms (event)
    (once-only (server)
      `(handler-bind ,(loop for name in (or event-names '(event)) collect
                            `(,name
                               (lambda (,event)
                                 (when ,server
                                   (receive-event ,server ,event))
                                 (ignore-event ,event))))
         ,@body))))

(defun run-repl (directory &key lisp source-registry quicklisp host port server)
  (use-keymap 'default)
  (rl:register-function :complete #'symbol-complete)
  (rl:register-hook :lsmatches (lambda (completions count longest-length)
                                 (unless (equalp (rest completions) *previous-completions*)
                                   (let* ((completions (rest completions))
                                          (column-size (nth-value 1 (rl:get-screen-size)))
                                          (item-size (+ 2 longest-length))
                                          (column-item-count (max 1 (floor column-size item-size)))
                                          (row-count (ceiling count column-item-count)))
                                     (when completions
                                       (setf *previous-completions* completions)
                                       (fresh-line)
                                       (loop for row below row-count
                                             do (loop for column below column-item-count
                                                      for item = (pop completions)
                                                      while item
                                                      do (format t "~vA" item-size item))
                                             (format t "~%"))
                                       (fresh-line)
                                       (rl:on-new-line nil))))))

  (let* ((directory (and directory
                         (or (uiop:directory-exists-p directory)
                             (error "Directory not exist: ~A" directory))))
         (swank-server (if (or host port)
                           (make-swank-server :host (or host "127.0.0.1")
                                              :port (or port 4005))
                           (create-swank-server :lisp lisp
                                                :source-registry (or source-registry
                                                                     (uiop:native-namestring directory))
                                                :quicklisp (or quicklisp
                                                               (and directory
                                                                    (directory-qlot-directory directory)))
                                                :port port)))
         (*connection* (connect-to-swank-server swank-server))
         (mondo-server (when server
                         (start-mondo-server server
                                             :swank-connection *connection*))))

    (setup-exit-hook :directory directory)
    (load-history :directory directory)

    (start-processing *connection*)
    (with-forward-events (mondo-server)
      (initialize-swank-repl *connection*))

    (loop
      (fresh-line)
      (handler-case
          (with-debugger (*connection*)
            (handler-bind ((error #'uiop:print-condition-backtrace)
                           (debug-return #'ignore-event))
              (with-forward-events (mondo-server (debug debug-activate debug-return
                                                        new-features indentation-update))
                (let ((input (read-input (prompt-string))))
                  (setf *previous-completions* nil)
                  (cond
                    (input
                     (fresh-line)
                     (handler-case
                         (let ((condvar (bt:make-condition-variable))
                               (condlock (bt:make-lock))
                               success
                               result
                               result-ready)
                           (swank-listener-eval input *connection*
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
                                                    (bt:condition-notify condvar))))
                           (loop
                             ;; Should be only when some clients are connected?
                             (if server
                                 #1=(progn
                                      (bt:with-lock-held (condlock)
                                        (loop until result-ready
                                              do (bt:condition-wait condvar condlock)))
                                      (return))
                                 (with-debugger (*connection*)
                                   #1#)))
                           (unless success
                             (format t "~&;; Aborted on ~A~%" result)))
                       #+sbcl
                       (sb-sys:interactive-interrupt ()
                         (swank-interrupt *connection*))))
                    (t
                     (format t "~&Bye.~%")
                     (return)))))))
        (error ())))))
