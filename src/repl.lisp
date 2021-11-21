(defpackage #:mondo/repl
  (:use #:cl)
  (:import-from #:mondo/readline
                #:read-input
                #:*line-buffer*
                #:use-keymap
                #:default
                #:load-history
                #:dump-history)
  (:import-from #:mondo/sexp/parse
                #:function-at-point)
  (:import-from #:mondo/swank
                #:connection-package-name
                #:connection-package-nicknames
                #:connection-host
                #:connection-port
                #:swank-require
                #:swank-create-repl
                #:swank-init-presentations
                #:swank-connection-info
                #:swank-listener-eval
                #:swank-complete
                #:swank-arglist
                #:swank-interrupt
                #:repl-action
                #:repl-action-thread
                #:invoke-repl-action
                #:abort-action)
  (:import-from #:mondo/debugger
                #:with-debugger)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:mondo/utils
                #:find-shortest-nickname
                #:add-exit-hook)
  (:import-from #:bordeaux-threads)
  (:import-from #:alexandria
                #:destructuring-ecase)
  (:export #:run-repl))
(in-package #:mondo/repl)

(defvar *connection*)

(defun initialize-swank-repl (connection)
  (log :debug "Initializing Swank REPL")
  (swank-require '(swank-repl swank-presentations swank-c-p-c) connection)
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
          (when (stringp name)
            (setf (connection-package-name connection) name))
          (when (stringp prompt)
            (setf (connection-package-nicknames connection) (list prompt))))))))

(defun prompt-string ()
  (format nil "~A> "
          (find-shortest-nickname
            (cons (connection-package-name *connection*)
                  (connection-package-nicknames *connection*)))))

(defun symbol-complete (text start end)
  (declare (ignore start end))
  (let ((func (function-at-point rl:*line-buffer* rl:*point*)))
    (unless (and (equal text "")
                 (null func))
      (if (and func
               (or (string= text "")
                   (not (equal text func))))
          (multiple-value-bind (result success)
              (swank-arglist func *connection*)
            (if success
                (and result
                     (list "" (color-text :gray result)))
                (progn
                  (log :error "Displaying arglist is failed")
                  nil)))
          (multiple-value-bind (result success)
              (swank-complete (or func text) *connection*)
            (if success
                (destructuring-bind (candidates &optional common)
                    result
                  (when candidates
                    (cons common candidates)))
                (progn
                  (log :error "Completion failed")
                  nil)))))))

(defvar *previous-completions* nil)

(defun handle-lsmatches (completions count longest-length)
  (unless (equalp (rest completions) *previous-completions*)
    (let* ((completions (rest completions))
           (column-size (nth-value 1 (rl:get-screen-size)))
           (item-size (+ 2 longest-length))
           (column-item-count (max 1 (floor column-size item-size)))
           (row-count (ceiling count column-item-count)))
      (when completions
        (setf *previous-completions* completions)
        (format t "~%")
        (loop for row below row-count
              do (loop for column below column-item-count
                       for item = (pop completions)
                       while item
                       do (format t "~vA" item-size item))
              (format t "~%"))
        (fresh-line)
        (rl:on-new-line nil)))))

(defun initialize-readline ()
  (use-keymap 'default)
  (rl:register-function :complete #'symbol-complete)
  (rl:register-hook :lsmatches #'handle-lsmatches))

(defmacro with-handling-mondo-errors (&body body)
  `(handler-case
       (handler-bind ((error #'uiop:print-condition-backtrace))
         ,@body)
     (error ())))

(defmacro with-debugger* ((connection &key is-enabled) &body body)
  `(flet ((main () ,@body))
     (if ,is-enabled
         (with-debugger (,connection)
           (main))
         (main))))

(defun run-repl (directory &key connection use-debugger)
  (initialize-readline)

  (let* ((*connection* connection))

    (add-exit-hook (lambda () (dump-history :directory directory)))
    (load-history :directory directory)

    (initialize-swank-repl *connection*)

    (when directory
      (format t "~&Project root: ~A~%" directory))

    (loop
      (fresh-line)
      (with-handling-mondo-errors
        (with-debugger* (*connection* :is-enabled use-debugger)
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
                       (handler-case
                           (with-debugger* (*connection* :is-enabled use-debugger)
                             (bt:with-lock-held (condlock)
                               (loop until result-ready
                                     do (bt:condition-wait condvar condlock)))
                             (return))
                         (repl-action (e)
                           (handler-case
                               (invoke-repl-action e)
                             (abort-action ())
                             #+sbcl
                             (sb-sys:interactive-interrupt ()
                               (swank-interrupt *connection* (repl-action-thread e)))))))
                     (unless success
                       (format t "~&;; Aborted on ~A~%" result)))
                 #+sbcl
                 (sb-sys:interactive-interrupt ()
                   (swank-interrupt *connection*))))
              (t
               (format t "~&Bye.~%")
               (return)))))))))
