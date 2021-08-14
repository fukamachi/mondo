(defpackage #:mondo/repl
  (:use #:cl)
  (:import-from #:mondo/readline
                #:print-prompt
                #:read-input
                #:*line-buffer*
                #:defkeymap
                #:use-keymap
                #:default)
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
                #:swank-connection-info
                #:swank-listener-eval
                #:swank-complete
                #:swank-arglist
                #:swank-interrupt
                #:debug-activate
                #:debug-return
                #:ignore-event)
  (:import-from #:mondo/debugger
                #:process-debugger-mode)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:bordeaux-threads)
  (:export #:run-repl))
(in-package #:mondo/repl)

(defvar *connection*)

(defun initialize-swank-repl (connection)
  (log :debug "Initializing Swank REPL")
  (swank-require '(swank-repl) connection)
  (swank-create-repl connection)
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

(defun run-repl (&key lisp source-registry host port)
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
                                       (print-prompt rl:+prompt+)
                                       (rl:on-new-line t))))))

  (let* ((server (if (or host port)
                     (make-swank-server :host (or host "127.0.0.1")
                                        :port (or port 4005))
                     (create-swank-server :lisp lisp
                                          :source-registry source-registry
                                          :port port)))
         (*connection* (connect-to-swank-server server)))
    (start-processing *connection*)
    (initialize-swank-repl *connection*)

    (loop
      (fresh-line)
      (handler-case
          (handler-bind ((error #'uiop:print-condition-backtrace)
                         (debug-return #'ignore-event))
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
                                            :ok (lambda (retval)
                                                  (bt:with-lock-held (condlock)
                                                    (setf success t
                                                          result retval
                                                          result-ready t)
                                                    (bt:condition-notify condvar)))
                                            :abort (lambda (condition)
                                                     (bt:with-lock-held (condlock)
                                                       (setf result condition
                                                             result-ready t)
                                                       (bt:condition-notify condvar))))
                       (loop
                         (handler-case
                             (progn
                               (bt:with-lock-held (condlock)
                                 (loop until result-ready
                                       do (bt:condition-wait condvar condlock)))
                               (return))
                           (debug-activate (event)
                             (process-debugger-mode *connection* event))))
                       (unless success
                         (format t "~&;; Aborted on ~A~%" result)))
                   #+sbcl
                   (sb-sys:interactive-interrupt ()
                     (swank-interrupt *connection*))))
                (t
                 (format t "~&Bye.~%")
                 (return)))))
        (error ())
        (debug-activate (event)
          (process-debugger-mode *connection* event))))))
