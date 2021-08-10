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
                #:connection-prompt
                #:create-swank-server
                #:make-swank-server
                #:connect-to-swank-server
                #:start-processing
                #:wait-for-response-of-call
                #:response
                #:swank-rex
                #:swank-eval
                #:swank-complete
                #:swank-arglist
                #:swank-interrupt)
  (:import-from #:mondo/process
                #:make-process-message-function)
  (:import-from #:mondo/debugger
                #:mondo-debugger
                #:process-debugger-mode)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:swank-protocol
                #:connection-package
                #:connection-hostname
                #:connection-port
                #:connection-request-count
                #:connection-thread
                #:request-swank-require
                #:request-create-repl)
  (:export #:run-repl))
(in-package #:mondo/repl)

(defvar *connection*)

(defun initialize-swank-repl (connection)
  (unless (eql (connection-thread connection) 1)
    (log :debug "Initializing Swank REPL")
    (let ((call-id (swank-rex connection '(swank:connection-info))))
      (request-swank-require connection '(swank-repl))
      (wait-for-response-of-call connection (connection-request-count connection))
      (request-create-repl connection)
      (destructuring-bind (&optional status connection-info)
          (response connection call-id)
        (when (eq status :ok)
          (destructuring-bind (&key pid lisp-implementation package &allow-other-keys)
              connection-info
            (format t "~&~A ~A running at ~A:~A (pid=~A)~%"
                    (getf lisp-implementation :type)
                    (getf lisp-implementation :version)
                    (connection-hostname connection)
                    (connection-port connection)
                    pid)
            (destructuring-bind (&key name prompt)
                package
              (setf (connection-package connection) name
                    (connection-prompt connection) prompt))))))))

(defun prompt-string ()
  (format nil "~A> "
          (connection-prompt *connection*)))

(defun symbol-complete (text start end)
  (declare (ignore start end))
  (let ((func (function-at-point rl:*line-buffer* rl:*point*)))
    (if (or (string= text "")
            (not (equal text func)))
        ;; TODO: Allow this to run asynchronously
        (progn
          (swank-arglist *connection* func)
          (let ((call-id (connection-request-count *connection*)))
            (wait-for-response-of-call *connection* call-id)
            (let ((result (response *connection* call-id)))
              (case (first result)
                (:ok (list "" (color-text :gray (second result))))
                (otherwise
                 (log :error "Displaying arglist is failed")
                 nil)))))
        ;; TODO: Allow this to run asynchronously
        (progn
          (swank-complete *connection* func)
          (let ((call-id (connection-request-count *connection*)))
            (wait-for-response-of-call *connection* call-id)
            (let ((result (response *connection* call-id)))
              (case (first result)
                (:ok (cons text (first (second result))))
                (otherwise
                 (log :error "Completion failed")
                 nil))))))))

(defvar *previous-completions* nil)

(defun run-repl (&key lisp host port)
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
                     (create-swank-server :lisp lisp :port port)))
         (*connection* (connect-to-swank-server server)))
    (start-processing *connection* (make-process-message-function *connection*))
    (initialize-swank-repl *connection*)

    (loop
      (fresh-line)
      (handler-case
          (let ((input (read-input (prompt-string))))
            (setf *previous-completions* nil)
            (cond
              (input
               (fresh-line)
               (handler-case (let ((call-id (swank-eval *connection* input)))
                               (loop
                                 (handler-case
                                     (progn
                                       (wait-for-response-of-call *connection* call-id)
                                       (destructuring-bind (status value)
                                           (response *connection* call-id)
                                         (when (eq status :abort)
                                           (format t "~&;; Aborted on ~A~%" value)))
                                       (return))
                                   (mondo-debugger (debugger)
                                     (process-debugger-mode *connection* debugger)))))
                 #+sbcl
                 (sb-sys:interactive-interrupt ()
                   (swank-interrupt *connection*))))
              (t
               (format t "~&Bye.~%")
               (return))))
        (mondo-debugger (debugger)
          (process-debugger-mode *connection* debugger))))))
