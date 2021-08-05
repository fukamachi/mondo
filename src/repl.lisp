(defpackage #:mondo/repl
  (:use #:cl
        #:mondo/utils)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/readline
                #:readline
                #:*line-buffer*
                #:insert-text
                #:add-history
                #:bind-key
                #:complete)
  (:import-from #:mondo/debugger
                #:in-debugger-p
                #:*debugger-level*)
  (:import-from #:mondo/sexp
                #:input-complete-p)
  (:import-from #:mondo/swank
                #:create-swank-server
                #:connect-to-swank-server
                #:initialize-swank-repl
                #:swank-eval
                #:invoke-debugger-restart
                #:process-messages
                #:debugger
                #:debugger-restarts
                #:debugger-level)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:swank-protocol
                #:connection-package
                #:read-all-messages)
  (:export #:run-repl))
(in-package #:mondo/repl)

(defvar *connection*)

(defun prompt-string ()
  (format nil "~:[~A>~;debugger [~:*~A]~] "
          (and (in-debugger-p)
               *debugger-level*)
          (connection-package *connection*)))

(defvar *line-continue-p* nil)

(defun read-input ()
  (setf *line-continue-p* nil)
  (let* ((prompt-string (prompt-string))
         (input (readline :prompt prompt-string)))
    (when input
      (add-history input)
      input)))

;; TODO
(defun complete-or-indent (&rest args)
  (declare (ignore args))
  (if *line-continue-p*
      (insert-text "     ")
      (complete))
  (values))

(defun newline-or-continue (&rest args)
  (declare (ignore args))
  (if (input-complete-p rl:*line-buffer*)
      (setf rl:*done* 1)
      (progn
        (setf *line-continue-p* t)
        (insert-text (format nil "~%")))))

(defun run-debugger-mode (connection initial-error)
  (let ((current-error initial-error))
    (labels ((request-restart (restart-num)
               (invoke-debugger-restart connection
                                        (debugger-level current-error)
                                        restart-num)
               (format t "~&~A~%" (color-text :red (process-messages connection)))
               (when (in-debugger-p)
                 (process-messages connection)))
             (request-restart-by-name (restart-name)
               (let ((num (position restart-name
                                    (debugger-restarts current-error)
                                    :key #'first
                                    :test #'string=)))
                 (if num
                     (request-restart num)
                     (log :warn "No restarts named '~A'" restart-name))))
             (request-abort ()
               (request-restart-by-name (if (eql *debugger-level* 1)
                                            "*ABORT"
                                            "ABORT"))))
      (loop
        (unless (in-debugger-p)
          (return))
        (handler-case
            (let ((input (read-input)))
              (if input
                  (if (starts-with "restart" input)
                      (request-restart (parse-integer (subseq input 7)))
                      (swank-eval *connection* input))
                  (request-abort)))
          (debugger (e)
            (setf current-error e)
            ;; Ignore redundant :debug & :debug-activate messages
            (read-all-messages connection)))))))

(defun run-repl ()
  (bind-key "\\C-i" #'complete-or-indent)
  (bind-key "\\C-m" #'newline-or-continue)

  (let* ((server (create-swank-server))
         (*connection* (connect-to-swank-server server)))
    (initialize-swank-repl *connection*)

    (loop
      (fresh-line)
      (let ((input (read-input)))
        (cond
          (input
           (fresh-line)
           (handler-case (swank-eval *connection* input)
             (debugger (e)
               (run-debugger-mode *connection* e))))
          (t
           (format t "~&Bye.~%")
           (return)))))))
