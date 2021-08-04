(defpackage #:mondo/repl
  (:use #:cl
        #:mondo/utils)
  (:import-from #:mondo/readline
                #:readline
                #:*line-buffer*
                #:insert-text
                #:add-history
                #:bind-key
                #:complete)
  (:import-from #:mondo/sexp
                #:input-complete-p)
  (:import-from #:mondo/swank
                #:create-swank-server
                #:connect-to-swank-server
                #:initialize-swank-repl
                #:swank-eval)
  (:import-from #:swank-protocol
                #:connection-package)
  (:export #:run-repl))
(in-package #:mondo/repl)

(defvar *connection*)

(defun prompt-string ()
  (format nil "(mondo) ~A> " (connection-package *connection*)))

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

(defun run-repl ()
  (bind-key "\\C-i" #'complete-or-indent)
  (bind-key "\\C-m" (lambda (&rest args)
                      (declare (ignore args))
                      (if (input-complete-p rl:*line-buffer*)
                          (setf rl:*done* 1)
                          (progn
                            (setf *line-continue-p* t)
                            (insert-text (format nil "~%"))))))

  (let* ((server (create-swank-server))
         (*connection* (connect-to-swank-server server)))
    (initialize-swank-repl *connection*)

    (loop
      (let ((input (read-input)))
        (unless input
          (format t "~&Bye.~%")
          (return))
        (fresh-line)
        (loop
          (handler-case
              (progn
                (swank-eval *connection* input)
                (fresh-line)
                (return))
            (end-of-file ())))))))
