(defpackage #:mondo/readline
  (:use #:cl)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:mondo/sexp/indent
                #:indent-input)
  (:import-from #:mondo/sexp/parse
                #:input-complete-p
                #:function-at-point)
  (:import-from #:cl-readline
                #:*line-buffer*
                #:insert-text)
  (:import-from #:cffi)
  (:export #:readline
           #:read-input
           #:print-prompt
           #:add-history
           #:bind-key
           #:defkeymap
           #:use-keymap
           #:with-keymap
           #:complete

           #:*line-buffer*
           #:insert-text
           #:replace-input

           #:default))
(in-package #:mondo/readline)

(defun print-prompt (prompt-string)
  (format t "~&~A" (color-text :green prompt-string))
  (force-output))

(defun readline (&key prompt)
  (when prompt
    (print-prompt prompt))
  (rl:readline :prompt prompt
               :erase-empty-line t
               :already-prompted t))

(defun add-history (input)
  (cffi:foreign-funcall "add_history" :string input :void))

(defun bind-key (key func &optional (keymap (rl:get-keymap)))
  (rl:bind-keyseq key func :keymap keymap))

(defvar *keymap* (make-hash-table :test 'eq))
(defvar *initial-keymap* (rl:get-keymap))

(defmacro defkeymap (name-and-options &rest clauses)
  (let ((keymap (gensym "KEYMAP")))
    (destructuring-bind (name &key base)
        (if (consp name-and-options)
            name-and-options
            (list name-and-options))
      `(let ((,keymap (rl:copy-keymap ,(case base
                                         ('nil '*initial-keymap*)
                                         (:current '(rl:get-keymap))
                                         (otherwise `(or (gethash ',base *keymap*)
                                                         (error "No keymap named '~A'" ',base)))))))
         ,@(loop for (key fn) in clauses
                 collect `(bind-key ,key ,fn ,keymap))
         (setf (gethash ',name *keymap*) ,keymap)))))

(defun use-keymap (name)
  (rl:set-keymap (gethash name *keymap*)))

(defmacro with-keymap ((keymap) &body body)
  (let ((before (gensym "BEFORE")))
    `(let ((,before (rl:get-keymap)))
       (use-keymap ',keymap)
       (unwind-protect
           (progn ,@body)
         (rl:set-keymap ,before)))))

(defun complete ()
  (let ((func (function-at-point rl:*line-buffer* rl:*point*)))
    (unless func
      (return-from complete)))
  (cffi:foreign-funcall "rl_complete" :int 9 :int 0)
  (cffi:foreign-funcall "rl_possible_completions" :int 1 :int 0))

(defun replace-input (new-input)
  (rl:replace-line "" t)
  (rl:insert-text new-input))

(defun read-input (prompt-string)
  (let ((input (readline :prompt prompt-string)))
    (when input
      (unless (equal input "")
        (add-history input))
      input)))

(defun indent ()
  (let ((new-input (indent-input rl:*line-buffer* rl:*point* (length rl:+prompt+))))
    (unless (equal rl:*line-buffer* new-input)
      (replace-input new-input)
      t)))

(defun newline-or-continue (&rest args)
  (declare (ignore args))
  (if (input-complete-p rl:*line-buffer*)
      (setf rl:*done* 1)
      (progn
        (insert-text (format nil "~%"))
        (indent))))

(defun complete-or-indent (&rest args)
  (declare (ignore args))
  (if (find #\Newline rl:*line-buffer*)
      (or (indent) (complete))
      (complete))
  (values))

(defkeymap default
  ("\\C-i" #'complete-or-indent)
  ("\\C-m" #'newline-or-continue)
  ("\\C-j" #'newline-or-continue))
