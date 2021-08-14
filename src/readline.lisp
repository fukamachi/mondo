(defpackage #:mondo/readline
  (:use #:cl)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:mondo/sexp/indent
                #:indent-input)
  (:import-from #:mondo/sexp/parse
                #:input-complete-p
                #:function-at-point)
  (:shadowing-import-from #:mondo/logger
                          #:log)
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

           #:default

           #:load-history
           #:dump-history))
(in-package #:mondo/readline)

(defun data-directory ()
  (let* ((data-home (uiop:getenv-absolute-directory "XDG_DATA_HOME"))
         (data-home
           (if data-home
               (merge-pathnames #P"mondo/" data-home)
               (merge-pathnames #P".mondo/" (user-homedir-pathname)))))
    (ensure-directories-exist data-home)
    data-home))

(defun history-directory ()
  (let ((history-dir (merge-pathnames #P"history/" (data-directory))))
    (ensure-directories-exist history-dir)
    history-dir))

(defun history-file (&key directory tag)
  (make-pathname
    :name (or tag "default")
    :defaults
    (if directory
        (merge-pathnames
          (make-pathname :directory (cons :relative
                                          (rest
                                            (pathname-directory
                                              (uiop:ensure-absolute-pathname directory)))))
          (history-directory))
        (history-directory))))

(defun print-prompt (prompt-string)
  (format t "~&~A" (color-text :green prompt-string))
  (force-output))

(defun readline (&key prompt)
  (when prompt
    (print-prompt prompt))
  (rl:readline :prompt prompt
               :erase-empty-line t
               :already-prompted t))

(defvar *max-history-entries* 1000)
(defvar *history-entries* '())

(defun add-history (input)
  (prog1 (cffi:foreign-funcall "add_history" :string input :void)
    (push input *history-entries*)))

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

(defun read-input (prompt-string &key no-history)
  (let ((input (readline :prompt prompt-string)))
    (when input
      (when (and (not no-history)
                 (not (equal input "")))
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

(defun load-history (&key directory tag)
  (let ((file (history-file :directory directory :tag tag)))
    (when (probe-file file)
      (log :debug "Reading a history list from '~A'" file)
      (mapc #'add-history (uiop:safe-read-file-form file)))
    file))

(defun dump-history (&key directory tag)
  (let ((file (history-file :directory directory :tag tag)))
    (log :debug "Writing history to '~A'" file)
    (ensure-directories-exist file)
    (uiop:with-output-file (out file
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (prin1
        (nreverse
          (subseq *history-entries*
                  0 (min (length *history-entries*) *max-history-entries*)))
        out))
    file))
