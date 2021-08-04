(defpackage #:mondo/readline
  (:use #:cl)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:cl-readline
                #:*line-buffer*
                #:insert-text)
  (:import-from #:cffi)
  (:export #:readline
           #:add-history
           #:bind-key
           #:complete

           #:*line-buffer*
           #:insert-text))
(in-package #:mondo/readline)

(defun print-prompt (prompt-string)
  (format t "~&~A" (color-text :green prompt-string))
  (force-output))

(defun readline (&key prompt)
  (when prompt
    (print-prompt prompt))
  (rl:readline :prompt prompt
               :already-prompted t))

(defun add-history (input)
  (cffi:foreign-funcall "add_history" :string input :void))

(defun bind-key (key func &optional (keymap (rl:get-keymap)))
  (rl:bind-keyseq key func :keymap keymap))

(defun complete ()
  (cffi:foreign-funcall "rl_complete" :int 9 :int))
