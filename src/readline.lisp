(defpackage #:mondo/readline
  (:use #:cl)
  (:import-from #:mondo/color
                #:color-text)
  (:import-from #:cl-readline
                #:*line-buffer*
                #:insert-text)
  (:import-from #:cffi)
  (:export #:readline
           #:read-input
           #:print-prompt
           #:add-history
           #:bind-key
           #:complete

           #:*line-buffer*
           #:insert-text
           #:replace-input))
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

(defun complete ()
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
