(defpackage #:mondo/color
  (:use #:cl)
  (:export #:*enable-colors*
           #:color-text))
(in-package #:mondo/color)

(defvar *enable-colors* t)

(defparameter *color-code*
  `((:red    . 31)
    (:green  . 32)
    (:yellow . 33)
    (:aqua   . 36)
    (:white  . 37)
    (:gray   . 90)))

(defvar *rl-prompt-start-ignore* (code-char 1))
(defvar *rl-prompt-end-ignore* (code-char 2))

(defun color-text (color text)
  (unless *enable-colors*
    (return-from color-text text))

  (if (= (length text) 0)
      text
      (let ((code (cdr (assoc color *color-code*))))
        (assert color)
        (format nil "~C~C[~Am~C~A~C~C[0m~C"
                *rl-prompt-start-ignore*
                #\Esc
                code
                *rl-prompt-end-ignore*
                text
                *rl-prompt-start-ignore*
                #\Esc
                *rl-prompt-end-ignore*))))
