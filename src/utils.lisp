(defpackage #:mondo/utils
  (:use #:cl)
  (:export #:find-shortest-nickname
           #:space-char-p
           #:starts-with))
(in-package #:mondo/utils)

(defun find-shortest-nickname (package-names)
  (first
    (sort package-names
          #'<=
          :key #'length)))

(defparameter *space-chars*
  '(#\Space #\Newline #\Tab #\Return))

(defun space-char-p (char)
  (check-type char character)
  (and (member char *space-chars*)
       t))

(defun starts-with (prefix string)
  (check-type string string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))
