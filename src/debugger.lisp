(defpackage #:mondo/debugger
  (:use #:cl)
  (:export #:*debugger-level*
           #:in-debugger-p))
(in-package #:mondo/debugger)

(defvar *debugger-level* 0)

(defun in-debugger-p ()
  (not (zerop *debugger-level*)))
