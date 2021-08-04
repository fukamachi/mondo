(defpackage #:mondo/logger
  (:use #:cl)
  (:shadow #:log)
  (:export #:log
           #:*log-level*
           #:*log-stream*
           #:+debug+
           #:+info+
           #:+warn+
           #:+error+))
(in-package #:mondo/logger)

(defconstant +debug+ 1)
(defconstant +info+ 2)
(defconstant +warn+ 3)
(defconstant +error+ 4)

(defvar *log-level* +info+)
(defvar *log-stream* (make-synonym-stream '*standard-output*))

(defun level-to-int (level)
  (check-type level keyword)
  (ecase level
    (:debug +debug+)
    (:info +info+)
    (:warn +warn+)
    (:error +error+)))

(defun now ()
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun log (level format-control &rest format-arguments)
  (when (<= *log-level* (level-to-int level))
    (format *log-stream*
            "~&[~A] <~A> ~A~%"
            (now)
            level
            (apply #'format nil format-control format-arguments))))
