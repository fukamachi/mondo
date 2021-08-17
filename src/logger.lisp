(defpackage #:mondo/logger
  (:use #:cl)
  (:import-from #:mondo/utils
                #:data-directory)
  (:import-from #:bordeaux-threads)
  (:shadow #:log)
  (:export #:log
           #:*log-level*
           #:*log-stream*
           #:+debug+
           #:+info+
           #:+warn+
           #:+error+
           #:with-logging
           #:remove-old-log-files))
(in-package #:mondo/logger)

(defconstant +debug+ 1)
(defconstant +info+ 2)
(defconstant +warn+ 3)
(defconstant +error+ 4)

(defvar *log-level* +info+)
(defvar *log-stream* (make-synonym-stream '*standard-output*))
(defvar *background-log-stream*)

(defparameter *log-file-format*
  "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D")
(defvar *log-file-keep-days* 1)

(defun level-to-int (level)
  (check-type level keyword)
  (ecase level
    (:debug +debug+)
    (:info +info+)
    (:warn +warn+)
    (:error +error+)))

(defun now ()
  (multiple-value-bind (sec min hour)
      (get-decoded-time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun logs-directory ()
  (merge-pathnames #P"logs/" (data-directory)))

(defun list-old-log-files ()
  "Return logs directories that is created more than 2 days ago."
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (- (get-universal-time) (* 60 60 24 *log-file-keep-days*)))
    (remove-if-not
      (lambda (file)
        (string<= (pathname-name file)
                  (format nil *log-file-format* year mon day hour min sec)))
      (uiop:directory-files (logs-directory) "*.log"))))

(defun remove-old-log-files ()
  (mapc #'delete-file (list-old-log-files)))

(defun log-file ()
  (multiple-value-bind (sec min hour day mon year)
      (get-decoded-time)
    (let ((log-file
            (merge-pathnames
              (make-pathname :name (format nil *log-file-format* year mon day hour min sec)
                             :type "log")
              (logs-directory))))
      (ensure-directories-exist log-file)
      log-file)))

(defmacro with-logging (&body body)
  `(with-open-file (*background-log-stream* (log-file)
                                            :if-does-not-exist :create
                                            :direction :output)
     (let ((bt:*default-special-bindings*
             (cons
               (cons '*background-log-stream* *background-log-stream*)
               bt:*default-special-bindings*)))
       ,@body)))

(defun log (level format-control &rest format-arguments)
  (let ((line (format nil "~&[~A] <~:@(~A~)> ~A~%"
                      (now)
                      level
                      (apply #'format nil format-control format-arguments))))
    (when (<= *log-level* (level-to-int level))
      (write-string line *log-stream*))
    (when (boundp '*background-log-stream*)
      (write-string line *background-log-stream*)
      (force-output *background-log-stream*))
    line))
