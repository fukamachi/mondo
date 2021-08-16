(defpackage #:mondo/server/vlime/protocol
  (:use #:cl)
  (:import-from #:mondo/utils
                #:octets-to-string
                #:string-to-octets)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:yason
                #:parse
                #:encode)
  (:import-from #:alexandria
                #:destructuring-case
                #:ensure-gethash
                #:when-let)
  (:export #:read-message
           #:write-message))
(in-package #:mondo/server/vlime/protocol)

(defstruct buffer
  (vector (make-array 1024 :element-type '(unsigned-byte 8))))

(defun extend-buffer (buffer)
  (with-slots (vector) buffer
    (let ((new-vector (make-array (* (length vector) 2) :element-type '(unsigned-byte 8))))
      (replace new-vector vector)
      (setf vector new-vector))))

(defun read-vlime-line (stream)
  (let ((buffer (make-buffer)))
    (loop for i from 0
          for byte = (read-byte stream)
          until (eql byte (char-code #\Newline))
          do (when (<= (length (buffer-vector buffer)) i)
               (extend-buffer buffer))
             (setf (aref (buffer-vector buffer) i) byte)
          finally
          (return (octets-to-string (buffer-vector buffer)
                                    :start 0 :end i)))))

(defun set-last-cdr (list obj)
  (if (consp (cdr list))
      (set-last-cdr (cdr list) obj)
      (setf (cdr list) obj)))

(defun json-to-form (json)
  (cond
    ((listp json)
     (mapcar #'json-to-form json))
    ((and (hash-table-p json) (nth-value 1 (gethash "name" json)))
     (let ((sym-name (gethash "name" json))
           (sym-package (gethash "package" json)))
       (intern sym-name sym-package)))
    ((and (hash-table-p json) (nth-value 1 (gethash "tail" json)))
     (let* ((head-list (gethash "head" json))
            (tail-obj (gethash "tail" json))
            (head (json-to-form head-list)))
       (set-last-cdr head (json-to-form tail-obj))
       head))
    (t
     ; Numbers & strings
     json)))

(defun list-form-to-json (list &optional (acc (list)))
  (if list
      (if (listp list)
          (progn
            (push (form-to-json (car list)) acc)
            (list-form-to-json (cdr list) acc))
          (let ((obj (make-hash-table :test #'equal)))
            (setf (gethash "head" obj) (reverse acc))
            (setf (gethash "tail" obj) list)
            obj))
      (reverse acc)))

(defvar *json-cache* (apply #'make-hash-table
                            :test #'eq
                            #+sbcl '(:synchronized t :weakness :key)
                            #+ccl '(:shared t :weak :key)
                            #+allegro '(:weak-keys t :values t)
                            #+ecl '(:synchronized t :weakness :key)
                            #+abcl '(:weakness :key)
                            #+lispworks '(:single-thread nil :weak-kind :key)
                            #-(or sbcl ccl allegro ecl abcl lispworks) '()))

(defun form-to-json (form)
  (cond
    ((listp form)
     (list-form-to-json form))
    ((eql form t)
     ; special case to prevent T from being serialized as a normal symbol,
     ; thus saving some space
     form)
    ((symbolp form)
     (ensure-gethash form
                     *json-cache*
                     (let ((sym-obj (make-hash-table :test #'equal))
                           (sym-name (symbol-name form))
                           (sym-package (when-let (pkg (symbol-package form))
                                          (package-name pkg))))
                       (setf (gethash "name" sym-obj) sym-name)
                       (when sym-package
                         (setf (gethash "package" sym-obj) sym-package))
                       sym-obj)))
    (t
     ; Numbers & strings
     form)))

(defun client-emacs-rex-p (form)
  (and (listp form)
       (listp (nth 1 form))
       (eql (car (nth 1 form)) :emacs-rex)))

(defun read-message (stream)
  (let* ((line (read-vlime-line stream))
         (json (yason:parse line))
         (vlime-message (json-to-form json)))
    (log :debug "Received VLIME message: ~A" line)
    (if (client-emacs-rex-p vlime-message)
        (destructuring-bind (id message)
            vlime-message
          (append message (list id)))
        (second vlime-message))))

(defun write-message (message stream)
  (let* ((vlime-message
           (destructuring-case message
             ((:return result id)
              `(,id ,(form-to-json `(:return ,result))))
             ((t &rest args)
              (declare (ignore args))
              `(0 ,(form-to-json message)))))
         (json (with-output-to-string (s)
                 (yason:encode vlime-message s)))
         (payload
           (string-to-octets (format nil "~A~C~C" json #\Return #\Newline))))
    (log :debug "Sending to VLIME: ~A" json)
    (write-sequence payload stream)
    (force-output stream)))
