(defpackage #:mondo/utils
  (:use #:cl)
  #-(or sbcl allegro ccl clisp)
  (:import-from #:babel)
  (:import-from #:usocket)
  (:import-from #:bordeaux-threads)
  (:export #:find-shortest-nickname
           #:space-char-p
           #:*space-chars*
           #:string-space-trim
           #:integer-string-p
           #:string-to-octets
           #:octets-to-string
           #:port-available-p
           #:random-port
           #:data-directory
           #:make-thread
           #:add-exit-hook))
(in-package #:mondo/utils)

(defun find-shortest-nickname (package-names)
  (first
    (sort (copy-seq package-names)
          #'<=
          :key #'length)))

(defparameter *space-chars*
  '(#\Space #\Newline #\Tab #\Return))

(defun space-char-p (char)
  (check-type char character)
  (and (member char *space-chars*)
       t))

(defun string-space-trim (value)
  (check-type value string)
  (string-trim *space-chars* value))

(defun integer-string-p (value)
  (check-type value string)
  (let ((trimmed (string-space-trim value)))
    (and (/= 0 (length trimmed))
         (every #'digit-char-p trimmed))))

(defun string-to-octets (string &key (start 0) (end (length string)))
  #+sbcl (sb-ext:string-to-octets string :start start :end end :external-format :utf-8)
  #+allegro (excl:string-to-octets string :start start :end end :external-format :utf8)
  #+ccl (ccl:encode-string-to-octets string :start start :end end :external-format :utf-8)
  #+clisp (ext:convert-string-to-bytes string charset:utf-8 :start start :end end)
  #-(or sbcl allegro ccl clisp)
  (babel:string-to-octets string :start start :end end :encoding :utf-8))

(defun octets-to-string (octets &key (start 0) (end (length octets)))
  #+sbcl (sb-ext:octets-to-string octets :start start :end end :external-format :utf-8)
  #+allegro (excl:octets-to-string octets :start start :end end :external-format :utf8)
  #+ccl (ccl:decode-string-from-octets octets :start start :end end :external-format :utf-8)
  #+clisp (ext:convert-string-from-bytes octets charset:utf-8 :start start :end end)
  #-(or sbcl allegro ccl clisp)
  (babel:octets-to-string octets :start start :end end :encoding :utf-8))

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (progn
                         (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address t))
                         t)
           (usocket:address-in-use-error () nil)
           (usocket:socket-error (e)
             (warn "USOCKET:SOCKET-ERROR: ~A" e)
             nil))
      (when socket
        (usocket:socket-close socket)
        t))))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))

(defun data-directory ()
  (let* ((data-home (uiop:getenv-absolute-directory "XDG_DATA_HOME"))
         (data-home
           (if data-home
               (merge-pathnames #P"mondo/" data-home)
               (merge-pathnames #P".mondo/" (user-homedir-pathname)))))
    (ensure-directories-exist data-home)
    data-home))

(defun make-thread (fn &key name)
  (bt:make-thread fn :name name
                  :initial-bindings
                  `((*standard-output* . ,*standard-output*)
                    (*error-output* . ,*error-output*)
                    (bt:*default-special-bindings* . ',bt:*default-special-bindings*)
                    ,@bt:*default-special-bindings*)))

(defun safety-funcall (fn)
  (ignore-errors
    (handler-bind ((error #'uiop:print-condition-backtrace))
      (funcall fn))))

(defun add-exit-hook (fn)
  (declare (ignorable fn))
  (check-type fn (or symbol function))
  #+allegro
  (push `(safety-funcall ,fn) sys:*exit-cleanup-forms*)
  #+(or sbcl ccl clisp)
  (push (lambda () (safety-funcall fn))
        #+sbcl sb-ext:*exit-hooks*
        #+ccl ccl:*lisp-cleanup-functions*
        #+clisp custom:*fini-hooks*))
