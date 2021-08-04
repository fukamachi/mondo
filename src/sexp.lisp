(defpackage #:mondo/sexp
  (:use #:cl)
  (:import-from #:mondo/utils
                #:space-char-p)
  (:export #:input-complete-p))
(in-package #:mondo/sexp)

(defun skip-while (fn stream)
  (loop for next-char = (peek-char nil stream nil nil)
        while next-char
        if (eql next-char #\\)
        do (read-char stream) (read-char stream)
        else if (not (funcall fn next-char))
        do (return)
        else
        do (read-char stream)))

(defun skip-until (fn stream)
  (skip-while (complement fn) stream))

#+nil
(defun skip-spaces (stream)
  (skip-while #'space-char-p stream))

(defun read-string (stream)
  (assert (eql (read-char stream) #\"))
  (loop
    (skip-until (lambda (char)
                  (eql char #\"))
                stream)
    (let ((next-char (peek-char nil stream nil nil)))
      (ecase next-char
        ('nil (error 'end-of-file))
        (#\" (read-char stream) (return))))))

(defun read-atom (stream)
  (skip-while (lambda (char)
                (member char '(#\' #\` #\#)))
              stream)
  (let ((next-char (peek-char nil stream nil nil)))
    (case next-char
      (#\|
       (skip-until (lambda (char) (eql char #\|)) stream)
       (unless (eql (peek-char nil stream nil nil) #\|)
         (error 'end-of-file)))
      ('nil)
      (otherwise
        (skip-until (lambda (char)
                      (or (space-char-p char)
                          (member char '(#\( #\) #\" #\'))))
                    stream)))))

(defun read-list (stream)
  (assert (eql (read-char stream) #\())
  (loop
    (skip-until (lambda (char)
                  (member char '(#\( #\) #\" #\|)))
                stream)
    (let ((next-char (peek-char nil stream nil nil)))
      (ecase next-char
        ('nil (error 'end-of-file))
        (#\( (read-list stream))
        (#\) (read-char stream) (return))
        (#\" (read-string stream))
        (#\| (read-atom stream))))))

(defun forward-sexp (stream)
  (loop
    (let ((char (peek-char t stream nil nil)))
      (case char
        ('nil (return))
        (#\( (read-list stream))
        (#\))
        (#\" (read-string stream))
        (otherwise (read-atom stream))))))

(defun input-complete-p (input)
  (check-type input string)
  (handler-case
      (with-input-from-string (s input)
        (forward-sexp s)
        t)
    (end-of-file () nil)))
