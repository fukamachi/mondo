(defpackage #:mondo/sexp
  (:use #:cl)
  (:import-from #:mondo/utils
                #:space-char-p)
  (:export #:input-complete-p
           #:function-at-point
           #:indent-input))
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

(defun start-at-point (input point)
  (let ((start (min (1- (length input)) point))
        (level 0))
    (loop for i from start downto 0
          for char = (aref input i)
          if (and (char= char #\()
                  (or (= i 0)
                      (char/= #\\ (aref input (1- i)))))
          do (when (= (decf level) -1)
               (return i))
          if (and (char= char #\))
                  (or (= i 0)
                      (char/= #\\ (aref input (1- i)))))
          do (incf level)
          finally
          (let ((atom-start-point
                  (position-if #'space-char-p
                               input
                               :end point
                               :from-end t)))
            (return
              (if atom-start-point
                  (1+ atom-start-point)
                  0))))))

(defun function-at-point (input point)
  (let ((start-point (start-at-point input point)))
    (subseq input (1+ start-point)
            (position-if
              (lambda (char)
                (or (space-char-p char)
                    (member char '(#\( #\) #\' #\` #\# #\"))))
              input :start (1+ start-point)))))

(defun indent-input (input prompt)
  (let* ((input (if prompt
                    (format nil "~A~A" prompt input)
                    input))
         (point (length input))
         (beginning-of-line (let ((pos (position #\Newline input
                                                 :end point
                                                 :from-end t)))
                              (if pos
                                  (1+ pos)
                                  0)))
         (start-of-line (or (position-if (lambda (ch) (or (char/= ch #\Space)
                                                          (char= ch #\Newline))) input
                                         :start beginning-of-line)
                            point))
         (form-point (start-at-point input (max 0 (1- beginning-of-line))))
         ;; XXX: Currently all forms are indented with +2 spaces.
         (padding (+ 2 (- form-point (let ((pos (position #\Newline input
                                                          :end form-point
                                                          :from-end t)))
                                       (if pos
                                           (1+ pos)
                                           0))))))
    (concatenate 'string
                 (subseq input (length prompt) beginning-of-line)
                 (make-string padding :initial-element #\Space)
                 (subseq input start-of-line))))
