(defpackage #:mondo/sexp-parse
  (:use #:cl)
  (:import-from #:mondo/utils
                #:space-char-p)
  (:export #:input-complete-p))
(in-package #:mondo/sexp-parse)

(defvar *context* nil)

(defstruct context
  function-name
  (skipped-count 0)
  inner-context)

(defun function-name ()
  (context-function-name *context*))

(defun (setf function-name) (new-value)
  (setf (context-function-name *context*) new-value))

(defun skipped-count ()
  (context-skipped-count *context*))

(defun (setf skipped-count) (new-value)
  (setf (context-skipped-count *context*) new-value))

(defmacro with-context (&body body)
  (let ((outer-context (gensym "OUTER-CONTEXT")))
    `(let ((,outer-context *context*)
           (*context* (make-context)))
       (when ,outer-context
         (setf (context-inner-context ,outer-context) *context*))
       ,@body)))

(defstruct (buffer
             (:constructor make-buffer (input &aux (end (length input)))))
  input
  (point 0)
  end)

(define-condition incomplete-form ()
  ((type :initarg :type)))

(defun incomplete-form (type)
  (error 'incomplete-form :type type))

(defun current-char (buffer)
  (with-slots (input point) buffer
    (aref input point)))

(defun buffer-beginning-p (buffer)
  (zerop (buffer-point buffer)))

(defun buffer-end-p (buffer)
  (with-slots (point end) buffer
    (= point end)))

(defun next-char (buffer)
  (with-slots (input point end) buffer
    (when (< (1+ point) end)
      (aref input (1+ point)))))

(defun forward-char (buffer)
  (when (buffer-end-p buffer)
    (return-from forward-char nil))
  (with-slots (point) buffer
    (incf point))
  (not (buffer-end-p buffer)))

(defun backward-char (buffer)
  (when (buffer-beginning-p buffer)
    (return-from backward-char nil))
  (with-slots (point) buffer
    (decf point))
  t)

(defun skip-while (buffer fn)
  (loop until (buffer-end-p buffer)
        for char = (current-char buffer)
        while (funcall fn char)
        do (forward-char buffer)))

;; '\'
(defun skip-next-char (buffer &key type)
  (assert (char= (current-char buffer) #\\))
  (or (forward-char buffer)
      (incomplete-form type))
  (forward-char buffer))

(defun skip-quoted (buffer quote-char &key type)
  (assert (char= (current-char buffer) quote-char))
  (unless (forward-char buffer)
    (return-from skip-quoted))
  (loop until (buffer-end-p buffer)
        for char = (current-char buffer)
        do (cond
             ((char= char quote-char)
              (forward-char buffer)
              (return))
             ((char= char #\\)
              (skip-next-char buffer :type type))
             (t
              (forward-char buffer)))
        finally
        (incomplete-form type)))

(defun skip-string (buffer)
  (skip-quoted buffer #\"
               :type 'string))

(defun skip-quoted-symbol (buffer)
  (skip-quoted buffer #\|
               :type 'symbol))

(defun skip-inline-comment (buffer)
  (assert (char= (current-char buffer) #\;))
  (loop do (forward-char buffer)
        until (buffer-end-p buffer)
        if (find (current-char buffer) '(#\Newline #\Return))
        do (loop do (forward-char buffer)
                 until (or (buffer-end-p buffer)
                           (not (find (current-char buffer) '(#\Newline #\Return)))))
           (return)))

(defun skip-block-comment (buffer)
  (assert (char= (current-char buffer) #\#))
  (assert (eql (next-char buffer) #\|))
  (forward-char buffer) ;; Skip first '#'
  (loop
    (skip-quoted-symbol buffer)
    (cond
      ((buffer-end-p buffer)
       (incomplete-form 'comment))
      ((eql (current-char buffer) #\#)
       (forward-char buffer) ;; Skip last '#'
       (return))
      (t
       (backward-char buffer)))))

(defun skip-spaces (buffer)
  (loop until (buffer-end-p buffer)
        for char = (current-char buffer)
        do (cond
             ((and (char= char #\#)
                   (eql (next-char buffer) #\|))
              (skip-block-comment buffer))
             ((char= char #\;)
              (skip-inline-comment buffer))
             ((space-char-p char)
              (forward-char buffer))
             (t
              (return)))))

(declaim (ftype (function (t) t) read-form))

(defun read-atom (buffer)
  (block nil
    (when (buffer-end-p buffer)
      (return))
    (let ((char (current-char buffer)))
      (case char
        (#\" (skip-string buffer))
        (#\#
         (forward-char buffer)
         (skip-while buffer #'digit-char-p)
         (when (or (buffer-end-p buffer)
                   (space-char-p (current-char buffer)))
           (return))
         (forward-char buffer)
         (skip-spaces buffer)
         (when (buffer-end-p buffer)
           (incomplete-form 'form))
         (read-form buffer))
        (#\| (skip-quoted-symbol buffer))
        (#\; (skip-inline-comment buffer))
        ((#\' #\` #\,)
         (forward-char buffer)
         (skip-spaces buffer)
         (when (buffer-end-p buffer)
           (incomplete-form 'form))
         (read-form buffer))
        (#\:
         (forward-char buffer)
         (when (buffer-end-p buffer)
           (incomplete-form 'symbol))
         (read-atom buffer))
        (otherwise
          (loop until (buffer-end-p buffer)
                for char = (current-char buffer)
                do (case char
                     ((#\" #\( #\) #\' #\` #\,)
                      (return))
                     (#\| (skip-quoted-symbol buffer))
                     (#\\ (skip-next-char buffer :type 'symbol))
                     (otherwise (forward-char buffer)))))))))

(defun read-list (buffer)
  (assert (char= (current-char buffer) #\())
  (forward-char buffer)
  (skip-spaces buffer)
  (when (buffer-end-p buffer)
    (incomplete-form 'list))
  (loop until (buffer-end-p buffer)
        for char = (current-char buffer)
        do (case char
             (#\) (forward-char buffer)
                  (return))
             (#\( (read-list buffer))
             (otherwise
               (read-atom buffer)))
           (skip-spaces buffer)
        finally
        (incomplete-form 'list)))

(defun read-form (buffer)
  (block nil
    (when (buffer-end-p buffer)
      (return))
    (let ((char (current-char buffer)))
      (case char
        (#\( (read-list buffer))
        (otherwise (read-atom buffer))))))

(defun input-complete-p (input)
  (let ((buffer (make-buffer input)))
    (skip-spaces buffer)
    (handler-case
        (loop until (buffer-end-p buffer)
              do (read-form buffer)
                 (skip-spaces buffer)
              finally (return t))
      (incomplete-form ()
        nil))))
