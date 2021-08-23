(defpackage #:mondo/sexp/parse
  (:use #:cl)
  (:import-from #:mondo/utils
                #:space-char-p
                #:*space-chars*)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:parse
           #:context-in
           #:input-complete-p
           #:function-at-point))
(in-package #:mondo/sexp/parse)

(defvar *context* nil)

(defstruct context
  in
  function-name
  func-base-point
  arg-base-point
  (skipped-count 0)
  inner-context)

(defmacro with-context-in (in &body body)
  (with-gensyms (before)
    `(let ((,before (context-in *context*)))
       (setf (context-in *context*) ,in)
       (prog1 (progn ,@body)
         (setf (context-in *context*) ,before)))))

(defun function-name ()
  (context-function-name *context*))

(defun (setf function-name) (new-value)
  (setf (context-function-name *context*) new-value))

(defun func-base-point ()
  (context-func-base-point *context*))

(defun (setf func-base-point) (new-value)
  (setf (context-func-base-point *context*) new-value))

(defun arg-base-point ()
  (context-arg-base-point *context*))

(defun (setf arg-base-point) (new-value)
  (setf (context-arg-base-point *context*) new-value))

(defun skipped-count ()
  (context-skipped-count *context*))

(defun (setf skipped-count) (new-value)
  (setf (context-skipped-count *context*) new-value))

(defun inner-context ()
  (context-inner-context *context*))

(defun (setf inner-context) (new-value)
  (setf (context-inner-context *context*) new-value))

(defmacro with-context (&body body)
  (let ((outer-context (gensym "OUTER-CONTEXT")))
    `(let ((,outer-context *context*)
           (*context* (make-context)))
       (when ,outer-context
         (setf (context-inner-context ,outer-context) *context*))
       (values (handler-case (prog1 (progn ,@body)
                               (when ,outer-context
                                 (setf (context-inner-context ,outer-context) nil)))
                 (incomplete-form (e)
                   (incomplete-form (slot-value e 'type))))
               *context*))))

(defun context-last-inner-context (context)
  (loop for inner-context = (context-inner-context context)
        if (not inner-context)
        do (return context)
        else do (setf context inner-context)))

(defstruct (buffer
             (:constructor make-buffer (input &key end
                                              &aux (end (or end (length input))))))
  input
  (line 0)
  (point 0)
  end)

(define-condition incomplete-form ()
  ((type :initarg :type)
   (context :initform *context*)))

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
    (incomplete-form type))
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
              (with-context
                (with-context-in "comment"
                  (skip-block-comment buffer))))
             ((char= char #\;)
              (with-context
                (with-context-in "comment"
                  (skip-inline-comment buffer))))
             ((char= char #\Return)
              (incf (buffer-line buffer))
              (or (forward-char buffer)
                  (return))
              (when (char= (current-char buffer) #\Newline)
                (forward-char buffer)))
             ((char= char #\Newline)
              (incf (buffer-line buffer))
              (forward-char buffer))
             ((space-char-p char)
              (forward-char buffer))
             (t
              (return)))))

(defun skip-unmatched-closed-parens (buffer)
  (loop until (buffer-end-p buffer)
        for char = (current-char buffer)
        do (case char
             (#\) (forward-char buffer))
             (otherwise (return)))))

(declaim (ftype (function (t) t) read-form))

(defun read-atom (buffer)
  (block nil
    (when (buffer-end-p buffer)
      (return))
    (with-context
      (let ((char (current-char buffer)))
        (case char
          (#\"
           (with-context-in "string"
             (skip-string buffer)))
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
          (#\; (with-context-in "comment"
                 (skip-inline-comment buffer)))
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
                      (#.*space-chars* (return))
                      (#\| (skip-quoted-symbol buffer))
                      (#\\ (skip-next-char buffer :type 'symbol))
                      (otherwise
                       (forward-char buffer))))))))))

(defun read-list (buffer)
  (assert (char= (current-char buffer) #\())
  (with-context
    (forward-char buffer)
    (skip-spaces buffer)
    (when (buffer-end-p buffer)
      (incomplete-form 'list))
    (let ((start (buffer-point buffer))
          (line (buffer-line buffer)))
      (setf (func-base-point) (1- start))
      (loop with initial = t
            until (buffer-end-p buffer)
            for char = (current-char buffer)
            do (case char
                 (#\)
                  (forward-char buffer)
                  (return))
                 (#\(
                  (when (or (/= line (buffer-line buffer))
                            (null (arg-base-point)))
                    (setf line (buffer-line buffer))
                    (setf (arg-base-point) (buffer-point buffer)))
                  (read-list buffer)
                  (incf (skipped-count)))
                 (otherwise
                  (when (and (not initial)
                             (or (/= line (buffer-line buffer))
                                 (null (arg-base-point))))
                    (setf line (buffer-line buffer))
                    (setf (arg-base-point) (buffer-point buffer)))
                  (read-atom buffer)
                  (if initial
                      (with-slots (input point) buffer
                        (setf (function-name) (subseq input start point)))
                      (incf (skipped-count)))))
               (setf initial nil)
               (skip-spaces buffer)
            finally
            (incomplete-form 'list)))))

(defun read-form (buffer)
  (unless (buffer-end-p buffer)
    (case (current-char buffer)
      (#\( (read-list buffer))
      (otherwise (read-atom buffer)))))

(defun parse (input &key end)
  (let ((buffer (make-buffer input :end end)))
    (handler-case
        (progn
          (skip-spaces buffer)
          (loop until (buffer-end-p buffer)
                do (read-form buffer)
                (skip-spaces buffer)
                (skip-unmatched-closed-parens buffer)
                finally (return nil)))
      (incomplete-form (e)
        (slot-value e 'context)))))

(defun input-complete-p (input)
  (not (parse input)))

(defun function-at-point (input point)
  (let ((context (parse input :end point)))
    (when context
      (context-function-name
        (context-last-inner-context context)))))
