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

(deftype context-in-type ()
  '(member :symbol :string :comment :list :character :atom :composite :form))

(defstruct context
  (in nil :type context-in-type)
  start
  function-name
  func-base-point
  arg-base-point
  (element-count 0)
  inner-context)

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

(defun element-count ()
  (context-element-count *context*))

(defun (setf element-count) (new-value)
  (setf (context-element-count *context*) new-value))

(defun inner-context ()
  (context-inner-context *context*))

(defun (setf inner-context) (new-value)
  (setf (context-inner-context *context*) new-value))

(defmacro with-context ((&optional in) &body body)
  (let ((outer-context (gensym "OUTER-CONTEXT")))
    `(let ((,outer-context *context*)
           (*context* (make-context :in ,in)))
       (when ,outer-context
         (setf (context-inner-context ,outer-context) *context*))
       (values (handler-case
                   (prog1 (progn ,@body)
                     (when ,outer-context
                       (setf (context-inner-context ,outer-context) nil)))
                 (incomplete-form (e)
                   ;; Rethrow with the upper context
                   (incomplete-form (slot-value e 'type))))
               *context*))))

(defun context-last-inner-context (context)
  (let ((inner-context (context-inner-context context)))
    (if inner-context
        (context-last-inner-context inner-context)
        context)))

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
           (return)
        finally (incomplete-form 'comment)))

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
    (let ((char (current-char buffer)))
      (case char
        (#\"
         (with-context (:string)
           (skip-string buffer))
         (incf (element-count)))
        (#\#
         (case (next-char buffer)
           (#\|
            (with-context (:comment)
              (skip-block-comment buffer)))
           (#\\
            (with-context (:character)
              (forward-char buffer)
              (forward-char buffer)
              (when (buffer-end-p buffer)
                (incomplete-form 'atom))
              (forward-char buffer)))
           (otherwise
             (with-context (:composite)
               (forward-char buffer)
               (skip-while buffer #'digit-char-p)
               (when (or (buffer-end-p buffer)
                         (space-char-p (current-char buffer)))
                 (incomplete-form 'form))
               (forward-char buffer)
               (skip-spaces buffer)
               (when (buffer-end-p buffer)
                 (incomplete-form 'composite)))
             (read-form buffer))))
        (#\|
         (if (and *context*
                  (eq (context-in *context*) :symbol))
             (skip-quoted-symbol buffer)
             (with-context (:symbol)
               (skip-quoted-symbol buffer)))
         (incf (element-count)))
        (#\; (with-context (:comment)
               (skip-inline-comment buffer)))
        ((#\' #\` #\,)
         (with-context (:form)
           (forward-char buffer)
           (skip-spaces buffer)
           (when (buffer-end-p buffer)
             (incomplete-form 'form)))
         (read-form buffer))
        (#\:
         (with-context (:symbol)
           (forward-char buffer)
           (when (buffer-end-p buffer)
             (incomplete-form 'symbol))
           (read-atom buffer)))
        (otherwise
         (flet ((read-symbol ()
                  (loop until (buffer-end-p buffer)
                        for char = (current-char buffer)
                        do (case char
                             ((#\" #\( #\) #\' #\` #\,)
                              (return))
                             (#.*space-chars* (return))
                             (#\| (skip-quoted-symbol buffer))
                             (#\\ (skip-next-char buffer :type 'symbol))
                             (otherwise
                               (forward-char buffer)))
                        finally (return *context*))))
           (if (eq (context-in *context*) :symbol)
               (read-symbol)
               (with-context (:symbol)
                 (read-symbol)))
           (incf (element-count))))))))

(defun read-list (buffer)
  (assert (char= (current-char buffer) #\())
  (with-context (:list)
    (setf (context-start *context*) (buffer-point buffer))
    (forward-char buffer)
    (skip-spaces buffer)
    (when (buffer-end-p buffer)
      (incomplete-form 'list))
    (let ((start (buffer-point buffer))
          (line (buffer-line buffer)))
      (setf (func-base-point) start)
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
                  (read-list buffer))
                 (otherwise
                  (when (and (not initial)
                             (or (/= line (buffer-line buffer))
                                 (null (arg-base-point))))
                    (setf line (buffer-line buffer))
                    (setf (arg-base-point) (buffer-point buffer)))
                  (read-atom buffer)
                  (when initial
                    (setf initial nil)
                    (with-slots (input point) buffer
                      (setf (function-name) (subseq input start point))))))
               (skip-spaces buffer)
            finally
            (incomplete-form 'list))))
  (when *context*
    (incf (element-count))))

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
