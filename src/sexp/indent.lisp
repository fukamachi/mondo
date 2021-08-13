(defpackage #:mondo/sexp/indent
  (:use #:cl)
  (:import-from #:mondo/sexp/parse
                #:parse
                #:context-function-name
                #:context-func-base-point
                #:context-arg-base-point
                #:context-skipped-count
                #:context-inner-context
                #:context-last-inner-context)
  (:export #:indent-input))
(in-package #:mondo/sexp/indent)

(defvar *indentation-rules*
  '((block 1)
    (case        (4 &rest (&whole 2 &rest 1)))
    (ccase       (as case))
    (ecase       (as case))
    (typecase    (as case))
    (etypecase   (as case))
    (ctypecase   (as case))
    (catch 1)
    (cond        (&rest (&whole 2 &rest nil)))
    ;; for DEFSTRUCT
    (:constructor (4 &lambda))
    (defvar      (4 2 2))
    (defclass    (6 (&whole 4 &rest 1)
                  (&whole 2 &rest 1)
                  (&whole 2 &rest 1)))
    (defconstant (as defvar))
    (defparameter     (as defvar))
    (define-condition (as defclass))
    (define-modify-macro (4 &lambda &body))
    (defsetf     (4 &lambda 4 &body))
    (defun       (4 &lambda &body))
    (defgeneric  (4 &lambda &body))
    (define-setf-method   (as defun))
    (define-setf-expander (as defun))
    (defmacro     (as defun))
    (deftype      (as defun))
    (defmethod   lisp-indent-defmethod)
    (defpackage  (4 2))
    (defstruct   ((&whole 4 &rest (&whole 2 &rest 1))
                  &rest (&whole 2 &rest 1)))
    (destructuring-bind (&lambda 4 &body))
    (do          2)
    (do*         (as do))
    (dolist      ((&whole 4 2 1) &body))
    (dotimes     (as dolist))
    (eval-when   1)
    (flet        ((&whole 4 &rest (&whole 1 4 &lambda &body)) &body))
    (labels         (as flet))
    (macrolet       (as flet))
    (handler-case (4 &rest (&whole 2 &lambda &body)))
    (restart-case (as handler-case))
    ;; single-else style (then and else equally indented)
    (if          (&rest nil))
    (lambda      (&lambda &body))
    (let         ((&whole 4 &rest (&whole 1 1 2)) &body))
    (let*         (as let))
    (compiler-let (as let))
    (handler-bind (as let))
    (restart-bind (as let))
    (locally 1)
    (loop           lisp-indent-loop)
    (:method        lisp-indent-defmethod) ; in `defgeneric'
    (multiple-value-bind ((&whole 6 &rest 1) 4 &body))
    (multiple-value-call (4 &body))
    (multiple-value-prog1 1)
    (multiple-value-setq (4 2))
    (pprint-logical-block (4 2))
    (print-unreadable-object ((&whole 4 1 &rest 1) &body))
    ;; Combines the worst features of BLOCK, LET and TAGBODY
    (prog        (&lambda &rest lisp-indent-tagbody))
    (prog1 1)
    (prog2 2)
    (progn 0)
    (progv       (4 4 &body))
    (return 0)
    (return-from (nil &body))
    (symbol-macrolet (as let))
    (tagbody     lisp-indent-tagbody)
    (throw 1)
    (unless 1)
    (unwind-protect (5 &body))
    (when 1)
    (with-accessors          (as multiple-value-bind))
    (with-compilation-unit   ((&whole 4 &rest 1) &body))
    (with-condition-restarts (as multiple-value-bind))
    (with-output-to-string (4 2))
    (with-slots              (as multiple-value-bind))
    (with-standard-io-syntax (2))))

(defun indentation-rule-of (function-name)
  (second
    (find function-name *indentation-rules*
          :key #'first
          :test 'string-equal)))

(defun take-nth-subrule-in-rule (rule nth)
  (labels ((take-last (el rest)
             (case el
               (&whole (let ((rule (rest rest)))
                         (take-last (first rule) (rest rule))))
               (&rest (first rest))
               (otherwise el))))
    (loop for el = (pop rule)
          while (and el (not (zerop nth)))
          do (etypecase el
               ((or integer cons (eql &lambda))
                (decf nth))
               (symbol
                 (ecase el
                   (&rest (return (first rule)))
                   (&body (return nil))
                   (&whole (pop rule)))))
          finally
          (return (take-last el rule)))))

(defun indent-level-by-rule (rule context)
  (flet ((indent-level-with-context (context)
           (let* ((function-name (context-function-name context))
                  (rule (indentation-rule-of function-name)))
             (when rule
               (indent-level-by-rule rule context)))))
    (block nil
      (let ((skipped-count (context-skipped-count context)))
        (values
          (etypecase rule
            (null nil)
            (integer (if (< skipped-count rule)
                         4
                         2))
            ;; TODO: function call
            (symbol nil)
            (cons
              (case (first rule)
                (as (return
                      (indent-level-by-rule (indentation-rule-of (second rule)) context)))
                (otherwise
                  (let ((subrule (take-nth-subrule-in-rule rule skipped-count)))
                    (etypecase subrule
                      (null (if (context-inner-context context)
                                (return (indent-level-with-context (context-inner-context context)))
                                nil))
                      (integer (if (context-inner-context context)
                                   (return (indent-level-with-context (context-last-inner-context context)))
                                   subrule))
                      (symbol
                        (case subrule
                          (&body
                            (let ((inner-context (context-inner-context context)))
                              (if inner-context
                                  (if (context-function-name inner-context)
                                      (return (indent-level-with-context inner-context))
                                      1)
                                  2)))
                          (&lambda 4)
                          ;; TODO: function call
                          (otherwise)))
                      (cons
                        (if (context-inner-context context)
                            (return (indent-level-by-rule subrule (context-inner-context context)))
                            ;; Find &whole
                          (second (member '&whole subrule))))))))))
          context)))))

(defun indent-level (input context prompt-length)
  (let* ((function-name (context-function-name context))
         (rule (indentation-rule-of function-name)))
    (multiple-value-bind (level context)
        (indent-level-by-rule rule context)
      (let ((func-base-point (context-func-base-point context))
            (arg-base-point (context-arg-base-point context)))
        (flet ((calc-padding (p)
                 (if p
                     (let ((base-padding (position #\Newline input :end p :from-end t)))
                       (if base-padding
                           (- p (1+ base-padding))
                           (+ p prompt-length)))
                     0)))
          (+ (or level 0)
             (calc-padding (if level
                               func-base-point
                               arg-base-point))))))))

(defun indent-input (input point prompt-length)
  (let* ((beginning-of-line (let ((pos (position #\Newline input
                                                 :end point
                                                 :from-end t)))
                              (if pos
                                  (1+ pos)
                                  0)))
         (start-of-line (or (position-if (lambda (ch) (or (char/= ch #\Space)
                                                          (char= ch #\Newline))) input
                                         :start beginning-of-line)
                            point))
         (context (parse input :end start-of-line))
         (padding (indent-level input context prompt-length)))
    (concatenate 'string
                 (subseq input 0 beginning-of-line)
                 (make-string padding :initial-element #\Space)
                 (subseq input start-of-line))))
