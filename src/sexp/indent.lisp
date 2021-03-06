(defpackage #:mondo/sexp/indent
  (:use #:cl)
  (:import-from #:mondo/sexp/parse
                #:parse
                #:context-in
                #:context-start
                #:context-function-name
                #:context-func-base-point
                #:context-arg-base-point
                #:context-element-count
                #:context-inner-context
                #:context-last-inner-context)
  (:export #:update-indentation-rules
           #:indent-level))
(in-package #:mondo/sexp/indent)

(defparameter *indentation-rules*
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

(defvar *custom-indentation-rules* '())

(defun indentation-rule-of (function-name &optional package-name)
  (let ((custom-rule (find function-name *custom-indentation-rules*
                           :key #'first
                           :test 'string-equal)))
    (second
      (if (and custom-rule
               (or (null package-name)
                   (find package-name (third custom-rule) :test 'equal)))
          custom-rule
          (find function-name *indentation-rules*
                :key #'first
                :test 'string-equal)))))

(defun add-indentation-rule (function-name rule package-names)
  (setf *custom-indentation-rules*
        (cons (list function-name rule package-names)
              *custom-indentation-rules*)))

(defun update-indentation-rules (rules)
  (dolist (rule rules)
    (apply #'add-indentation-rule rule)))

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
             (if rule
                 (indent-level-by-rule rule context)
                 (values nil context)))))
    (block nil
      (let ((skipped-count (1- (context-element-count context))))
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
                                (return (indent-level-with-context (context-last-inner-context context)))
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

(defun calc-padding-at (input point)
  (let ((newline-pos (position #\Newline input :end point :from-end t)))
    (if newline-pos
        (1+ newline-pos)
        0)))

(defun indent-input-with-context (input context)
  (when (member (context-in context)
                '(:string :comment :symbol))
    (return-from indent-input-with-context
                 (values 0 (context-start context))))

  (when (and (eq (context-in context) :list)
             (null (context-function-name context)))
    (return-from indent-input-with-context
                 (values (1+ (context-start context))
                         (1+ (context-start context)))))

  (let* ((function-name (context-function-name context))
         (rule (indentation-rule-of function-name)))
    (multiple-value-bind (level context)
        (indent-level-by-rule rule context)
      (let ((form-start (context-start context))
            (func-base-point (context-func-base-point context))
            (arg-base-point (context-arg-base-point context)))
        (flet ((calc-padding (p)
                 (if p
                     (- p (calc-padding-at input p))
                     0)))
          (let ((base-point (if level
                                form-start
                                ;; arg-base-point will be NIL when no args exist
                                (or arg-base-point
                                    func-base-point))))
            (values
              (+ (or level 0)
                 (calc-padding base-point))
              base-point)))))))

(defun indent-level (input &optional (point (length input)))
  (let* ((beginning-of-line (calc-padding-at input point))
         (start-of-line (or (position-if (lambda (ch) (or (char/= ch #\Space)
                                                          (char= ch #\Newline))) input
                                         :start beginning-of-line)
                            point))
         (context (parse input :end start-of-line)))
    (multiple-value-bind (level base-point)
        (indent-input-with-context input (context-last-inner-context context))
      (values level
              base-point
              beginning-of-line
              start-of-line))))
