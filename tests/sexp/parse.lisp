(defpackage #:mondo/tests/sexp/parse
  (:use #:cl
        #:rove)
  (:import-from #:mondo/sexp/parse
                #:parse
                #:context-in
                #:context-function-name
                #:context-func-base-point
                #:context-arg-base-point
                #:context-element-count
                #:context-inner-context
                #:context-last-inner-context))
(in-package #:mondo/tests/sexp/parse)

(deftest parse
  (flet ((parse* (input)
           (let ((context (parse input)))
             (when context
               (context-last-inner-context context)))))
    (ok (null (parse* "\"a\"")))
    (ok (null (parse* "'a")))
    (ok (null (parse* "123")))
    (ok (null (parse* "|aBc|")))
    (ok (eq (context-in (parse* "(")) :list))
    (ok (eq (context-in (parse* "\"")) :string))
    (ok (eq (context-in (parse* "|")) :symbol))
    (ok (eq (context-in (parse* ";")) :comment))
    (ok (eq (context-in (parse* "#|")) :comment))
    (ok (eq (context-in (parse* "( \"")) :string))
    (ok (eq (context-in (parse* "( |")) :symbol))
    (ok (eq (context-in (parse* "( ;;")) :comment))
    (ok (eq (context-in (parse* "( #|")) :comment))
    (ok (eq (context-in (parse* "\" |")) :string))
    (ok (eq (context-in (parse* "\" (")) :string))
    (ok (eq (context-in (parse* "\" ;;")) :string))
    (ok (eq (context-in (parse* "\" #|")) :string))
    (ok (eq (context-in (parse* "| (")) :symbol))
    (ok (eq (context-in (parse* "| \"")) :symbol))
    (ok (eq (context-in (parse* "| ;;")) :symbol))
    (ok (eq (context-in (parse* ";; (")) :comment))
    (ok (eq (context-in (parse* ";; \"")) :comment))
    (ok (null (parse* "#\\(")))
    (ok (null (parse* "#\\\"")))
    (ok (null (parse* "#\\'")))
    (ok (null (parse* "#\\#")))
    (ok (null (parse* "#\\|")))
    (ok (null (parse* "#\\:")))
    (ok (eq (context-in (parse* "#\\")) :character))
    (ok (eq (context-in (parse* "'")) :form))
    (ok (null (parse* ";; \"
                      ")))
    (ok (eq (context-in (parse* ";; |")) :comment))
    (ok (null (parse* ";; |
                      ")))
    (ok (eq (context-in (parse* ";; #|")) :comment))
    (ok (null (parse* ";; #|
                      "))))
  (let* ((context (parse "(list 1 (+ 0.5) #2A((0 1 5) (foo 2 (hot dog))) #|(comment)|# (+ 10"))
         (inner-context (context-inner-context context)))
    (ok (eq (context-in context) :list))
    (ok (equal (context-function-name context) "list"))
    (ok (eql (context-func-base-point context) 1))
    (ok (eql (context-arg-base-point context) 6))
    (ok (eql (context-element-count context) 4))
    (ok inner-context)
    (ok (eq (context-in inner-context) :list))
    (ok (equal (context-function-name inner-context) "+"))
    (ok (eql (context-func-base-point inner-context) 62))
    (ok (eql (context-arg-base-point inner-context) 64))
    (ok (eql (context-element-count inner-context) 2))
    (ok (null (context-inner-context inner-context)))))
