(defpackage #:mondo/tests/sexp/indent
  (:use #:cl
        #:rove)
  (:import-from #:mondo/sexp/indent
                #:indent-level)
  (:import-from #:cl-interpol))
(in-package #:mondo/tests/sexp/indent)

(cl-interpol:enable-interpol-syntax)

(deftest indent-level
  (ok (eql 1 (indent-level #?"(\n")))
  (ok (eql 0 (indent-level #?"\"\n")))
  (ok (eql 0 (indent-level #?"#|\n")))
  (ok (eql 2 (indent-level #?"(progn\n")))
  (ok (eql 4 (indent-level #?"(if (zerop n)\n")))
  (ok (eql 9 (indent-level #?"(funcall #'fact\n")))
  (ok (eql 0 (indent-level #?"( #|\n")))
  (ok (eql 4 (indent-level #?"(defun fact (n)\n(if (zerop n)\n")))
  (ok (eql 13 (indent-level #?"(defun fact (n)\n         (if (zerop n)\n")))
  (ok (eql 4 (indent-level #?"(progn\n  (progn\n")))
  (ok (eql 11 (indent-level #?"(funcall #'+\n         (progn\n")))
  (ok (eql 2 (indent-level #?"'(\n")))
  (ok (eql 2 (indent-level #?"'((\"a\" . 1)\n")))
  (ok (eql 2 (indent-level #?"'((\"a\" . 1)\n           (\"b\" . 2)\n")))
  (ok (eql 2 (indent-level #?"(do ()\n             ()\n"))))
