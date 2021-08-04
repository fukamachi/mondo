(defpackage #:mondo
  (:nicknames #:mondo/main)
  (:use #:cl)
  (:import-from #:mondo/repl
                #:run-repl)
  (:export #:start))
(in-package #:mondo)

(defun start ()
  (run-repl))
