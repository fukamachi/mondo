(defsystem "mondo"
  :class :package-inferred-system
  :version "0.1.0"
  :description "Common Lisp REPL interface"
  :author "Eitaro Fukamachi"
  :license "GPL-3.0"
  :depends-on ("mondo/main")
  :pathname "src"
  :in-order-to ((test-op (test-op "mondo/tests"))))

(defsystem "mondo/command"
  :depends-on ("mondo/cli"
               "mondo/repl")
  :build-operation "program-op"
  :build-pathname "mondo"
  :entry-point "mondo/cli:main")

(defsystem "mondo/tests"
  :depends-on ("mondo"
               "rove")
  :pathname "tests"
  :components
  ((:file "utils"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
