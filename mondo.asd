(defsystem "mondo"
  :class :package-inferred-system
  :version "0.1.0"
  :description "Common Lisp REPL interface"
  :author "Eitaro Fukamachi"
  :depends-on ("mondo/main")
  :pathname "src")

(defsystem "mondo/command"
  :depends-on ("mondo/cli"
               "mondo/repl")
  :build-operation "program-op"
  :build-pathname "mondo"
  :entry-point "mondo/cli:main")
