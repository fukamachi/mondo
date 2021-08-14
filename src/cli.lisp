(defpackage #:mondo/cli
  (:use #:cl)
  (:import-from #:mondo/repl
                #:run-repl)
  (:import-from #:mondo/color
                #:color-text
                #:*enable-colors*)
  (:import-from #:mondo/logger
                #:*log-level*
                #:+debug+)
  (:import-from #:mondo/utils
                #:starts-with)
  (:export #:main
           #:mondo-command))
(in-package #:mondo/cli)

(define-condition mondo-cli-error (error) ())
(define-condition unknown-option (mondo-cli-error)
  ((option :initarg :option))
  (:report (lambda (condition stream)
             (with-slots (option) condition
               (format stream "Unknown option: ~A" option)))))
(define-condition extra-arguments (mondo-cli-error)
  ((args :initarg :args))
  (:report (lambda (condition stream)
             (with-slots (args) condition
               (format stream "Extra arguments: ~{~A~^ ~}" args)))))
(define-condition missing-option-value (mondo-cli-error)
  ((option :initarg :option))
  (:report (lambda (condition stream)
             (with-slots (option) condition
               (format stream "The option '~A' requires a following argument" option)))))
(define-condition invalid-option-value (mondo-cli-error)
  ((option :initarg :option)
   (value :initarg :value))
  (:report (lambda (condition stream)
             (with-slots (option value) condition
               (format stream "Invalid value for the option '~A': ~A" option value)))))

(defun command-line-arguments ()
  #+allegro (system:command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+ecl (si:command-args)
  #+cmu ext:*command-line-words*
  #+ccl ccl:*command-line-argument-list*
  #+lispworks system:*line-arguments-list*)

(defmacro case-equal (keyform &body cases)
  (let ((g-keyform (gensym "KEYFORM")))
    `(let ((,g-keyform ,keyform))
       (declare (ignorable ,g-keyform))
       (cond
         ,@(loop for (case . body) in cases
                 if (eq case 'otherwise)
                   collect `(t ,@body)
                 else
                   collect `((find ,g-keyform ',(if (listp case)
                                                    case
                                                    (list case))
                                   :test #'equal)
                             ,@body))))))

(defun print-version ()
  (format *error-output*
          "~&mondo ~A~%" (asdf:component-version (asdf:find-system :mondo)))
  (uiop:quit))

(defun print-usage ()
  (format *error-output*
          "~&Usage: mondo [OPTIONS...]

OPTIONS:
    -L, --lisp [NAME]
        Run the specific Lisp implementation (Default: sbcl-bin)
    -S, --source-registry [SOURCE REGISTRY]
        Overwrite source registry of ASDF with the argument.
    -h, --host [NAME]
        Hostname of the running Swank server to connect to
    -p, --port [PORT]
        Port of the running Swank server to connect to
    --no-color
        Disable colors
    --version
        Print version
    --help
        Print this message
    --debug
        Print debug logs
")
  (uiop:quit))

(defun parse-argv (argv)
  (loop with lisp = nil
        with host = nil
        with port = nil
        with source-registry = nil
        for option = (pop argv)
        while (and option
                   (starts-with "-" option))
        do (case-equal option
             (("-L" "--lisp")
              (unless argv
                (error 'missing-option-value :option option))
              (setf lisp (pop argv)))
             (("-S" "--source-registry")
              (setf source-registry (pop argv)))
             (("-h" "--host")
              (unless argv
                (error 'missing-option-value :option option))
              (setf host (pop argv)))
             (("-p" "--port")
              (unless argv
                (error 'missing-option-value :option option))
              (unless (every #'digit-char-p (first argv))
                (error 'invalid-option-value :option option :value (first argv)))
              (setf port (parse-integer (pop argv))))
             ("--no-color" (setf *enable-colors* nil))
             ("--version" (print-version))
             ("--help" (print-usage))
             ("--debug" (setf *log-level* +debug+))
             (otherwise
               (error 'unknown-option
                      :option option)))
        finally (return (values (append (when lisp `(:lisp ,lisp))
                                        (when source-registry `(:source-registry ,source-registry))
                                        (when host `(:host ,host))
                                        (when port `(:port ,port)))
                                (if option
                                    (cons option argv)
                                    argv)))))

(defun mondo-command (argv)
  (handler-case
      (multiple-value-bind (options args)
          (parse-argv argv)
        (when args
          (error 'extra-arguments :args args))
        (apply #'run-repl options))
    #+sbcl
    (sb-sys:interactive-interrupt ()
      (format *error-output* "~&Bye.~%")
      (uiop:quit -1))
    (mondo-cli-error (e)
      (format *error-output* "~&~A~%" (color-text :red (princ-to-string e))))))

(defun main ()
  (destructuring-bind (&optional $0 &rest argv)
      (command-line-arguments)
    (declare (ignore $0))
    (mondo-command argv)))
