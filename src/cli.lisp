(defpackage #:mondo/cli
  (:use #:cl)
  (:import-from #:mondo/repl
                #:run-repl)
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

(defun parse-argv (argv)
  (loop for option = (pop argv)
        while (and option
                   (starts-with "-" option))
        do (case-equal option
             ("--debug" (setf *log-level* +debug+))
             (otherwise
               (error 'unknown-option
                      :option option)))
        finally (return (values '()
                                (if option
                                    (cons option argv)
                                    argv)))))

(defun mondo-command (argv)
  (handler-case
      (multiple-value-bind (options args)
          (parse-argv argv)
        (declare (ignore options))
        (when args
          (error 'extra-arguments :args args))
        (run-repl))
    #+sbcl
    (sb-sys:interactive-interrupt ()
      (format *error-output* "~&Bye.~%")
      (uiop:quit -1 t))
    (mondo-cli-error (e)
      (format *error-output* "~&~C[31m~A~C[0m~%" #\Esc e #\Esc))))

(defun main ()
  (destructuring-bind (&optional $0 &rest argv)
      (command-line-arguments)
    (declare (ignore $0))
    (mondo-command argv)))
