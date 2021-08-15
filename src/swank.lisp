(uiop:define-package #:mondo/swank
  (:use #:cl)
  (:shadow #:debug)
  (:use-reexport #:mondo/swank/server
                 #:mondo/swank/client
                 #:mondo/swank/connection
                 #:mondo/swank/protocol))
(in-package #:mondo/swank)
