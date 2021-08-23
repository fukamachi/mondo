#!/bin/bash

exec sbcl --noinform --non-interactive \
  --eval '(progn (write-line "Loading...") (ql:quickload (list :rove :mondo/tests) :silent t))' \
  --eval '(or (rove:run :mondo/tests) (uiop:quit -1))'
