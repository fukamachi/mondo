#!/bin/bash

exec sbcl --noinform --non-interactive \
  --eval '(ql:quickload :mondo/tests)' \
  --eval '(asdf:test-system :mondo)'
