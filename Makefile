#!/bin/sh

.phony:
	test
	clean

test:
	sbcl --disable-debugger  --load ./.bootstrap-test.lisp  --load ./elo100.lisp  --eval '(elo100::run-unit-tests)'

clean:
	@rm ./*.fasl > /dev/null 2>/dev/null || true
