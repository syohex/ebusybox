.PHONY : test

EMACS ?= emacs

LOADPATH = -L .
LOAD_HELPER = -l t/test-helper.el

all: test test-elisp

test:
	@env PATH=$(PWD)/bin:$(PATH) prove -vr t/

test-elisp: test-elisp-bin

test-elisp-bin:
	$(EMACS) -Q -batch $(LOAD_HELPER) -l src/bin/expr/expr.el \
		-l src/bin/expr/expr-test.el -f ert-run-tests-batch-and-exit
