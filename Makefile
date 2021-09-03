EMACS ?= emacs
BATCH := $(EMACS) -Q -batch -L .

TESTS   := $(wildcard *-tests.el)

.PHONY: test
test: $(TESTS)
	$(BATCH) -l ert $(foreach file,$^,-l $(file)) -f ert-run-tests-batch-and-exit
