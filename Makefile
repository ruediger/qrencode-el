EMACS ?= emacs
BATCH := $(EMACS) -Q -batch -L .

TESTS   := $(wildcard *-tests.el)

.PHONY: all
all: doc qrencode.elc qrencode.sh

.PHONY: test
test: $(TESTS)
	$(BATCH) -l ert $(foreach file,$^,-l $(file)) -f ert-run-tests-batch-and-exit

README.html: README.org
	$(info Creating documentation: $@)
	$(BATCH) -l org --visit=$< -f org-html-export-to-html

.PHONY: doc
doc: README.html

%.elc: WERROR := '(setq byte-compile-error-on-warn t)'
%.elc: %.el
	$(BATCH) -eval $(WERROR) -f batch-byte-compile $<

qrencode.sh: qrencode.sh.tmpl qrencode.el
	sed -e '/#!!!REPLACEME!!!/ {' -e 'r qrencode.el' -e 'd' -e '}' $< > $@

.PHONY: clean
clean:
	$(info Cleaning up)
	@$(RM) README.html qrencode.elc qrencode.sh
