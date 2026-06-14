EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L .

EL = org-chronicle.el $(wildcard tests/*.el)

.PHONY: all compile checkdoc test package-lint clean

all: compile checkdoc test

compile:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(EL)

checkdoc:
	$(BATCH) --eval "(checkdoc-file \"org-chronicle.el\")"

test:
	$(BATCH) -l tests/org-chronicle-tests.el -f ert-run-tests-batch-and-exit

package-lint:
	$(BATCH) --eval "(progn (require 'package) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) (package-initialize) (unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint)))" -f package-lint-batch-and-exit org-chronicle.el
