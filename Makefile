# Tests for this configuration. See .emacs.d/tests/README.org.
#
# Everything runs under `emacs -Q' against the tangled init.el, so a failure
# means the configuration is wrong rather than that a copy of it drifted.

EMACS ?= $(shell command -v emacs 2>/dev/null || echo /Applications/Emacs.app/Contents/MacOS/Emacs)
TESTS := $(wildcard .emacs.d/tests/*-tests.el)

.PHONY: test test-elisp test-shell tangle clean-elc

test: test-elisp test-shell

## Run the Emacs Lisp suites, each in its own process so one cannot leak
## state into another.
test-elisp:
	@status=0; \
	for f in $(TESTS); do \
	  echo "==> $$f"; \
	  "$(EMACS)" -Q --batch -l "$$f" -f ert-run-tests-batch-and-exit || status=1; \
	done; \
	exit $$status

test-shell:
	@echo "==> .emacs.d/tests/claude-acct-tests.sh"
	@.emacs.d/tests/claude-acct-tests.sh

## Regenerate init.el from Emacs.org. `make test' checks the committed copy
## already matches, so this is only needed after editing the org file.
tangle:
	@cd .emacs.d && "$(EMACS)" --batch \
	  --eval "(require 'org)" \
	  --eval '(org-babel-tangle-file "Emacs.org")'

clean-elc:
	@find .emacs.d/tests -name '*.elc' -delete
