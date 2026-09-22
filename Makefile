# Tests for this configuration. See .emacs.d/tests/README.org.
#
# Everything runs under `emacs -Q' against the tangled init.el, so a failure
# means the configuration is wrong rather than that a copy of it drifted.

EMACS ?= $(shell command -v emacs 2>/dev/null || echo /Applications/Emacs.app/Contents/MacOS/Emacs)
TESTS := $(wildcard .emacs.d/tests/*-tests.el)

# A glob that matches nothing makes the loop below run zero times and exit 0 --
# a rename or a move would turn the suite off and still report success. There
# is no version of this repository with no tests, so an empty list is a bug.
ifeq ($(strip $(TESTS)),)
$(error No test files matched .emacs.d/tests/*-tests.el)
endif

.PHONY: test test-elisp test-strict test-shell tangle clean-elc

test: test-elisp test-shell

## Run the Emacs Lisp suites, each in its own process so one cannot leak
## state into another.
test-elisp:
	@command -v "$(EMACS)" >/dev/null 2>&1 || { \
	  echo "No Emacs at '$(EMACS)'. Set EMACS=/path/to/emacs." >&2; exit 1; }
	@status=0; \
	for f in $(TESTS); do \
	  echo "==> $$f"; \
	  "$(EMACS)" -Q --batch -l "$$f" -f ert-run-tests-batch-and-exit || status=1; \
	done; \
	exit $$status

## Everything `test' runs, with skipping disabled. Tests needing a package skip
## when it is absent, and a skip is indistinguishable from a pass in the exit
## status -- so a skip predicate that quietly broke would report green while
## checking nothing. Run this where the packages are installed, i.e. the machine
## this configuration actually runs on. It is not what CI runs: a runner has no
## straight directory, and every package-dependent suite would fail by design.
##
## It must stay a superset of `test'. A target named "strict" that ran fewer
## checks than the plain one would be precisely the failure this suite exists
## to catch, so test-shell is a prerequisite rather than an afterthought.
test-strict: test-shell
	@CFG_TEST_STRICT=1 $(MAKE) --no-print-directory test-elisp

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
