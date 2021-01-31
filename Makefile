SHELL=/usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

WINDOWS-INSTALL=-l test/windows-bootstrap.el

INIT="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (package-refresh-contents))"

LINT="(progn \
		(unless (package-installed-p 'package-lint) \
		  (package-install 'package-lint)) \
		(require 'package-lint) \
		(setq package-lint-main-file \"lsp-dart.el\") \
		(package-lint-batch-and-exit))"

build:
	cask install

unix-ci: WINDOWS-INSTALL=
unix-ci: clean build compile checkdoc lint unix-test

windows-ci: CASK=
windows-ci: clean checkdoc

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		$(WINDOWS-INSTALL) \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		*.el

checkdoc:
	$(eval LOG := $(shell mktemp -d)/checklog.log)
	@touch $(LOG)

	@echo "checking doc..."

	@for f in *.el ; do \
		$(CASK) $(EMACS) -Q --batch \
			-L . \
			--eval "(checkdoc-file \"$$f\")" \
			*.el 2>&1 | tee -a $(LOG); \
	done

	@if [ -s $(LOG) ]; then \
		echo ''; \
		exit 1; \
	else \
		echo 'checkdoc ok!'; \
	fi

lint:
	@echo "package linting..."
	@$(CASK) $(EMACS) -Q --batch \
		$(WINDOWS-INSTALL) \
		-L . \
		--eval $(INIT) \
		--eval $(LINT) \
		*.el

unix-test:
	@$(CASK) exec ert-runner

windows-test:
	@$(EMACS) -Q --batch \
		$(WINDOWS-INSTALL) \
		-L . \
		$(LOAD-TEST-FILES) \
		--eval "(ert-run-tests-batch-and-exit \
		'(and (not (tag no-win)) (not (tag org))))"

clean:
	rm -rf .cask *.elc

tag:
	$(eval TAG := $(filter-out $@,$(MAKECMDGOALS)))
	sed -i "s/;; Version: [0-9]\+.[0-9]\+.[0-9]\+/;; Version: $(TAG)/g" lsp-dart.el
	git add lsp-dart.el
	git commit -m "Bump lsp-dart: $(TAG)"
	git tag $(TAG)
	git push origin HEAD
	git push origin --tags

# Allow args to make commands
%:
	@:

.PHONY : ci compile checkdoc lint unix-test windows-test clean tag
