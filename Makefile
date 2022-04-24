SHELL=/usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

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

ARCHIVES-INIT="(progn \
  (require 'package) \
  (setq package-archives '((\"melpa\" . \"https://melpa.org/packages/\") \
						   (\"gnu\" . \"http://elpa.gnu.org/packages/\"))))"

build:
	$(EASK) package
	$(EASK) install

ci: clean build compile checkdoc lint test

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	$(EASK) checkdoc

lint:
	@echo "package linting..."
	$(EASK) lint

test:
	$(EASK) install-deps --dev
	$(EASK) ert

clean:
	$(EASK) clean-all

tag:
	$(eval TAG := $(filter-out $@,$(MAKECMDGOALS)))
	sed -i "s/;; Version: [0-9]\+.[0-9]\+.[0-9]\+/;; Version: $(TAG)/g" lsp-dart.el
	sed -i "s/lsp-dart-version-string \"[0-9]\+.[0-9]\+.[0-9]\+\"/lsp-dart-version-string \"$(TAG)\"/g" lsp-dart.el
	git add lsp-dart.el
	git commit -m "Bump lsp-dart: $(TAG)"
	git tag $(TAG)
	git push origin HEAD
	git push origin --tags

# Allow args to make commands
%:
	@:

.PHONY : ci compile checkdoc lint test clean tag
