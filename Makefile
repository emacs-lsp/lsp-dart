SHELL=/usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

build:
	$(EASK) package
	$(EASK) install

ci: clean build compile checkdoc lint test

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	$(EASK) lint checkdoc

lint:
	@echo "package linting..."
	$(EASK) lint package

test:
	$(EASK) install-deps --dev
	$(EASK) test ert ./test/*.el

clean:
	$(EASK) clean-all

tag:
	$(eval TAG := $(filter-out $@,$(MAKECMDGOALS)))
	sed -i "s/;; Version: [0-9]\+.[0-9]\+.[0-9]\+/;; Version: $(TAG)/g" lsp-dart.el
	sed -i "s/lsp-dart-version-string \"[0-9]\+.[0-9]\+.[0-9]\+\"/lsp-dart-version-string \"$(TAG)\"/g" lsp-dart.el
	sed -i "s/\"lsp-dart\" \"[0-9]\+.[0-9]\+.[0-9]\+\"/\"lsp-dart\" \"$(TAG)\"/g" Eask
	git add lsp-dart.el Eask
	git commit -m "Bump lsp-dart: $(TAG)"
	git tag $(TAG)
	git push origin HEAD
	git push origin --tags

# Allow args to make commands
%:
	@:

.PHONY : ci compile checkdoc lint test clean tag
