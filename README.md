# lsp-dart

[![Build Status](https://travis-ci.com/emacs-lsp/lsp-dart.svg?branch=master)](https://travis-ci.com/emacs-lsp/lsp-dart)

Emacs Dart IDE using [lsp-mode](https://github.com/emacs-lsp/lsp-mode) to connect to [Dart Analysis Server](https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server).

## Quickstart

An example with a minimal configuration to start using `lsp-dart`:
```elisp
(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t)
(use-package company-lsp :ensure t)

(use-package lsp-dart 
  :ensure t 
  :hook (dart-mode . lsp))
```

## Features
Beside the `lsp-mode` features, `lsp-dart` implements the [`dart_analysis_server` custom methods](https://github.com/dart-lang/sdk/blob/master/pkg/analysis_server/tool/lsp_spec/README.md#custom-methods) features too.

**Closing labels**

![closing-labels](https://github.com/emacs-lsp/lsp-dart/blob/master/screenshots/closing-labels.png)

### Treemacs views
`lsp-dart` uses `lsp-treemacs` for rendering some tree views features.

**`lsp-dart-show-outline`**

![outline](https://github.com/emacs-lsp/lsp-dart/blob/master/screenshots/outline.png)

**`lsp-dart-show-flutter-outline`**

![flutter-outline](https://github.com/emacs-lsp/lsp-dart/blob/master/screenshots/flutter-outline.png)

## Supported settings

* `lsp-dart-sdk-dir` - Install directory for dart-sdk.
* `lsp-dart-server-command` - The analysis_server executable to use.
* `lsp-dart-only-analyze-projects-with-open-files` - analysis will only be performed for projects that have open files rather than the root workspace folder.
* `lsp-dart-suggest-from-unimported-libraries` - completion will not include symbols that are not already imported into the current file.
* `lsp-dart-closing-labels` - Enable the closing labels feature on server lsp.
* `lsp-dart-closing-labels-prefix` - The prefix string to be concatened with the closing label.
* `lsp-dart-closing-labels-size` - The font size factor to be multiplied by the closing labels font size.
* `lsp-dart-outline` - Enable the outline tree view feature on server lsp.
* `lsp-dart-flutter-outline` - Enable the Flutter outline tree view feature on server lsp.
* `lsp-dart-outline-position-params` - The outline tree position params. Defaults to side.
* `lsp-dart-flutter-outline-position-params` - The Flutter outline tree position params. Defaults to side.

## Additional packages
* [lsp-ui](https://github.com/emacs-lsp/lsp-ui) : Flycheck, documentation and code actions support.
* [company-lsp](https://github.com/tigersoldier/company-lsp) : LSP company backend.
* [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) : `lsp-mode` GUI controls implemented using treemacs.

## FAQ

* Q. Emacs can't find my package, it keeps searching up until the dart root package folder.

Configure projectile to find the package pubspec.yaml and set the folder as project root:
```elisp
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml"))
```
