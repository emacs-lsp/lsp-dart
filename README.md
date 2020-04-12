# lsp-dart

<img align="right" width="64" alt="dart_logo" src="https://user-images.githubusercontent.com/7820865/78992432-9acce080-7b11-11ea-8576-5c7b72e4be5d.png">

[![Build Status](https://travis-ci.com/emacs-lsp/lsp-dart.svg?branch=master)](https://travis-ci.com/emacs-lsp/lsp-dart) 
[![Gitter](https://badges.gitter.im/emacs-lsp/lsp-mode.svg)](https://gitter.im/emacs-lsp/lsp-mode)


Emacs Dart IDE using [lsp-mode](https://github.com/emacs-lsp/lsp-mode) to connect to [Dart Analysis Server](https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server).

<img width="22" alt="flutter_logo" align="top" src="https://user-images.githubusercontent.com/7820865/78991346-1aa57b80-7b0f-11ea-9992-25cd1a9ac974.png"> LSP works great with [Flutter](https://flutter.dev/), but there are also other packages that may help to improve your development. See [Additional packages](#additional-packages) for more info.

## Quickstart

An example with a minimal configuration to start using `lsp-dart`:
```elisp
(use-package lsp-mode :ensure t)

(use-package lsp-dart 
  :ensure t 
  :hook (dart-mode . lsp))

;; Optional packages
(use-package lsp-ui :ensure t)
(use-package company-capf :ensure t)

```

## Features

Besides the `lsp-mode` features, `lsp-dart` implements the [custom methods features from the `dart_analysis_server` ](https://github.com/dart-lang/sdk/blob/master/pkg/analysis_server/tool/lsp_spec/README.md#custom-methods).

### Closing labels

![closing-labels](https://github.com/emacs-lsp/lsp-dart/blob/master/screenshots/closing-labels.png)

### Tree views
`lsp-dart` uses `lsp-treemacs` for rendering some tree views features.

**`lsp-dart-show-outline`**

![outline](https://github.com/emacs-lsp/lsp-dart/blob/master/screenshots/outline.png)

**`lsp-dart-show-flutter-outline`**

![flutter-outline](https://github.com/emacs-lsp/lsp-dart/blob/master/screenshots/flutter-outline.png)

### Run tests

Run dart or flutter tests.

![test](https://user-images.githubusercontent.com/7820865/79058546-cea92280-7c45-11ea-9a31-c55f3301f949.gif)

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
* [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) : `lsp-mode` GUI controls implemented using treemacs.
* [company-capf](https://github.com/company-mode/company-mode) : Completion backend support.
* [flutter.el](https://github.com/amake/flutter.el) : Tool to run emulator and tests from emacs.
* [hover.el](https://github.com/ericdallo/hover.el) : Tool to run flutter mobile apps from desktop without the need of an emulator.

## FAQ

:grey_question: Emacs can't find my package, it keeps searching up until the dart root package folder.

:small_blue_diamond: Configure projectile to find the package pubspec.yaml and set the folder as project root:
```elisp
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml"))
```

:grey_question: `LSP :: No LSP server for dart-mode(check *lsp-log*).`

:small_blue_diamond: Try to set the `lsp-dart-sdk-dir` to the Dart SDK dir instalation or if you are using Flutter, `<your-flutter-dir>/bin/cache/dart-sdk/`.
