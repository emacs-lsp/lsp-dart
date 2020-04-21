# lsp-dart

<img align="right" width="64" alt="dart_logo" src="https://user-images.githubusercontent.com/7820865/78992432-9acce080-7b11-11ea-8576-5c7b72e4be5d.png">

[![MELPA](https://melpa.org/packages/lsp-dart-badge.svg)](https://melpa.org/#/lsp-dart)
[![MELPA](https://stable.melpa.org/packages/lsp-dart-badge.svg)](https://stable.melpa.org/#/lsp-dart)
[![Build Status](https://travis-ci.com/emacs-lsp/lsp-dart.svg?branch=master)](https://travis-ci.com/emacs-lsp/lsp-dart) 
[![Gitter](https://badges.gitter.im/emacs-lsp/lsp-mode.svg)](https://gitter.im/emacs-lsp/lsp-mode)

Emacs Dart IDE using [lsp-mode](https://github.com/emacs-lsp/lsp-mode) to connect to [Dart Analysis Server](https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server).

<img width="22" alt="flutter_logo" align="top" src="https://user-images.githubusercontent.com/7820865/78991346-1aa57b80-7b0f-11ea-9992-25cd1a9ac974.png"> LSP works great with [Flutter](https://flutter.dev/), but there are also other packages that may help to improve your development, for more info see [Additional packages](#additional-packages).

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

Besides the `lsp-mode` features, `lsp-dart` implements the [custom methods features from the `dart_analysis_server`](https://github.com/dart-lang/sdk/blob/master/pkg/analysis_server/tool/lsp_spec/README.md#custom-methods).

### Closing labels

![closing-labels](https://raw.githubusercontent.com/emacs-lsp/lsp-dart/screenshots/closing-labels.png)

### Tree views

`lsp-dart` uses `lsp-treemacs` for rendering some tree views features.

**`lsp-dart-show-outline`** [:warning:*](#warning-features-only-available-for-dart-sdk-version-280-currently-the-dev-branch-and-above)

![outline](https://raw.githubusercontent.com/emacs-lsp/lsp-dart/screenshots/outline.png)

**`lsp-dart-show-flutter-outline`** [:warning:*](#warning-features-only-available-for-dart-sdk-version-280-currently-the-dev-branch-and-above)

![flutter-outline](https://raw.githubusercontent.com/emacs-lsp/lsp-dart/screenshots/flutter-outline.gif)

### Run tests

`lsp-dart-run-test-file` - Run all tests from current test buffer.

`lsp-dart-run-test-at-point` - Run single test at point. [:warning:*](#warning-features-only-available-for-dart-sdk-version-280-currently-the-dev-branch-and-above)

Running a test interactively: 

![test](https://raw.githubusercontent.com/emacs-lsp/lsp-dart/screenshots/run-test.gif)

### Flutter colors

Display the flutter colors on left fringe.

![flutter-fringe-colors](https://raw.githubusercontent.com/emacs-lsp/lsp-dart/screenshots/flutter-fringe-colors.gif)

You can disable the feature setting `lsp-dart-fringe-colors` to `nil`.

### Flutter widget guides

Display widget guide lines from parent to children widgets on flutter. [:warning:*](#warning-features-only-available-for-dart-sdk-version-280-currently-the-dev-branch-and-above)

![flutter-widget-guides](https://raw.githubusercontent.com/emacs-lsp/lsp-dart/screenshots/flutter-widget-guides.png)

You can disable the feature setting `lsp-dart-flutter-widget-guides` to `nil`.

### Debug

For emacs debugging, `lsp-dart` uses [`dap-mode`](https://github.com/emacs-lsp/dap-mode#dart) to debug.
You only need to run `dap-dart-setup` one time to setup the debugger.

![debug](https://raw.githubusercontent.com/emacs-lsp/lsp-dart/screenshots/debug.gif)

##### :warning:* Features only available for Dart SDK version 2.8.0 (currently the dev branch) or above.

## Supported settings

* `lsp-dart-sdk-dir` - Install directory for dart-sdk.
* `lsp-dart-server-command` - The analysis_server executable to use.
* `lsp-dart-only-analyze-projects-with-open-files` - analysis will only be performed for projects that have open files rather than the root workspace folder.
* `lsp-dart-suggest-from-unimported-libraries` - completion will not include symbols that are not already imported into the current file.
* `lsp-dart-closing-labels` - Enable the closing labels feature on server lsp.
* `lsp-dart-closing-labels-prefix` - The prefix string to be concatenated with the closing label.
* `lsp-dart-closing-labels-size` - The font size factor to be multiplied by the closing labels font size.
* `lsp-dart-outline` - Enable the outline tree view feature on server lsp.
* `lsp-dart-flutter-outline` - Enable the Flutter outline tree view feature on server lsp.
* `lsp-dart-outline-position-params` - The outline tree position params. Defaults to side.
* `lsp-dart-flutter-outline-position-params` - The Flutter outline tree position params. Defaults to side.
* `lsp-dart-fringe-colors` - Enable the Flutter colors on fringe. Defaults to t.
* `lsp-dart-flutter-widget-guides` - Enable the Flutter widget guide lines from parent to children widgets. Defaults to t.

## Additional packages
* [lsp-ui](https://github.com/emacs-lsp/lsp-ui) : Flycheck, documentation and code actions support.
* [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) : `lsp-mode` GUI controls implemented using treemacs.
* [company-capf](https://github.com/company-mode/company-mode) : Completion backend support.
* [flutter.el](https://github.com/amake/flutter.el) : Tool to run emulator from emacs.
* [hover.el](https://github.com/ericdallo/hover.el) : Tool to run flutter mobile apps from desktop without the need of an emulator.

## FAQ

:grey_question: Emacs can't find my package, it keeps searching up until the dart root package folder.

:small_blue_diamond: Configure projectile to find the package pubspec.yaml and set the folder as project root:
```elisp
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
```

:grey_question: `LSP :: No LSP server for dart-mode(check *lsp-log*).`

:small_blue_diamond: Try to set the `lsp-dart-sdk-dir` to the Dart SDK dir instalation or if you are using Flutter, `<your-flutter-dir>/bin/cache/dart-sdk/`.

## Community
All feedback and suggestions are welcome!

You can [open a issue](https://github.com/emacs-lsp/lsp-dart/issues/new/choose) or for a quick anwser, send a message on [Gitter](https://gitter.im/emacs-lsp/lsp-mode).
