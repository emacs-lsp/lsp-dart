lsp-dart
=========

<img align="right" width="64" alt="dart_logo" src="images/logo.svg">

[![MELPA](https://melpa.org/packages/lsp-dart-badge.svg)](https://melpa.org/#/lsp-dart)
[![MELPA stable](https://stable.melpa.org/packages/lsp-dart-badge.svg)](https://stable.melpa.org/#/lsp-dart)
[![CI](https://github.com/emacs-lsp/lsp-dart/workflows/CI/badge.svg)](https://github.com/emacs-lsp/lsp-dart/actions) 
[![Gitter](https://badges.gitter.im/emacs-lsp/lsp-mode.svg)](https://gitter.im/emacs-lsp/lsp-mode)

Emacs Dart IDE using [lsp-mode](https://github.com/emacs-lsp/lsp-mode) to connect to [Dart Analysis Server](https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server).

<img width="22" alt="flutter_logo" align="top" src="images/flutter-mini-logo.png"> LSP works great with [Flutter](https://flutter.dev/), but there are also other packages that may help to improve your development, for more info see [Additional packages](#additional-packages).

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

![closing-labels](images/closing-labels.png)

### Tree views

`lsp-dart` uses `lsp-treemacs` for rendering some tree views features.

**`lsp-dart-show-outline`** [:warning:*](#features-only-available-for-dart-sdk-version-280-currently-the-dev-branch-or-above)

![outline](images/outline.png)

**`lsp-dart-show-flutter-outline`** [:warning:*](#features-only-available-for-dart-sdk-version-280-currently-the-dev-branch-or-above)

![flutter-outline](images/flutter-outline.gif)

### Run tests

`lsp-dart-run-test-file` - Run all tests from current test buffer.

`lsp-dart-run-test-at-point` - Run single test at point. [:warning:*](#features-only-available-for-dart-sdk-version-280-currently-the-dev-branch-or-above)

Running a test interactively: 

![test](images/run-test.gif)

### Flutter colors

Display the flutter colors on left fringe.

![flutter-fringe-colors](images/flutter-fringe-colors.gif)

You can disable the feature setting `lsp-dart-flutter-fringe-colors` to `nil`.

### Flutter widget guides

Display widget guide lines from parent to child widgets on flutter. [:warning:*](#features-only-available-for-dart-sdk-version-280-currently-the-dev-branch-or-above)

![flutter-widget-guides](images/flutter-widget-guides.png)

You can disable the feature setting `lsp-dart-flutter-widget-guides` to `nil`.

### Debug

For debugging, `lsp-dart` uses [`dap-mode`](https://github.com/emacs-lsp/dap-mode#dart).
You only need to run `dap-dart-setup` one time to setup the debugger to your Emacs and `dap-debug` to start the debug.

![debug](images/debug.gif)

#### Flutter

`lsp-dart` support Flutter debugging too via `dap-debug` with an option to select the device to debug.

![flutter-debug](images/flutter-debug.gif)

#### Custom templates

You can register a custom template for debugging with `dap-register-debug-template`, check the following example:

```elisp
(use-package lsp-dart 
  :ensure t 
  :hook (dart-mode . lsp)
  :init
  (dap-register-debug-template "Flutter :: Custom debug"
                               (list :flutterPlatform "x86_64"
                                     :args '("--flavor" "staging"))))
```

#### DevTools

You can also open the [Dart DevTools](https://dart.dev/tools/dart-devtools) on the current debug session with `lsp-dart-dap-devtools-open`.

###### :warning:* Features only available for Dart SDK version 2.8.0 (currently the dev branch) or above.

## Supported settings

| Variable                                         | Description                                                                                             | Default                               |
|:-------------------------------------------------|:--------------------------------------------------------------------------------------------------------|:--------------------------------------|
| `lsp-dart-project-sdk-dir`                       | Install directory for dart-sdk                                                                          | `$PATH`                               |
| `lsp-dart-project-flutter-command`               | The flutter executable path                                                                             | "flutter" that may be in the `$PATH`. |
| `lsp-dart-server-command`                        | `analysis_server` executable to use                                                                     | Check source file                     |
| `lsp-dart-extra-library-directories`             | Extra libs to analyze besides Dart SDK libs                                                             | `'()`                                 |
| `lsp-dart-only-analyze-projects-with-open-files` | Analysis will only be performed for projects that have open files rather than the root workspace folder | `t`                                   |
| `lsp-dart-suggest-from-unimported-libraries`     | Completion will not include symbols that are not already imported into the current file.                | `t`                                   |
| `lsp-dart-closing-labels`                        | Enable the closing labels feature on server lsp                                                         | `t`                                   |
| `lsp-dart-closing-labels-prefix`                 | The prefix string to be concatenated with the closing label                                             | `""`                                  |
| `lsp-dart-closing-labels-size`                   | The font size factor to be multiplied by the closing labels font size                                   | 0.9                                   |
| `lsp-dart-outline`                               | Enable the outline tree view feature on server lsp                                                      | `t`                                   |
| `lsp-dart-outline-position-params`               | The outline tree position params.                                                                       | Left side                             |
| `lsp-dart-flutter-outline`                       | Wheter to enable the Flutter outline tree view feature on server lsp                                    | `t`                                   |
| `lsp-dart-flutter-outline-position-params`       | The Flutter outline tree position params                                                                | Left side                             |
| `lsp-dart-flutter-fringe-colors`                 | Enable the Flutter colors on fringe.                                                                    | `t`                                   |
| `lsp-dart-flutter-widget-guides`                 | Enable the Flutter widget guide lines from parent to child widgets                                      | `t`                                   |
| `lsp-dart-test-code-lens`                        | Enable the `Run` code lens on tests.                                                                    | `t`                                   |
| `lsp-dart-dap-extension-version`                 | The debugger extension version.                                                                         | 3.9.1                                 |
| `lsp-dart-dap-debugger-path`                     | The debugger extension path                                                                             | Check source file                     |
| `lsp-dart-dap-dart-debugger-program`             | The command to execute the debugger extension on dart projects.                                         | Check source file                     |
| `lsp-dart-dap-flutter-debugger-program`          | The command to execute the debugger extension on flutter projects.                                      | Check source file                     |
| `lsp-dart-dap-debug-external-libraries`          | Whether to enable the debug on external libraries                                                       | `nil`                                 |
| `lsp-dart-dap-debug-sdk-libraries`               | Whether to enable the debug on Dart SDK libraries                                                       | `nil`                                 |
| `lsp-dart-dap-flutter-track-widget-creation`     | Whether to pass –track-widget-creation to Flutter apps. Required to support 'Inspect Widget'.           | `t`                                   |
| `lsp-dart-dap-flutter-structured-errors`         | Whether to use Flutter’s structured error support for improve error display.                            | `t`                                   |
| `lsp-dart-dap-flutter-verbose-log`               | Whether to enable verbose logs from Flutter DAP                                                         | `nil`                                 |
| `lsp-dart-dap-devtools-theme`                    | The devtools theme when openning via `lsp-dart-dap-open-devtools`                                       | `"dark"`                              |
| `lsp-dart-dap-devtools-hide-options`             | What to hide when openning DevTools via `lsp-dart-dap-open-devtools`                                    | `debugger`                            |

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

:small_blue_diamond: Try to set the `lsp-dart-project-sdk-dir` to the Dart SDK dir instalation or if you are using Flutter, `<your-flutter-dir>/bin/cache/dart-sdk/`.

## Community
All feedback and suggestions are welcome!

You can [open a issue](https://github.com/emacs-lsp/lsp-dart/issues/new/choose) or for a quick anwser, send a message on [Gitter](https://gitter.im/emacs-lsp/lsp-mode).
