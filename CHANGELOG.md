# Changelog

## 1.19.0

* Supports web debug on google chrome via dap-mode.
* Deprecate `dap-dart-setup`, use `lsp-dart-dap-setup` now.
* Improve debugger setup downloading directly from Dart-Code repo.
* Bump debugger from 3.19.2 to 3.23.0.
* Fix device label for web/chrome

## 1.18.3

* Improve `lsp-dart-version` command with more details to help debug issues

## 1.18.2

* Support lsp-dart on pubspec.yaml, allowing completions

## 1.18.1

* Change the default of `lsp-dart-only-analyze-projects-with-open-files` to `nil` as a recommendation from Dart LSP server.

## 1.18.0

* Add `lsp-dart-show-todos` as false by default.
* Add `lsp-dart-complete-function-calls` as true by default.

## 1.17.14

* Fix cache when debugging devices/emulators - Fixes #96

## 1.17.13

* Bump debugger extension: 3.17 -> 3.19.2

## 1.17.9
* Remove group name from test name on test tree making it clear.

## 1.17.8
* Fix tests trees with latest dart sdk 2.10.0.

## 1.17.6
* Add `lsp-dart-flutter-executable`.

## 1.17.0
* Add test tree feature.
* Add `lsp-dart-test-show-tree`.

## 1.16.0
* Improve tests backend and output.
* Prepare for test tree feature.

## 1.15.0
* Add `lsp-dart-enable-sdk-formatter` setting.
* Add `lsp-dart-line-length` setting.

## 1.14.0
* Add `lsp-dart-pub-get` command.
* Add `lsp-dart-pub-upgrade` command.
* Add `lsp-dart-pub-outdated` command.
* Add keybindings to all lsp-dart test commands.

## 1.13.4
* Safe checks when dart sdk is not present for most commands usage.

## 1.13.3
* Improve flutter widget guide lines performance.

## 1.13.1
* Fix anaylerStatus notification performance.

## 1.13.0
* Drop emacs 25.X support.
* Add mode to most features like `closing-labels`, `outline`, `widget-guide-lines` and `flutter-fringe-colors` .

## 1.12.0
* Migrate lsp-dart to use lsp-protocol following lsp-mode.

## 1.11.9
* Add Dart SDK version to `lsp-dart-version` command.

## 1.11.0
* Add main code lens support, can be disable setting `lsp-dart-main-code-lens` to `nil`.

## 1.10.5
* Add `lsp-dart-run-all-tests` command.

## 1.10.0
* Support for debugging Dart/Flutter tests.
* Add "Debug" code lens on tests.
* Add `lsp-dart-debug-test-at-point` command.
* Add `lsp-dart-debug-last-test` command.

## 1.9.0
* Add `lsp-dart-visit-last-test` command.
* Add `lsp-dart-run-last-test` command.

## 1.8.0
* Flutter debug support using DAP.
* Flutter DevTools support debugging.

## 1.7.0
* Support for open DevTools in the browser for the current debug session.

## 1.6.7
* Add `lsp-dart-version` command.

## 1.6.0
* Support for Flutter widgets guides on widget tree.

## 1.5.0
* Support for Dart debug via `dap-mode` using the DAP (Debugger Adapter Protocol).

## 1.4.1
* Change hook for update the Flutter colors on fringe.

## 1.4.0
* Add Flutter colors on Emacs fringe.

## 1.3.0
* Add test support for Dart/Flutter tests.
* Run all buffer tests or a test at point.

## 1.2.0
* Add actions on outline tree view with mouse support.

## 1.1.0
* Smart search for Dart SDK on $PATH.

## 1.0.0
* Migrate from `lsp-mode`.
