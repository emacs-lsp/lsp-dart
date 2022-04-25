;;; lsp-dart-test.el --- Tests for lsp-dart-utils.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for lsp-dart-utils.el

;;; Code:

(require 'lsp-dart-utils)
(require 'el-mock)

(ert-deftest lsp-dart--flutter-repo-p--true-test ()
  (with-mock
    (mock (locate-dominating-file * "flutter") => "/sdk/bin")
    (mock (file-regular-p (f-join (f-root) "/sdk/bin/flutter")) => t)
    (mock (file-directory-p (f-join (f-root) "/sdk/bin/cache/dart-sdk")) => t)
    (should (lsp-dart--flutter-repo-p))))

(ert-deftest lsp-dart--flutter-repo-p--not-flutter-executable-test ()
  (with-mock
    (mock (locate-dominating-file * "flutter") => "/not-sdk/bin")
    (mock (file-regular-p "/sdk/bin/flutter") => nil)
    (should-not (lsp-dart--flutter-repo-p))))

(ert-deftest lsp-dart--flutter-repo-p--not-flutter-executable-test ()
  (with-mock
    (mock (locate-dominating-file * "flutter") => "/not-sdk/bin")
    (mock (file-regular-p (f-join (f-root) "/not-sdk/bin/flutter")) => t)
    (mock (file-directory-p (f-join (f-root) "/not-sdk/bin/cache/dart-sdk")) => nil)
    (should-not (lsp-dart--flutter-repo-p))))

(ert-deftest lsp-dart-flutter-project-p--flutter-repo-test ()
  (with-mock
    (mock (lsp-dart--flutter-repo-p) => t)
    (should (lsp-dart-flutter-project-p))))

(ert-deftest lsp-dart-flutter-project-p--flutter-project-test ()
  (lsp-dart-test-from-flutter-project
   (should (lsp-dart-flutter-project-p))))

(ert-deftest lsp-dart-flutter-project-p--dart-project-test ()
  (lsp-dart-test-from-dart-project
   (should-not (lsp-dart-flutter-project-p))))

(ert-deftest lsp-dart-flutter-project-p--other-project-test ()
  (should-not (lsp-dart-flutter-project-p)))

(ert-deftest lsp-dart-get-sdk-dir--custom-dir-test ()
  (let ((lsp-dart-sdk-dir "/some/sdk"))
    (should (equal (lsp-dart-get-sdk-dir) "/some/sdk"))))

(ert-deftest lsp-dart-get-sdk-dir--dart-project-with-dart-on-path-test ()
  (let ((dart-command (lsp-dart-test-command-fixture)))
    (lsp-dart-test-from-dart-project
     (mock (executable-find "dart") => dart-command)
     (should (equal (lsp-dart-get-sdk-dir) (concat (-> dart-command
                                                       f-parent
                                                       f-parent)
                                                   "/"))))))

(ert-deftest lsp-dart-get-sdk-dir--flutter-project-test ()
  (lsp-dart-test-from-flutter-project
   (mock (lsp-dart-flutter-project-p) => t)
   (mock (lsp-dart-get-flutter-sdk-dir) => "/flutter-sdk")
   (mock (file-exists-p "/flutter-sdk/bin/cache/dart-sdk/") => t)
   (should (equal (lsp-dart-get-sdk-dir)
                  (if (eq system-type 'windows-nt)
                      (f-join (f-root) "/flutter-sdk/bin/cache/dart-sdk/")
                    "/flutter-sdk/bin/cache/dart-sdk/")))))

(ert-deftest lsp-dart-get-sdk-dir--project-without-dart-on-path-test ()
  (lsp-dart-test-from-dart-project
   (mock (executable-find "dart") => nil)
   (should (equal (lsp-dart-get-sdk-dir) nil))))

(ert-deftest lsp-dart-get-flutter-sdk-dir--custom-dir-test ()
  (let ((lsp-dart-flutter-sdk-dir "/some/sdk"))
    (should (equal (lsp-dart-get-flutter-sdk-dir) "/some/sdk"))))

(ert-deftest lsp-dart-get-flutter-sdk-dir--with-flutter-on-path-test ()
  (let ((flutter-command (lsp-dart-test-flutter-command-fixture)))
    (lsp-dart-test-from-flutter-project
     (mock (executable-find "flutter") => flutter-command)
     (should (equal (lsp-dart-get-flutter-sdk-dir) (concat (-> flutter-command
                                                               f-parent
                                                               f-parent)
                                                           "/"))))))

(ert-deftest lsp-dart-get-flutter-sdk-dir--with-flutter-on-env-test ()
  (lsp-dart-test-from-flutter-project
   (mock (executable-find "flutter") => nil)
   (mock (getenv "FLUTTER_ROOT") => "/flutter/sdk")
   (should (equal (lsp-dart-get-flutter-sdk-dir) "/flutter/sdk"))))

(ert-deftest lsp-dart-pub-command--test ()
  (lsp-dart-test-with-dart-sdk
   (should (equal (lsp-dart-pub-command) (f-expand "bin/pub" dart-sdk)))))

(ert-deftest lsp-dart-pub-snapshot-command--test ()
  (lsp-dart-test-with-dart-sdk
   (should (equal (lsp-dart-pub-snapshot-command) (f-expand "bin/snapshots/pub.dart.snapshot" dart-sdk)))))

(ert-deftest lsp-dart-dart-command--test ()
  (lsp-dart-test-with-dart-sdk
   (should (equal (lsp-dart-dart-command) (f-expand (if (eq system-type 'windows-nt) "bin/dart.bat" "bin/dart") dart-sdk)))))

(ert-deftest lsp-dart-flutter-command--test ()
  (lsp-dart-test-with-flutter-sdk
   (should (equal (lsp-dart-flutter-command) (f-expand (if (eq system-type 'windows-nt) "bin/flutter.bat" "bin/flutter") flutter-sdk)))))

(ert-deftest lsp-dart-get-project-root--test ()
  (lsp-dart-test-from-dart-project
   (should (equal (lsp-dart-get-project-root) default-directory))))

(ert-deftest lsp-dart-get-project-entrypoint--lib-test ()
  (lsp-dart-test-from-dart-project
   (f-mkdir "lib")
   (f-touch "lib/main.dart")
   (should (equal (lsp-dart-get-project-entrypoint) (f-expand "lib/main.dart")))
   (f-delete "lib" t)))

(ert-deftest lsp-dart-get-project-entrypoint--lib-test ()
  (lsp-dart-test-from-dart-project
   (f-mkdir "bin")
   (f-touch "bin/main.dart")
   (should (equal (lsp-dart-get-project-entrypoint) (f-expand "bin/main.dart")))
   (f-delete "bin" t)))

(ert-deftest lsp-dart-log--without-args-test ()
  (should (equal (lsp-dart-log "test") "[LSP Dart] test")))

(ert-deftest lsp-dart-log--with-args-test ()
  (should (equal (lsp-dart-log "test %d %s" 1 "arg") "[LSP Dart] test 1 arg")))

(ert-deftest lsp-dart-custom-log--without-args-test ()
  (should (equal (lsp-dart-custom-log "Some" "test") "[LSP Dart] Some test")))

(ert-deftest lsp-dart-custom-log--with-args-test ()
  (should (equal (lsp-dart-custom-log "Some" "test %d %s" 1 "arg") "[LSP Dart] Some test 1 arg")))

(ert-deftest lsp-dart-workspace-status--non-nil-message-test ()
  (with-mock
    (mock (lsp-workspace-status "[LSP Dart] test" "workspace"))
    (lsp-dart-workspace-status "test" "workspace")))

(ert-deftest lsp-dart-workspace-status--nil-message-test ()
  (with-mock
    (mock (lsp-workspace-status nil "workspace"))
    (lsp-dart-workspace-status nil "workspace")))

(ert-deftest lsp-dart-get-full-dart-version--test ()
  (with-mock
    (mock (lsp-dart-dart-command) => "dart")
    (mock (shell-command-to-string "dart --version") => "Dart VM version: 2.9.0-4.0.dev")
    (should (equal (lsp-dart-get-full-dart-version) "Dart VM version: 2.9.0-4.0.dev"))))

(ert-deftest lsp-dart--get-dart-version--test ()
  (with-mock
    (mock (lsp-dart-dart-command) => "dart")
    (mock (shell-command-to-string "dart --version") => "Dart VM version: 2.9.0-4.0.dev")
    (should (equal (lsp-dart-get-dart-version) "2.9.0-4.0.dev"))))

(ert-deftest lsp-dart-version->number--dart-test ()
  (should (equal (lsp-dart-version->number "2.9.0-dev.10.0") "2.9.0.0.10.0")))

(ert-deftest lsp-dart-version->number--flutter-test ()
  (should (equal (lsp-dart-version->number "2.9.0-5.0.dev.10-flutter-4da5b40fb6") "2.9.0.5.0.0.10.0")))

(ert-deftest lsp-dart-version-at-least-p--a-test ()
  (with-mock
    (mock (lsp-dart-get-dart-version) => "2.9.0-dev.10.0")
    (should (lsp-dart-version-at-least-p "2.9.0-dev.9.0"))))

(ert-deftest lsp-dart-version-at-least-p--b-test ()
  (with-mock
    (mock (lsp-dart-get-dart-version) => "2.9.0-dev.10.0")
    (should-not (lsp-dart-version-at-least-p "2.9.0-dev.11.0"))))

(ert-deftest lsp-dart-version-at-least-p--c-test ()
  (with-mock
    (mock (lsp-dart-get-dart-version) => "2.10.0")
    (should (lsp-dart-version-at-least-p "2.9.0-dev.11.0"))))

(ert-deftest lsp-dart-version-at-least-p--d-test ()
  (with-mock
    (mock (lsp-dart-get-dart-version) => "2.8.0")
    (should-not (lsp-dart-version-at-least-p "2.9.0-dev.11.0"))))

(ert-deftest lsp-dart-version-at-least-p--e-test ()
  (with-mock
    (mock (lsp-dart-get-dart-version) => "2.9.0-5.0.dev.10-flutter-4da5b40fb6")
    (should-not (lsp-dart-version-at-least-p "2.9.0-5.0.dev.11-flutter-asd"))))

(ert-deftest lsp-dart-version-at-least-p--f-test ()
  (with-mock
    (mock (lsp-dart-get-dart-version) => "2.9.0-5.0.dev.10-flutter-4da5b40fb6")
    (should (lsp-dart-version-at-least-p "2.9.0-5.0.dev.09-flutter-asd"))))

(ert-deftest lsp-dart-version-at-least-p--g-test ()
  (with-mock
    (mock (lsp-dart-get-dart-version) => "2.10.0-dev.1.2")
    (should (lsp-dart-version-at-least-p "2.10.0-dev.1.2"))))

(ert-deftest lsp-dart-assert-sdk-min-version ()
  (with-mock
    (mock (lsp-dart-version-at-least-p "2.8.0") => t)
    (lsp-dart-assert-sdk-min-version "2.8.0")))

;;; lsp-dart-utils-test.el ends here
