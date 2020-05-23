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
    (mock (file-regular-p "/sdk/bin/flutter") => t)
    (mock (file-directory-p "/sdk/bin/cache/dart-sdk") => t)
    (should (lsp-dart--flutter-repo-p))))

(ert-deftest lsp-dart--flutter-repo-p--not-flutter-executable-test ()
  (with-mock
    (mock (locate-dominating-file * "flutter") => "/not-sdk/bin")
    (mock (file-regular-p "/sdk/bin/flutter") => nil)
    (should-not (lsp-dart--flutter-repo-p))))

(ert-deftest lsp-dart--flutter-repo-p--not-flutter-executable-test ()
  (with-mock
    (mock (locate-dominating-file * "flutter") => "/not-sdk/bin")
    (mock (file-regular-p "/not-sdk/bin/flutter") => t)
    (mock (file-directory-p "/not-sdk/bin/cache/dart-sdk") => nil)
    (should-not (lsp-dart--flutter-repo-p))))

(ert-deftest lsp-dart--flutter-project-p--flutter-repo-test ()
  (with-mock
    (mock (lsp-dart--flutter-repo-p) => t)
    (should (lsp-dart--flutter-project-p))))

(ert-deftest lsp-dart--flutter-project-p--flutter-project-test ()
  (lsp-dart-test-from-flutter-project
   (should (lsp-dart--flutter-project-p))))

(ert-deftest lsp-dart--flutter-project-p--dart-project-test ()
  (lsp-dart-test-from-dart-project
   (should-not (lsp-dart--flutter-project-p))))

(ert-deftest lsp-dart--flutter-project-p--other-project-test ()
  (should-not (lsp-dart--flutter-project-p)))

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
   (mock (lsp-dart-get-flutter-sdk-dir) => "/flutter-sdk")
   (should (equal (lsp-dart-get-sdk-dir) "/flutter-sdk/bin/cache/dart-sdk/"))))

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
   (should (equal (lsp-dart-pub-command) (expand-file-name "bin/pub" dart-sdk)))))

(ert-deftest lsp-dart-pub-snapshot-command--test ()
  (lsp-dart-test-with-dart-sdk
   (should (equal (lsp-dart-pub-snapshot-command) (expand-file-name "bin/snapshots/pub.dart.snapshot" dart-sdk)))))

(ert-deftest lsp-dart-dart-command--test ()
  (lsp-dart-test-with-dart-sdk
   (should (equal (lsp-dart-dart-command) (expand-file-name "bin/dart" dart-sdk)))))

(ert-deftest lsp-dart-flutter-command--test ()
  (lsp-dart-test-with-flutter-sdk
   (should (equal (lsp-dart-flutter-command) (expand-file-name "bin/flutter" flutter-sdk)))))

;;; lsp-dart-utils-test.el ends here
