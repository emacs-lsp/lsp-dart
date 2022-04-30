;;; lsp-dart-test.el --- Tests for lsp-dart.el

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

;; Tests for lsp-dart.el

;;; Code:

(require 'lsp-dart)
(require 'el-mock)

(ert-deftest lsp-dart--library-folders--non-lib-file-test ()
  (with-mock
    (stub lsp-dart-get-sdk-dir => "/sdk")
    (stub buffer-file-name => "/project/main.dart")
    (should (equal (lsp-dart--library-folders) '()))))

(ert-deftest lsp-dart--library-folders--lib-file-test ()
  (with-mock
    (stub lsp-dart-get-sdk-dir => "/sdk")
    (stub buffer-file-name => "/sdk/lib/main.dart")
    (should (equal (lsp-dart--library-folders) '("/sdk/lib/")))))

(ert-deftest lsp-dart--library-folders--extra-folder-test ()
  (with-mock
    (stub lsp-dart-get-sdk-dir => "/sdk")
    (stub buffer-file-name => "/project/main.dart")
    (let ((lsp-dart-extra-library-directories '("/some/lib/")))
      (should (equal (lsp-dart--library-folders) '("/some/lib/"))))))

(ert-deftest lsp-dart--library-folders--extra-folder-and-lib-file-test ()
  (with-mock
    (stub lsp-dart-get-sdk-dir => "/sdk")
    (stub buffer-file-name => "/sdk/lib/main.dart")
    (let ((lsp-dart-extra-library-directories '("/some/lib/")))
      (should (equal (lsp-dart--library-folders) '("/sdk/lib/" "/some/lib/"))))))

(ert-deftest lsp-dart--server-command--custom-test ()
  (let ((lsp-dart-server-command "/some/path/to/server"))
    (should (equal (lsp-dart--server-command) "/some/path/to/server"))))

(ert-deftest lsp-dart--server-command--default-test ()
  (with-mock
    (stub lsp-dart-dart-command => "/sdk/bin/dart")
    (stub lsp-dart-get-sdk-dir => "/sdk")
    (should (equal (lsp-dart--server-command)
                   `("/sdk/bin/dart"
                     ,(f-expand "/sdk/bin/snapshots/analysis_server.dart.snapshot" (f-root))
                     "--lsp"
                     "--client-id emacs.lsp-dart"
                     ,(concat "--client-version " lsp-dart-version-string))))))

(ert-deftest lsp-dart-version--test ()
  (with-mock
   (stub lsp-dart-get-full-dart-version => "2.8.2")
   (stub lsp-dart-get-sdk-dir => t)
   (stub lsp-dart-get-flutter-sdk-dir => "flutter-sdk")
   (stub lsp-dart-flutter-project-p => t)
   (stub lsp-dart-get-project-entrypoint => "/path/to/entrypoint")
   (should (equal (lsp-dart-version) (concat (format "[LSP Dart] %s at %s @ Emacs %s\n"
                                                     lsp-dart-version-string
                                                     (format-time-string "%Y.%m.%d" (current-time))
                                                     emacs-version)
                                             "[Dart SDK] 2.8.2\n"
                                             "[Flutter SDK] flutter-sdk\n"
                                             "[Flutter project] true\n"
                                             "[Project entrypoint] /path/to/entrypoint")))))

;;; lsp-dart-test.el ends here
