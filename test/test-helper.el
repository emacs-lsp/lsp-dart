;;; test-helper.el --- Helpers for tests

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

;; Helpers for tests

;;; Code:

(require 'f)

(when (require 'undercover nil t)
  (undercover "*.el"))

(add-to-list 'load-path
             (file-name-as-directory (f-parent (f-parent (f-this-file)))))

(defun lsp-dart-test-package-version (file)
  "Return the version header from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward ";; Version: \\([0-9]+.[0-9]+.[0-9]+\\)" nil t)
    (match-string 1)))

(defmacro lsp-dart-test-from-dart-project (&rest body)
  "Execute BODY from a fixture dart project."
  `(let ((default-directory (file-truename "test/fixtures/dart-project/")))
     (with-mock
       ,@body)))

(defmacro lsp-dart-test-from-flutter-project (&rest body)
  "Execute BODY from a fixture flutter project."
  `(let ((default-directory (file-truename "test/fixtures/flutter-project/")))
     (with-mock
       ,@body)))

(defmacro lsp-dart-test-with-dart-sdk (&rest body)
  "Execute BODY with mocked dart-sdk."
  `(let ((dart-sdk (file-truename "test/fixtures/dart-sdk/")))
     (with-mock
       (stub lsp-dart-get-sdk-dir => dart-sdk)
       ,@body)))

(defmacro lsp-dart-test-with-flutter-sdk (&rest body)
  "Execute BODY with mocked dart-sdk."
  `(let ((flutter-sdk (file-truename "test/fixtures/flutter-sdk/")))
     (with-mock
       (stub lsp-dart-get-flutter-sdk-dir => flutter-sdk)
       ,@body)))

(defun lsp-dart-test-command-fixture ()
  "Return the dart command fixture."
  (file-truename (expand-file-name "test/fixtures/dart-sdk/bin/dart")))

(defun lsp-dart-test-flutter-command-fixture ()
  "Return the dart command fixture."
  (file-truename (expand-file-name "test/fixtures/flutter-sdk/bin/flutter")))

(require 'el-mock)
(require 'ht)

;;; test-helper.el ends here
