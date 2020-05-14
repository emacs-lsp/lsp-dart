;;; lsp-dart-project.el --- Functions for lsp-dart -*- lexical-binding: t; -*-
;;
;; Version: 1.3
;; Keywords: languages, extensions
;; Package-Requires: ((emacs "25.2") (dash "2.14.1"))
;; URL: https://emacs-lsp.github.io/lsp-dart
;;
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

;; Helper functions for lsp-dart

;;; Code:

(require 'dash)

(defcustom lsp-dart-project-sdk-dir nil
  "Path to the Dart SDK.
If nil, lsp-dart will attempt to find it from the PATH environment variable.
When editing a Flutter project, the version of Dart included in the Flutter SDK
is used in preference."
  :group 'lsp-dart
  :risky t
  :type '(choice directory nil))

(defcustom lsp-dart-project-flutter-sdk-dir nil
  "Path to the Flutter SDK.
If nil, Dart Code will attempt to find it from the project folder,
FLUTTER_ROOT environment variable and the PATH environment variable."
  :group 'lsp-dart
  :risky t
  :type '(choice directory nil))


;; Internal

(defun lsp-dart-project--flutter-project-p ()
  "Return non-nil if buffer is a flutter project."
  (when-let (pubspec-path (-some->> (lsp-dart-project-get-root)
                            (expand-file-name "pubspec.yaml")))
    (with-temp-buffer
      (insert-file-contents pubspec-path)
      (goto-char (point-min))
      (re-search-forward "sdk\s*:\s*flutter" nil t))))


;; SDKs

(defun lsp-dart-project-get-sdk-dir ()
  "Return the Dart SDK path.
Check for PATH environment variable.
When editing a Flutter project, the version of Dart included in the Flutter SDK
is used in preference."
  (or lsp-dart-project-sdk-dir
      (when (lsp-dart-project--flutter-project-p)
        (expand-file-name "bin/cache/dart-sdk/" (lsp-dart-project-get-flutter-sdk-dir)))
      (-some-> (executable-find "dart")
        file-truename
        (locate-dominating-file "bin")
        file-truename)))

(defun lsp-dart-project-get-flutter-sdk-dir ()
  "Return the Flutter SDK path.
Check for `lsp-dart-project-flutter-sdk-dir` then
flutter executable on PATH then
FLUTTER_ROOT environment variable."
  (or lsp-dart-project-flutter-sdk-dir
      (-some-> (executable-find "flutter")
        file-truename
        (locate-dominating-file "bin")
        file-truename)
      (getenv "FLUTTER_ROOT")))

(defun lsp-dart-project-pub-command ()
  "Return the pub executable path from Dart SDK path."
  (expand-file-name "bin/pub" (lsp-dart-project-get-sdk-dir)))

(defun lsp-dart-project-pub-snapshot-command ()
  "Return the pub snapshot executable path from Dart SDK path."
  (expand-file-name "bin/snapshots/pub.dart.snapshot" (lsp-dart-project-get-sdk-dir)))

(defun lsp-dart-project-dart-command ()
  "Return the dart executable from Dart SDK dir."
  (expand-file-name "bin/dart" (lsp-dart-project-get-sdk-dir)))

(defun lsp-dart-project-flutter-command ()
  "Return the flutter executable from Flutter SDK dir."
  (expand-file-name "bin/flutter" (lsp-dart-project-get-flutter-sdk-dir)))


;; Project

(defun lsp-dart-project-get-root ()
  "Return the dart or flutter project root."
  (-some-> default-directory
    (locate-dominating-file "pubspec.yaml")
    file-truename))

(defun lsp-dart-project-get-entrypoint ()
  "Return the dart or flutter project entrypoint."
  (let* ((root (lsp-dart-project-get-root))
         (lib-entry (expand-file-name "lib/main.dart" root))
         (bin-entry (expand-file-name "bin/main.dart" root)))
    (cond
     ((file-exists-p lib-entry)
      lib-entry)

     ((file-exists-p bin-entry)
      bin-entry))))


;; Log

(defun lsp-dart-project-log (msg &rest args)
  "Log MSG with ARGS and custom prefix."
  (let ((prefix (propertize "[LSP Dart]" 'face 'font-lock-keyword-face)))
    (apply #'message (concat prefix " " msg) args)))

(defun lsp-dart-project-custom-log (prefix msg &rest args)
  "Log with custom PREFIX the MSG and ARGS."
  (let ((base-prefix (propertize "[LSP Dart]" 'face 'font-lock-keyword-face))
        (custom-prefix (propertize prefix 'face 'font-lock-function-name-face)))
    (apply #'message (concat base-prefix " " custom-prefix " " msg) args)))

(provide 'lsp-dart-project)
;;; lsp-dart-project.el ends here
