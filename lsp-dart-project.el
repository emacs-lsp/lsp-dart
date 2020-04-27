;;; lsp-dart-project.el --- Functions for lsp-dart -*- lexical-binding: t; -*-
;;
;; Version: 1.3
;; Keywords: languages, extensions
;; Package-Requires: ((emacs "25.2") (dash "2.14.1"))
;; URL: https://github.com/emacs-lsp/lsp-dart.el
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
  "Install directory for dart-sdk.
When nil, it will try to find the dart sdk from the dart or flutter executables
in the PATH env."
  :group 'lsp-dart
  :risky t
  :type '(choice directory nil))

(defcustom lsp-dart-project-flutter-command "flutter"
  "Flutter command for running tests."
  :group 'lsp-dart
  :type 'string)

(defun lsp-dart-project-get-sdk-dir ()
  "Return the dart sdk.
Check for `lsp-dart-project-sdk-dir` otherwise search for dart executable or
flutter cache dir."
  (or lsp-dart-project-sdk-dir
      (-when-let (dart (or (executable-find "dart")
                           (-when-let (flutter (-> lsp-dart-project-flutter-command
                                                   executable-find
                                                   file-truename))
                             (expand-file-name "cache/dart-sdk/bin/dart"
                                               (file-name-directory flutter)))))
        (-> dart
            file-truename
            (locate-dominating-file "bin")))))

(defun lsp-dart-project-get-pub-command ()
  "Return the pub executable path from dart SDK path."
  (-> (lsp-dart-project-get-sdk-dir)
      file-name-as-directory
      (concat "bin/pub")))

(defun lsp-dart-project-dart-command ()
  "Return the dart executable from dart SDK dir."
  (expand-file-name "bin/dart" (lsp-dart-project-get-sdk-dir)))

(defun lsp-dart-project-get-root ()
  "Return the dart or flutter project root."
  (file-truename (locate-dominating-file default-directory "pubspec.yaml")))

(defun lsp-dart-project-log (msg &rest args)
  "Log MSG with ARGS and custom prefix."
  (let ((prefix (propertize "[LSP Dart]" 'face 'font-lock-keyword-face)))
    (apply #'message (concat prefix " " msg) args)))

(provide 'lsp-dart-project)
;;; lsp-dart-project.el ends here
