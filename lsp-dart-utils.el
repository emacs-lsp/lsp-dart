;;; lsp-dart-utils.el --- Functions for lsp-dart -*- lexical-binding: t; -*-
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
(require 'lsp-mode)

(defcustom lsp-dart-sdk-dir nil
  "Path to the Dart SDK.
If nil, lsp-dart will attempt to find it from the PATH environment variable.
When editing a Flutter project, the version of Dart included in the Flutter SDK
is used in preference."
  :group 'lsp-dart
  :risky t
  :type '(choice directory nil))

(defcustom lsp-dart-flutter-sdk-dir nil
  "Path to the Flutter SDK.
If nil, Dart Code will attempt to find it from the project folder,
FLUTTER_ROOT environment variable and the PATH environment variable."
  :group 'lsp-dart
  :risky t
  :type '(choice directory nil))


;; Internal

(defun lsp-dart--flutter-repo-p ()
  "Return non-nil if buffer is the flutter repository."
  (if-let (bin-path (locate-dominating-file default-directory "flutter"))
      (and (file-regular-p (expand-file-name "flutter" bin-path))
           (->> bin-path
                (expand-file-name "cache/dart-sdk")
                file-directory-p))))

(defun lsp-dart--flutter-project-p ()
  "Return non-nil if buffer is a flutter project."
  (or (lsp-dart--flutter-repo-p)
      (-when-let (pubspec-path (-some->> (lsp-dart-get-project-root)
                                 (expand-file-name "pubspec.yaml")))
        (with-temp-buffer
          (insert-file-contents pubspec-path)
          (goto-char (point-min))
          (re-search-forward "sdk\s*:\s*flutter" nil t)))))

(defun lsp-dart-remove-from-alist (key alist)
  "Remove item with KEY from ALIST."
  (delq (assoc key alist) alist))


;; SDKs

(defun lsp-dart-get-sdk-dir ()
  "Return the Dart SDK path.
Check for PATH environment variable.
When editing a Flutter project, the version of Dart included in the Flutter SDK
is used in preference."
  (or lsp-dart-sdk-dir
      (when (lsp-dart--flutter-project-p)
        (expand-file-name "bin/cache/dart-sdk/" (lsp-dart-get-flutter-sdk-dir)))
      (-some-> (executable-find "dart")
        file-truename
        (locate-dominating-file "bin")
        file-truename)))

(defun lsp-dart-get-flutter-sdk-dir ()
  "Return the Flutter SDK path.
Check for `lsp-dart-flutter-sdk-dir` then
flutter executable on PATH then
FLUTTER_ROOT environment variable."
  (or lsp-dart-flutter-sdk-dir
      (-some-> (executable-find "flutter")
        file-truename
        (locate-dominating-file "bin")
        file-truename)
      (getenv "FLUTTER_ROOT")))

(defun lsp-dart-pub-command ()
  "Return the pub executable path from Dart SDK path."
  (expand-file-name "bin/pub" (lsp-dart-get-sdk-dir)))

(defun lsp-dart-pub-snapshot-command ()
  "Return the pub snapshot executable path from Dart SDK path."
  (expand-file-name "bin/snapshots/pub.dart.snapshot" (lsp-dart-get-sdk-dir)))

(defun lsp-dart-dart-command ()
  "Return the dart executable from Dart SDK dir."
  (expand-file-name "bin/dart" (lsp-dart-get-sdk-dir)))

(defun lsp-dart-flutter-command ()
  "Return the flutter executable from Flutter SDK dir."
  (expand-file-name "bin/flutter" (lsp-dart-get-flutter-sdk-dir)))


;; Project

(defun lsp-dart-get-project-root ()
  "Return the dart or flutter project root."
  (-some-> default-directory
    (locate-dominating-file "pubspec.yaml")
    file-truename))

(defun lsp-dart-get-project-entrypoint ()
  "Return the dart or flutter project entrypoint."
  (let* ((root (lsp-dart-get-project-root))
         (lib-entry (expand-file-name "lib/main.dart" root))
         (bin-entry (expand-file-name "bin/main.dart" root)))
    (cond
     ((file-exists-p lib-entry)
      lib-entry)

     ((file-exists-p bin-entry)
      bin-entry))))


;; Log

(defun lsp-dart-log (msg &rest args)
  "Log MSG with ARGS and custom prefix."
  (let ((prefix (propertize "[LSP Dart]" 'face 'font-lock-keyword-face)))
    (apply #'message (concat prefix " " msg) args)))

(defun lsp-dart-custom-log (prefix msg &rest args)
  "Log with custom PREFIX the MSG and ARGS."
  (let ((base-prefix (propertize "[LSP Dart]" 'face 'font-lock-keyword-face))
        (custom-prefix (propertize prefix 'face 'font-lock-function-name-face)))
    (apply #'message (concat base-prefix " " custom-prefix " " msg) args)))

(defun lsp-dart-workspace-status (message workspace)
  "Set MESSAGE on lsp WORKSPACE status."
  (if message
      (lsp-workspace-status (concat (propertize "[LSP Dart]"
                                                'face 'font-lock-keyword-face)
                                    " "
                                    message)
                            workspace)
    (lsp-workspace-status nil workspace)))


;; Version

(defun lsp-dart--get-dart-version ()
  "Retrieve the dart version from shell command."
  (->> (concat (lsp-dart-dart-command) " --version")
       shell-command-to-string
       split-string
       (nth 3)))

(defun lsp-dart-version->number (version)
  "Transform VERSION into a comparable version number."
  (--> version
       (replace-regexp-in-string "flutter-[[:word:]]+" "0" it nil 'literal)
       (replace-regexp-in-string "-" "." it nil 'literal)
       (replace-regexp-in-string "[[:alpha:]]+" "0" it nil 'literal)))

(defun lsp-dart-version-at-least-p (version)
  "Return non-nil if Dart SDK version is at least VERSION."
  (let ((sdk-version (lsp-dart--get-dart-version)))
    (version<= (lsp-dart-version->number version)
               (lsp-dart-version->number sdk-version))))

(defun lsp-dart-assert-sdk-min-version (version)
  "Assert dart sdk min version is VERSION."
  (cl-assert (lsp-dart-version-at-least-p version)
             t
             "Feature not supported before dart SDK %s"))

(provide 'lsp-dart-utils)
;;; lsp-dart-utils.el ends here
