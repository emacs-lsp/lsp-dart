;;; lsp-dart-utils.el --- Functions for lsp-dart -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Eric Dallo
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

(defcustom lsp-dart-flutter-executable "flutter"
  "Flutter executable name.
Useful if multiple Flutter installations are present."
  :group 'lsp-dart
  :risky t
  :type 'string)


;; Internal

(defvar-local lsp-dart--project-type-cache nil)

(defun lsp-dart--set-project-type-cache (flutter?)
  "Update project type cache checking FLUTTER?."
  (if flutter?
      (setq lsp-dart--project-type-cache 'flutter)
    (setq lsp-dart--project-type-cache 'dart))
  flutter?)

(defun lsp-dart--flutter-repo-p ()
  "Return non-nil if buffer is the flutter repository."
  (if-let (bin-path (locate-dominating-file default-directory lsp-dart-flutter-executable))
      (and (file-regular-p (expand-file-name lsp-dart-flutter-executable bin-path))
           (->> bin-path
                (expand-file-name "cache/dart-sdk")
                file-directory-p))))

(defun lsp-dart--flutter-project-p ()
  "Return non-nil if buffer is a flutter project."
  (if lsp-dart--project-type-cache
      (eq lsp-dart--project-type-cache 'flutter)
    (let ((flutter-project? (or (-when-let (pubspec-path (-some->> (lsp-dart-get-project-root)
                                                           (expand-file-name "pubspec.yaml")))
                                  (with-temp-buffer
                                    (insert-file-contents pubspec-path)
                                    (goto-char (point-min))
                                    (re-search-forward "sdk\s*:\s*flutter" nil t)))
                                (lsp-dart--flutter-repo-p))))
      (lsp-dart--set-project-type-cache flutter-project?)
      flutter-project?)))

(defun lsp-dart-remove-from-alist (key alist)
  "Remove item with KEY from ALIST."
  (delq (assoc key alist) alist))

(defun lsp-dart-assoc-if (list predicate value)
  "Assoc VALUE to LIST if PREDICATE is t."
  (if predicate
      (append list (list value))
    list))


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
      (-some-> (executable-find lsp-dart-flutter-executable)
        file-truename
        (locate-dominating-file "bin")
        file-truename)
      (getenv "FLUTTER_ROOT")))

(defun lsp-dart-pub-command ()
  "Return the pub executable path from Dart SDK path."
  (if (eq system-type 'windows-nt)
      (expand-file-name "bin/pub.bat" (lsp-dart-get-sdk-dir)))
  (expand-file-name "bin/pub" (lsp-dart-get-sdk-dir)))

(defun lsp-dart-pub-snapshot-command ()
  "Return the pub snapshot executable path from Dart SDK path."
  (expand-file-name "bin/snapshots/pub.dart.snapshot" (lsp-dart-get-sdk-dir)))

(defun lsp-dart-dart-command ()
  "Return the dart executable from Dart SDK dir."
  (let* ((executable-path (if (eq system-type 'windows-nt) "bin/dart.exe" "bin/dart"))
         (command (expand-file-name executable-path (lsp-dart-get-sdk-dir))))
    (if (file-exists-p command)
        command
      (lsp-dart-log "Dart command not found in path '%s'" command))))

(defun lsp-dart-flutter-command ()
  "Return the flutter executable from Flutter SDK dir."
  (let* ((executable-path (if (eq system-type 'windows-nt)
                              "bin/flutter.bat"
                            (concat "bin/" lsp-dart-flutter-executable)))
         (command (expand-file-name executable-path (lsp-dart-get-flutter-sdk-dir))))
    (if (file-exists-p command)
        command
      (lsp-dart-log "Flutter command not found in path '%s'" command))))


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

(defmacro lsp-dart-from-project-root (&rest body)
  "Execute BODY with cwd set to the project root."
  `(let ((project-root (lsp-dart-get-project-root)))
     (if project-root
         (let ((default-directory project-root))
           ,@body)
       (error "Project not found (pubspec.yaml not found)"))))


;; Keymap

(defun lsp-dart-define-key (key action)
  "Define KEY with ACTION."
  (when lsp-keymap-prefix
    (define-key lsp-mode-map (kbd (concat lsp-keymap-prefix  " D " key)) action)))


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

(defun lsp-dart-get-full-dart-version ()
  "Retrieve the dart version from shell command."
  (->> (concat (lsp-dart-dart-command) " --version")
       shell-command-to-string))

(defun lsp-dart-get-full-flutter-version ()
  "Retrieve the Flutter version from shell command."
  (->> (concat (lsp-dart-flutter-command) " --version")
       shell-command-to-string))

(defun lsp-dart-get-dart-version ()
  "Retrieve the dart version from shell command."
  (->> (lsp-dart-get-full-dart-version)
       split-string
       (nth 3)))

(defun lsp-dart-get-flutter-version ()
  "Retrieve the Flutter version from shell command."
  (->> (lsp-dart-get-full-flutter-version)
       split-string
       (nth 1)))

(defun lsp-dart-version->number (version)
  "Transform VERSION into a comparable version number."
  (--> version
       (replace-regexp-in-string "flutter-[[:word:]]+" "0" it nil 'literal)
       (replace-regexp-in-string "-" "." it nil 'literal)
       (replace-regexp-in-string "[[:alpha:]]+" "0" it nil 'literal)))

(defun lsp-dart-version-at-least-p (version)
  "Return non-nil if Dart SDK version is at least VERSION."
  (let ((sdk-version (lsp-dart-get-dart-version)))
    (version<= (lsp-dart-version->number version)
               (lsp-dart-version->number sdk-version))))

(defmacro lsp-dart-assert (test-form message)
  "Assert TEST-FORM return non-nil otherwise prints MESSAGE."
  `(when (not ,test-form)
     (error "Assertion failed: %s" ,message)))

(defun lsp-dart-assert-sdk-min-version (version)
  "Assert dart sdk min version is VERSION."
  (lsp-dart-assert (lsp-dart-version-at-least-p version)
                   "Feature not supported before dart SDK %s"))

(provide 'lsp-dart-utils)
;;; lsp-dart-utils.el ends here
