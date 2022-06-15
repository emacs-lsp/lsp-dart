;;; lsp-dart-commands.el --- LSP dart commands -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Eric Dallo
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
;;
;;; Commentary:
;;
;;  LSP dart commands
;;
;;; Code:

(require 'lsp-dart-utils)

(defconst lsp-dart-commands-buffer-name "*LSP Dart commands*")

(defun lsp-dart--run-command (command args)
  "Run COMMAND with ARGS from the project root."
  (lsp-dart-from-project-root
   (compilation-start (format "%s %s" (string-join command " ") args)
                      t
                      (lambda (_) lsp-dart-commands-buffer-name))))

(defun lsp-dart--pub-get ()
  "Run `pub get` from the root project."
  (lsp-dart--run-command (lsp-dart-pub-command) "get"))

(defun lsp-dart--flutter-pub-get ()
  "Run `flutter pub get` from the root project."
  (lsp-dart--run-command (lsp-dart-flutter-command) "pub get"))

(defun lsp-dart--pub-upgrade ()
  "Run `pub upgrade` from the root project."
  (lsp-dart--run-command (lsp-dart-pub-command) "upgrade"))

(defun lsp-dart--flutter-pub-upgrade ()
  "Run `flutter pub upgrade` from the root project."
  (lsp-dart--run-command (lsp-dart-flutter-command) "pub upgrade"))

(defun lsp-dart--pub-outdated ()
  "Run `pub outdated` from the root project."
  (lsp-dart--run-command (lsp-dart-pub-command) "outdated"))

(defun lsp-dart--flutter-pub-outdated ()
  "Run `flutter pub outdated` from the root project."
  (lsp-dart--run-command (lsp-dart-flutter-command) "pub outdated"))

;;;###autoload
(defun lsp-dart-pub-get ()
  "Run pub get on a Dart or Flutter project.
If it is Flutter project, run `flutter pub get` otherwise run
`pub get`."
  (interactive)
  (if (lsp-dart-flutter-project-p)
      (lsp-dart--flutter-pub-get)
    (lsp-dart--pub-get)))

;;;###autoload
(defun lsp-dart-pub-upgrade ()
  "Run pub upgrade on a Dart or Flutter project.
If it is Flutter project, run `flutter pub upgrade` otherwise run
`pub upgrade`."
  (interactive)
  (if (lsp-dart-flutter-project-p)
      (lsp-dart--flutter-pub-upgrade)
    (lsp-dart--pub-upgrade)))

;;;###autoload
(defun lsp-dart-pub-outdated ()
  "Run pub outdated on a Dart or Flutter project.
If it is Flutter project, run `flutter pub outdated` otherwise run
`pub outdated`."
  (interactive)
  (if (lsp-dart-flutter-project-p)
      (lsp-dart--flutter-pub-outdated)
    (lsp-dart--pub-outdated)))

(provide 'lsp-dart-commands)
;;; lsp-dart-commands.el ends here
