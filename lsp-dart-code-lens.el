;;; lsp-dart-code-lens.el --- Code lens support for LSP Dart -*- lexical-binding: t; -*-
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
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Code lens support for LSP Dart
;;
;;; Code:

(require 'dash)

(require 'lsp-lens)

(require 'lsp-dart-protocol)
(require 'lsp-dart-dap)
(require 'lsp-dart-test-support)

(defcustom lsp-dart-main-code-lens t
  "Enable the main code lens overlays."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-test-code-lens t
  "Enable the test code lens overlays."
  :type 'boolean
  :group 'lsp-dart)

(defface lsp-dart-code-lens-separator
  '((t :height 0.3))
  "The face used for separate test code lens overlays."
  :group 'lsp-dart-test-support)


;; Internal

(defun lsp-dart-code-lens--find-main-outline (outlines)
  "Return the main outline if exists in OUTLINES."
  (if-let (main (seq-find (-lambda ((&Outline :element (&Element :name)))
                            (string= name "main"))
                          outlines))
      main
    (->> outlines
         (seq-map (-lambda ((&Outline :children))
                    (when children
                      (lsp-dart-code-lens--find-main-outline children))))
         -first-item)))

(defun lsp-dart-code-lens--build-action (label help-echo action-fn &rest action-args)
  "Build a String action to overlay.
LABEL is teh text to display.
HELP-ECHO is the mouse hover text.
ACTION-FN is the function to be called with ACTION-ARGS when clicked."
  (propertize label
              'pointer 'hand
              'help-echo help-echo
              'mouse-face 'lsp-lens-mouse-face
              'local-map (-doto (make-sparse-keymap)
                           (define-key [mouse-1] (lambda ()
                                                   (interactive)
                                                   (apply action-fn action-args))))
              'font-lock-face 'lsp-lens-face))

(defun lsp-dart-code-lens--run-application (path test-file?)
  "Run Dart/Flutter application from PATH.
If TEST-FILE? debug tests otherwise debug application."
  (if test-file?
      (if (lsp-dart--flutter-project-p)
          (lsp-dart-dap-debug-flutter-test path)
        (lsp-dart-dap-debug-dart-test path))
    (if (lsp-dart--flutter-project-p)
        (lsp-dart-dap-run-flutter path)
      (lsp-dart-dap-run-dart path))))

(defun lsp-dart-code-lens--debug-application (path test-file?)
  "Debug Dart/Flutter application from PATH.
If TEST-FILE? debug tests otherwise debug application."
  (if test-file?
      (if (lsp-dart--flutter-project-p)
          (lsp-dart-dap-debug-flutter-test path)
        (lsp-dart-dap-debug-dart-test path))
    (if (lsp-dart--flutter-project-p)
        (lsp-dart-dap-debug-flutter path)
      (lsp-dart-dap-debug-dart path))))

(lsp-defun lsp-dart-code-lens--build-main-overlay (buffer (&Outline :range
                                                                    (range &as &Range :start
                                                                           (&Position :character beg-position))))
  "Build main overlay code lens for BUFFER from main outline."
  (-let* (((beg . end) (lsp--range-to-region range))
          (beg-line (progn (goto-char beg)
                           (line-beginning-position)))
          (overlay (make-overlay beg-line end buffer))
          (spaces (make-string beg-position ?\s))
          (test-file? (lsp-dart-test-file-p (buffer-file-name buffer)))
          (separator (propertize " " 'font-lock-face 'lsp-dart-code-lens-separator)))
    (overlay-put overlay 'lsp-dart-main-code-lens t)
    (overlay-put overlay 'before-string
                 (concat spaces
                         (lsp-dart-code-lens--build-action "Run"
                                                           (if test-file?
                                                               "mouse-1: Run tests"
                                                             "mouse-1: Run application")
                                                           #'lsp-dart-code-lens--run-application
                                                           (buffer-file-name buffer)
                                                           test-file?)
                         separator
                         (propertize "|" 'font-lock-face 'lsp-lens-face)
                         separator
                         (lsp-dart-code-lens--build-action "Debug"
                                                           (if test-file?
                                                               "mouse-1: Debug tests"
                                                             "mouse-1: Debug application")
                                                           #'lsp-dart-code-lens--debug-application
                                                           (buffer-file-name buffer)
                                                           test-file?)
                         "\n"))))

(lsp-defun lsp-dart-code-lens--build-test-overlay (buffer names kind test-range (range &as &Range :start
                                                                                       (&Position :character beg-position)))
  "Build an overlay in BUFFER for a test NAMES of KIND.
TEST-RANGE is the test method range.
RANGE is the overlay range to build."
  (-let* (((beg . end) (lsp--range-to-region range))
          (beg-line (progn (goto-char beg)
                           (line-beginning-position)))
          (spaces (make-string beg-position ?\s))
          (overlay (make-overlay beg-line end buffer))
          (test (make-lsp-dart-test :file-name (buffer-file-name buffer)
                                    :names names
                                    :position beg
                                    :kind kind))
          (separator (propertize " " 'font-lock-face 'lsp-dart-code-lens-separator)))
    (overlay-put overlay 'lsp-dart-test-code-lens t)
    (overlay-put overlay 'lsp-dart-test test)
    (overlay-put overlay 'lsp-dart-code-lens-overlay-test-range (lsp--range-to-region test-range))
    (overlay-put overlay 'before-string
                 (concat spaces
                         (lsp-dart-code-lens--build-action "Run"
                                                           "mouse-1: Run this test"
                                                           #'lsp-dart-test--run
                                                           test)
                         separator
                         (propertize "|" 'font-lock-face 'lsp-lens-face)
                         separator
                         (lsp-dart-code-lens--build-action "Debug"
                                                           "mouse-1: Debug this test"
                                                           #'lsp-dart-test--debug
                                                           test)
                         "\n"))))

(defun lsp-dart-code-lens--add-test (buffer items &optional names)
  "Add test code lens to BUFFER for ITEMS.
NAMES arg is optional and are the group of tests representing a test name."
  (seq-doseq (item items)
    (-let* (((&Outline :children :code-range test-range :element
                       (&Element :kind :name :range)) item)
            (test-kind? (lsp-dart-test--test-kind-p kind))
            (concatened-names (if test-kind?
                                  (append names (list name))
                                names)))
      (when test-kind?
        (lsp-dart-code-lens--build-test-overlay buffer (append names (list name)) kind test-range range))
      (unless (seq-empty-p children)
        (lsp-dart-code-lens--add-test buffer children concatened-names)))))

(lsp-defun lsp-dart--main-code-lens-check ((&OutlineNotification :uri :outline (&Outline :children)))
  "Check URI and outline for main method adding lens to it."
  (-let* ((buffer (find-buffer-visiting (lsp--uri-to-path uri)))
          (main-outline (lsp-dart-code-lens--find-main-outline children)))
    (when buffer
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'lsp-dart-main-code-lens t)
        (when main-outline
          (save-excursion
            (lsp-dart-code-lens--build-main-overlay buffer main-outline)))))))

(lsp-defun lsp-dart--test-code-lens-check ((&OutlineNotification :uri :outline (&Outline :children)))
  "Check URI and outline for test adding lens to it."
  (when (lsp-dart-test-file-p uri)
    (when-let (buffer (find-buffer-visiting (lsp--uri-to-path uri)))
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'lsp-dart-test-code-lens t)
        (save-excursion
          (lsp-dart-code-lens--add-test buffer children))))))


;; Public

(define-minor-mode lsp-dart-main-code-lens-mode
  "Mode for displaying code lens on main methods."
  nil nil nil
  (cond
   (lsp-dart-main-code-lens-mode
    (add-hook 'lsp-dart-outline-arrived-hook #'lsp-dart--main-code-lens-check nil t))
   (t
    (progn
      (remove-overlays (point-min) (point-max) 'lsp-dart-main-code-lens t)
      (remove-hook 'lsp-dart-outline-arrived-hook #'lsp-dart--main-code-lens-check t)))))

(define-minor-mode lsp-dart-test-code-lens-mode
  "Mode for displaying code lens on main methods."
  nil nil nil
  (cond
   (lsp-dart-test-code-lens-mode
    (lsp-dart-define-key "t t" #'lsp-dart-run-test-at-point)
    (lsp-dart-define-key "t T" #'lsp-dart-run-test-at-point)
    (lsp-dart-define-key "t f" #'lsp-dart-run-test-file)
    (lsp-dart-define-key "t l" #'lsp-dart-run-last-test)
    (lsp-dart-define-key "t L" #'lsp-dart-debug-last-test)
    (lsp-dart-define-key "t a" #'lsp-dart-run-all-tests)
    (lsp-dart-define-key "t v" #'lsp-dart-visit-last-test)
    (add-hook 'lsp-dart-outline-arrived-hook #'lsp-dart--test-code-lens-check nil t))
   (t
    (lsp-dart-define-key "t t" 'ignore)
    (lsp-dart-define-key "t T" 'ignore)
    (lsp-dart-define-key "t f" 'ignore)
    (lsp-dart-define-key "t l" 'ignore)
    (lsp-dart-define-key "t L" 'ignore)
    (lsp-dart-define-key "t a" 'ignore)
    (lsp-dart-define-key "t v" 'ignore)
    (remove-overlays (point-min) (point-max) 'lsp-dart-test-code-lens t)
    (remove-hook 'lsp-dart-outline-arrived-hook #'lsp-dart--test-code-lens-check t))))

(provide 'lsp-dart-code-lens)
;;; lsp-dart-code-lens.el ends here
