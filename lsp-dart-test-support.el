;;; lsp-dart-test-support.el --- Test support for LSP Dart -*- lexical-binding: t; -*-
;;
;; Version: 1.3
;; Keywords: languages, extensions
;; Package-Requires: ((emacs "25.2") (lsp-mode "6.0") (dash "2.14.1"))
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

;; UI guide lines on flutter widgets

;;; Code:

(require 'lsp-mode)

(require 'lsp-dart-project)

(defconst lsp-dart-test-support-tests-buffer-name "*LSP Dart tests*")


;;; Internal

(defun lsp-dart-test-support--test-kind-p (kind)
  "Return non-nil if KIND is a test type."
  (or (string= kind "UNIT_TEST_TEST")
      (string= kind "UNIT_TEST_GROUP")))

(defun lsp-dart-test-support--flutter-test-file-p (buffer)
  "Return non-nil if the BUFFER appears to be a flutter test file."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^import 'package:flutter_test/flutter_test.dart';"
                         nil t))))

(defun lsp-dart-test-support--last-index-of (regex str &optional ignore-case)
  "Find the last index of a REGEX in a string STR.
IGNORE-CASE is a optional arg to ignore the case sensitive on regex search."
  (let ((start 0)
        (case-fold-search ignore-case)
        idx)
    (while (string-match regex str start)
      (setq idx (match-beginning 0))
      (setq start (match-end 0)))
    idx))

(defmacro lsp-dart-test-support--from-project-root (&rest body)
  "Execute BODY with cwd set to the project root."
  `(let ((project-root (lsp-dart-project-get-root)))
     (if project-root
         (let ((default-directory project-root))
           ,@body)
       (error "Dart or Flutter project not found (pubspec.yaml not found)"))))

(defun lsp-dart-test-support--build-command (buffer)
  "Build the dart or flutter build command.
If the given BUFFER is a flutter test file, return the flutter command
otherwise the dart command."
  (if (lsp-dart-test-support--flutter-test-file-p buffer)
      lsp-dart-project-flutter-command
    (concat (file-name-as-directory (lsp-dart-project-get-sdk-dir)) "bin/pub run")))

(defun lsp-dart-test-support--build-test-name (names)
  "Build the test name from a group of test NAMES."
  (when (and names
             (not (seq-empty-p names)))
    (->> names
         (--map (substring it
                           (+ (cl-search "(" it) 2)
                           (- (lsp-dart-test-support--last-index-of ")" it) 1)))
         (--reduce (format "%s %s" acc it)))))

(defun lsp-dart-test-support--escape-test-name (name)
  "Return the dart safe escaped test NAME."
  (let ((escaped-str (regexp-quote name)))
    (seq-doseq (char '("(" ")" "{" "}"))
      (setq escaped-str (replace-regexp-in-string char
                                                  (concat "\\" char)
                                                  escaped-str nil t)))
    escaped-str))

(defun lsp-dart-test-support-run (buffer &optional names kind)
  "Run Dart/Flutter test command in a compilation buffer for BUFFER file.
If NAMES is non nil, it will run only for KIND the test joining the name
from NAMES."
  (interactive)
  (lsp-dart-test-support--from-project-root
   (let* ((test-file (file-relative-name (buffer-file-name buffer)
                                         (lsp-dart-project-get-root)))
          (test-name (lsp-dart-test-support--build-test-name names))
          (group-kind? (string= kind "UNIT_TEST_GROUP"))
          (test-arg (when test-name
                      (concat "--name '^"
                              (lsp-dart-test-support--escape-test-name test-name)
                              (if group-kind? "'" "$'")))))
     (compilation-start (format "%s test %s %s"
                                (lsp-dart-test-support--build-command buffer)
                                (or test-arg "")
                                test-file)
                        t
                        (lambda (_) lsp-dart-test-support-tests-buffer-name)))))

(defun lsp-dart-test-support--build-overlay (buffer names kind range test-range)
  "Build an overlay in BUFFER for a test NAMES of KIND.
RANGE is the overlay range to build.
TEST-RANGE is the test method range."
  (-let* ((beg-position (gethash "character" (gethash "start" range)))
          ((beg . end) (lsp--range-to-region range))
          (beg-line (progn (goto-char beg)
                           (line-beginning-position)))
          (spaces (make-string beg-position ?\s))
          (overlay (make-overlay beg-line end buffer)))
    (overlay-put overlay 'lsp-dart-test-code-lens t)
    (overlay-put overlay 'lsp-dart-test-names names)
    (overlay-put overlay 'lsp-dart-test-kind kind)
    (overlay-put overlay 'lsp-dart-test-overlay-test-range (lsp--range-to-region test-range))
    (overlay-put overlay 'before-string
                 (concat spaces
                         (propertize "Run\n"
                                     'help-echo "mouse-1: Run this test"
                                     'mouse-face 'lsp-lens-mouse-face
                                     'local-map (-doto (make-sparse-keymap)
                                                 (define-key [mouse-1] (lambda ()
                                                                          (interactive)
                                                                          (lsp-dart-test-support-run buffer names kind))))
                                     'font-lock-face 'lsp-lens-face)))))

(defun lsp-dart-test-support--add-code-lens (buffer items &optional names)
  "Add test code lens to BUFFER for ITEMS.
NAMES arg is optional and are the group of tests representing a test name."
  (seq-doseq (item items)
    (-let* (((&hash "children" "codeRange" test-range "element"
                    (&hash "kind" "name" "range")) item)
            (test-kind? (lsp-dart-test-support--test-kind-p kind))
            (concatened-names (if test-kind?
                                  (append names (list name))
                                names)))
      (when test-kind?
        (lsp-dart-test-support--build-overlay buffer (append names (list name)) kind range test-range))
      (unless (seq-empty-p children)
        (lsp-dart-test-support--add-code-lens buffer children concatened-names)))))

(defun lsp-dart-test-support-check-code-lens (params)
  "Check for test adding lens to it.
PARAMS is the notification data from outline."
  (-let* (((&hash "uri" "outline" (&hash "children")) params)
          (buffer (lsp--buffer-for-file (lsp--uri-to-path uri))))
    (when buffer
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'lsp-dart-test-code-lens t)
        (save-excursion
          (lsp-dart-test-support--add-code-lens buffer children))))))

(defun lsp-dart-test-support-test-file-p (file-name)
  "Return non-nil if FILE-NAME is a dart test files."
  (string-match "_test.dart" file-name))


(provide 'lsp-dart-test-support)
;;; lsp-dart-test-support.el ends here
