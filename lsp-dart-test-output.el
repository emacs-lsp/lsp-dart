;;; lsp-dart-test-output.el --- Test output features and decorations -*- lexical-binding: t; -*-
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
;;; Commentary:
;;
;;  Test output features and decorations
;;
;;; Code:


(require 'rx)

(require 'lsp-dart-protocol)
(require 'lsp-dart-utils)

(defcustom lsp-dart-test-pop-to-buffer-on-run 'display-only
  "Controls whether to pop to the tests buffer on run.

When set to nil the buffer will only be created, and not displayed.
When set to `display-only' the buffer will be displayed, but it will
not become focused, otherwise the buffer is displayed and focused."
  :group 'lsp-dart
  :type '(choice (const :tag "Create the buffer, but don't display it" nil)
                 (const :tag "Create and display the buffer, but don't focus it" display-only)
                 (const :tag "Create, display, and focus the buffer" t)))


;;; Internal

(defconst lsp-dart-test-output--passed-icon "★")
(defconst lsp-dart-test-output--success-icon "✔")
(defconst lsp-dart-test-output--skipped-icon "•")
(defconst lsp-dart-test-output--hidden-icon "○")
(defconst lsp-dart-test-output--error-icon "✖")

(defvar lsp-dart-test-output--tests-count 0)
(defvar lsp-dart-test-output--tests-passed 0)
(defvar lsp-dart-test-output--first-log t)

(defconst lsp-dart-test-output--buffer-name "*LSP Dart tests*")

(defconst lsp-dart-test-output--exception-re
  (rx (or (and (zero-or-more any)
               (or "exception" "EXCEPTION")
               (zero-or-more any))
          "<asynchronous suspension>"
          (and "#"
               (one-or-more
                any)))))

(defconst lsp-dart-test-output--expected-actual-re
  (rx (or (and (zero-or-more blank)
               "Expected:"
               (zero-or-more any))
          (and (zero-or-more blank)
               "Actual:"
               (zero-or-more any)))))

(defconst lsp-dart-test--font-lock
  `((,lsp-dart-test-output--exception-re . 'error)
    (,lsp-dart-test-output--expected-actual-re . 'warning)))

(defvar lsp-dart-test--output-font-lock
  '((lsp-dart-test--font-lock)))

(lsp-defun lsp-dart-test-output--get-icon ((&TestDoneNotification :result :skipped :hidden))
  "Return the icon for test done notification."
  (cond
   (hidden
    lsp-dart-test-output--hidden-icon)

   ((and (string= result "success")
         skipped)
    lsp-dart-test-output--skipped-icon)

   ((and (string= result "success")
         (not skipped))
    lsp-dart-test-output--success-icon)

   (t lsp-dart-test-output--error-icon)))

(lsp-defun lsp-dart-test-output--get-face ((&TestDoneNotification :result :skipped :hidden))
  "Return the icon for test done notification."
  (cond
   (hidden
    'font-lock-comment-face)

   ((and (string= result "success")
         skipped)
    'homoglyph)

   ((and (string= result "success")
         (not skipped))
    'success)

   (t 'error)))

(defun lsp-dart-test-output--send (message &rest args)
  "Send MESSAGE with ARGS to test buffer."
  (let* ((inhibit-read-only t))
    (with-current-buffer (lsp-dart-test-output--get-buffer-create)
      (save-excursion
        (goto-char (point-max))
        (insert (apply #'format (concat message "\n") args))))))

(defun lsp-dart-test-output--get-buffer-create ()
  "Create a buffer for test display."
  (let ((buffer (get-buffer-create lsp-dart-test-output--buffer-name)))
    (with-current-buffer buffer
      (setq-local default-directory (or (lsp-dart-get-project-root) default-directory))
      (unless (derived-mode-p 'lsp-dart-test-output-content-mode)
        (lsp-dart-test-output-content-mode))
      (current-buffer))))

(defun lsp-dart-test-output--show-buffer ()
  "Show test buffer."
  (let ((test-buffer (lsp-dart-test-output--get-buffer-create))
        (inhibit-read-only t))
    (with-current-buffer test-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (pcase lsp-dart-test-pop-to-buffer-on-run
      (`display-only
       (let ((orig-buffer (current-buffer)))
         (display-buffer test-buffer)
         (set-buffer orig-buffer)))
      ((pred identity) (pop-to-buffer test-buffer)))))

(defun lsp-dart-test-output--handle-run-started ()
  "Handle test run started."
  (setq lsp-dart-test-output--first-log t)
  (lsp-dart-test-output--show-buffer)
  (lsp-dart-test-output--send "Running tests...\n"))

(defun lsp-dart-test-output--handle-all-start (_notification)
  "Handle all start notification."
  (setq lsp-dart-test-output--tests-count 0)
  (setq lsp-dart-test-output--tests-passed 0))

(lsp-defun lsp-dart-test-output--handle-start ((&TestStartNotification :test (&Test :group-i-ds)))
  (unless (seq-empty-p group-i-ds)
    (setq lsp-dart-test-output--tests-count (1+ lsp-dart-test-output--tests-count))))

(lsp-defun lsp-dart-test-output--handle-done ((notification &as &TestDoneNotification :result :time :hidden) test-name test-start-time)
  "Handle test done notification."
  (when lsp-dart-test-output--first-log
    (with-current-buffer (lsp-dart-test-output--get-buffer-create)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (setq lsp-dart-test-output--first-log nil))
  (let ((text (propertize (concat (lsp-dart-test-output--get-icon notification)
                                  " "
                                  test-name)
                          'font-lock-face (lsp-dart-test-output--get-face notification))))
    (if hidden
        (lsp-dart-test-output--send "%s" text)
      (progn
        (when (string= result "success")
          (setq lsp-dart-test-output--tests-passed (1+ lsp-dart-test-output--tests-passed)))
        (let ((formatted-time (propertize (format "(%s ms)"
                                                  (- time test-start-time))
                                          'font-lock-face 'font-lock-comment-face)))
          (lsp-dart-test-output--send "%s %s" text formatted-time))))))

(lsp-defun lsp-dart-test-output--handle-all-done ((&DoneNotification :success))
  "Handle all tests done notification."
  (if success
      (lsp-dart-test-output--send (propertize (format "\n%s All ran tests passed %s" lsp-dart-test-output--passed-icon lsp-dart-test-output--passed-icon)
                                              'font-lock-face 'success))
    (lsp-dart-test-output--send (propertize (format "\n● %s/%s tests passed" lsp-dart-test-output--tests-passed lsp-dart-test-output--tests-count)
                                            'font-lock-face font-lock-warning-face))))

(lsp-defun lsp-dart-test-output--handle-print ((&PrintNotification :message))
  "Handle test print notification."
  (lsp-dart-test-output--send "%s" message))

(lsp-defun lsp-dart-test-output--handle-error ((&ErrorNotification :error :stack-trace))
  "Handle test error notification."
  (lsp-dart-test-output--send "%s" error)
  (lsp-dart-test-output--send "%s" stack-trace))

(define-derived-mode lsp-dart-test-output-content-mode special-mode lsp-dart-test-output--buffer-name
  "Major mode for buffer running tests."
  (setq font-lock-defaults lsp-dart-test--output-font-lock))

(add-hook 'lsp-dart-test-run-started-hook #'lsp-dart-test-output--handle-run-started)
(add-hook 'lsp-dart-test-all-start-notification-hook #'lsp-dart-test-output--handle-all-start)
(add-hook 'lsp-dart-test-start-notification-hook #'lsp-dart-test-output--handle-start)
(add-hook 'lsp-dart-test-done-notification-hook #'lsp-dart-test-output--handle-done)
(add-hook 'lsp-dart-test-all-done-notification-hook #'lsp-dart-test-output--handle-all-done)
(add-hook 'lsp-dart-test-print-notification-hook #'lsp-dart-test-output--handle-print)
(add-hook 'lsp-dart-test-error-notification-hook #'lsp-dart-test-output--handle-error)

(provide 'lsp-dart-test-output)
;;; lsp-dart-test-output.el ends here
