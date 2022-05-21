;;; lsp-dart-closing-labels.el --- Closing labels support for LSP Dart -*- lexical-binding: t; -*-
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
;;  Closing labels support for LSP Dart
;;
;;; Code:

(require 'lsp-mode)

(require 'lsp-dart-protocol)

(defcustom lsp-dart-closing-labels t
  "Enable the analysis server closing labels feature.
When set to non-nil, dart/textDocument/publishClosingLabel notifications will
be sent with information to render editor closing labels."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-closing-labels-prefix ""
  "The prefix string to be concatenated with the closing label."
  :type 'string
  :group 'lsp-dart)

(defcustom lsp-dart-closing-labels-size 0.9
  "The font size factor to be multiplied by the closing labels font size."
  :type 'float
  :group 'lsp-dart)

(defcustom lsp-dart-closing-labels-arrived-hook nil
  "Hooks to run after a closing labels notification arrive.
The hook will receive the notification data as argument."
  :type 'hook
  :group 'lsp-dart)

(lsp-defun lsp-dart--closing-labels-check ((&ClosingLabelsNotification :uri :labels))
  "Closing labels notification handler."
  (when-let (buffer (find-buffer-visiting (lsp--uri-to-path uri)))
    (with-current-buffer buffer
      (remove-overlays (point-min) (point-max) 'lsp-dart-closing-labels t)
      (seq-doseq (label labels)
        (save-excursion
          (-let* (((beg . end) (lsp--range-to-region (lsp:closing-label-range label)))
                  (end-line (progn
                              (goto-char end)
                              (line-end-position)))
                  (overlay (make-overlay beg end-line buffer)))
            (overlay-put overlay 'lsp-dart-closing-labels t)
            (overlay-put overlay 'after-string (propertize (concat lsp-dart-closing-labels-prefix " " (lsp:closing-label-label label))
                                                           'display `((height ,lsp-dart-closing-labels-size))
                                                           'cursor t
                                                           'font-lock-face 'font-lock-comment-face))))))))

(define-minor-mode lsp-dart-closing-labels-mode
  "Mode for displaying flutter closing labels on the end of methods/contructors."
  :global nil
  :init-value nil
  :lighter nil
  (cond
   (lsp-dart-closing-labels-mode
    (add-hook 'lsp-dart-closing-labels-arrived-hook #'lsp-dart--closing-labels-check nil t))
   (t
    (progn
      (remove-overlays (point-min) (point-max) 'lsp-dart-closing-labels t)
      (remove-hook 'lsp-dart-closing-labels-arrived-hook #'lsp-dart--closing-labels-check t)))))

(provide 'lsp-dart-closing-labels)
;;; lsp-dart-closing-labels.el ends here
