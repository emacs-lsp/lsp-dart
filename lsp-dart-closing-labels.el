;;; lsp-dart-closing-labels.el --- Closing labels support for LSP Dart -*- lexical-binding: t; -*-
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

(defun lsp-dart-closing-labels-handle (_workspace params)
  "Closing labels notification handling.
PARAMS closing labels notification data sent from WORKSPACE."
  (-let* (((&hash "uri" "labels") params)
          (buffer (lsp--buffer-for-file (lsp--uri-to-path uri))))
    (when buffer
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'lsp-dart-closing-labels t)
        (seq-doseq (label-ht labels)
          (save-excursion
            (-let* ((label (gethash "label" label-ht))
                    (range (gethash "range" label-ht))
                    ((beg . end) (lsp--range-to-region range))
                    (end-line (progn
                                (goto-char end)
                                (line-end-position)))
                    (overlay (make-overlay beg end-line buffer)))
              (overlay-put overlay 'lsp-dart-closing-labels t)
              (overlay-put overlay 'after-string (propertize (concat lsp-dart-closing-labels-prefix " " label)
                                                             'display `((height ,lsp-dart-closing-labels-size))
                                                             'cursor t
                                                             'font-lock-face 'font-lock-comment-face)))))))))

(provide 'lsp-dart-closing-labels)
;;; lsp-dart-closing-labels.el ends here
