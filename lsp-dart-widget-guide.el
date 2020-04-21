;;; lsp-dart-widget-guide.el --- UI guides -*- lexical-binding: t; -*-
;;
;; Version: 1.6
;; Keywords: languages, extensions
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

;; UI guide lines on dart constructors

;;; Code:

(require 'dash)
(require 'ht)

(defconst lsp-dart-widget-guide-space " ")
(defconst lsp-dart-widget-guide-vertical-line "│")
(defconst lsp-dart-widget-guide-horizontal-line "─")
(defconst lsp-dart-widget-guide-bottom-corner "└")
(defconst lsp-dart-widget-guide-middle-corner "├")

(defun lsp-dart-widget-guide--add-to (buffer line col string)
  ""
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line line)
      (move-to-column col)
      (when (string= lsp-dart-widget-guide-space (string (following-char)))
        (let ((ov (make-overlay (point) (1+ (point)) buffer)))
          (overlay-put ov 'category 'lsp-dart-widget-guide)
          (overlay-put ov 'display (propertize string
                                               'face 'font-lock-comment-face)))))))

(defun lsp-dart-widget-guide--first-non-whitespace-pos (line)
  ""
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (back-to-indentation)
    (ht ("line" line)
        ("character" (current-column)))))

(defun lsp-dart-widget-guide--outline->guide (outline)
  ""
  (-let* (((&hash "children" "kind" "range") outline))
    (when (string= kind "NEW_INSTANCE")
      (-let* ((parent-line (->> range (gethash "start") (gethash "line")))
              (children-start (unless (seq-empty-p children)
                                (->> children
                                     (--map (->> it
                                                 (gethash "range")
                                                 (gethash "start")
                                                 (gethash "line")))
                                     (--filter (> it parent-line))))))
        (when children-start
          (let ((start-pos (lsp-dart-widget-guide--first-non-whitespace-pos parent-line)))
            (->> children-start
                 (--map (ht ("start" start-pos)
                            ("end" (lsp-dart-widget-guide--first-non-whitespace-pos it))))
                 (-flatten))))))))

(defun lsp-dart-widget-guide--extract (outline)
  ""
  (-let* (((&hash "children") outline))
    (unless (seq-empty-p children)
      (let ((ext-children (->> children
                               (--map (lsp-dart-widget-guide--extract it))
                               (-non-nil)
                               (-flatten)))
            (ext-outline (lsp-dart-widget-guide--outline->guide outline)))
        (if ext-outline
            (-concat ext-outline ext-children)
          ext-children)))))

(defun lsp-dart-widget-guide--guides->guides-by-line (guides)
  "."
  (let ((guides-by-line (ht-create)))
    (seq-doseq (guide guides)
      (-let* (((&hash "start" (&hash "line" start-line)
                      "end" (&hash "line" end-line)) guide))
        (while (<= start-line end-line)
          (if-let ((cur-guides (ht-get guides-by-line start-line)))
              (ht-set! guides-by-line start-line (append cur-guides (list guide)))
            (ht-set! guides-by-line start-line (list guide)))
          (setq start-line (1+ start-line)))))
    guides-by-line))

(defun lsp-dart-widget-guide--last-char-at (line)
  ""
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (end-of-line)
    (current-column)))

(defun lsp-dart-widget-guide--build-chars (line guide-lines size anchor)
  ""
  (setq chars (make-list size lsp-dart-widget-guide-space))
  (seq-doseq (guide guide-lines)
    (-let* (((&hash "start" (&hash "character" start-char)
                    "end" (&hash "line" end-line "character" end-char)) guide)
            (start-char-at (nth start-char chars)))
      (if (not (= line end-line))
          (progn
            (if (string= lsp-dart-widget-guide-space start-char-at)
                (setf (nth start-char chars) lsp-dart-widget-guide-vertical-line)
              (when (string= start-char-at lsp-dart-widget-guide-bottom-corner)
                (setf (nth start-char chars) lsp-dart-widget-guide-middle-corner))))
        (progn
          (setq char start-char)
          (while (<= char end-char)
            (if (= char start-char)
                (setf (nth char chars) lsp-dart-widget-guide-bottom-corner)
              (if (nth char chars)
                  (setf (nth char chars) lsp-dart-widget-guide-horizontal-line)
                (setq chars (append chars (list lsp-dart-widget-guide-horizontal-line)))))
            (setq char (1+ char)))))))
  (-let (((&hash "character" first-non-whitespace-index) (lsp-dart-widget-guide--first-non-whitespace-pos line)))
    (->> chars
         (--map-indexed (if (>= it-index first-non-whitespace-index) lsp-dart-widget-guide-space it))
         (--map-indexed (if (>= it-index anchor) it ""))
         (--filter (not (string= "" it)))
         )))

(defun lsp-dart-widget-guide--analyze (outline-params)
  "."
  (let* ((buffer (lsp--buffer-for-file (lsp--uri-to-path (gethash "uri" outline-params))))
         (guides (lsp-dart-widget-guide--extract (gethash "outline" outline-params)))
         (guides-by-line (lsp-dart-widget-guide--guides->guides-by-line guides)))
    (remove-overlays (point-min) (point-max) 'category 'lsp-dart-widget-guide)
    (ht-each (lambda (line guide-lines)
               (let* ((first-guide-char (-min (--map (min (gethash "character" (gethash "start" it))
                                                          (gethash "character" (gethash "end" it))) guide-lines)))
                      (last-guide-char (-max (--map (max (gethash "character" (gethash "start" it))
                                                         (gethash "character" (gethash "end" it))) guide-lines)))
                      (last-line-char (lsp-dart-widget-guide--last-char-at line))
                      (anchor (max 0 (if (< last-line-char first-guide-char) 0 first-guide-char)))
                      (chars (lsp-dart-widget-guide--build-chars line guide-lines last-guide-char anchor)))
                 (--each-indexed chars (lsp-dart-widget-guide--add-to buffer line (+ it-index anchor) it))))
             guides-by-line)))

(defvar-local lsp-dart-widget-guide-timer nil)

(defun lsp-dart-widget-guide-check (outline-params)
  ""
  (when lsp-dart-widget-guide-timer
    (cancel-timer lsp-dart-widget-guide-timer))
  (setq lsp-dart-widget-guide-timer
        (run-with-idle-timer 0.3 nil #'lsp-dart-widget-guide--analyze outline-params)))

(provide 'lsp-dart-widget-guide)
;;; lsp-dart-widget-guide.el ends here
