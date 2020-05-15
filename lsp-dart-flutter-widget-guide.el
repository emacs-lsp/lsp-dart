;;; lsp-dart-flutter-widget-guide.el --- UI guides -*- lexical-binding: t; -*-
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

(require 'dash)
(require 'ht)

(require 'lsp-mode)

(defcustom lsp-dart-flutter-widget-guides t
  "Enable showing ui guides for flutter widgets hierarchy."
  :type 'boolean
  :group 'lsp-dart)

(defconst lsp-dart-flutter-widget-guide-space " ")
(defconst lsp-dart-flutter-widget-guide-vertical-line "│")
(defconst lsp-dart-flutter-widget-guide-horizontal-line "─")
(defconst lsp-dart-flutter-widget-guide-bottom-corner "└")
(defconst lsp-dart-flutter-widget-guide-middle-corner "├")

(defun lsp-dart-flutter-widget-guide--add-overlay-to (buffer line col string)
  "Add to BUFFER at LINE and COL a STRING overlay."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (move-to-column col)
    (when (string= lsp-dart-flutter-widget-guide-space (string (following-char)))
      (let ((ov (make-overlay (point) (1+ (point)) buffer)))
        (overlay-put ov 'category 'lsp-dart-flutter-widget-guide)
        (overlay-put ov 'display (propertize string
                                             'face 'font-lock-comment-face))))))

(defun lsp-dart-flutter-widget-guide--first-non-whitespace-pos (line)
  "Return the first non whitepaces position at LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (back-to-indentation)
    (ht ("line" line)
        ("character" (current-column)))))

(defun lsp-dart-flutter-widget-guide--last-col-at (line)
  "Return the last col at LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (end-of-line)
    (current-column)))

(defun lsp-dart-flutter-widget-guide--outline->guide (outline)
  "Build a widget guide from an OUTLINE.
Return nil if the widget guilde does not apply."
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
          (let ((start-pos (lsp-dart-flutter-widget-guide--first-non-whitespace-pos parent-line)))
            (->> children-start
                 (--map (ht ("start" start-pos)
                            ("end" (lsp-dart-flutter-widget-guide--first-non-whitespace-pos it))))
                 (-flatten))))))))

(defun lsp-dart-flutter-widget-guide--outline->guides (outline)
  "Build the widget guides from OUTLINE recursively."
  (-let* (((&hash "children") outline))
    (unless (seq-empty-p children)
      (let ((ext-children (->> children
                               (--map (lsp-dart-flutter-widget-guide--outline->guides it))
                               (-non-nil)
                               (-flatten)))
            (ext-outline (lsp-dart-flutter-widget-guide--outline->guide outline)))
        (if ext-outline
            (-concat ext-outline ext-children)
          ext-children)))))

(defun lsp-dart-flutter-widget-guide--guides->guides-by-line (guides)
  "Convert a widget guide GUIDES to a map by line."
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

(defun lsp-dart-flutter-widget-guide--build-chars (line guide-lines size last-line-char anchor)
  "Return the widget guides characters list by LINE from GUIDE-LINES.
SIZE is the length of the characters list.
LAST-LINE-CHAR is the last column position of LINE.
ANCHOR is the anchor point of the widget guide at LINE."
  (let ((chars (make-list size lsp-dart-flutter-widget-guide-space)))
    (seq-doseq (guide guide-lines)
      (-let* (((&hash "start" (&hash "character" start-char)
                      "end" (&hash "line" end-line "character" end-char)) guide)
              (start-char-at (nth start-char chars)))
        (if (not (= line end-line))
            (if (string= lsp-dart-flutter-widget-guide-space start-char-at)
                (setf (nth start-char chars) lsp-dart-flutter-widget-guide-vertical-line)
              (when (string= start-char-at lsp-dart-flutter-widget-guide-bottom-corner)
                (setf (nth start-char chars) lsp-dart-flutter-widget-guide-middle-corner)))
          (let ((char start-char))
            (while (<= char end-char)
              (if (= char start-char)
                  (setf (nth char chars) lsp-dart-flutter-widget-guide-bottom-corner)
                (if (nth char chars)
                    (setf (nth char chars) lsp-dart-flutter-widget-guide-horizontal-line)
                  (setq chars (append chars (list lsp-dart-flutter-widget-guide-horizontal-line)))))
              (setq char (1+ char)))))))
    (-let (((&hash "character" first-non-whitespace-index) (lsp-dart-flutter-widget-guide--first-non-whitespace-pos line)))
      (->> chars
           (--map-indexed (if (and (>= it-index first-non-whitespace-index)
                                   (<= it-index last-line-char)) lsp-dart-flutter-widget-guide-space it))
           (--map-indexed (if (>= it-index anchor) it ""))
           (--filter (not (string= "" it)))))))

(defun lsp-dart-flutter-widget-guide-check (outline-params)
  "Check if there is any widget guide on buffer from uri of OUTLINE-PARAMS."
  (let* ((buffer (lsp--buffer-for-file (lsp--uri-to-path (gethash "uri" outline-params)))))
    (when buffer
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'category 'lsp-dart-flutter-widget-guide)
        (let* ((guides (lsp-dart-flutter-widget-guide--outline->guides (gethash "outline" outline-params)))
               (guides-by-line (lsp-dart-flutter-widget-guide--guides->guides-by-line guides)))
          (ht-each (lambda (line guide-lines)
                     (let* ((first-guide-char (-min (--map (min (gethash "character" (gethash "start" it))
                                                                (gethash "character" (gethash "end" it))) guide-lines)))
                            (last-guide-char (-max (--map (max (gethash "character" (gethash "start" it))
                                                               (gethash "character" (gethash "end" it))) guide-lines)))
                            (last-line-char (lsp-dart-flutter-widget-guide--last-col-at line))
                            (anchor (max 0 (if (< last-line-char first-guide-char) 0 first-guide-char)))
                            (chars (lsp-dart-flutter-widget-guide--build-chars line guide-lines last-guide-char last-line-char anchor)))
                       (--each-indexed chars (lsp-dart-flutter-widget-guide--add-overlay-to buffer line (+ it-index anchor) it))))
                   guides-by-line))))))

(provide 'lsp-dart-flutter-widget-guide)
;;; lsp-dart-flutter-widget-guide.el ends here
