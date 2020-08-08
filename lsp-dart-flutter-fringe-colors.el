;;; lsp-dart-flutter-fringe-colors.el --- Flutter fringe colors -*- lexical-binding: t; -*-
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

;; Flutter support for fringe colors

;;; Code:

(require 'lsp-mode)
(require 'lsp-dart-flutter-colors)

(defconst lsp-dart-flutter-fringe-color-name-pattern "Colors\\.\\([][:word:]\\[\\.]+\\)")
(defconst lsp-dart-flutter-fringe-color-hex-pattern "Color(0x\\(\\w\\{8\\}\\))")
(defconst lsp-dart-flutter-fringe-color-argb-pattern "Color.fromARGB(\\(\\w+\\), \\(\\w+\\), \\(\\w+\\), \\(\\w+\\))")
(defconst lsp-dart-flutter-fringe-color-rgbo-pattern "Color.fromRGBO(\\(\\w+\\), \\(\\w+\\), \\(\\w+\\), \\([0-9.]+\\))")

(defcustom lsp-dart-flutter-fringe-colors t
  "Enable the color overlays on fringe."
  :type 'boolean
  :group 'lsp-dart)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'lsp-dart-flutter-fringe-color-bitmap
  [255 255 255 255 255 255 255 255 255]))

(defun lsp-dart-flutter-fringe--create-face (hex)
  "Create a face for HEX color."
  `((t :foreground ,hex)))

(defun lsp-dart-flutter-fringe--add-color (hex buffer point)
  "Add color HEX overlay to BUFFER line at POINT."
  (let ((face-name (intern (concat "lsp-dart-futter-fringe-face-" (substring hex 1))))
        (ov (make-overlay point point buffer t t)))
    (if (facep face-name)
        (face-remap-add-relative face-name :foreground hex)
      (eval`(defface ,face-name (lsp-dart-flutter-fringe--create-face ,hex)
              (format "Fringe color face %s." ,hex)
              :group 'lsp-dart)))
    (overlay-put ov 'lsp-dart-flutter-fringe-colors t)
    (overlay-put ov 'priority 1)
    (overlay-put ov 'before-string
                 (propertize "." 'display `((left-fringe
                                             lsp-dart-flutter-fringe-color-bitmap
                                             ,face-name))))))

(defun lsp-dart-flutter-fringe--rgb-to-hex (r g b)
  "Convert a R G B color into a hexadecimal color format."
  (format "#%02x%02x%02x"
          (string-to-number r)
          (string-to-number g)
          (string-to-number b)))

(defun lsp-dart-flutter-fringe--update-colors (buffer)
  "Search for color patterns in BUFFER and if match, add a color overlay."
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'lsp-dart-flutter-fringe-colors t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward lsp-dart-flutter-fringe-color-name-pattern nil t)
        (if-let ((color (-> (match-string 1)
                         (assoc lsp-dart-flutter-colors)
                         cdr)))
          (lsp-dart-flutter-fringe--add-color (concat "#" color) buffer (point-at-bol))))
      (goto-char (point-min))
      (while (re-search-forward lsp-dart-flutter-fringe-color-hex-pattern nil t)
        (let ((color (concat "#" (substring (match-string 1) 2))))
          (lsp-dart-flutter-fringe--add-color color buffer (point-at-bol))))
      (goto-char (point-min))
      (while (re-search-forward lsp-dart-flutter-fringe-color-argb-pattern nil t)
        (let ((color (lsp-dart-flutter-fringe--rgb-to-hex (match-string 2)
                                                   (match-string 3)
                                                   (match-string 4))))
          (lsp-dart-flutter-fringe--add-color color buffer (point-at-bol))))
      (goto-char (point-min))
      (while (re-search-forward lsp-dart-flutter-fringe-color-rgbo-pattern nil t)
        (let ((color (lsp-dart-flutter-fringe--rgb-to-hex (match-string 1)
                                                   (match-string 2)
                                                   (match-string 3))))
          (lsp-dart-flutter-fringe--add-color color buffer (point-at-bol)))))))

(define-minor-mode lsp-dart-flutter-fringe-colors-mode
  "Mode for displaying colors in fringe."
  nil nil nil
  (cond
   (lsp-dart-flutter-fringe-colors-mode
    (add-hook 'lsp-on-change-hook (-partial #'lsp-dart-flutter-fringe--update-colors (current-buffer)) nil t))
   (t
    (progn
      (remove-overlays (point-min) (point-max) 'lsp-dart-flutter-fringe-colors t)
      (remove-hook 'lsp-on-change-hook #'lsp-dart-flutter-fringe--update-colors t)))))

(provide 'lsp-dart-flutter-fringe-colors)
;;; lsp-dart-flutter-fringe-colors.el ends here
