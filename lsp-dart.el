;;; lsp-dart.el --- Dart support lsp-mode -*- lexical-binding: t; -*-

;; Version: 1.10.2
;; Package-Requires: ((emacs "25.2") (lsp-treemacs "0.1") (lsp-mode "6.0") (dap-mode "0.3") (ht "2.0") (f "0.20.0") (dash "2.14.1") (pkg-info "0.4") (dart-mode "1.0.5"))
;; Keywords: languages, extensions
;; URL: https://emacs-lsp.github.io/lsp-dart

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

;; Dart analysis server client for LSP mode

;;; Code:

(require 'ht)
(require 'f)
(require 'dash)
(require 'lsp-mode)

(require 'lsp-dart-utils)
(require 'lsp-dart-outline)
(require 'lsp-dart-dap)
(require 'lsp-dart-flutter-fringe)

(defgroup lsp-dart nil
  "LSP support for Dart, using dart analysis server."
  :prefix "lsp-dart-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/emacs-lsp/lsp-dart"))

(defcustom lsp-dart-server-command nil
  "The analysis_server executable to use."
  :type '(repeat string)
  :group 'lsp-dart)

(defcustom lsp-dart-extra-library-directories '()
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-dart)

(defcustom lsp-dart-only-analyze-projects-with-open-files t
  "Analyze only open files.
When set to non-nil, analysis will only be performed for projects that have
open files rather than the root workspace folder."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-suggest-from-unimported-libraries t
  "Import suggestions happens only for non imported symbols.
When set to nil, completion will not include symbols that are not already
imported into the current file."
  :type 'boolean
  :group 'lsp-dart)

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


;;; Internal

(declare-function pkg-info-version-info "ext:pkg-info")

(defun lsp-dart-library-folders ()
  "Return the library folders path to analyze."
  (let ((sdk-lib (expand-file-name "lib" (lsp-dart-get-sdk-dir))))
    (if (string-prefix-p sdk-lib (buffer-file-name))
        (append (list (file-name-directory (buffer-file-name))) lsp-dart-extra-library-directories)
      lsp-dart-extra-library-directories)))

(defun lsp-dart--server-command ()
  "Generate LSP startup command."
  (or lsp-dart-server-command
      `(,(lsp-dart-dart-command)
        ,(expand-file-name (f-join (lsp-dart-get-sdk-dir) "bin/snapshots/analysis_server.dart.snapshot"))
        "--lsp")))

(defun lsp-dart--handle-closing-labels (_workspace params)
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

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection 'lsp-dart--server-command)
                  :major-modes '(dart-mode)
                  :priority -1
                  :initialization-options
                  `((onlyAnalyzeProjectsWithOpenFiles . ,lsp-dart-only-analyze-projects-with-open-files)
                    (suggestFromUnimportedLibraries . ,lsp-dart-suggest-from-unimported-libraries)
                    (closingLabels . ,lsp-dart-closing-labels)
                    (outline . ,lsp-dart-outline)
                    (flutterOutline . ,lsp-dart-flutter-outline))
                  :library-folders-fn (lambda (_workspace) (lsp-dart-library-folders))
                  :notification-handlers (ht ("dart/textDocument/publishClosingLabels" #'lsp-dart--handle-closing-labels)
                                             ("dart/textDocument/publishOutline" #'lsp-dart-outline-handle-outline)
                                             ("dart/textDocument/publishFlutterOutline" #'lsp-dart-outline-handle-flutter-outline))
                  :server-id 'dart_analysis_server))


;;; Public interface

;;;###autoload
(defun lsp-dart-version ()
  "Get the lsp-dart version as string.

The returned string includes the version from main file header,
 the current time and the Emacs version.

If the version number could not be determined, signal an error."
  (interactive)
  (if (require 'pkg-info nil t)
      (let ((version (pkg-info-version-info 'lsp-dart)))
        (lsp-dart-log
         "%s at %s @ Emacs %s"
         version
         (format-time-string "%Y.%m.%d" (current-time))
         emacs-version))
    (error "Cannot determine version without package 'pkg-info'")))


;;;###autoload(with-eval-after-load 'lsp-mode (require 'lsp-dart))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; End:
