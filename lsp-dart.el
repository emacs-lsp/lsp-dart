;;; lsp-dart.el --- Dart support lsp-mode -*- lexical-binding: t; -*-

;; Version: 1.7.1
;; Package-Requires: ((emacs "25.2") (lsp-treemacs "0.1") (lsp-mode "6.0") (dap-mode "0.3") (ht "2.0") (f "0.20.0") (dash "2.14.1") (pkg-info "0.4") (dart-mode "1.0.5"))
;; Keywords: languages, extensions
;; URL: https://github.com/emacs-lsp/lsp-dart.el

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
(require 'lsp-treemacs)
(require 'lsp-mode)

(require 'lsp-dart-project)
(require 'lsp-dart-test-support)
(require 'lsp-dart-dap)
(require 'lsp-dart-flutter-fringe)
(require 'lsp-dart-flutter-widget-guide)

(defgroup lsp-dart nil
  "LSP support for Dart, using dart analysis server."
  :prefix "lsp-dart-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/emacs-lsp/lsp-dart"))

(defcustom lsp-dart-server-command nil
  "The analysis_server executable to use."
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

(defcustom lsp-dart-outline t
  "Enable the analysis server outline custom method.
When set to non-nil, dart/textDocument/publishOutline notifications will
be sent with outline information for open files."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-outline-position-params
  `((side . ,treemacs-position)
    (slot . 2)
    (window-width . ,treemacs-width))
  "The outline tree position params.
Defaults to side following treemacs default."
  :type 'list
  :group 'lsp-dart)

(defcustom lsp-dart-flutter-outline t
  "Enable the analysis server Flutter outline custom method.
When set to non-nil, dart/textDocument/publishFlutterOutline notifications will
be sent with Flutter outline information for open files."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-flutter-outline-position-params
  `((side . ,treemacs-position)
    (slot . 2)
    (window-width . ,treemacs-width))
  "The Flutter outline tree position params.
Defaults to side following treemacs default."
  :type 'list
  :group 'lsp-dart)

(defcustom lsp-dart-flutter-widget-guides t
  "Enable showing ui guides for flutter widgets hierarchy."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-test-code-lens t
  "Enable the test code lens overlays."
  :type 'boolean
  :group 'lsp-dart)


;;; Internal

(declare-function pkg-info-version-info "ext:pkg-info")

(defun lsp-dart--get-dart-version ()
  "Retrieve the dart version from shell command."
  (->> (concat (lsp-dart-project-get-sdk-dir) "bin/dart --version")
       shell-command-to-string
       split-string
       (nth 3)))

(defun lsp-dart--assert-sdk-min-version (version)
  "Assert dart sdk min version is VERSION."
  (cl-assert (string-prefix-p version (lsp-dart--get-dart-version))
             t
             "Feature not supported before dart SDK %s"))

(defun lsp-dart--set-metadata (workspace params key-prefix)
  "Save in WORKSPACE the PARAMS metadata with KEY-PREFIX.
The key is composed of the KEY-PREFIX with PARAMS uri path."
  (-let* (((&hash "uri") params)
          (uri-path (lsp--uri-to-path uri))
          (key (concat key-prefix "--" uri-path)))
    (lsp-workspace-set-metadata key params workspace)))

(defun lsp-dart--get-metadata (buffer key-prefix)
  "Return the metadata saved in current workspace of BUFFER for KEY-PREFIX."
  (let ((key (concat key-prefix "--" (buffer-file-name buffer))))
    (lsp-workspace-get-metadata key (lsp-find-workspace 'lsp-find-workspace))))

(defun lsp-dart--outline-kind->icon (kind)
  "Maps an outline KIND to a treemacs icon symbol.
Kinds from https://github.com/dart-lang/sdk/blob/master/pkg/analysis_server/tool/spec/generated/java/types/ElementKind.java"
  (pcase kind
    ("CLASS" 'class)
    ("CLASS_TYPE_ALIAS" 'class)
    ("COMPILATION_UNIT" 'document)
    ("FIELD" 'field)
    ("METHOD" 'method)
    ("CONSTRUCTOR" 'namespace)
    ("CONSTRUCTOR_INVOCATION" 'namespace)
    ("GETTER" 'property)
    ("SETTER" 'property)
    ("TOP_LEVEL_VARIABLE" 'constant)
    ("FUNCTION" 'method)
    ("FUNCTION_INVOCATION" 'method)
    ("FUNCTION_TYPE_ALIAS" 'method)
    ("LABEL" 'number)
    ("LIBRARY" 'template)
    ("EXTENSION" 'interface)
    ("LOCAL_VARIABLE" 'field)
    ("MIXIN" 'interface)
    ("PARAMETER" 'string)
    ("TYPE_PARAMETER" 'string)
    ("UNIT_TEST_GROUP" 'structure)
    ("UNIT_TEST_TEST" 'method)
    ("ENUM" 'enumerator)
    ("ENUM_CONSTANT" 'enumitem)))

(defun lsp-dart--outline-tree-ret-action (uri range)
  "Build the ret action for an item in the outline tree view.
URI is the source of the item.
RANGE is the range of positions to where this item should point."
  (interactive)
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (goto-char (lsp--position-to-point (gethash "start" range)))
  (run-hooks 'xref-after-jump-hook))

(defun lsp-dart--outline->tree (uri items)
  "Builds an outline tree.
URI is the source of the outline.
ITEMS is the outline items data."
  (seq-map (-lambda ((&hash "children"
                            "element" (&hash "kind" "name" "parameters" "range")))
             (let ((label (concat name (when parameters
                                         (propertize (concat " " parameters)
                                                     'face 'lsp-lens-face)))))
               (list :key label
                     :label label
                     :icon (lsp-dart--outline-kind->icon kind)
                     :children (lambda (&rest _)
                                 (unless (seq-empty-p children)
                                   (lsp-dart--outline->tree uri children)))
                     :ret-action (lambda (&rest _) (lsp-dart--outline-tree-ret-action uri range)))))
           items))

(defun lsp-dart--build-flutter-outline-widget-actions (uri range)
  "Build the action options for the Flutter outline tree view.
URI is the source of the outline.
RANGE is the range for currently build item."
  (-when-let (buffer (lsp--buffer-for-file (lsp--uri-to-path uri)))
    (with-current-buffer buffer
        (goto-char (-> range lsp--range-to-region car))
        (->> (lsp-code-actions-at-point)
             (-filter (-lambda ((&hash "kind"))
                        (and kind (equal "refactor" kind))))
             (-map (-lambda ((action &as &hash "title"))
                     `[,title (lsp-execute-code-action ,action)]))))))

(defun lsp-dart--flutter-outline->tree (uri items)
  "Builds a Flutter outline tree.
URI is the source of the outline.
ITEMS is the outline items data."
  (seq-map (lambda (item)
             (-let* (((&hash "children" "kind"
                             "dartElement" element "className" class-name "label" extra-label "codeRange" range) item)
                     (widget? (not (string= kind "DART_ELEMENT")))
                     (label (if widget?
                                (concat class-name " " extra-label)
                              (concat (gethash "name" element)
                                      (when (gethash "parameters" element)
                                        (propertize (concat " " (gethash "parameters" element))
                                                    'face 'lsp-lens-face)))))
                     (icon (if widget?
                               'flutter
                             (lsp-dart--outline-kind->icon (gethash "kind" element)))))
               (list :key label
                     :label label
                     :icon icon
                     :children (lambda (&rest _)
                                 (unless (seq-empty-p children)
                                   (lsp-dart--flutter-outline->tree uri children)))
                     :ret-action (lambda (&rest _) (lsp-dart--outline-tree-ret-action uri range))
                     :actions (when widget?
                                (lsp-dart--build-flutter-outline-widget-actions uri range))
                     :uri uri)))
           items))

(defun lsp-dart--render-outline-tree (uri outline)
  "Render an outline view with the source URI and an OUTLINE data."
  (save-excursion
    (lsp-treemacs-render
     (lsp-dart--outline->tree uri outline)
     "Outline"
     t
     "*Dart Outline*")))

(defun lsp-dart--render-flutter-outline-tree (uri outline)
  "Render an Flutter outline view with the source URI and an OUTLINE data."
  (save-excursion
    (lsp-treemacs-render
     (lsp-dart--flutter-outline->tree uri outline)
     "Flutter Outline"
     t
     "*Flutter Outline*")))

(defun lsp-dart--show-outline (buffer ignore-focus?)
  "Show an outline tree for BUFFER.
Focus on it if IGNORE-FOCUS? is nil."
  (-let* ((current-outline (lsp-dart--get-metadata buffer "current-outline"))
          ((&hash "uri" "outline" (&hash "children")) current-outline)
          (tree-buffer (lsp-dart--render-outline-tree uri children))
          (window (display-buffer-in-side-window tree-buffer lsp-dart-outline-position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

(defun lsp-dart--show-flutter-outline (buffer ignore-focus?)
  "Show a Flutter outline tree for BUFFER.
Focus on it if IGNORE-FOCUS? is nil."
  (-let* ((current-outline (lsp-dart--get-metadata buffer "current-flutter-outline"))
          ((&hash "uri" "outline" (&hash "children")) current-outline)
          (buffer (lsp-dart--render-flutter-outline-tree uri children))
          (window (display-buffer-in-side-window buffer lsp-dart-flutter-outline-position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

(defun lsp-dart--handle-outline (workspace params)
  "Outline notification handling from WORKSPACE.
PARAMS outline notification data sent.
It updates the outline view if it already exists."
  (lsp-dart--set-metadata workspace params "current-outline")
  (when (and lsp-dart-test-code-lens
             (lsp-dart-test-support-test-file-p (gethash "uri" params)))
    (lsp-dart-test-support-check-code-lens params))
  (when (get-buffer-window "*Dart Outline*")
    (lsp-dart--show-outline (lsp--buffer-for-file (lsp--uri-to-path (gethash "uri" params))) t)))

(defun lsp-dart--handle-flutter-outline (workspace params)
  "Flutter outline notification handling from WORKSPACE.
PARAMS Flutter outline notification data sent.
It updates the Flutter outline view if it already exists."
  (lsp-dart--set-metadata workspace params "current-flutter-outline")
  (when lsp-dart-flutter-widget-guides
    (lsp-dart-flutter-widget-guide-check params))
  (when (get-buffer-window "*Flutter Outline*")
    (lsp-dart--show-flutter-outline (lsp--buffer-for-file (lsp--uri-to-path (gethash "uri" params))) t)))

(defun lsp-dart--server-command ()
  "Generate LSP startup command."
  (or lsp-dart-server-command
      (let ((sdk-dir (lsp-dart-project-get-sdk-dir)))
        `(,(expand-file-name (f-join sdk-dir "bin/dart"))
          ,(expand-file-name (f-join sdk-dir "bin/snapshots/analysis_server.dart.snapshot"))
          "--lsp"))))

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
                  :notification-handlers (ht ("dart/textDocument/publishClosingLabels" 'lsp-dart--handle-closing-labels)
                                             ("dart/textDocument/publishOutline" 'lsp-dart--handle-outline)
                                             ("dart/textDocument/publishFlutterOutline" 'lsp-dart--handle-flutter-outline))
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
        (lsp-dart-project-log
         "%s at %s @ Emacs %s"
         version
         (format-time-string "%Y.%m.%d" (current-time))
         emacs-version))
    (error "Cannot determine version without package 'pkg-info'")))

;;;###autoload
(defun lsp-dart-show-outline (ignore-focus?)
  "Show an outline tree and focus on it if IGNORE-FOCUS? is nil."
  (interactive "P")
  (lsp-dart--assert-sdk-min-version "2.8.0")
  (lsp-dart--show-outline (current-buffer) ignore-focus?))

;;;###autoload
(defun lsp-dart-show-flutter-outline (ignore-focus?)
  "Show a Flutter outline tree and focus on it if IGNORE-FOCUS? is nil."
  (interactive "P")
  (lsp-dart--assert-sdk-min-version "2.8.0")
  (lsp-dart--show-flutter-outline (current-buffer) ignore-focus?))

;;;###autoload
(defun lsp-dart-run-test-at-point ()
  "Run test checking for the previous overlay at point.
Run test of the overlay which has the smallest range of
all test overlays in the current buffer."
  (interactive)
  (-some--> (overlays-in (point-min) (point-max))
    (--filter (when (overlay-get it 'lsp-dart-test-code-lens)
                (-let* (((beg . end) (overlay-get it 'lsp-dart-test-overlay-test-range)))
                  (and (>= (point) beg)
                       (<= (point) end)))) it)
    (--min-by (-let* (((beg1 . end1) (overlay-get it 'lsp-dart-test-overlay-test-range))
                      ((beg2 . end2) (overlay-get other 'lsp-dart-test-overlay-test-range)))
                (and (< beg1 beg2)
                     (> end1 end2))) it)
    (lsp-dart-test-support-run (current-buffer)
                        (overlay-get it 'lsp-dart-test-names)
                        (overlay-get it 'lsp-dart-test-kind))))

;;;###autoload
(defun lsp-dart-run-test-file ()
  "Run dart/Flutter test command only for current buffer."
  (interactive)
  (if (lsp-dart-test-support-test-file-p (buffer-file-name))
      (lsp-dart-test-support-run (current-buffer))
    (user-error "Current buffer is not a Dart/Flutter test file")))


;;;###autoload(with-eval-after-load 'lsp-mode (require 'lsp-dart))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; End:
