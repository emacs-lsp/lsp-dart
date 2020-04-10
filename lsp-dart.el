;;; lsp-dart.el --- Dart support lsp-mode -*- lexical-binding: t; -*-

;; Version: 1.0
;; Package-Requires: ((emacs "25.2") (lsp-treemacs "0.1") (lsp-mode "6.0") (ht "2.0") (f "0.20.0") (dash "2.14.1") (dart-mode "1.0.5"))
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
(require 'lsp-mode)
(require 'lsp-treemacs)

(defgroup lsp-dart nil
  "LSP support for Dart, using dart analysis server."
  :prefix "lsp-dart-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/emacs-lisp/lsp-java"))

(defcustom lsp-dart-sdk-dir nil
  "Install directory for dart-sdk.
When nil, it will try to find the dart sdk from the dart or flutter executables
in the PATH env."
  :group 'lsp-dart
  :risky t
  :type 'directory)

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

(defcustom lsp-dart-closing-labels-prefix " "
  "The prefix string to be concatened with the closing label."
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

(defcustom lsp-dart-flutter-outline t
  "Enable the analysis server Flutter outline custom method.
When set to non-nil, dart/textDocument/publishFlutterOutline notifications will
be sent with Flutter outline information for open files."
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

(defcustom lsp-dart-flutter-outline-position-params
  `((side . ,treemacs-position)
    (slot . 2)
    (window-width . ,treemacs-width))
  "The Flutter outline tree position params.
Defaults to side following treemacs default."
  :type 'list
  :group 'lsp-dart)


;;; Internal

(defun lsp-dart--find-sdk-dir ()
  "Find dart sdk by searching for dart executable or flutter cache dir."
  (-when-let (dart (or (executable-find "dart")
                       (-when-let (flutter (executable-find "flutter"))
                         (expand-file-name "cache/dart-sdk/bin/dart"
                                           (file-name-directory flutter)))))
    (-> dart
        (file-truename)
        (locate-dominating-file "bin"))))

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
                     :ret-action (lambda (&rest _) (lsp-dart--outline-tree-ret-action uri range)))))
           items))

(defun lsp-dart--render-outline-tree (uri outline)
  "Render an outline view with the source URI and an OUTLINE data."
  (lsp-treemacs-render
   (lsp-dart--outline->tree uri outline)
   "Outline"
   t
   "*Dart Outline*"))

(defun lsp-dart--render-flutter-outline-tree (uri outline)
  "Render an Flutter outline view with the source URI and an OUTLINE data."
  (lsp-treemacs-render
   (lsp-dart--flutter-outline->tree uri outline)
   "Flutter Outline"
   t
   "*Flutter Outline*"))

(defun lsp-dart--show-outline (ignore-focus?)
  "Show an outline tree.
Focus on it if IGNORE-FOCUS? is nil."
  (-let* (((&hash "uri" "outline" (&hash "children")) (lsp-workspace-get-metadata "current-outline"
                                                                                  (lsp-find-workspace 'lsp-find-workspace)))
          (buffer (lsp-dart--render-outline-tree uri children))
          (window (display-buffer-in-side-window buffer lsp-dart-outline-position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

(defun lsp-dart--show-flutter-outline (ignore-focus?)
  "Show an Flutter outline tree.
Focus on it if IGNORE-FOCUS? is nil."
  (-let* (((&hash "uri" "outline" (&hash "children")) (lsp-workspace-get-metadata "current-flutter-outline"
                                                                                  (lsp-find-workspace 'lsp-find-workspace)))
          (buffer (lsp-dart--render-flutter-outline-tree uri children))
          (window (display-buffer-in-side-window buffer lsp-dart-flutter-outline-position-params)))
    (unless ignore-focus?
      (select-window window)
      (set-window-dedicated-p window t))))

(defun lsp-dart--handle-outline (workspace params)
  "Outline notification handling.
PARAMS outline notification data sent from WORKSPACE.
It updates the outline view if it already exists."
  (lsp-workspace-set-metadata "current-outline" params workspace)
  (when (get-buffer-window "*Dart Outline*")
    (lsp-dart--show-outline t)))

(defun lsp-dart--handle-flutter-outline (workspace params)
  "Flutter outline notification handling.
PARAMS Flutter outline notification data sent from WORKSPACE.
It updates the Flutter outline view if it already exists."
  (lsp-workspace-set-metadata "current-flutter-outline" params workspace)
  (when (get-buffer-window "*Flutter Outline*")
    (lsp-dart--show-flutter-outline t)))

(defun lsp-dart--server-command ()
  "Generate LSP startup command."
  (or lsp-dart-server-command
      (let ((sdk-dir (or lsp-dart-sdk-dir (lsp-dart--find-sdk-dir))))
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
(defun lsp-dart-show-outline (ignore-focus?)
  "Show an outline tree and focus on it if IGNORE-FOCUS? is nil."
  (interactive "P")
  (lsp-dart--show-outline ignore-focus?))

;;;###autoload
(defun lsp-dart-show-flutter-outline (ignore-focus?)
  "Show a Flutter outline tree and focus on it if IGNORE-FOCUS? is nil."
  (interactive "P")
  (lsp-dart--show-flutter-outline ignore-focus?))


;;;###autoload(with-eval-after-load 'lsp-mode (require 'lsp-dart))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; End:
