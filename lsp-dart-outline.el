;;; lsp-dart-outline.el --- Outline support for lsp-dart -*- lexical-binding: t; -*-
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
;;  Outline support for lsp-dart
;;
;;; Code:

(require 'lsp-lens)
(require 'lsp-treemacs)

(require 'lsp-dart-protocol)
(require 'lsp-dart-utils)

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

(defcustom lsp-dart-outline-arrived-hook nil
  "Hooks to run after a outline notification arrive.
The hook will receive the notification data as argument."
  :type 'hook
  :group 'lsp-dart)

(defcustom lsp-dart-flutter-outline-arrived-hook nil
  "Hooks to run after a flutter outline notification arrive.
The hook will receive the notification data as argument."
  :type 'hook
  :group 'lsp-dart)


;;; Internal

(defvar-local lsp-dart-current-outline nil)
(defvar-local lsp-dart-current-flutter-outline nil)

(defconst lsp-dart--outline-buffer-name "*Dart Outline*")
(defconst lsp-dart--flutter-outline-buffer-name "*Flutter Outline*")

(defun lsp-dart-outline--set-metadata (workspace params key-prefix)
  "Save in WORKSPACE the PARAMS metadata with KEY-PREFIX.
The key is composed of the KEY-PREFIX with PARAMS uri path."
  (-let* ((uri-path (-> params lsp:outline-notification-uri lsp--uri-to-path))
          (key (concat key-prefix "--" uri-path)))
    (lsp-workspace-set-metadata key params workspace)))

(defun lsp-dart-outline--get-metadata (buffer key-prefix)
  "Return the metadata saved in current workspace of BUFFER for KEY-PREFIX."
  (let ((key (concat key-prefix "--" (buffer-file-name buffer))))
    (lsp-workspace-get-metadata key (lsp-find-workspace 'lsp-find-workspace))))

(defun lsp-dart-outline--outline-kind->icon (kind)
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

(defun lsp-dart-outline--outline-tree-ret-action (uri range)
  "Build the ret action for an item in the outline tree view.
URI is the source of the item.
RANGE is the range of positions to where this item should point."
  (interactive)
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (goto-char (lsp--position-to-point (lsp:range-start range)))
  (run-hooks 'xref-after-jump-hook))

(defun lsp-dart-outline--outlines->tree (uri outlines)
  "Builds an outline tree.
URI is the source of the outline.
OUTLINES are the outline items."
  (seq-map (-lambda ((&Outline :children
                               :element (&Element :kind :name :range :parameters?)))
             (let ((label (concat name (when parameters?
                                         (propertize (concat " " parameters?)
                                                     'face 'lsp-lens-face)))))
               (list :key label
                     :label label
                     :icon (lsp-dart-outline--outline-kind->icon kind)
                     :children (lambda (&rest _)
                                 (unless (seq-empty-p children)
                                   (lsp-dart-outline--outlines->tree uri children)))
                     :ret-action (lambda (&rest _) (lsp-dart-outline--outline-tree-ret-action uri range)))))
           outlines))

(defun lsp-dart-outline--build-flutter-outline-widget-actions (uri range)
  "Build the action options for the Flutter outline tree view.
URI is the source of the outline.
RANGE is the range for currently build item."
  (-when-let (buffer (find-buffer-visiting (lsp--uri-to-path uri)))
    (with-current-buffer buffer
      (goto-char (-> range lsp--range-to-region car))
      (->> (lsp-code-actions-at-point)
           (-filter (-lambda ((&CodeAction :kind?))
                      (and kind? (string-match "^refactor" kind?))))
           (-map (-lambda ((action &as &CodeAction :title))
                   `[,title (lsp-execute-code-action ,action)]))))))

(defun lsp-dart-outline--flutter-outline->tree (uri outlines)
  "Builds a Flutter outline tree.
URI is the source of the outline.
OUTLINES are the outline items."
  (seq-map
   (-lambda ((&FlutterOutline :children :kind :code-range :class-name? :label? :dart-element?))
     (-let* ((widget? (not (string= kind "DART_ELEMENT")))
             (label (if widget?
                        (concat class-name? " " label?)
                      (concat (lsp:element-name dart-element?)
                              (when (lsp:element-parameters? dart-element?)
                                (propertize (concat " " (lsp:element-parameters? dart-element?))
                                            'face 'lsp-lens-face)))))
             (icon (if widget?
                       'flutter
                     (lsp-dart-outline--outline-kind->icon (lsp:element-kind dart-element?)))))
       (list :key label
             :label label
             :icon icon
             :children (lambda (&rest _)
                         (unless (seq-empty-p children)
                           (lsp-dart-outline--flutter-outline->tree uri children)))
             :ret-action (lambda (&rest _) (lsp-dart-outline--outline-tree-ret-action uri code-range))
             :actions (when widget?
                        (lsp-dart-outline--build-flutter-outline-widget-actions uri code-range))
             :uri uri)))
   outlines))

(defun lsp-dart-outline--render-outline-tree (uri outlines)
  "Render an outline view with the source URI and OUTLINES data."
  (save-excursion
    (lsp-treemacs-render
     (lsp-dart-outline--outlines->tree uri outlines)
     "Outline"
     t
     lsp-dart--outline-buffer-name)))

(defun lsp-dart-outline--render-flutter-outline-tree (uri outlines)
  "Render an Flutter outline view with the source URI and OUTLINES data."
  (save-excursion
    (lsp-treemacs-render
     (lsp-dart-outline--flutter-outline->tree uri outlines)
     "Flutter Outline"
     t
     lsp-dart--flutter-outline-buffer-name)))

(defun lsp-dart-outline--show-outline (ignore-focus?)
  "Show an outline tree for BUFFER.
Focus on it if IGNORE-FOCUS? is nil."
  (-if-let ((&OutlineNotification? :uri :outline (&Outline :children)) lsp-dart-current-outline)
    (-let* ((tree-buffer (lsp-dart-outline--render-outline-tree uri children))
            (window (display-buffer-in-side-window tree-buffer lsp-dart-outline-position-params)))
      (unless ignore-focus?
        (select-window window)
        (set-window-dedicated-p window t)))
    (lsp-dart-log "No Dart outline data found")))

(defun lsp-dart-outline--show-flutter-outline (ignore-focus?)
  "Show a Flutter outline tree for BUFFER.
Focus on it if IGNORE-FOCUS? is nil."
  (-if-let ((&FlutterOutlineNotification? :uri :outline (&FlutterOutline :children)) lsp-dart-current-flutter-outline)
    (-let* ((tree-buffer (lsp-dart-outline--render-flutter-outline-tree uri children))
            (window (display-buffer-in-side-window tree-buffer lsp-dart-flutter-outline-position-params)))
      (unless ignore-focus?
        (select-window window)
        (set-window-dedicated-p window t)))
    (lsp-dart-log "No Flutter outline data found")))

(lsp-defun lsp-dart--outline-check ((notification &as &OutlineNotification :uri))
  "Outline notification handling from WORKSPACE.
NOTIFICATION is outline notification data received from server.
It updates the outline view if it already exists."
   (when-let (buffer (find-buffer-visiting (lsp--uri-to-path uri)))
     (with-current-buffer buffer
      (setq lsp-dart-current-outline notification)
       (when (get-buffer-window lsp-dart--outline-buffer-name)
         (lsp-dart-outline--show-outline t)))))

(lsp-defun lsp-dart--flutter-outline-check ((notification &as &FlutterOutlineNotification :uri))
  "Flutter outline notification handling from WORKSPACE.
NOTIFICATION is Flutter outline notification data received from server.
It updates the Flutter outline view if it already exists."
  (when-let (buffer (find-buffer-visiting (lsp--uri-to-path uri)))
    (with-current-buffer buffer
      (setq lsp-dart-current-flutter-outline notification)
      (when (get-buffer-window lsp-dart--flutter-outline-buffer-name)
        (lsp-dart-outline--show-flutter-outline t)))))


;;; Public interface

(define-minor-mode lsp-dart-outline-mode
  "Mode for updating outline."
  nil nil nil
  (cond
   (lsp-dart-outline-mode
    (add-hook 'lsp-dart-outline-arrived-hook #'lsp-dart--outline-check nil t))
   (t
    (remove-hook 'lsp-dart-outline-arrived-hook #'lsp-dart--outline-check t))))

(define-minor-mode lsp-dart-flutter-outline-mode
  "Mode for updating flutter outline."
  nil nil nil
  (cond
   (lsp-dart-flutter-outline-mode
    (add-hook 'lsp-dart-flutter-outline-arrived-hook #'lsp-dart--flutter-outline-check nil t))
   (t
    (remove-hook 'lsp-dart-flutter-outline-arrived-hook #'lsp-dart--flutter-outline-check t))))

;;;###autoload
(defun lsp-dart-show-outline (ignore-focus?)
  "Show an outline tree and focus on it if IGNORE-FOCUS? is nil."
  (interactive "P")
  (lsp-dart-assert-sdk-min-version "2.8.0")
  (lsp-dart-outline--show-outline ignore-focus?))

;;;###autoload
(defun lsp-dart-show-flutter-outline (ignore-focus?)
  "Show a Flutter outline tree and focus on it if IGNORE-FOCUS? is nil."
  (interactive "P")
  (lsp-dart-assert-sdk-min-version "2.8.0")
  (lsp-dart-outline--show-flutter-outline ignore-focus?))

(provide 'lsp-dart-outline)
;;; lsp-dart-outline.el ends here
