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
  :link '(url-link :tag "GitHub" "https://github.com/emacs-lsp/lsp-dart"))

(defcustom lsp-dart-sdk-dir nil
  "Install directory for dart-sdk.
When nil, it will try to find the dart sdk from the dart or flutter executables
in the PATH env."
  :group 'lsp-dart
  :risky t
  :type '(choice directory nil))

(defcustom lsp-dart-flutter-command "flutter"
  "Flutter command for running tests."
  :group 'lsp-dart
  :type 'string)

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

(defcustom lsp-dart-test-code-lens t
  "Enable the test code lens overlays."
  :type 'boolean
  :group 'lsp-dart)


;;; Internal

(defun lsp-dart--find-sdk-dir ()
  "Find dart sdk by searching for dart executable or flutter cache dir."
  (-when-let (dart (or (executable-find "dart")
                       (-when-let (flutter (-> lsp-dart-flutter-command
                                               executable-find
                                               file-truename))
                         (expand-file-name "cache/dart-sdk/bin/dart"
                                           (file-name-directory flutter)))))
    (-> dart
        file-truename
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
  (when (and lsp-dart-test-code-lens
             (lsp-dart-test-file-p (gethash "uri" params)))
    (lsp-dart-check-test-code-lens params))
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

;;; test

(defun lsp-dart--test-method-p (kind)
  "Return non-nil if KIND is a test type."
  (or (string= kind "UNIT_TEST_TEST")
      (string= kind "UNIT_TEST_GROUP")))

(defun lsp-dart--test-flutter-test-file-p (buffer)
  "Return non-nil if the BUFFER appears to be a flutter test file."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^import 'package:flutter_test/flutter_test.dart';"
                         nil t))))

(defun lsp-dart--last-index-of (regex str &optional ignore-case)
  "Find the last index of a REGEX in a string STR.
IGNORE-CASE is a optional arg to ignore the case sensitive on regex search."
  (let ((start 0)
        (case-fold-search ignore-case)
        idx)
    (while (string-match regex str start)
      (setq idx (match-beginning 0))
      (setq start (match-end 0)))
    idx))

(defun lsp-dart--test-get-project-root ()
  "Return the dart or flutter project root."
  (locate-dominating-file default-directory "pubspec.yaml") )

(defmacro lsp-dart--test-from-project-root (&rest body)
  "Execute BODY with cwd set to the project root."
  `(let ((project-root (lsp-dart--test-get-project-root)))
     (if project-root
         (let ((default-directory project-root))
           ,@body)
       (error "Dart or Flutter project not found (pubspec.yaml not found)"))))

(defun lsp-dart--build-command (buffer)
  "Build the dart or flutter build command.
If the given BUFFER is a flutter test file, return the flutter command
otherwise the dart command."
  (let ((sdk-dir (or lsp-dart-sdk-dir (lsp-dart--find-sdk-dir))))
    (if (lsp-dart--test-flutter-test-file-p buffer)
        lsp-dart-flutter-command
      (concat (file-name-as-directory sdk-dir) "bin/pub run"))))

(defun lsp-dart--build-test-name (names)
  "Build the test name from a group of test NAMES."
  (when (and names
             (not (seq-empty-p names)))
    (->> names
         (--map (substring it
                           (+ (cl-search "(" it) 2)
                           (- (lsp-dart--last-index-of ")" it) 1)))
         (--reduce (format "%s %s" acc it)))))

(defun lsp-dart--escape-test-name (name)
  "Return the dart safe escaped test NAME."
  (let ((escaped-str (regexp-quote name)))
    (seq-doseq (char '("(" ")" "{" "}"))
      (setq escaped-str (replace-regexp-in-string char
                                                  (concat "\\" char)
                                                  escaped-str nil t)))
    escaped-str))

(defun lsp-dart--run-test (buffer &optional names kind)
  "Run Dart/Flutter test command in a compilation buffer for BUFFER file.
If NAMES is non nil, it will run only for KIND the test joining the name
from NAMES."
  (interactive)
  (lsp-dart--test-from-project-root
   (let* ((test-file (file-relative-name (buffer-file-name buffer)
                                         (lsp-dart--test-get-project-root)))
          (test-name (lsp-dart--build-test-name names))
          (group-kind? (string= kind "UNIT_TEST_GROUP"))
          (test-arg (when test-name
                      (concat "--name '^"
                              (lsp-dart--escape-test-name test-name)
                              (if group-kind? "'" "$'")))))
     (compilation-start (format "%s test %s %s"
                                (lsp-dart--build-command buffer)
                                (or test-arg "")
                                test-file)
                        t
                        (lambda (_) "*LSP Dart tests*")))))

(defun lsp-dart--build-test-overlay (buffer names kind range test-range)
  "Build an overlay for a test NAMES of KIND in BUFFER file.
RANGE is the overlay range to build."
  (-let* ((beg-position (gethash "character" (gethash "start" range)))
          ((beg . end) (lsp--range-to-region range))
          (beg-line (progn (goto-char beg)
                           (line-beginning-position)))
          (spaces (make-string beg-position ?\s))
          (overlay (make-overlay beg-line end buffer)))
    (overlay-put overlay 'lsp-dart-test-code-lens t)
    (overlay-put overlay 'lsp-dart-test-names names)
    (overlay-put overlay 'lsp-dart-test-kind kind)
    (overlay-put overlay 'lsp-dart-test-overlay-test-range (lsp--range-to-region test-range))
    (overlay-put overlay 'before-string
                 (concat spaces
                         (propertize "Run\n"
                                     'help-echo "mouse-1: Run this test"
                                     'mouse-face 'lsp-lens-mouse-face
                                     'local-map (-doto (make-sparse-keymap)
                                                 (define-key [mouse-1] (lambda ()
                                                                          (interactive)
                                                                          (lsp-dart--run-test buffer names kind))))
                                     'font-lock-face 'lsp-lens-face)))))

(defun lsp-dart--add-test-code-lens (buffer items &optional names)
  "Add test code lens to BUFFER for ITEMS.
NAMES arg is optional and are the group of tests representing a test name."
  (seq-doseq (item items)
    (-let* (((&hash "children" "codeRange" test-range "element"
                    (&hash "kind" "name" "range")) item)
            (test-kind? (lsp-dart--test-method-p kind))
            (concatened-names (if test-kind?
                                  (append names (list name))
                                names)))
      (when test-kind?
        (lsp-dart--build-test-overlay buffer (append names (list name)) kind range test-range))
      (unless (seq-empty-p children)
        (lsp-dart--add-test-code-lens buffer children concatened-names)))))

(defun lsp-dart-test-file-p (file-name)
  "Return non-nil if FILE-NAME is a dart test files."
  (string-match "_test.dart" file-name))

(defun lsp-dart-check-test-code-lens (params)
  "Check for test adding lens to it.
PARAMS is the notification data from outline."
  (-let* (((&hash "uri" "outline" (&hash "children")) params)
          (buffer (lsp--buffer-for-file (lsp--uri-to-path uri))))
    (when buffer
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'lsp-dart-test-code-lens t)
        (save-excursion
          (lsp-dart--add-test-code-lens buffer children))))))


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
    (lsp-dart--run-test (current-buffer)
                        (overlay-get it 'lsp-dart-test-names)
                        (overlay-get it 'lsp-dart-test-kind))))

;;;###autoload
(defun lsp-dart-run-test-file ()
  "Run dart/Flutter test command only for current buffer."
  (interactive)
  (if (lsp-dart-test-file-p (buffer-file-name))
      (lsp-dart--run-test (current-buffer))
    (user-error "Current buffer is not a Dart/Flutter test file")))


;;;###autoload(with-eval-after-load 'lsp-mode (require 'lsp-dart))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; End:
