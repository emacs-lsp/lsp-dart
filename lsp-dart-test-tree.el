;;; lsp-dart-test-tree.el --- Test tree support -*- lexical-binding: t; -*-
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
;;  Test tree support
;;
;;; Code:

(require 'lsp-treemacs)

(require 'lsp-dart-protocol)
(require 'lsp-dart-test-support)

(defcustom lsp-dart-test-tree-on-run t
  "Enable the test tree when running tests."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-test-tree-position-params
  `((side . ,treemacs-position)
    (slot . 2)
    (window-width . ,treemacs-width))
  "The test tree position params.
Defaults to side following treemacs default."
  :type 'list
  :group 'lsp-dart)

(defcustom lsp-dart-test-tree-line-spacing 4
  "The test tree line spacing between nodes."
  :type 'integer
  :group 'lsp-dart)

(defface lsp-dart-test-tree-waiting-face
  '((t :inherit font-lock-comment-face))
  "The face used for tests with status waiting on tests tree."
  :group 'lsp-dart)

(defface lsp-dart-test-tree-running-face
  '((t :inherit warning))
  "The face used for tests with status running on tests tree."
  :group 'lsp-dart)

(defface lsp-dart-test-tree-passed-face
  '((t :inherit success))
  "The face used for tests with status passed on tests tree."
  :group 'lsp-dart)

(defface lsp-dart-test-tree-error-face
  '((t :inherit error))
  "The face used for tests with status error on tests tree."
  :group 'lsp-dart)

(defface lsp-dart-test-tree-failed-face
  '((((background dark)) :foreground "#ce5643")
    (((background light)) :foreground "#843031"))
  "The face used for tests with status failed on tests tree."
  :group 'lsp-dart)

(defface lsp-dart-test-tree-skipped-face
  '((t :inherit homoglyph))
  "The face used for tests with status skipped on tests tree."
  :group 'lsp-dart)

(defface lsp-dart-test-tree-time-face
  '((t :height 0.8 :inherit font-lock-comment-face))
  "The face used for tests time on tests tree."
  :group 'lsp-dart)


;;; Internal

(defconst lsp-dart-test-tree--buffer-name "*LSP Dart tests tree*")

(defvar lsp-dart-test-tree--suites nil)
(defvar lsp-dart-test-tree--groups-by-id nil)
(defvar lsp-dart-test-tree--tests-by-id nil)

(treemacs-modify-theme (treemacs-theme->name treemacs--current-theme)
  :config
  (progn
    (treemacs-create-icon :icon (propertize "⌛ " 'face 'lsp-dart-test-tree-waiting-face)
                          :extensions (waiting)
                          :fallback "⌛ ")
    (treemacs-create-icon :icon (propertize "▶ " 'face 'lsp-dart-test-tree-running-face)
                          :extensions (running)
                          :fallback "▶ ")
    (treemacs-create-icon :icon (propertize "✔ " 'face 'lsp-dart-test-tree-passed-face)
                          :extensions (passed)
                          :fallback "✔ ")
    (treemacs-create-icon :icon (propertize "✖ " 'face 'lsp-dart-test-tree-failed-face)
                          :extensions (failed)
                          :fallback "✖ ")
    (treemacs-create-icon :icon (propertize "! " 'face 'lsp-dart-test-tree-error-face)
                          :extensions (errored)
                          :fallback "! ")
    (treemacs-create-icon :icon (propertize "✔ " 'face 'lsp-dart-test-tree-skipped-face)
                          :extensions (skipped)
                          :fallback "✔ ")))

(defun lsp-dart--plist-set! (plist key id val)
  "Update PLIST in KEY with ID for VAL."
  (let* ((old-list (plist-get plist key))
         (new-list (if-let (existing-val (alist-get id old-list))
                       (progn
                         (setq existing-val val)
                         old-list)
                     (append old-list (list (cons id val))))))
    (setq plist (plist-put plist key new-list))))

(defun lsp-dart-test-tree--result->status (result skipped)
  "Return the corresponding status from RESULT and SKIPPED."
  (if skipped
      'skipped
    (pcase result
      ("success" 'passed)
      ("failure" 'failed)
      ("error" 'errored))))

(defun lsp-dart-test-tree--colorize-name (name status &optional time?)
  "Colorize NAME with the corresponding face for STATUS and TIME."
  (let* ((face (pcase status
                 ('skipped 'lsp-dart-test-tree-skipped-face)
                 ('passed 'lsp-dart-test-tree-passed-face)
                 ('failed 'lsp-dart-test-tree-failed-face)
                 ('errored 'lsp-dart-test-tree-error-face)
                 ('waiting 'lsp-dart-test-tree-waiting-face)
                 ('running 'lsp-dart-test-tree-running-face)))
         (test-name (propertize name
                                'face face)))
    (if time?
        (concat test-name " " (propertize (concat "("
                                                  (number-to-string time?)
                                                  "ms)")
                                          'face 'lsp-dart-test-tree-time-face))
      test-name)))

(defun lsp-dart-test-tree--substring-name (parent-plist child-plist)
  "Return the name substringing from PARENT-PLIST and CHILD-PLIST."
  (let* ((parent-name (plist-get parent-plist :name))
         (child-name (plist-get child-plist :name))
         (parent-length (length parent-name))
         (child-length (length child-name)))
    (if (and (> (length parent-name) 0)
             (>= child-length parent-length))
        (-if-let (parent-position (cl-search parent-name child-name))
            (substring child-name (1+ (+ parent-position parent-length)))
          child-name)
      child-name)))

(defun lsp-dart-test--add-suite (suite)
  "Add a test SUITE."
  (add-to-list 'lsp-dart-test-tree--suites
               (cons (plist-get suite :id) suite)
               t))

(defun lsp-dart-test-tree--set-child-group (parent-group group target-parent-id)
  "Recursively upsert GROUP for TARGET-PARENT-ID and PARENT-GROUP."
  (if (= (plist-get parent-group :id) target-parent-id)
      (let ((new-name (lsp-dart-test-tree--substring-name parent-group group)))
        (lsp-dart--plist-set! parent-group :groups (plist-get group :id) (plist-put group :name new-name)))
    (seq-doseq (groups-by-id (plist-get parent-group :groups))
      (lsp-dart-test-tree--set-child-group (cdr groups-by-id)
                                           group
                                           target-parent-id))))

(defun lsp-dart-test-tree--set-group (suite-id group &optional parent-id)
  "Upsert GROUP with PARENT-id for SUITE-ID."
  (let ((suite (alist-get suite-id lsp-dart-test-tree--suites)))
    (if parent-id
        (seq-doseq (groups-by-id (plist-get suite :groups))
          (lsp-dart-test-tree--set-child-group (cdr groups-by-id)
                                               group
                                               parent-id))
      (lsp-dart--plist-set! suite :groups (plist-get group :id) group))))

(defun lsp-dart-test-tree--set-child-test (parent-group group-ids last-group-id test)
  "Upsert TEST with GROUP-IDS and LAST-GROUP-ID for PARENT-GROUP."
  (let ((parent-group-id (plist-get parent-group :id))
        (parent-group-status (plist-get parent-group :status)))
    (when (and (member parent-group-id group-ids)
               (not (or (equal parent-group-status 'failed)
                        (equal parent-group-status 'errored))))
      (if (equal (plist-get test :status) 'skipped)
          (setq parent-group (plist-put parent-group :status 'skipped))
        (setq parent-group (plist-put parent-group :status (plist-get test :status)))))
    (if (= parent-group-id last-group-id)
        (let ((new-name (lsp-dart-test-tree--substring-name parent-group test)))
          (lsp-dart--plist-set! parent-group :tests (plist-get test :id) (plist-put test :name new-name)))
      (seq-doseq (groups-by-id (plist-get parent-group :groups))
        (lsp-dart-test-tree--set-child-test (cdr groups-by-id)
                                            group-ids
                                            last-group-id
                                            test)))))

(defun lsp-dart-test-tree--set-test (suite-id group-ids test)
  "Upsert TEST with GROUP-IDS for SUITE-ID."
  (when-let (suite (alist-get suite-id lsp-dart-test-tree--suites))
    (let ((suite-status (plist-get suite :status)))
      (unless (or (equal suite-status 'failed)
                  (equal suite-status 'errored))
        (setq suite (plist-put suite :status (plist-get test :status)))))
    (seq-doseq (groups-by-id (plist-get suite :groups))
      (lsp-dart-test-tree--set-child-test (cdr groups-by-id)
                                          (append group-ids nil)
                                          (car (last (append group-ids nil)))
                                          test))))

(defun lsp-dart-test-tree--ret-action (uri &optional position)
  "Build the ret action for an item in the test tree view.
URI is the test uri.
POSITION is the test start position."
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (when position
    (goto-char (lsp--position-to-point position)))
  (run-hooks 'xref-after-jump-hook))

(defun lsp-dart-test-tree--run-test (uri &optional position)
  "Run test from POSITION and URI from tree."
  (lsp-treemacs--open-file-in-mru (lsp--uri-to-path uri))
  (if position
      (progn
        (goto-char (lsp--position-to-point position))
        (lsp-dart-test--run (lsp-dart-test--test-at-point)))
    (lsp-dart-test--run (list :file-name (lsp--uri-to-path uri)))))

(defun lsp-dart-test-tree--build-suite-actions (suite)
  "Build the action options for SUITE in test tree view."
  (let ((uri (plist-get suite :path)))
    `(["Go to file" (lsp-dart-test-tree--ret-action ,uri)]
      ["Run file tests again" (lsp-dart-test-tree--run-test ,uri)])))

(defun lsp-dart-test-tree--build-group-actions (group)
  "Build the action options for GROUP in test tree view."
  (let ((uri (plist-get group :uri))
        (position (plist-get group :position)))
    `(["Go to group" (lsp-dart-test-tree--ret-action ,uri ,position)]
      ["Run group tests again" (lsp-dart-test-tree--run-test ,uri ,position)])))

(defun lsp-dart-test-tree--build-test-actions (test)
  "Build the action options for TEST in test tree view."
  (let ((uri (plist-get test :uri))
        (position (plist-get test :position)))
    `(["Go to test" (lsp-dart-test-tree--ret-action ,uri ,position)]
      ["Run test again" (lsp-dart-test-tree--run-test ,uri ,position)])))

(defun lsp-dart-test-tree--suite->tree (suite-or-group)
  "Map SUITE-OR-GROUP to treemacs tree items."
  (let ((tests (seq-map
                (-lambda ((test-id . test))
                  (list :key (concat "test-" (number-to-string test-id))
                        :label (lsp-dart-test-tree--colorize-name (plist-get test :name)
                                                                  (plist-get test :status)
                                                                  (plist-get test :time))
                        :icon (plist-get test :status)
                        :ret-action (lambda (&rest _) (lsp-dart-test-tree--ret-action (plist-get test :uri)
                                                                                      (plist-get test :position)))
                        :actions (lsp-dart-test-tree--build-test-actions test)))
                (plist-get suite-or-group :tests)))
        (groups (seq-map
                 (-lambda ((group-id . group))
                   (if (= (length (plist-get group :name)) 0)
                       (list :key (concat "suite-" (number-to-string (plist-get suite-or-group :id)))
                             :label (lsp-dart-test-tree--colorize-name (f-filename (plist-get suite-or-group :path))
                                                                       (plist-get suite-or-group :status))
                             :icon (plist-get suite-or-group :status)
                             :children (lsp-dart-test-tree--suite->tree group)
                             :ret-action (lambda (&rest _) (lsp-dart-test-tree--ret-action (plist-get group :uri)))
                             :actions (lsp-dart-test-tree--build-suite-actions suite-or-group))
                     (list :key (concat "group-" (number-to-string group-id))
                           :icon (plist-get group :status)
                           :label (lsp-dart-test-tree--colorize-name (plist-get group :name)
                                                                     (plist-get group :status))
                           :children (lsp-dart-test-tree--suite->tree group)
                           :ret-action (lambda (&rest _) (lsp-dart-test-tree--ret-action (plist-get group :uri)
                                                                                         (plist-get group :position)))
                           :actions (lsp-dart-test-tree--build-group-actions group))))
                 (plist-get suite-or-group :groups))))
    (when (or tests groups)
      (append tests groups))))

(defun lsp-dart-test-tree--build ()
  "Build the test tree for treemacs."
  (or (->> lsp-dart-test-tree--suites
           map-values
           (seq-map #'lsp-dart-test-tree--suite->tree)
           (-flatten-n 1)
           -non-nil)
      (list (list :label "Running tests..."))))

(defun lsp-dart-test-tree--render ()
  "Show the test tree buffer."
  (save-excursion
    (lsp-treemacs-render
     (lsp-dart-test-tree--build)
     "Tests"
     t
     lsp-dart-test-tree--buffer-name)))

(defun lsp-dart-test-tree--handle-run-started ()
  "Handle run started notification."
  (lsp-dart-test-tree-clean)
  (when lsp-dart-test-tree-on-run
    (lsp-dart-test-show-tree)))

(lsp-defun lsp-dart-test-tree--handle-suite ((&SuiteNotification :suite))
  "Handle suite notification."
  (lsp-dart-test-tree-add-suite suite))

(lsp-defun lsp-dart-test-tree--handle-group ((&GroupNotification :group))
  (lsp-dart-test-tree-set-group group))

(lsp-defun lsp-dart-test-tree--handle-start ((&TestStartNotification :test))
  "Handle test start notification."
  (lsp-dart-test-tree-set-test test 'running))

(lsp-defun lsp-dart-test-tree--handle-done ((&TestDoneNotification :test-id :result :time :skipped) _test-name test-start-time)
  "Handle test done notification."
  (lsp-dart-test-tree-mark-as-done test-id (- time test-start-time) result skipped))

(defun lsp-dart-test-tree--render-final (suite-or-group)
  "Rebuild the test tree for SUITE-OR-GROUP one last time."
  (seq-map
   (-lambda ((_group-id . group))
     (lsp-dart-test-tree--render-final group)
     (lsp-dart-test-tree--render))
   (plist-get suite-or-group :groups)))

(defun lsp-dart-test-tree--handle-all-done (_params)
  "Handle test done notification."
  (seq-map #'lsp-dart-test-tree--render-final (map-values lsp-dart-test-tree--suites)))


;;; Public

(lsp-defun lsp-dart-test-tree-add-suite ((&Suite :id :path))
  "Add suite to test tree."
  (lsp-dart-test--add-suite (list :id id :path path :status 'waiting))
  (lsp-dart-test-tree--render))

(lsp-defun lsp-dart-test-tree-set-group ((&Group :id :suite-id :name? :parent-id?
                                                 :url? :line? :column?))
  "Upsert group to test tree."
  (let* ((new-group (list :id id
                          :name name?
                          :uri url?
                          :position (lsp-make-position :line (when line? (1- line?))
                                                       :character (when column? (1- column?))))))
    (add-to-list 'lsp-dart-test-tree--groups-by-id (cons id (list :name name?
                                                                  :parent-id parent-id?)))
    (lsp-dart-test-tree--set-group suite-id
                                   new-group
                                   parent-id?))
  (lsp-dart-test-tree--render))

(lsp-defun lsp-dart-test-tree-set-test ((&Test :id :suite-id :name? :group-i-ds
                                               :root-url? :url?
                                               :root-line? :root-column?
                                               :line? :column?) status)
  "Upsert test to test tree."
  (unless (seq-empty-p group-i-ds)
    (let ((new-test (list :id id
                          :suite-id suite-id
                          :name name?
                          :status status
                          :uri (or root-url? url?)
                          :position (lsp-make-position :line (1- (or root-line? line? 0))
                                                       :character (1- (or root-column? column? 0)))
                          :group-ids group-i-ds)))
      (add-to-list 'lsp-dart-test-tree--tests-by-id (cons id new-test))
      (lsp-dart-test-tree--set-test suite-id
                                    group-i-ds
                                    new-test))
    (lsp-dart-test-tree--render)))

(defun lsp-dart-test-tree-mark-as-done (test-id time result skipped)
  "Update TEST-ID with TIME and status from RESULT and SKIPPED."
  (let* ((test (alist-get test-id lsp-dart-test-tree--tests-by-id))
         (updated-test (-> test
                           (plist-put :status (lsp-dart-test-tree--result->status result skipped))
                           (plist-put :time time))))
    (lsp-dart-test-tree--set-test (plist-get test :suite-id)
                                  (plist-get test :group-ids)
                                  updated-test)
    (lsp-dart-test-tree--render)))

(defun lsp-dart-test-tree-clean ()
  "Clean test tree."
  (setq lsp-dart-test-tree--suites nil)
  (setq lsp-dart-test-tree--groups-by-id nil)
  (setq lsp-dart-test-tree--tests-by-id nil))


;;; Public Interface

(defun lsp-dart-test-show-tree ()
  "Show test tree of the current/last ran test."
  (interactive)
  (let ((tree-buffer (lsp-dart-test-tree--render)))
    (with-current-buffer tree-buffer
      (setq-local line-spacing lsp-dart-test-tree-line-spacing))
    (display-buffer-in-side-window tree-buffer lsp-dart-test-tree-position-params)))

(add-hook 'lsp-dart-test-run-started-hook #'lsp-dart-test-tree--handle-run-started)
(add-hook 'lsp-dart-test-suite-notification-hook #'lsp-dart-test-tree--handle-suite)
(add-hook 'lsp-dart-test-group-notification-hook #'lsp-dart-test-tree--handle-group)
(add-hook 'lsp-dart-test-start-notification-hook #'lsp-dart-test-tree--handle-start)
(add-hook 'lsp-dart-test-done-notification-hook #'lsp-dart-test-tree--handle-done)
(add-hook 'lsp-dart-test-all-done-notification-hook #'lsp-dart-test-tree--handle-all-done)

(provide 'lsp-dart-test-tree)
;;; lsp-dart-test-tree.el ends here
