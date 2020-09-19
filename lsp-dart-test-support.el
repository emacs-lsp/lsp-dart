;;; lsp-dart-test-support.el --- Test support for LSP Dart -*- lexical-binding: t; -*-
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

;; UI guide lines on flutter widgets

;;; Code:

(require 'cl-lib)
(require 'lsp-mode)

(require 'lsp-dart-protocol)
(require 'lsp-dart-utils)
(require 'lsp-dart-dap)


;;; Internal

(defconst lsp-dart-test--process-buffer-name "*LSP Dart - tests process*")

(defvar-local lsp-dart-test--tests nil)
(defvar lsp-dart-test--running-tests nil)

(cl-defstruct lsp-dart-running-test
  (id nil)
  (name nil)
  (start-time nil))

(defun lsp-dart-test--test-kind-p (kind)
  "Return non-nil if KIND is a test type."
  (or (string= kind "UNIT_TEST_TEST")
      (string= kind "UNIT_TEST_GROUP")))

(defun lsp-dart-test--last-index-of (regex str &optional ignore-case)
  "Find the last index of a REGEX in a string STR.
IGNORE-CASE is a optional arg to ignore the case sensitive on regex search."
  (let ((start 0)
        (case-fold-search ignore-case)
        idx)
    (while (string-match regex str start)
      (setq idx (match-beginning 0))
      (setq start (match-end 0)))
    idx))

(defun lsp-dart-test--build-command ()
  "Build the dart or flutter build command checking project type."
  (if (lsp-dart--flutter-project-p)
      (lsp-dart-flutter-command)
    (lsp-dart-pub-command)))

(defun lsp-dart-test--build-command-extra-args ()
  "Build the dart or flutter extra args."
  (if (lsp-dart--flutter-project-p)
      '("test" "--machine")
    '("run" "test" "-r" "json")))

(defun lsp-dart-test--build-test-name (names)
  "Build the test name from a group of test NAMES."
  (when (and names
             (not (seq-empty-p names)))
    (->> names
         (--map (substring it
                           (+ (cl-search "(" it) 2)
                           (- (lsp-dart-test--last-index-of ")" it) 1)))
         (--reduce (format "%s %s" acc it)))))

(defun lsp-dart-test--escape-test-name (name)
  "Return the dart safe escaped test NAME."
  (let ((escaped-str (regexp-quote name)))
    (seq-doseq (char '("(" ")" "{" "}"))
      (setq escaped-str (replace-regexp-in-string char
                                                  (concat "\\" char)
                                                  escaped-str nil t)))
    escaped-str))

(defun lsp-dart-test--running-p ()
  "Return non-nil if some test is already running."
  (comint-check-proc lsp-dart-test--process-buffer-name))

(defun lsp-dart-test--set-running-test (id test)
  "Add TEST with key ID."
  (setf (alist-get id lsp-dart-test--running-tests) test))

(defun lsp-dart-test--get-running-test (id)
  "Return the test from ID if exists."
  (alist-get id lsp-dart-test--running-tests))

(cl-defgeneric lsp-dart-test--handle-notification (type notification)
  "Extension point for handling custom events.
TYPE is the event to handle.
NOTIFICATION is the event notification.")

(cl-defmethod lsp-dart-test--handle-notification (type _notification)
  "Default handler for TYPE."
  (message "No event handler for '%s'" type))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql start)) notification)
  "Handle start NOTIFICATION."
  (setq lsp-dart-test--running-tests nil)
  (run-hook-with-args 'lsp-dart-test-all-start-notification-hook notification))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql testStart)) notification)
  "Handle testStart NOTIFICATION."
  (-let (((&TestStartNotification :time :test (&Test :id :name?)) notification))
    (lsp-dart-test--set-running-test id (make-lsp-dart-running-test :id id
                                                    :name name?
                                                    :start-time time))
    (run-hook-with-args 'lsp-dart-test-start-notification-hook notification)))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql allSuites)) _notification)
  "Handle allSuites NOTIFICATION.")

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql suite)) notification)
  "Handle suite NOTIFICATION."
  (run-hook-with-args 'lsp-dart-test-suite-notification-hook notification))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql group)) notification)
  "Handle group NOTIFICATION."
  (run-hook-with-args 'lsp-dart-test-group-notification-hook notification))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql testDone)) notification)
  "Handle test done NOTIFICATION."
  (-let (((&TestDoneNotification :test-id) notification))
    (when-let (test (lsp-dart-test--get-running-test test-id))
      (run-hook-with-args 'lsp-dart-test-done-notification-hook
                          notification
                          (lsp-dart-running-test-name test)
                          (lsp-dart-running-test-start-time test)))))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql done)) notification)
  "Handle done NOTIFICATION."
  (run-hook-with-args 'lsp-dart-test-all-done-notification-hook notification))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql print)) notification)
  "Handle print NOTIFICATION."
  (run-hook-with-args 'lsp-dart-test-print-notification-hook notification))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql error)) notification)
  "Handle error NOTIFICATION."
  (run-hook-with-args 'lsp-dart-test-error-notification-hook notification))

(defun lsp-dart-test--clean-process-buffer ()
  "Clean test process buffer."
  (when-let (process-buffer (get-buffer lsp-dart-test--process-buffer-name))
    (when (get-buffer-process process-buffer)
      (delete-process (get-buffer-process process-buffer)))
    (with-current-buffer process-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun lsp-dart-test--raw->response (raw-response)
  "Parse RAW-RESPONSE into a list of responses."
  (when (string-prefix-p "{" (string-trim raw-response))
    (--> raw-response
         string-trim
         (replace-regexp-in-string (regexp-quote "}\n{") "}|||{" it nil 'literal)
         (split-string it "|||")
         (-map (lambda (el) (lsp--read-json el)) it))))

(defun lsp-dart-test--run-process (command &optional args)
  "Spawn COMMAND with ARGS on a separated buffer."
  (lsp-dart-test--clean-process-buffer)
  (let ((process-buffer (get-buffer-create lsp-dart-test--process-buffer-name))
        (project-root (lsp-dart-get-project-root)))
    (with-current-buffer process-buffer
      (setq-local default-directory (or project-root default-directory))
      (unless (derived-mode-p 'lsp-dart-test-process-mode)
        (lsp-dart-test-process-mode))
      (apply #'make-comint-in-buffer lsp-dart-test--process-buffer-name process-buffer command nil args))))

(defun lsp-dart-test--run (&optional test)
  "Run Dart/Flutter test command in a compilation buffer.
If TEST is nil, it will run all tests from project.
If TEST is non nil, it will check if contains any test specific name
to run otherwise run all tests from file-name in TEST."
  (if test
      (let* ((names (plist-get test :names))
             (kind (plist-get test :kind))
             (test-file (file-relative-name (plist-get test :file-name)
                                            (lsp-dart-get-project-root)))
             (test-name (lsp-dart-test--build-test-name names))
             (group-kind? (string= kind "UNIT_TEST_GROUP"))
             (test-arg (when test-name
                         (concat "^"
                                 (lsp-dart-test--escape-test-name test-name)
                                 (unless group-kind? "$")))))
        (when names
          (lsp-workspace-set-metadata "last-ran-test" test))
        (lsp-dart-test--run-process (lsp-dart-test--build-command)
                                    (-> (lsp-dart-test--build-command-extra-args)
                                        (lsp-dart-assoc-if test-arg "--name")
                                        (lsp-dart-assoc-if test-arg test-arg)
                                        (append (list test-file)))))
  (lsp-dart-test--run-process (lsp-dart-test--build-command) (lsp-dart-test--build-command-extra-args)))
  (run-hooks 'lsp-dart-test-run-started-hook))

(defun lsp-dart-test--debug (test)
  "Debug Dart/Flutter TEST."
  (let* ((file-name (plist-get test :file-name))
         (names (plist-get test :names))
         (kind (plist-get test :kind))
         (test-name (lsp-dart-test--build-test-name names))
         (group-kind? (string= kind "UNIT_TEST_GROUP"))
         (regex (concat "^"
                        (lsp-dart-test--escape-test-name test-name)
                        (unless group-kind? "$")))
         (test-arg `("--name" ,regex)))
    (lsp-workspace-set-metadata "last-ran-test" test)
    (if (lsp-dart--flutter-project-p)
        (lsp-dart-dap-debug-flutter-test file-name test-arg)
      (lsp-dart-dap-debug-dart-test file-name test-arg))))

(defun lsp-dart-test--test-at-point ()
  "Return the test at point.
Return the test which has the smallest range of all tests ranges in the
current buffer."
  (-some--> lsp-dart-test--tests
    (--filter (-let* (((beg . end) (plist-get it :code-range)))
                (and (>= (point) beg)
                     (<= (point) end))) it)
    (--min-by (-let* (((beg1 . end1) (plist-get it :code-range))
                      ((beg2 . end2) (plist-get other :code-range)))
                (and (< beg1 beg2)
                     (> end1 end2))) it)))

(defun lsp-dart-test--handle-process-response (raw-response)
  "Handle test process RAW-RESPONSE."
  (-map (lambda (notification)
          (lsp-dart-test--handle-notification (intern (lsp-get notification :type)) notification))
        (lsp-dart-test--raw->response raw-response)))

(defun lsp-dart-test--add-test (items &optional names)
  "Add to test listfor ITEMS.
NAMES arg is optional and are the group of tests representing a test name."
  (seq-doseq (item items)
    (-let* (((&Outline :children :code-range test-range :element
                       (&Element :kind :name :range)) item)
            (test-kind? (lsp-dart-test--test-kind-p kind))
            (concatened-names (if test-kind?
                                  (append names (list name))
                                names))
            (new-test (list :file-name (file-truename (buffer-file-name))
                            :names (append names (list name))
                            :kind kind
                            :code-range (lsp--range-to-region test-range)
                            :element-range (lsp--range-to-region range))))
      (when test-kind?
        (add-to-list 'lsp-dart-test--tests new-test))
      (unless (seq-empty-p children)
        (lsp-dart-test--add-test children concatened-names)))))

(lsp-defun lsp-dart-test--check-tests ((&OutlineNotification :uri :outline (&Outline :children)))
  "Check URI and outline for test adding them."
  (when (lsp-dart-test-file-p uri)
    (when-let (buffer (find-buffer-visiting (lsp--uri-to-path uri)))
      (with-current-buffer buffer
        (setq lsp-dart-test--tests nil)
        (lsp-dart-test--add-test children)
        (run-hook-with-args 'lsp-dart-tests-added-hook lsp-dart-test--tests)))))


;;; Public

(defun lsp-dart-test-file-p (file-name)
  "Return non-nil if FILE-NAME is a dart test files."
  (string-match "_test.dart" file-name))


;;; Public interface

;;;###autoload
(defun lsp-dart-run-test-at-point ()
  "Run test at point."
  (interactive)
  (if-let (test (lsp-dart-test--test-at-point))
      (lsp-dart-test--run test)
    (lsp-dart-log "No test found at point.")))

;;;###autoload
(defun lsp-dart-debug-test-at-point ()
  "Debug test at point."
  (interactive)
  (if-let (test (lsp-dart-test--test-at-point))
      (lsp-dart-test--debug test)
    (lsp-dart-log "No test found at point.")))

;;;###autoload
(defun lsp-dart-run-test-file ()
  "Run Dart/Flutter test command only for current buffer."
  (interactive)
  (if (lsp-dart-test-file-p (buffer-file-name))
      (lsp-dart-test--run (->> (current-buffer)
                               buffer-name
                               file-truename
                               (list :file-name)))
    (lsp-dart-log "Current buffer is not a Dart/Flutter test file.")))

;;;###autoload
(defun lsp-dart-run-all-tests ()
  "Run each test from project."
  (interactive)
  (lsp-dart-test--run))

;;;###autoload
(defun lsp-dart-visit-last-test ()
  "Visit the last ran test going to test definition."
  (interactive)
  (-if-let* ((test (lsp-workspace-get-metadata "last-ran-test"))
             (file-name (plist-get test :file-name))
             (buffer (or (get-file-buffer file-name)
                         (find-file file-name)))
             (beg-position (car (plist-get test :element-range))))
      (if-let ((window (get-buffer-window buffer 'visible)))
          (progn
            (select-window window)
            (goto-char beg-position))
        (with-current-buffer buffer
          (switch-to-buffer buffer nil t)
          (goto-char beg-position)))
    (lsp-dart-log "No last test found.")))

;;;###autoload
(defun lsp-dart-run-last-test ()
  "Run the last ran test."
  (interactive)
  (if-let ((test (lsp-workspace-get-metadata "last-ran-test")))
      (lsp-dart-test--run test)
    (lsp-dart-log "No last test found.")))

;;;###autoload
(defun lsp-dart-debug-last-test ()
  "Debug the last ran test."
  (interactive)
  (if-let ((test (lsp-workspace-get-metadata "last-ran-test")))
      (lsp-dart-test--debug test)
    (lsp-dart-log "No last test found.")))

;;;###autoload
(define-derived-mode lsp-dart-test-process-mode comint-mode lsp-dart-test--process-buffer-name
  "Major mode for dart tests process."
  (setq comint-prompt-read-only nil)
  (setq comint-process-echoes nil)
  (setq process-connection-type nil)
  (if (lsp-dart--flutter-project-p)
      (setenv "PATH" (concat (lsp-dart-flutter-command) ":" (getenv "PATH")))
    (setenv "PATH" (concat (lsp-dart-pub-command) ":" (getenv "PATH"))))
  (setq-local comint-output-filter-functions #'lsp-dart-test--handle-process-response))

(define-minor-mode lsp-dart-test-mode
  "Mode for saving tests info for runs."
  nil nil nil
  (cond
   (lsp-dart-test-mode
    (add-hook 'lsp-dart-outline-arrived-hook #'lsp-dart-test--check-tests nil t))
   (t
    (remove-hook 'lsp-dart-outline-arrived-hook #'lsp-dart-test--check-tests t))))


(provide 'lsp-dart-test-support)
;;; lsp-dart-test-support.el ends here
