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
(require 'rx)
(require 'dart-mode)
(require 'lsp-mode)

(require 'lsp-dart-protocol)
(require 'lsp-dart-utils)
(require 'lsp-dart-dap)

(defcustom lsp-dart-test-pop-to-buffer-on-run 'display-only
  "Controls whether to pop to the tests buffer on run.

When set to nil the buffer will only be created, and not displayed.
When set to `display-only' the buffer will be displayed, but it will
not become focused. Otherwise the buffer is displayed and focused."
  :group 'lsp-dart
  :type '(choice (const :tag "Create the buffer, but don't display it" nil)
                 (const :tag "Create and display the buffer, but don't focus it" display-only)
                 (const :tag "Create, display, and focus the buffer" t)))


;;; Internal

(defconst lsp-dart-test--process-buffer-name "*LSP Dart - tests process*")
(defconst lsp-dart-test--buffer-name "*LSP Dart tests*")

(defconst lsp-dart-test--passed-icon "★")
(defconst lsp-dart-test--success-icon "✔")
(defconst lsp-dart-test--skipped-icon "•")
(defconst lsp-dart-test--error-icon "✖")

(defvar lsp-dart-test--suites nil)
(defvar lsp-dart-test--tests nil)

(cl-defstruct lsp-dart-test-suite
  (status nil)
  (path nil))

(cl-defstruct lsp-dart-test
  (id nil)
  (name nil)
  (start-time nil))

(cl-defstruct lsp-dart-test-len
  (file-name nil)
  (names nil)
  (position nil)
  (kind nil))

(defconst lsp-dart-test--exception-re
  (rx (or (and (zero-or-more any)
               (or "exception" "EXCEPTION")
               (zero-or-more any))
          "<asynchronous suspension>"
          (and "#"
               (one-or-more
                any)))))

(defconst lsp-dart-test--expected-actual-re
  (rx (or (and (zero-or-more blank)
               "Expected:"
               (zero-or-more any))
          (and (zero-or-more blank)
               "Actual:"
               (zero-or-more any)))))

(defconst lsp-dart-test--font-lock
  `((,lsp-dart-test--exception-re . 'error)
    (,lsp-dart-test--expected-actual-re . 'warning)))

(defvar lsp-dart-test--output-font-lock
  '((lsp-dart-test--font-lock)))

(defun lsp-dart-test--get-buffer-create ()
  "Create a buffer for test display."
  (let ((buffer (get-buffer-create lsp-dart-test--buffer-name)))
    (with-current-buffer buffer
      (setq-local default-directory (or (lsp-dart-get-project-root) default-directory))
      (unless (derived-mode-p 'lsp-dart-test-mode)
        (lsp-dart-test-mode))
      (current-buffer))))

(defun lsp-dart-test--send-output (message &rest args)
  "Send MESSAGE with ARGS to test buffer."
  (let* ((inhibit-read-only t))
    (with-current-buffer (lsp-dart-test--get-buffer-create)
      (save-excursion
        (goto-char (point-max))
        (insert (apply #'format (concat message "\n") args))))))

(lsp-defun lsp-dart-test--get-icon ((&TestDoneNotification :result :skipped))
  "Return the icon for test done notification."
  (if (string= result "success")
      (if skipped
          lsp-dart-test--skipped-icon
        lsp-dart-test--success-icon)
    lsp-dart-test--error-icon))

(lsp-defun lsp-dart-test--get-face ((&TestDoneNotification :result :skipped))
  "Return the icon for test done notification."
  (if (string= result "success")
      (if skipped
          'homoglyph
        'success)
    'error))

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

(defun lsp-dart-test--set-test (id test)
  "Add TEST with key ID."
  (setf (alist-get id lsp-dart-test--tests) test))

(defun lsp-dart-test--get-test (id)
  "Return the test from ID if exists."
  (alist-get id lsp-dart-test--tests))

(defun lsp-dart-test--set-suite (path suite)
  "Add SUITE with key PATH."
  (setf (alist-get path lsp-dart-test--suites nil nil #'string=) suite))

(defun lsp-dart-test--get-suite (path)
  "Return the suite from PATH if exists."
  (alist-get path lsp-dart-test--suites nil nil #'string=))

(cl-defgeneric lsp-dart-test--handle-notification (type notification)
  "Extension point for handling custom events.
TYPE is the event to handle.
NOTIFICATION is the event notification.")

(cl-defmethod lsp-dart-test--handle-notification (type _notification)
  "Default handler for TYPE."
  (message "No event handler for '%s'" type))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql start)) _notification)
  "Handle start NOTIFICATION."
  (setq lsp-dart-test--tests nil))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql testStart)) notification)
  "Handle testStart NOTIFICATION."
  (-let (((&TestStartNotification :time :test (&Test :id :name?)) notification))
    (lsp-dart-test--set-test id (make-lsp-dart-test :id id
                                                    :name name?
                                                    :start-time time))))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql allSuites)) _notification)
  "Handle allSuites NOTIFICATION.")

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql suite)) notification)
  "Handle suites NOTIFICATION."
  (-let (((&SuiteNotification :suite (&Suite :path)) notification))
    (if-let (suite (lsp-dart-test--get-suite path))
        (progn
          (setf (lsp-dart-test-suite-status suite) 'waiting)
          (lsp-dart-test--set-suite path suite))
      (lsp-dart-test--set-suite path (make-lsp-dart-test-suite :status 'waiting
                                                               :path path)))))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql group)) _notification)
  "Handle group NOTIFICATION.")

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql testDone)) notification)
  "Handle test done NOTIFICATION."
  (-let (((&TestDoneNotification :test-id :time :hidden) notification))
    (unless hidden
      (-when-let* ((test (lsp-dart-test--get-test test-id))
                   (time (propertize (format "(%s ms)"
                                             (- time (lsp-dart-test-start-time test)))
                                     'font-lock-face 'font-lock-comment-face))
                   (text (propertize (concat (lsp-dart-test--get-icon notification)
                                             " "
                                             (lsp-dart-test-name test))
                                     'font-lock-face (lsp-dart-test--get-face notification))))
        (lsp-dart-test--send-output "%s %s" text time)))))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql done)) notification)
  "Handle done NOTIFICATION."
  (-let (((&DoneNotification :success) notification))
    (if success
        (lsp-dart-test--send-output (propertize (format "\n%s All ran tests passed %s" lsp-dart-test--passed-icon lsp-dart-test--passed-icon)
                                                'font-lock-face 'success))
      (lsp-dart-test--send-output "\nFinished running tests"))))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql print)) notification)
  "Handle print NOTIFICATION."
  (-let (((&PrintNotification :message) notification))
    (lsp-dart-test--send-output "%s" message)))

(cl-defmethod lsp-dart-test--handle-notification ((_event (eql error)) notification)
  "Handle error NOTIFICATION."
  (-let (((&ErrorNotification :error :stack-trace) notification))
    (lsp-dart-test--send-output "%s" error)
    (lsp-dart-test--send-output "%s" stack-trace)))

(defun lsp-dart-test--clean-process-buffer ()
  "Clean test process buffer."
  (when-let (process-buffer (get-buffer lsp-dart-test--process-buffer-name))
    (when (get-buffer-process process-buffer)
      (delete-process (get-buffer-process process-buffer)))
    (with-current-buffer process-buffer
      (erase-buffer))))

(defun lsp-dart-test--raw->response (response)
  "Parse raw RESPONSE into a list of responses."
  (when (string-prefix-p "{" (string-trim response))
    (--> response
         string-trim
         (replace-regexp-in-string (regexp-quote "}\n{") "}|||{" it nil 'literal)
         (split-string it "|||")
         (-map (lambda (el) (lsp--read-json el)) it))))

(defun lsp-dart-test--handle-process-output (raw-output)
  "Handle test process RAW-OUTPUT."
  (-map (lambda (notification)
          (lsp-dart-test--handle-notification (intern (lsp-get notification :type)) notification))
        (lsp-dart-test--raw->response raw-output)))

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

(defun lsp-dart-test--show-buffer ()
  "Show test buffer."
  (let ((test-buffer (lsp-dart-test--get-buffer-create))
        (inhibit-read-only t))
    (with-current-buffer test-buffer
      (erase-buffer))
    (pcase lsp-dart-test-pop-to-buffer-on-run
      (`display-only
       (let ((orig-buffer (current-buffer)))
         (display-buffer test-buffer)
         (set-buffer orig-buffer)))
      ((pred identity) (pop-to-buffer test-buffer)))))

(defun lsp-dart-test--run (&optional test)
  "Run Dart/Flutter test command in a compilation buffer.
If TEST is nil, it will run all tests from project.
If TEST is non nil, it will check if contains any test specific name
to run otherwise run all tests from file-name in TEST."
  (if test
      (let* ((names (lsp-dart-test-len-names test))
             (kind (lsp-dart-test-len-kind test))
             (test-file (file-relative-name (lsp-dart-test-len-file-name test)
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
  (lsp-dart-test--show-buffer)
  (lsp-dart-test--send-output (concat "Running tests...\n")))

(defun lsp-dart-test--debug (test)
  "Debug Dart/Flutter TEST."
  (let* ((file-name (lsp-dart-test-len-file-name test))
         (names (lsp-dart-test-len-names test))
         (kind (lsp-dart-test-len-kind test))
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

(defun lsp-dart-test-file-p (file-name)
  "Return non-nil if FILE-NAME is a dart test files."
  (string-match "_test.dart" file-name))

(defun lsp-dart-test--overlay-at-point ()
  "Return test overlay at point.
Return the overlay which has the smallest range of all test overlays in
the current buffer."
  (-some--> (overlays-in (point-min) (point-max))
    (--filter (when (overlay-get it 'lsp-dart-test-code-lens)
                (-let* (((beg . end) (overlay-get it 'lsp-dart-code-lens-overlay-test-range)))
                  (and (>= (point) beg)
                       (<= (point) end)))) it)
    (--min-by (-let* (((beg1 . end1) (overlay-get it 'lsp-dart-code-lens-overlay-test-range))
                      ((beg2 . end2) (overlay-get other 'lsp-dart-code-lens-overlay-test-range)))
                (and (< beg1 beg2)
                     (> end1 end2))) it)))

;;;###autoload
(defun lsp-dart-run-test-at-point ()
  "Run test at point.
Search for the last test overlay."
  (interactive)
  (if-let (overlay (lsp-dart-test--overlay-at-point))
      (lsp-dart-test--run (overlay-get overlay 'lsp-dart-test))
    (lsp-dart-log "No test found at point.")))

;;;###autoload
(defun lsp-dart-debug-test-at-point ()
  "Debug test at point.
Search for the last test overlay."
  (interactive)
  (if-let (overlay (lsp-dart-test--overlay-at-point))
      (lsp-dart-test--debug (overlay-get overlay 'lsp-dart-test))
    (lsp-dart-log "No test found at point.")))

;;;###autoload
(defun lsp-dart-run-test-file ()
  "Run Dart/Flutter test command only for current buffer."
  (interactive)
  (if (lsp-dart-test-file-p (buffer-file-name))
      (lsp-dart-test--run (->> (current-buffer)
                               buffer-name
                               file-truename
                               (make-lsp-dart-test-len :file-name)))
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
             (file-name (lsp-dart-test-len-file-name test))
             (buffer (or (get-file-buffer file-name)
                         (find-file file-name)))
             (position (lsp-dart-test-len-position test)))
      (if-let ((window (get-buffer-window buffer 'visible)))
          (progn
            (select-window window)
            (goto-char position))
        (with-current-buffer buffer
          (switch-to-buffer buffer nil t)
          (goto-char position)))
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
(define-derived-mode lsp-dart-test-mode special-mode lsp-dart-test--buffer-name
  "Major mode for buffer running tests."
  (setq font-lock-defaults lsp-dart-test--output-font-lock))

;;;###autoload
(define-derived-mode lsp-dart-test-process-mode comint-mode lsp-dart-test--process-buffer-name
  "Major mode for dart tests process."
  (setq comint-prompt-read-only nil)
  (setq comint-process-echoes nil)
  (setq process-connection-type nil)
  (if (lsp-dart--flutter-project-p)
      (setenv "PATH" (concat (lsp-dart-flutter-command) ":" (getenv "PATH")))
    (setenv "PATH" (concat (lsp-dart-pub-command) ":" (getenv "PATH"))))
  (setq-local comint-output-filter-functions #'lsp-dart-test--handle-process-output))


(provide 'lsp-dart-test-support)
;;; lsp-dart-test-support.el ends here
