;;; lsp-dart-dap-devtools.el --- Support for Dart DevTools on debugger -*- lexical-binding: t; -*-
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
;;  Support for Dart DevTools on debugger
;;
;;; Code:

(require 'dash)
(require 'lsp-mode)
(require 'dap-mode)

(require 'lsp-dart-project)

(defcustom lsp-dart-dap-devtools-theme "dark"
  "The theme to Dart DevTools."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-dap-devtools-hide-options "debugger"
  "What to hide when openning Dart DevTools."
  :group 'lsp-dart
  :type 'string)

(defconst lsp-dart-dap-devtools--buffer-name "*LSP Dart - DevTools*")
(defconst lsp-dart-dap-devtools--pub-list-packages-buffer-name "*LSP Dart - Pub list packages*")

(defun lsp-dart-dap-devtools-log (msg &rest args)
  "Custom logger for MSG and ARGS."
  (apply #'lsp-dart-project-custom-log "[DEVTOOLS]" msg args))

(cl-defmethod dap-handle-event ((_event (eql dart.debuggerUris)) _session params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (-let* (((&hash "vmServiceUri" vm-service-uri) params))
    (lsp-workspace-set-metadata "devtools-vm-service-uri" vm-service-uri)))

(defun lsp-dart-dap-devtools--clean-buffer (buffer)
  "Clean BUFFER content."
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer))))

(defun lsp-dart-dap-devtools--buffer-whole-string (buffer)
  "Return all content of BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun lsp-dart-dap-devtools--check-devtools-uri (callback)
  "Check for uri on devtools buffer and call CALLBACK with it.
If URI is not found on buffer, schedule re-check."
  (let ((content (lsp-dart-dap-devtools--buffer-whole-string lsp-dart-dap-devtools--buffer-name)))
    (if (string= content "")
        (run-with-idle-timer 0.3 nil #'lsp-dart-dap-devtools--check-devtools-uri callback)
      (-let* (((&hash "params" (&hash "host" "port")) (lsp--read-json content))
              (uri (concat host ":" (number-to-string port))))
        (lsp-workspace-set-metadata "dart-debug-devtools-uri" uri)
        (funcall callback uri)))))

(defun lsp-dart-dap-devtools--activated-p ()
  "Return non-nil if devtools is activated otherwise nil."
  (lsp-dart-dap-devtools--clean-buffer lsp-dart-dap-devtools--pub-list-packages-buffer-name)
  (let* ((pub (lsp-dart-project-pub-command))
         (_proc (call-process pub
                              nil
                              lsp-dart-dap-devtools--pub-list-packages-buffer-name
                              nil
                              "global" "list"))
         (content (lsp-dart-dap-devtools--buffer-whole-string lsp-dart-dap-devtools--pub-list-packages-buffer-name)))
    (string-match-p "devtools \\([0-9]\\.[0-9]\\.[0-9]\\)" content)))

(defun lsp-dart-dap-devtools--activate (callback)
  "Activate Dart Devtools via pub then call CALLBACK."
  (lsp-dart-dap-devtools-log "Activating...")
  (let ((pub (lsp-dart-project-pub-command)))
    (lsp-async-start-process
     (lambda ()
       (lsp-dart-dap-devtools-log "Activated successfully!")
       (funcall callback))
     (lambda (_) (lsp-dart-dap-devtools-log "Could not activate DevTools. \
Try to activate manually running 'pub global activate devtools'"))
     pub "global" "activate" "devtools")))

(defun lsp-dart-dap-devtools--check-activated (callback)
  "Check if devtools is activated otherwise prompt for activate it.
If it is already activated or after activated successfully, call CALLBACK."
  (if (lsp-dart-dap-devtools--activated-p)
      (funcall callback)
    (when (y-or-n-p "Dart DevTools needs to be activated with \
'pub global activate devtools' to use this feature.\nActivate DevTools? ")
      (lsp-dart-dap-devtools--activate callback))))

(defun lsp-dart-dap-devtools--kill-proc (proc _session)
  "Kill the devtools PROC process of SESSION."
  (lsp-workspace-set-metadata "dart-debug-devtools-uri" nil)
  (delete-process proc)
  (lsp-dart-dap-devtools--clean-buffer lsp-dart-dap-devtools--buffer-name))

(defvar-local lsp-dart-dap-devtools--check-uri-timer nil)

(defun lsp-dart-dap-devtools--start (callback)
  "Start Dart DevTools process and call CALLBACK after started successfully."
  (lsp-dart-dap-devtools--check-activated
   (lambda ()
     (if-let ((uri (lsp-workspace-get-metadata "dart-debug-devtools-uri")))
      (funcall callback uri)
    (let* ((pub (lsp-dart-project-pub-command))
           (proc (start-process "Start DevTools"
                                lsp-dart-dap-devtools--buffer-name
                                pub "global" "run" "devtools"
                                "--machine"
                                "--enable-notifications"
                                "--try-ports" "10")))
      (add-hook 'dap-terminated-hook (-partial #'lsp-dart-dap-devtools--kill-proc proc))
      (when lsp-dart-dap-devtools--check-uri-timer
        (cancel-timer lsp-dart-dap-devtools--check-uri-timer))
      (setq lsp-dart-dap-devtools--check-uri-timer
            (run-with-idle-timer 0.3 nil #'lsp-dart-dap-devtools--check-devtools-uri callback)))))))

(defun lsp-dart-dap-devtools--open (uri vm-service-uri)
  "Open DevTools URI with VM-SERVICE-URI param at browser."
  (let* ((params (url-build-query-string `((ide Emacs)
                                           (uri ,vm-service-uri)
                                           (hide ,lsp-dart-dap-devtools-hide-options)
                                           (theme ,lsp-dart-dap-devtools-theme))))
         (url (concat "http://" uri "?" params)))
    (browse-url url)))


;;; Public interface

;;;###autoload
(defun lsp-dart-dap-devtools-open ()
  "Open Dart DevTools for the current debug session."
  (interactive)
  (let ((session (dap--cur-session))
        (vm-service-uri (lsp-workspace-get-metadata "devtools-vm-service-uri")))
    (when (and session vm-service-uri)
      (lsp-dart-dap-devtools--start
       (lambda (uri)
         (lsp-dart-dap-devtools-log "Openning at browser...")
         (lsp-dart-dap-devtools--open uri vm-service-uri))))))

(provide 'lsp-dart-dap-devtools)
;;; lsp-dart-dap-devtools.el ends here
