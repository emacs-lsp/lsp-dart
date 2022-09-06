;;; lsp-dart-devtools.el --- Support for Dart DevTools on debugger -*- lexical-binding: t; -*-
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
;;  Support for Dart DevTools on debugger
;;
;;; Code:

(require 'dash)
(require 'lsp-mode)
(require 'dap-mode)

(require 'lsp-dart-utils)

(defcustom lsp-dart-devtools-theme "dark"
  "The theme to Dart DevTools."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-devtools-hide-options "debugger"
  "What to hide when openning Dart DevTools."
  :group 'lsp-dart
  :type 'string)

(defconst lsp-dart-devtools--buffer-name "*LSP Dart - DevTools*")

(defun lsp-dart-devtools-log (msg &rest args)
  "Custom logger for MSG and ARGS."
  (apply #'lsp-dart-custom-log "[DEVTOOLS]" msg args))

(cl-defmethod dap-handle-event ((_event (eql dart.debuggerUris)) _session params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (-let* (((&hash "vmServiceUri" vm-service-uri) params))
    (lsp-workspace-set-metadata "devtools-vm-service-uri" vm-service-uri)))

(defun lsp-dart-devtools--clean-buffer (buffer)
  "Clean BUFFER content."
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer))))

(defun lsp-dart-devtools--buffer-whole-string (buffer)
  "Return all content of BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun lsp-dart-devtools--check-devtools-uri (callback)
  "Check for uri on devtools buffer and call CALLBACK with it.
If URI is not found on buffer, schedule re-check."
  (let ((content (lsp-dart-devtools--buffer-whole-string lsp-dart-devtools--buffer-name)))
    (if (string= content "")
        (run-with-idle-timer 0.3 nil #'lsp-dart-devtools--check-devtools-uri callback)
      (-let* (((&hash "params" (&hash "host" "port")) (lsp--read-json content))
              (uri (concat host ":" (number-to-string port))))
        (lsp-workspace-set-metadata "dart-debug-devtools-uri" uri)
        (funcall callback uri)))))

(defun lsp-dart-devtools--activated-p ()
  "Return non-nil if devtools is activated otherwise nil."
  (let ((output (shell-command-to-string
                 (mapconcat 'identity `(,@(lsp-dart-pub-command) "global" "list") " "))))
    (string-match-p "devtools \\([0-9]\\.[0-9]\\.[0-9]\\)" output)))

(defun lsp-dart-devtools--activate (callback)
  "Activate Dart Devtools via pub then call CALLBACK."
  (lsp-dart-devtools-log "Activating...")
  (let ((pub (lsp-dart-pub-command)))
    (lsp-async-start-process
     (lambda ()
       (lsp-dart-devtools-log "Activated successfully!")
       (funcall callback))
     (lambda (_) (lsp-dart-devtools-log "Could not activate DevTools. \
Try to activate manually running 'pub global activate devtools'"))
     (mapconcat 'identity `(,@pub "global" "activate" "devtools") " "))))

(defun lsp-dart-devtools--check-activated (callback)
  "Check if devtools is activated otherwise prompt for activate it.
If it is already activated or after activated successfully, call CALLBACK."
  (if (lsp-dart-devtools--activated-p)
      (funcall callback)
    (when (y-or-n-p "Dart DevTools needs to be activated with \
'pub global activate devtools' to use this feature.\nActivate DevTools? ")
      (lsp-dart-devtools--activate callback))))

(defun lsp-dart-devtools--kill-proc (proc _session)
  "Kill the devtools PROC process of SESSION."
  (lsp-workspace-set-metadata "dart-debug-devtools-uri" nil)
  (delete-process proc)
  (lsp-dart-devtools--clean-buffer lsp-dart-devtools--buffer-name))

(defvar-local lsp-dart-devtools--check-uri-timer nil)

(defun lsp-dart-devtools--start (callback)
  "Start Dart DevTools process and call CALLBACK after started successfully."
  (lsp-dart-devtools--check-activated
   (lambda ()
     (if-let ((uri (lsp-workspace-get-metadata "dart-debug-devtools-uri")))
      (funcall callback uri)
      (let* ((pub (lsp-dart-pub-command))
             (proc (apply #'start-process "Start DevTools"
                          lsp-dart-devtools--buffer-name
                          `(,@pub "global" "run" "devtools"
                                  "--machine"
                                  "--enable-notifications"
                                  "--try-ports" "10"))))
      (add-hook 'dap-terminated-hook (-partial #'lsp-dart-devtools--kill-proc proc))
      (when lsp-dart-devtools--check-uri-timer
        (cancel-timer lsp-dart-devtools--check-uri-timer))
      (setq lsp-dart-devtools--check-uri-timer
            (run-with-idle-timer 0.3 nil #'lsp-dart-devtools--check-devtools-uri callback)))))))

(defun lsp-dart-devtools--open (uri vm-service-uri)
  "Open DevTools URI with VM-SERVICE-URI param at browser."
  (let* ((params (url-build-query-string `((ide Emacs)
                                           (uri ,vm-service-uri)
                                           (hide ,lsp-dart-devtools-hide-options)
                                           (theme ,lsp-dart-devtools-theme))))
         (url (concat "http://" uri "?" params)))
    (browse-url url)))


;;; Public interface

;;;###autoload
(defun lsp-dart-open-devtools ()
  "Open Dart DevTools for the current debug session."
  (interactive)
  (let ((session (dap--cur-session))
        (vm-service-uri (lsp-workspace-get-metadata "devtools-vm-service-uri")))
    (when (and session vm-service-uri)
      (lsp-dart-devtools--start
       (lambda (uri)
         (lsp-dart-devtools-log "Openning at browser...")
         (lsp-dart-devtools--open uri vm-service-uri))))))

(provide 'lsp-dart-devtools)
;;; lsp-dart-devtools.el ends here
