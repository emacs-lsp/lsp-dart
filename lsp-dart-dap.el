;;; lsp-dart-dap.el --- DAP support for lsp-dart -*- lexical-binding: t; -*-
;;
;; Version: 1.5
;; Keywords: languages, extensions
;; Package-Requires: ((emacs "25.2") (lsp-mode "6.0") (dash "2.14.1"))
;; URL: https://github.com/emacs-lsp/lsp-dart.el
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

;; DAP support for lsp-dart

;;; Code:

(require 'lsp-mode)
(require 'dap-mode)
(require 'dap-utils)

(require 'lsp-dart-project)

(defcustom lsp-dart-dap-extension-version "3.9.1"
  "The extension version."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-dap-debugger-path
  (expand-file-name "github/Dart-Code.Dart-Code"
                    dap-utils-extension-path)
  "The path to dart vscode extension."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-dap-debugger-program
  `("node" ,(f-join lsp-dart-dap-debugger-path "extension/out/src/debug/dart_debug_entry.js"))
  "The path to the dart debugger."
  :group 'lsp-dart
  :type '(repeat string))

(defcustom lsp-dart-dap-devtools-theme "dark"
  "The theme to Dart DevTools."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-dap-devtools-hide-options "debugger"
  "What to hide when openning Dart DevTools."
  :group 'lsp-dart
  :type 'string)

(defconst lsp-dart-dap--devtools-buffer-name "*LSP Dart - DevTools*")
(defconst lsp-dart-dap--pub-list-pacakges-buffer-name "*LSP Dart - Pub list packages*")

(defun lsp-dart-dap--setup-extension ()
  "Setup dart debugger extension to run `lsp-dart-dap-debugger-program`."
  (lsp-dart-project-log "Setting up DAP...")
  (lsp-async-start-process
   (lambda ()
     (lsp-async-start-process
      (lambda () (lsp-dart-project-log "DAP setup done!"))
      (lambda (_) (lsp-dart-project-log "Error setting up lsp-dart-dap, check if `npm` is on $PATH"))
      (f-join lsp-dart-dap-debugger-path "extension/node_modules/typescript/bin/tsc")
      "--project" (f-join lsp-dart-dap-debugger-path "extension")))
   (lambda (_) (lsp-dart-project-log "Error setting up lsp-dart-dap, check if `npm` is on $PATH"))
   "npm" "install" "--prefix" (f-join lsp-dart-dap-debugger-path "extension")
   "--no-package-lock" "--silent" "--no-save"))

(dap-utils-github-extension-setup-function
 "dap-dart"
 "Dart-Code"
 "Dart-Code"
 lsp-dart-dap-extension-version
 lsp-dart-dap-debugger-path
 #'lsp-dart-dap--setup-extension)

(defun lsp-dart-dap--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path lsp-dart-dap-debugger-program)
      (dap--put-if-absent :type "dart")
      (dap--put-if-absent :cwd (lsp-dart-project-get-root))
      (dap--put-if-absent :program (buffer-file-name))
      (dap--put-if-absent :name "Dart Debug")
      (dap--put-if-absent :dartPath (lsp-dart-project-dart-command))
      (dap--put-if-absent :debuggerType 0)
      (dap--put-if-absent :debugExternalLibraries nil)
      (dap--put-if-absent :debugSdkLibraries nil)))

(dap-register-debug-provider "dart" 'lsp-dart-dap--populate-start-file-args)
(dap-register-debug-template "Dart :: Run Configuration"
                             (list :type "dart"
                                   :cwd nil
                                   :request "launch"
                                   :program nil
                                   :name "Dart::Run"))

(cl-defmethod dap-handle-event ((_event (eql dart.debuggerUris)) _session params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (-let* (((&hash "vmServiceUri" vm-service-uri) params))
    (lsp-workspace-set-metadata "dart-debug-vm-service-uri" vm-service-uri)))

(defun lsp-dart-dap--clean-buffer (buffer)
  "Clean BUFFER content."
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer))))

(defun lsp-dart-dap--buffer-whole-string (buffer)
  "Return all content of BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun lsp-dart-dap--check-devtools-uri (callback)
  "Check for uri on devtools buffer and call CALLBACK with it.
If URI is not found on buffer, schedule re-check."
  (let ((content (lsp-dart-dap--buffer-whole-string lsp-dart-dap--devtools-buffer-name)))
    (if (string= content "")
        (run-with-idle-timer 0.3 nil #'lsp-dart-dap--check-devtools-uri callback)
      (-let* (((&hash "params" (&hash "host" "port")) (lsp--read-json content))
              (uri (concat host ":" (number-to-string port))))
        (lsp-workspace-set-metadata "dart-debug-devtools-uri" uri)
        (funcall callback uri)))))

(defun lsp-dart-dap--devtools-activated-p ()
  "Return non-nil if devtools is activated otherwise nil."
  (lsp-dart-dap--clean-buffer lsp-dart-dap--pub-list-pacakges-buffer-name)
  (let* ((pub (lsp-dart-project-get-pub-command))
         (_proc (call-process pub
                              nil
                              lsp-dart-dap--pub-list-pacakges-buffer-name
                              nil
                              "global" "list"))
         (content (lsp-dart-dap--buffer-whole-string lsp-dart-dap--pub-list-pacakges-buffer-name)))
    (string-match-p "devtools \\([0-9]\\.[0-9]\\.[0-9]\\)" content)))

(defun lsp-dart-dap--activate-devtools (callback)
  "Activate Dart Devtools via pub then call CALLBACK."
  (lsp-dart-project-log "Activating DevTools...")
  (let ((pub (lsp-dart-project-get-pub-command)))
    (lsp-async-start-process
     (lambda ()
       (lsp-dart-project-log "DevTools activated successfully!")
       (funcall callback))
     (lambda (_) (lsp-dart-project-log "Could not Activate DevTools. \
Try to activate manually running 'pub global activate devtools'"))
     pub "global" "activate" "devtools")))

(defun lsp-dart-dap--check-devtools-activated (callback)
  "Check if devtools is activated otherwise prompt for activate it.
If it is already activated or after activated successfully, call CALLBACK."
  (if (lsp-dart-dap--devtools-activated-p)
      (funcall callback)
    (when (y-or-n-p "Dart DevTools needs to be activated with \
'pub global activate devtools' to use this feature.\nActivate DevTools? ")
      (lsp-dart-dap--activate-devtools callback))))

(defun lsp-dart-dap--kill-devtools-proc (proc _session)
  "Kill the devtools PROC process of SESSION."
  (lsp-workspace-set-metadata "dart-debug-devtools-uri" nil)
  (delete-process proc)
  (lsp-dart-dap--clean-buffer lsp-dart-dap--devtools-buffer-name))

(defvar-local lsp-dart-dap--check-devtools-uri-timer nil)

(defun lsp-dart-dap--start-devtools (callback)
  "Start Dart DevTools process and call CALLBACK after started successfully."
  (lsp-dart-dap--check-devtools-activated
   (lambda ()
     (if-let ((uri (lsp-workspace-get-metadata "dart-debug-devtools-uri")))
      (funcall callback uri)
    (let* ((pub (lsp-dart-project-get-pub-command))
           (proc (start-process "Start DevTools"
                                lsp-dart-dap--devtools-buffer-name
                                pub "global" "run" "devtools"
                                "--machine"
                                "--enable-notifications"
                                "--try-ports" "10")))
      (add-hook 'dap-terminated-hook (-partial #'lsp-dart-dap--kill-devtools-proc proc))
      (when lsp-dart-dap--check-devtools-uri-timer
        (cancel-timer lsp-dart-dap--check-devtools-uri-timer))
      (setq lsp-dart-dap--check-devtools-uri-timer
            (run-with-idle-timer 0.3 nil #'lsp-dart-dap--check-devtools-uri callback)))))))

(defun lsp-dart-dap--open-devtools (uri vm-service-uri)
  "Open DevTools URI with VM-SERVICE-URI param at browser."
  (let* ((params (url-build-query-string `((ide Emacs)
                                           (uri ,vm-service-uri)
                                           (hide ,lsp-dart-dap-devtools-hide-options)
                                           (theme ,lsp-dart-dap-devtools-theme))))
         (url (concat "http://" uri "?" params)))
    (browse-url url)))

;;;###autoload
(defun lsp-dart-dap-open-devtools ()
  "Open Dart DevTools for the current debug session."
  (interactive)
  (let ((session (dap--cur-session))
        (vm-service-uri (lsp-workspace-get-metadata "dart-debug-vm-service-uri")))
    (when (and session vm-service-uri)
      (lsp-dart-dap--start-devtools
       (lambda (uri)
         (lsp-dart-project-log "Openning DevTools at browser...")
         (lsp-dart-dap--open-devtools uri vm-service-uri))))))

(provide 'lsp-dart-dap)
;;; lsp-dart-dap.el ends here
