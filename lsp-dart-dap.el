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
(require 'lsp-dart-flutter-daemon)

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

(defcustom lsp-dart-dap-dart-debugger-program
  `("node" ,(f-join lsp-dart-dap-debugger-path "extension/out/src/debug/dart_debug_entry.js"))
  "The path to the dart debugger."
  :group 'lsp-dart
  :type '(repeat string))

(defcustom lsp-dart-dap-flutter-debugger-program
  `("node" ,(f-join lsp-dart-dap-debugger-path "extension/out/src/debug/flutter_debug_entry.js"))
  "The path to the Flutter debugger."
  :group 'lsp-dart
  :type '(repeat string))

(defcustom lsp-dart-dap-debug-external-libraries nil
  "If non-nil, enable the debug on external libraries."
  :group 'lsp-dart
  :type 'boolean)

(defcustom lsp-dart-dap-debug-sdk-libraries nil
  "If non-nil, enable the debug on Dart SDK libraries."
  :group 'lsp-dart
  :type 'boolean)

(defcustom lsp-dart-dap-devtools-theme "dark"
  "The theme to Dart DevTools."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-dap-devtools-hide-options "debugger"
  "What to hide when openning Dart DevTools."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-dap-flutter-track-widget-creation t
  "Whether to pass –track-widget-creation to Flutter apps.
Required to support 'Inspect Widget'."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-dap-flutter-structured-errors t
  "Whether to use Flutter's structured error support for improve error display."
  :group 'lsp-dart
  :type 'string)

(defcustom lsp-dart-dap-flutter-verbose-log nil
  "Whether to enable logs from Flutter DAP."
  :group 'lsp-dart
  :type 'boolean)


;;; Internal

(defconst lsp-dart-dap--devtools-buffer-name "*LSP Dart - DevTools*")
(defconst lsp-dart-dap--pub-list-pacakges-buffer-name "*LSP Dart - Pub list packages*")

(defun lsp-dart-dap-log (msg &rest args)
  "Log MSG with ARGS adding lsp-dart-dap prefix."
  (lsp-dart-project-log (concat
                         (propertize "[DAP] "
                                     'face 'font-lock-function-name-face)
                         msg
                         args)))

(defun lsp-dart-dap--setup-extension ()
  "Setup dart debugger extension to run `lsp-dart-dap-dart-debugger-program`."
  (lsp-dart-dap-log "Setting up debugger...")
  (lsp-async-start-process
   (lambda ()
     (lsp-async-start-process
      (lambda () (lsp-dart-dap-log "Setup done!"))
      (lambda (_) (lsp-dart-dap-log "Error setting up DAP, check if `npm` is on $PATH"))
      (f-join lsp-dart-dap-debugger-path "extension/node_modules/typescript/bin/tsc")
      "--project" (f-join lsp-dart-dap-debugger-path "extension")))
   (lambda (_) (lsp-dart-dap-log "Error setting up DAP, check if `npm` is on $PATH"))
   "npm" "install" "--prefix" (f-join lsp-dart-dap-debugger-path "extension")
   "--no-package-lock" "--silent" "--no-save"))

(dap-utils-github-extension-setup-function
 "dap-dart"
 "Dart-Code"
 "Dart-Code"
 lsp-dart-dap-extension-version
 lsp-dart-dap-debugger-path
 #'lsp-dart-dap--setup-extension)

(defun lsp-dart-dap--populate-dart-start-file-args (conf)
  "Populate CONF with the required arguments for dart debug."
  (-> conf
      (dap--put-if-absent :dap-server-path lsp-dart-dap-dart-debugger-program)
      (dap--put-if-absent :cwd (lsp-dart-project-get-root))
      (dap--put-if-absent :program (buffer-file-name))
      (dap--put-if-absent :dartPath (lsp-dart-project-dart-command))
      (dap--put-if-absent :debugExternalLibraries lsp-dart-dap-debug-external-libraries)
      (dap--put-if-absent :debugSdkLibraries lsp-dart-dap-debug-sdk-libraries)))

(dap-register-debug-provider "dart" 'lsp-dart-dap--populate-dart-start-file-args)
(dap-register-debug-template "Dart :: Debug"
                             (list :type "dart"
                                   :request "launch"
                                   :name "Dart"))

;; Flutter

(defun lsp-dart-dap--flutter-get-or-create-device ()
  "Return the device to debug or prompt to start it."
  (-let* ((devices (lsp-dart-flutter-daemon-get-emulators))
          (chosen-device (dap--completing-read "Select a device to use: "
                                               devices
                                               (-lambda ((&hash "id" "name" "category" "platformType" platform))
                                                 (format "%s - %s" platform (if name name id)))
                                               nil
                                               t))
          (emulator (lsp-dart-flutter-daemon-launch chosen-device)))
    (ht ('id "emulator-5554")
        ('name "device"))
    ))

(defun lsp-dart-dap--populate-flutter-start-file-args (conf)
  "Populate CONF with the required arguments for Flutter debug."
  (-let* ((root (lsp-dart-project-get-root))
          ((&hash "id" device-id "name" device-name) (lsp-dart-dap--flutter-get-or-create-device)))
    (-> conf
        (dap--put-if-absent :dap-server-path lsp-dart-dap-flutter-debugger-program)
        (dap--put-if-absent :cwd root)
        (dap--put-if-absent :program (lsp-dart-project-get-entrypoint))
        (dap--put-if-absent :dartPath (lsp-dart-project-dart-command))
        (dap--put-if-absent :flutterPath (lsp-dart-project-get-flutter-path))
        (dap--put-if-absent :flutterTrackWidgetCreation lsp-dart-dap-flutter-track-widget-creation)
        (dap--put-if-absent :useFlutterStructuredErrors lsp-dart-dap-flutter-structured-errors)
        (dap--put-if-absent :debugExternalLibraries lsp-dart-dap-debug-external-libraries)
        (dap--put-if-absent :debugSdkLibraries lsp-dart-dap-debug-sdk-libraries)
        (dap--put-if-absent :deviceId device-id)
        (dap--put-if-absent :deviceName device-name)
        (dap--put-if-absent :name (concat "Flutter (" device-name ")")))))

(dap-register-debug-provider "flutter" 'lsp-dart-dap--populate-flutter-start-file-args)
(dap-register-debug-template "Flutter :: Debug"
                             (list :type "flutter"
                                   :request "launch"
                                   :flutterMode "debug"
                                   :flutterPlatform "default"
                                   :name "Flutter"))

(defvar lsp-dart-dap--flutter-progress-reporter nil)

(cl-defmethod dap-handle-event ((_event (eql dart.log)) _session params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (when lsp-dart-dap-flutter-verbose-log
    (-let* (((&hash "message") params)
            (msg (replace-regexp-in-string (regexp-quote "\n") "" message nil 'literal)))
      (lsp-dart-dap-log msg))))

(cl-defmethod dap-handle-event ((_event (eql dart.launching)) _session params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (let ((prefix (concat (propertize "[LSP Dart] "
                                     'face 'font-lock-keyword-face)
                         (propertize "[DAP] "
                                     'face 'font-lock-function-name-face))))
    (setq lsp-dart-dap--flutter-progress-reporter
          (make-progress-reporter (concat prefix (gethash "message" params))))))

(cl-defmethod dap-handle-event ((_event (eql dart.launched)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (when lsp-dart-dap--flutter-progress-reporter
    (progress-reporter-done lsp-dart-dap--flutter-progress-reporter))
  (lsp-dart-dap-log "Loading app..."))

(cl-defmethod dap-handle-event ((_event (eql dart.progress)) _session params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (-let (((&hash "message") params)
         (prefix (concat (propertize "[LSP Dart] "
                                     'face 'font-lock-keyword-face)
                         (propertize "[DAP] "
                                     'face 'font-lock-function-name-face))))
    (when (and message lsp-dart-dap--flutter-progress-reporter)
      (progress-reporter-force-update lsp-dart-dap--flutter-progress-reporter nil (concat prefix message)))))

(cl-defmethod dap-handle-event ((_event (eql dart.flutter.firstFrame)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (setq lsp-dart-dap--flutter-progress-reporter nil)
  (lsp-dart-dap-log "App ready!"))

(cl-defmethod dap-handle-event ((_event (eql dart.serviceRegistered)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS.")
(cl-defmethod dap-handle-event ((_event (eql dart.serviceExtensionAdded)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS.")
(cl-defmethod dap-handle-event ((_event (eql dart.flutter.serviceExtensionStateChanged)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS.")
(cl-defmethod dap-handle-event ((_event (eql dart.hotRestartRequest)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS.")
(cl-defmethod dap-handle-event ((_event (eql dart.hotReloadRequest)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS.")
(cl-defmethod dap-handle-event ((_event (eql dart.debugMetrics)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS.")
(cl-defmethod dap-handle-event ((_event (eql dart.navigate)) _session _params)
  "Handle debugger uris EVENT for SESSION with PARAMS.")

;; DevTools

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
  (lsp-dart-dap-log "Activating DevTools...")
  (let ((pub (lsp-dart-project-get-pub-command)))
    (lsp-async-start-process
     (lambda ()
       (lsp-dart-dap-log "DevTools activated successfully!")
       (funcall callback))
     (lambda (_) (lsp-dart-dap-log "Could not Activate DevTools. \
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


;;; Public interface

;;;###autoload
(defun lsp-dart-dap-open-devtools ()
  "Open Dart DevTools for the current debug session."
  (interactive)
  (let ((session (dap--cur-session))
        (vm-service-uri (lsp-workspace-get-metadata "dart-debug-vm-service-uri")))
    (when (and session vm-service-uri)
      (lsp-dart-dap--start-devtools
       (lambda (uri)
         (lsp-dart-dap-log "Openning DevTools at browser...")
         (lsp-dart-dap--open-devtools uri vm-service-uri))))))

(provide 'lsp-dart-dap)
;;; lsp-dart-dap.el ends here
