;;; lsp-dart-dap.el --- DAP support for lsp-dart -*- lexical-binding: t; -*-
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

(require 'lsp-dart-utils)
(require 'lsp-dart-flutter-daemon)
(require 'lsp-dart-devtools)

(defcustom lsp-dart-dap-extension-version "3.10.1"
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

(defcustom lsp-dart-dap-dart-test-debugger-program
  `("node" ,(f-join lsp-dart-dap-debugger-path "extension/out/src/debug/dart_test_debug_entry.js"))
  "The path to the dart test debugger."
  :group 'lsp-dart
  :type '(repeat string))

(defcustom lsp-dart-dap-flutter-debugger-program
  `("node" ,(f-join lsp-dart-dap-debugger-path "extension/out/src/debug/flutter_debug_entry.js"))
  "The path to the Flutter debugger."
  :group 'lsp-dart
  :type '(repeat string))

(defcustom lsp-dart-dap-flutter-test-debugger-program
  `("node" ,(f-join lsp-dart-dap-debugger-path "extension/out/src/debug/flutter_test_debug_entry.js"))
  "The path to the dart test debugger."
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

(defcustom lsp-dart-dap-vm-additional-args ""
  "Additional args for dart debugging VM."
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

(defun lsp-dart-dap-log (msg &rest args)
  "Log MSG with ARGS adding lsp-dart-dap prefix."
  (apply #'lsp-dart-custom-log "[DAP]" msg args))

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

(defun lsp-dart-dap--capabilities-debugger-args (conf)
  "Add capabilities args on CONF checking dart SDK version."
  (-> conf
      (dap--put-if-absent :useWriteServiceInfo (lsp-dart-version-at-least-p "2.7.1"))
      (dap--put-if-absent :debuggerHandlesPathsEverywhereForBreakpoints (lsp-dart-version-at-least-p "2.2.1-edge"))))

(defun lsp-dart-dap--base-debugger-args (conf)
  "Return the base args for debugging merged with CONF."
  (-> conf
      (dap--put-if-absent :request "launch")
      (dap--put-if-absent :dartPath (lsp-dart-dart-command))
      (dap--put-if-absent :cwd (lsp-dart-get-project-root))
      (dap--put-if-absent :pubPath (lsp-dart-pub-command))
      (dap--put-if-absent :pubSnapshotPath (lsp-dart-pub-snapshot-command))
      (dap--put-if-absent :vmAdditionalArgs lsp-dart-dap-vm-additional-args)
      (dap--put-if-absent :debugExternalLibraries lsp-dart-dap-debug-external-libraries)
      (dap--put-if-absent :debugSdkLibraries lsp-dart-dap-debug-sdk-libraries)
      (dap--put-if-absent :flutterPath (lsp-dart-flutter-command))
      (dap--put-if-absent :flutterTrackWidgetCreation lsp-dart-dap-flutter-track-widget-creation)
      (dap--put-if-absent :useFlutterStructuredErrors lsp-dart-dap-flutter-structured-errors)
      lsp-dart-dap--capabilities-debugger-args))

;; Dart

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
      lsp-dart-dap--base-debugger-args
      (dap--put-if-absent :type "dart")
      (dap--put-if-absent :name "Dart")
      (dap--put-if-absent :dap-server-path lsp-dart-dap-dart-debugger-program)
      (dap--put-if-absent :program (lsp-dart-get-project-entrypoint))))

(dap-register-debug-provider "dart" 'lsp-dart-dap--populate-dart-start-file-args)
(dap-register-debug-template "Dart :: Debug"
                             (list :type "dart"))

;; Flutter

(declare-function all-the-icons-faicon "ext:all-the-icons")

(defun lsp-dart-dap--device-label (id name is-device platform)
  "Return a friendly label for device with ID, NAME IS-DEVICE and PLATFORM.
Check for icons if supports it."
  (let* ((device-name (if name name id))
         (type-text (if is-device "device" "emulator"))
         (default (concat platform " - " type-text " - " device-name)))
    (if (featurep 'all-the-icons)
        (pcase platform
          ("android" (concat (all-the-icons-faicon "android" :face 'all-the-icons-green) " " type-text " - " device-name))
          ("ios" (concat (all-the-icons-faicon "apple" :face 'all-the-icons-lsilver) " " type-text " - " device-name))
          (_ default))
      default)))

(defun lsp-dart-dap--flutter-get-or-start-device (callback)
  "Return the device to debug or prompt to start it.
Call CALLBACK when the device is chosen and started successfully."
  (lsp-dart-flutter-daemon-get-devices
   (lambda (devices)
     (if (seq-empty-p devices)
         (lsp-dart-log "No devices found. Try to create a device first via `flutter emulators` command")
       (-let* ((chosen-device (dap--completing-read "Select a device to use: "
                                                    devices
                                                    (-lambda ((&hash "id" "name" "is-device" "platformType" platform))
                                                      (lsp-dart-dap--device-label id name is-device platform))
                                                    nil
                                                    t)))
         (lsp-dart-flutter-daemon-launch chosen-device callback))))))

(defun lsp-dart-dap--populate-flutter-start-file-args (conf)
  "Populate CONF with the required arguments for Flutter debug."
  (let ((pre-conf (-> conf
                      lsp-dart-dap--base-debugger-args
                      (dap--put-if-absent :type "flutter")
                      (dap--put-if-absent :flutterMode "debug")
                      (dap--put-if-absent :dap-server-path lsp-dart-dap-flutter-debugger-program)
                      (dap--put-if-absent :program (lsp-dart-get-project-entrypoint)))))
    (lambda (start-debugging-callback)
      (lsp-dart-dap--flutter-get-or-start-device
       (-lambda ((&hash "id" device-id "name" device-name))
         (funcall start-debugging-callback
                  (-> pre-conf
                      (dap--put-if-absent :deviceId device-id)
                      (dap--put-if-absent :deviceName device-name)
                      (dap--put-if-absent :flutterPlatform "default")
                      (dap--put-if-absent :name (concat "Flutter (" device-name ")")))))))))

(dap-register-debug-provider "flutter" 'lsp-dart-dap--populate-flutter-start-file-args)
(dap-register-debug-template "Flutter :: Debug"
                             (list :type "flutter"))

(defvar lsp-dart-dap--flutter-progress-reporter nil)
(defvar lsp-dart-dap--flutter-progress-reporter-timer nil)

(defun lsp-dart-dap--cancel-flutter-progress (_debug-session)
  "Cancel the Flutter progress timer for DEBUG-SESSION."
  (setq lsp-dart-dap--flutter-progress-reporter nil)
  (when lsp-dart-dap--flutter-progress-reporter-timer
    (cancel-timer lsp-dart-dap--flutter-progress-reporter-timer))
  (setq lsp-dart-dap--flutter-progress-reporter-timer nil))

(add-hook 'dap-terminated-hook #'lsp-dart-dap--cancel-flutter-progress)

(defun lsp-dart-dap--flutter-tick-progress-update ()
  "Update the flutter progress reporter."
  (when lsp-dart-dap--flutter-progress-reporter
    (progress-reporter-update lsp-dart-dap--flutter-progress-reporter)))

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
          (make-progress-reporter (concat prefix (gethash "message" params))))
    (setq lsp-dart-dap--flutter-progress-reporter-timer
          (run-with-timer 0.2 0.2 #'lsp-dart-dap--flutter-tick-progress-update))))

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
  (lsp-dart-dap--cancel-flutter-progress (dap--cur-session))
  (lsp-dart-dap-log "App ready!"))

(cl-defmethod dap-handle-event ((_event (eql dart.serviceRegistered)) _session _params)
  "Ignore this event.")
(cl-defmethod dap-handle-event ((_event (eql dart.serviceExtensionAdded)) _session _params)
  "Ignore this event.")
(cl-defmethod dap-handle-event ((_event (eql dart.flutter.serviceExtensionStateChanged)) _session _params)
  "Ignore this event.")
(cl-defmethod dap-handle-event ((_event (eql dart.hotRestartRequest)) _session _params)
  "Ignore this event.")
(cl-defmethod dap-handle-event ((_event (eql dart.hotReloadRequest)) _session _params)
  "Ignore this event.")
(cl-defmethod dap-handle-event ((_event (eql dart.debugMetrics)) _session _params)
  "Ignore this event.")
(cl-defmethod dap-handle-event ((_event (eql dart.navigate)) _session _params)
  "Ignore this event.")

(cl-defmethod dap-handle-event ((_event (eql dart.testRunNotification)) _session _params)
  "Ignore this event.")


;; Public

(defun lsp-dart-dap-run-dart (path)
  "Start Dart application without debugging from PATH."
  (-> (list :type "dart"
            :name "Dart Run"
            :program path
            :noDebug t
            :shouldConnectDebugger nil)
      lsp-dart-dap--populate-dart-start-file-args
      dap-start-debugging))

(defun lsp-dart-dap-run-flutter (path)
  "Start Flutter app without debugging from PATH."
  (-> (list :type "flutter"
            :name "Flutter Run"
            :program path
            :noDebug t
            :shouldConnectDebugger nil)
      lsp-dart-dap--populate-flutter-start-file-args
      (funcall #'dap-start-debugging)))

(defun lsp-dart-dap-debug-dart (path)
  "Debug dart application from PATH."
  (-> (list :program path)
      lsp-dart-dap--populate-dart-start-file-args
      dap-start-debugging))

(defun lsp-dart-dap-debug-flutter (path)
  "Debug Flutter application from PATH."
  (-> (list :program path)
      lsp-dart-dap--populate-flutter-start-file-args
      (funcall #'dap-start-debugging)))

(defun lsp-dart-dap-debug-dart-test (path &optional args)
  "Start dart test debugging from PATH with ARGS."
  (-> (list :type "dart"
            :name "Dart Tests"
            :dap-server-path lsp-dart-dap-dart-test-debugger-program
            :program path
            :noDebug nil
            :shouldConnectDebugger t
            :args args)
      lsp-dart-dap--base-debugger-args
      dap-start-debugging))

(defun lsp-dart-dap-debug-flutter-test (path &optional args)
  "Start dart test debugging from PATH with ARGS."
  (-> (list :name "Flutter Tests"
            :type "flutter"
            :dap-server-path lsp-dart-dap-flutter-test-debugger-program
            :program path
            :noDebug nil
            :shouldConnectDebugger t
            :flutterMode "debug"
            :args args)
      lsp-dart-dap--base-debugger-args
      dap-start-debugging))


;; Public Interface

;;;###autoload
(defun lsp-dart-dap-flutter-hot-restart ()
  "Hot restart current Flutter debug session."
  (interactive)
  (seq-doseq (debug-session (dap--get-sessions))
    (dap-request debug-session "hotRestart")))

;;;###autoload
(defun lsp-dart-dap-flutter-hot-reload ()
  "Hot reload current Flutter debug session."
  (interactive)
  (seq-doseq (debug-session (dap--get-sessions))
    (dap-request debug-session "hotReload")))

(provide 'lsp-dart-dap)
;;; lsp-dart-dap.el ends here
