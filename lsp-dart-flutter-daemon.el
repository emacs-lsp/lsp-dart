;;; lsp-dart-flutter-daemon.el --- Helper for the Flutter daemon -*- lexical-binding: t; -*-
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
;;  Helper for the Flutter daemon
;;
;;; Code:

(require 'comint)
(require 'dash)
(require 'lsp-mode)

(require 'lsp-dart-protocol)
(require 'lsp-dart-utils)

(defconst lsp-dart-flutter-daemon-buffer-name "*LSP Dart - Flutter daemon*")
(defconst lsp-dart-flutter-daemon-name "LSP Dart - Flutter daemon")

(defvar lsp-dart-flutter-daemon-devices '())
(defvar lsp-dart-flutter-daemon-commands '())
(defvar lsp-dart-flutter-daemon-device-added-listeners '())

(defvar lsp-dart-flutter-daemon-current-device nil)

(defun lsp-dart-flutter-daemon--log (level msg &rest args)
  "Log for LEVEL, MSG with ARGS adding lsp-dart-flutter-daemon prefix."
  (unless (string= "STATUS" (upcase level))
    (apply #'lsp-dart-custom-log (concat "[FLUTTER " (upcase level) "] ")
           msg
           args)))

(defun lsp-dart-flutter-daemon--generate-command-id ()
  "Generate a random command id."
  (random 10000))

(defun lsp-dart-flutter-daemon--running-p ()
  "Return non-nil if the Flutter daemon is already running."
  (comint-check-proc lsp-dart-flutter-daemon-buffer-name))

(defun lsp-dart-flutter-daemon-start ()
  "Start the Flutter daemon."
  (unless (lsp-dart-flutter-daemon--running-p)
    (let ((buffer (get-buffer-create lsp-dart-flutter-daemon-buffer-name)))
      (make-comint-in-buffer lsp-dart-flutter-daemon-name buffer (lsp-dart-flutter-command) nil "daemon")
      (with-current-buffer buffer
        (unless (derived-mode-p 'lsp-dart-flutter-daemon-mode)
          (lsp-dart-flutter-daemon-mode))
        (remove-hook 'dap-terminated-hook #'lsp-dart-flutter-daemon--reset-current-device t)
        (add-hook 'dap-terminated-hook #'lsp-dart-flutter-daemon--reset-current-device nil t)
        (setq-local comint-output-filter-functions #'lsp-dart-flutter-daemon--handle-responses))
      (lsp-dart-flutter-daemon--send "device.enable"))))

(defun lsp-dart-flutter-daemon--build-command (id method &optional params)
  "Build a command from an ID and METHOD.
PARAMS is the optional method params."
  (let ((command (lsp-make-flutter-daemon-command :id id
                                                  :method method)))
    (when params
      (lsp:set-flutter-daemon-command-params? command params))
    (concat "["
            (lsp--json-serialize command)
            "]\n")))

(defun lsp-dart-flutter-daemon--raw->response (response)
  "Parse raw RESPONSE into a list of responses."
  (when (string-prefix-p "[" (string-trim response))
    (--> response
         string-trim
         (replace-regexp-in-string (regexp-quote "\n") "" it nil 'literal)
         (replace-regexp-in-string (regexp-quote "][") "]\n[" it nil 'literal)
         (split-string it "\n")
         (-map (lambda (el) (lsp-seq-first (lsp--read-json el))) it))))

(defun lsp-dart-flutter-daemon--handle-responses (raw-response)
  "Handle Flutter daemon response from RAW-RESPONSE."
  (-map (-lambda ((&FlutterDaemonResponse :id :event? :result?
                                          :params? (params &as &FlutterDaemonResponseParams? :level? :message?)))
          (if event?
              (pcase event?
                ("device.removed" (lsp-dart-flutter-daemon--device-removed params))

                ("device.added" (lsp-dart-flutter-daemon--device-added params))

                ("daemon.logMessage" (lsp-dart-flutter-daemon--log level? message?)))
            (let* ((command (alist-get id lsp-dart-flutter-daemon-commands))
                   (callback (plist-get command :callback)))
              (when command
                (setq lsp-dart-flutter-daemon-commands
                      (lsp-dart-remove-from-alist id lsp-dart-flutter-daemon-commands)))
              (when callback
                (when result?
                  (funcall callback result?))))))
        (lsp-dart-flutter-daemon--raw->response raw-response)))

(defun lsp-dart-flutter-daemon--send (method &optional params callback)
  "Build and send command with METHOD with optional PARAMS.
Call CALLBACK if provided when the receive a response with the built id
of this command."
  (unless (lsp-dart-flutter-daemon--running-p)
    (lsp-dart-flutter-daemon-start))
  (let* ((id (lsp-dart-flutter-daemon--generate-command-id))
         (command (lsp-dart-flutter-daemon--build-command id method params)))
    (add-to-list 'lsp-dart-flutter-daemon-commands (cons id (list :callback callback)))
    (comint-send-string (get-buffer-process lsp-dart-flutter-daemon-buffer-name) command)))

(defun lsp-dart-flutter-daemon--device-removed (device)
  "Remove DEVICE from the devices list."
  (--> (gethash "id" device)
       (lsp-dart-remove-from-alist it lsp-dart-flutter-daemon-devices)
       (setq lsp-dart-flutter-daemon-devices it)))

(lsp-defun lsp-dart-flutter-daemon--device-added ((device &as &FlutterDaemonDevice :id))
  "Add DEVICE to the devices list."
  (-let ((device-to-add (cons id device)))
    (lsp:set-flutter-daemon-device-is-device? device t)
    (setq lsp-dart-flutter-daemon-devices
          (lsp-dart-remove-from-alist id lsp-dart-flutter-daemon-devices))
    (add-to-list 'lsp-dart-flutter-daemon-devices device-to-add)
    (-when-let (listener (alist-get id lsp-dart-flutter-daemon-device-added-listeners))
      (setq lsp-dart-flutter-daemon-device-added-listeners
            (lsp-dart-remove-from-alist id lsp-dart-flutter-daemon-device-added-listeners))
      (funcall (plist-get listener :callback) device))))

(defun lsp-dart-flutter-daemon-get-devices (callback)
  "Call CALLBACK with the available emulators and devices from Flutter daemon."
  (lsp-dart-flutter-daemon--send
   "emulator.getEmulators"
   nil
   (-lambda (emulators)
     (->> lsp-dart-flutter-daemon-devices
          (-map #'cdr)
          (append emulators)
          (funcall callback)))))

(lsp-defun lsp-dart-flutter-daemon-launch ((device &as &FlutterDaemonDevice :id :is-device?) callback)
  "Launch DEVICE and wait for connected state and call CALLBACK."
  (if lsp-dart-flutter-daemon-current-device
      (funcall callback lsp-dart-flutter-daemon-current-device)
    (progn
      (setq lsp-dart-flutter-daemon-current-device device)
      (if is-device?
          (funcall callback device)
        (-let* ((params (lsp-make-flutter-daemon-emulator-launch :emulator-id id)))
          (add-to-list 'lsp-dart-flutter-daemon-device-added-listeners
                       (cons id (list :callback callback)))
          (lsp-dart-flutter-daemon--send "emulator.launch" params callback))))))

(defun lsp-dart-flutter-daemon--reset-current-device (_session)
  "Reset the current device."
  (setq lsp-dart-flutter-daemon-current-device nil))

;;;###autoload
(define-derived-mode lsp-dart-flutter-daemon-mode comint-mode lsp-dart-flutter-daemon-name
  "Major mode for `lsp-dart-flutter-daemon-start`."
  (setq comint-prompt-read-only nil)
  (setq comint-process-echoes nil)
  (setenv "PATH" (concat (lsp-dart-flutter-command) ":" (getenv "PATH"))))

(provide 'lsp-dart-flutter-daemon)
;;; lsp-dart-flutter-daemon.el ends here
