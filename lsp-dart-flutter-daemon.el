;;; lsp-dart-flutter-daemon.el --- Helper for the Flutter daemon -*- lexical-binding: t; -*-
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
;;  Helper for the Flutter daemon
;;
;;; Code:

(require 'dash)
(require 'lsp-mode)

(require 'lsp-dart-protocol)
(require 'lsp-dart-utils)

(defconst lsp-dart-flutter-daemon-buffer-name "*LSP Dart - Flutter daemon*")
(defconst lsp-dart-flutter-daemon-name "LSP Dart - Flutter daemon")

(defvar lsp-dart-flutter-daemon-devices '())
(defvar lsp-dart-flutter-daemon-commands '())
(defvar lsp-dart-flutter-daemon-device-added-listeners '())

(defcustom lsp-dart-flutter-daemon--port 9290
  "Port to listen for connections on."
  :group 'lsp-dart
  :type 'integer)

(defun lsp-dart-flutter-daemon--log (level msg &rest args)
  "Log for LEVEL, MSG with ARGS adding lsp-dart-flutter-daemon prefix."
  (unless (string= "STATUS" (upcase level))
    (apply #'lsp-dart-custom-log (concat "[FLUTTER " (upcase level) "] ")
           msg
           args)))

(defun lsp-dart-flutter-daemon--generate-command-id ()
  "Generate a random command id."
  (random 100000))

(defun lsp-dart-flutter-daemon--running-p ()
  "Return non-nil if the Flutter daemon is already running."
  (process-status lsp-dart-flutter-daemon-name))

(defun lsp-dart-flutter-daemon-start ()
  "Start the Flutter daemon."
  (unless (lsp-dart-flutter-daemon--running-p)
    (if (null (condition-case err
                  (let ((proc (make-process
                               :name lsp-dart-flutter-daemon-name
                               :command `(,(lsp-dart-flutter-command) "daemon" "--listen-on-tcp-port" ,(number-to-string lsp-dart-flutter-daemon--port))
                               :buffer lsp-dart-flutter-daemon-buffer-name
                               :stderr (get-buffer-create (format "*%s stderr*" lsp-dart-flutter-daemon-name)))))
                    (dolist (buf `(,lsp-dart-flutter-daemon-buffer-name ,(format "*%s stderr*" lsp-dart-flutter-daemon-name)))
                      (when (get-buffer buf)
                        (with-current-buffer (get-buffer buf)
                          (special-mode)
                          (read-only-mode))))
                    (lsp-dart-flutter-daemon--send "device.enable")
                    proc)
                (file-error nil)))
        (error "Failed to start a flutter daemon"))))

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
         (split-string it "\n")
         (-map (lambda (el)
                 (lsp--read-json (substring el 1 -1)))
               it))))

(defun lsp-dart-flutter-daemon--handle-responses (raw-response callback)
  "Handle Flutter daemon response from RAW-RESPONSE."
  (-map (-lambda ((&FlutterDaemonResponse :id :event? :result?
                                          :params? (params &as &FlutterDaemonResponseParams? :level? :message?)))
          (if event?
              (pcase event?
                ("device.removed" (lsp-dart-flutter-daemon--device-removed params))

                ("device.added" (lsp-dart-flutter-daemon--device-added params))

                ("daemon.logMessage" (lsp-dart-flutter-daemon--log level? message?)))
            (message "%S" `(,id ,result? ,event?))
            (when callback
              (when result?
                (funcall callback result?)))))
        (lsp-dart-flutter-daemon--raw->response raw-response)))

(defun lsp-dart-flutter-daemon--process-filter (process message &optional callback)
  "Handle MESSAGE from PROCESS."
  (message "[filter] Got %S from %S" message process)
  (lsp-dart-flutter-daemon--handle-responses message callback)
  (lambda ()
    (delete-process process)))

(defun lsp-dart-flutter-daemon--send (method &optional params callback)
  "Build and send command with METHOD with optional PARAMS.
Call CALLBACK if provided when the receive a response with the built id
of this command."
  (unless (lsp-dart-flutter-daemon--running-p)
    (lsp-dart-flutter-daemon-start))
  (let* ((id (lsp-dart-flutter-daemon--generate-command-id))
         (command (lsp-dart-flutter-daemon--build-command id method params))
         (client (open-network-stream
                  "lsp-flutter-client"
                  (get-buffer-create (format "*%s client*" lsp-dart-flutter-daemon-name))
                  "localhost"
                  (number-to-string lsp-dart-flutter-daemon--port))))
    (set-process-filter-multibyte client t)
    (set-process-coding-system client 'utf-8 'utf-8)
    (set-process-filter client (lambda (proc msg)
                                 (lsp-dart-flutter-daemon--process-filter proc msg callback)))
    (set-process-sentinel client #'lsp-dart-flutter-daemon--process-sentinel)
    (process-send-string client command)))

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
     (let ((devices-excluding-emulators (-remove (-lambda ((&FlutterDaemonDevice :emulator-id?))
                                                   (and emulator-id?
                                                        (-first (lambda (emulator) (string= emulator-id? (lsp:flutter-daemon-device-id emulator)))
                                                                (append emulators nil))))
                                                 (-map #'cdr lsp-dart-flutter-daemon-devices))))
       (->> devices-excluding-emulators
            (append emulators)
            (funcall callback))))))

(lsp-defun lsp-dart-flutter-daemon-launch ((device &as &FlutterDaemonDevice :id :is-device?) callback)
  "Launch DEVICE and wait for connected state and call CALLBACK."
  (if is-device?
      (funcall callback device)
    (lsp-dart-flutter-daemon--send
     "device.getDevices"
     nil
     (-lambda (devices)
       (if-let (emulator-running? (-first (-lambda ((&FlutterDaemonDevice :emulator-id?)) (string= emulator-id? id)) (append devices nil)))
           (funcall callback device)
         (-let* ((params (lsp-make-flutter-daemon-emulator-launch :emulator-id id)))
           (add-to-list 'lsp-dart-flutter-daemon-device-added-listeners
                        (cons id (list :callback callback)))
           (lsp-dart-flutter-daemon--send "emulator.launch" params callback)))))))

(provide 'lsp-dart-flutter-daemon)
;;; lsp-dart-flutter-daemon.el ends here
