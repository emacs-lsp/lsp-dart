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

(require 'jsonrpc)
(require 'ht)
(require 'dash)
(require 'lsp-mode)

(require 'lsp-dart-protocol)
(require 'lsp-dart-utils)

(defconst lsp-dart-flutter-daemon-buffer-name "*LSP Dart - Flutter daemon*")
(defconst lsp-dart-flutter-daemon-name "LSP Dart - Flutter daemon")

(defvar lsp-dart-flutter-daemon-devices '())
(defvar lsp-dart-flutter-daemon-commands '())
(defvar lsp-dart-flutter-daemon-device-added-listeners '())

(defvar lsp-dart-flutter-daemon--conn nil
  "JSONRPC Connection object to the Flutter daemon process.")

(defclass lsp-dart-flutter-daemon-connection (jsonrpc-process-connection) ()
  "A connection based on stdio to connect to a Flutter daemon.")

(cl-defmethod jsonrpc-connection-send ((conn lsp-dart-flutter-daemon-connection)
                                       &rest args
                                       &key method params &allow-other-keys)
  "Implement send method to format JSON properly.
CONN ARGS METHOD PARAMS"
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method))
                     ((stringp method) method))))
  (unless params
    (cl-remf args :params))
  (let ((json (concat "[" (jsonrpc--json-encode args) "]\r\n")))
    (jsonrpc--log-event conn args 'client)
    (process-send-string
     (jsonrpc--process conn)
     json)))

(cl-defmethod initialize-instance ((conn lsp-dart-flutter-daemon-connection) _slots)
  "CONN."
  (cl-call-next-method)
  (let ((proc (jsonrpc--process conn)))
    (when proc
      (set-process-filter proc #'lsp-dart-flutter-daemon--process-filter))))

(defun lsp-dart-flutter-daemon--plist->hash-table (plist &optional test)
  "Create a hash table initialized from PLIST.
Optionally use TEST to compare the hash keys."
  (let ((ht (ht-create test)))
    (dolist (pair (nreverse (-partition 2 plist)) ht)
      (let ((key (lsp-keyword->string (car pair)))
            (value (cadr pair)))
        (ht-set! ht key value)))))

(defun lsp-dart-flutter-daemon--hash-table->plist (h)
  "Convert hash-table H to plist recurevely."
  (if (hash-table-p h)
      (apply 'append
             (ht-map (lambda (key value)
                       (list (intern (concat ":" key))
                             (if (or (vectorp value)
                                     (listp value))
                                 (-map #'lsp-dart-flutter-daemon--hash-table->plist value)
                               (lsp-dart-flutter-daemon--hash-table->plist value))))
                     h))
    h))

(defun lsp-dart-flutter-daemon--process-filter (proc response)
  "Invoked when a new RESPONSE has arrived from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (process-mark proc))
      (let* ((response (string-trim response))
             (should-trim (and (string-prefix-p (regexp-quote "][") response))))
        (when (string-prefix-p "[" response)
          (-map (lambda (msg)
                  (let ((json-message (condition-case-unless-debug oops
                                          (lsp--read-json (if should-trim
                                                              (substring msg 1 -1)
                                                            msg))
                                        (error
                                         (jsonrpc--warn "Invalid JSON: %S %s %s"
                                                        oops (buffer-string) msg)
                                         nil)))
                        (conn (process-get proc 'jsonrpc-connection)))
                    (when-let ((json (if (vectorp json-message)
                                         (aref json-message 0)
                                       json-message)))
                      (if (lsp-get json :error)
                          (lsp-dart-flutter-daemon--log "ERROR" (lsp-get json :error) (lsp-get json :trace)))
                      (if (lsp-get json :event)
                          (pcase (lsp-get json :event)
                            ("device.removed" (lsp-dart-flutter-daemon--device-removed (lsp-get json :params)))
                            ("device.added" (lsp-dart-flutter-daemon--device-added (lsp-get json :params)))
                            ("daemon.connected" (lsp-dart-flutter-daemon--send "device.enable"))
                            ("daemon.logMessage" (lsp-dart-flutter-daemon--log (lsp-get (lsp-get json :params) :level)
                                                                               (lsp-get (lsp-get json :params) :message))))
                        (with-temp-buffer
                          (jsonrpc-connection-receive conn (if lsp-use-plists
                                                               json
                                                             (lsp-dart-flutter-daemon--hash-table->plist json))))))))
                (split-string response "\n")))))))

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
  (and (not (null lsp-dart-flutter-daemon--conn))
       (process-live-p (get-process lsp-dart-flutter-daemon-name))))

(defun lsp-dart-flutter-daemon-start ()
  "Start the Flutter daemon."
  (unless (lsp-dart-flutter-daemon--running-p)
    (let ((conn (lsp-dart-flutter-daemon-connection
                 :name lsp-dart-flutter-daemon-name
                 :process (lambda ()
                            (make-process
                             :name lsp-dart-flutter-daemon-name
                             :command `(,@(lsp-dart-flutter-command) "daemon")
                             :coding 'utf-8-emacs-unix
                             :connection-type 'pipe
                             :buffer lsp-dart-flutter-daemon-buffer-name
                             :stderr (get-buffer-create
                                      (format "*%s stderr*" lsp-dart-flutter-daemon-name)))))))
      (setq lsp-dart-flutter-daemon--conn conn))))

(defun lsp-dart-flutter-daemon-->safe-object-type (obj)
  "Convert OBJ to the object type following lsp-use-plists variable.
OBJ should be a hash-table or plist."
  (if lsp-use-plists
      (if (or (vectorp obj)
              (listp obj))
          (-map #'lsp-dart-flutter-daemon--hash-table->plist obj)
        (lsp-dart-flutter-daemon--hash-table->plist obj))
    (if (or (vectorp obj)
            (listp obj))
        (-map #'lsp-dart-flutter-daemon--plist->hash-table obj)
      (lsp-dart-flutter-daemon--plist->hash-table obj))))

(defun lsp-dart-flutter-daemon--send (method &optional params callback)
  "Build and send command with METHOD with optional PARAMS.
Call CALLBACK if provided when the receive a response with the built id
of this command."
  (unless (lsp-dart-flutter-daemon--running-p)
    (lsp-dart-flutter-daemon-start))
  (jsonrpc-async-request lsp-dart-flutter-daemon--conn
                         method (when params
                                  (if lsp-use-plists
                                      params
                                    (lsp-dart-flutter-daemon--hash-table->plist params)))
                         :success-fn (lambda (result)
                                       (when result
                                         (funcall callback
                                                  (lsp-dart-flutter-daemon-->safe-object-type result))))))

(defun lsp-dart-flutter-daemon--device-removed (device)
  "Remove DEVICE from the devices list."
  (--> (lsp-get device :id)
       (lsp-dart-remove-from-alist it lsp-dart-flutter-daemon-devices)
       (setq lsp-dart-flutter-daemon-devices it)))

(defun lsp-dart-flutter-daemon--device-added (device)
  "Add DEVICE to the devices list."
  (-let* ((device-id (lsp:flutter-daemon-device-id device))
          (device-to-add (cons device-id device)))
    (lsp:set-flutter-daemon-device-is-device? device t)
    (setq lsp-dart-flutter-daemon-devices
          (lsp-dart-remove-from-alist device-id lsp-dart-flutter-daemon-devices))
    (add-to-list 'lsp-dart-flutter-daemon-devices device-to-add)
    (-when-let (listener (alist-get device-id lsp-dart-flutter-daemon-device-added-listeners))
      (setq lsp-dart-flutter-daemon-device-added-listeners
            (lsp-dart-remove-from-alist device-id lsp-dart-flutter-daemon-device-added-listeners))
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

(defun lsp-dart-flutter-daemon-launch (device callback)
  "Launch DEVICE and wait for connected state and call CALLBACK."
  (let* ((device-id (lsp:flutter-daemon-device-id device))
         (is-device? (lsp:flutter-daemon-device-is-device? device)))
    (if is-device?
        (funcall callback device)
      (lsp-dart-flutter-daemon--send
       "device.getDevices"
       nil
       (-lambda (devices)
         (if-let (emulator-running (-first (-lambda ((&FlutterDaemonDevice :emulator-id?)) (string= emulator-id? device-id)) (append devices nil)))
             (funcall callback emulator-running)
           (-let* ((params (lsp-make-flutter-daemon-emulator-launch :emulator-id device-id)))
             (add-to-list 'lsp-dart-flutter-daemon-device-added-listeners
                          (cons device-id (list :callback callback)))
             (lsp-dart-flutter-daemon--send "emulator.launch" params callback))))))))

(provide 'lsp-dart-flutter-daemon)
;;; lsp-dart-flutter-daemon.el ends here
