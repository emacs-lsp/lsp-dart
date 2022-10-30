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
  "Implement send method to format JSON properly."
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
  (cl-call-next-method)
  (let ((proc (jsonrpc--process conn)))
    (when proc
      (set-process-filter proc #'lsp-dart-flutter-daemon--process-filter))))

(defun lsp-dart-flutter-daemon--json-read-string (s)
  (if (fboundp 'json-parse-string)
      (json-parse-string s
                         :object-type 'plist
                         :false-object nil
                         :null-object nil)
    (let ((json-object-type 'plist)
          (json-false :json-false)
          (json-null nil))
      (json-read-from-string s))))

;; TODO: Greatly reduce this complexity
(defun lsp-dart-flutter-daemon--process-filter (proc string)
  "Invoked when a new STRING has arrived from PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((should-trim (not (string-match-p (regexp-quote "][") (string-trim string))))
            (inhibit-read-only t))
        (goto-char (process-mark proc))
        (when (string-prefix-p "[" (string-trim string))
          (--> string
               string-trim
               (replace-regexp-in-string (regexp-quote "\n") "" it nil 'literal)
               (replace-regexp-in-string (regexp-quote "][") "," it nil 'literal)
               (split-string it "\n")
               (-map (lambda (response)
                       (let ((hash-table (json-parse-string (if should-trim (substring response 1 -1) response)
                                                            :object-type 'hash-table
                                                            :false-object :json-false
                                                            :null-object nil))
                             (json-message (condition-case-unless-debug oops
                                               (lsp-dart-flutter-daemon--json-read-string (if should-trim
                                                                                              (substring response 1 -1)
                                                                                            response))
                                             (error
                                              (jsonrpc--warn "Invalid JSON: %S %s"
                                                             oops (buffer-string))
                                              nil)))
                             (conn (process-get proc 'jsonrpc-connection)))
                         (when json-message
                           (message "json: %S\nresponse: %S" json-message response)
                           (if (plist-get json-message :error)
                               (lsp-dart-flutter-daemon--log "ERROR" (plist-get json-message :error) (plist-get json-message :trace)))
                           (if (plist-get json-message :event)
                               (pcase (plist-get json-message :event)
                                 ("device.removed" (lsp-dart-flutter-daemon--device-removed (gethash "params" hash-table)))
                                 ("device.added" (lsp-dart-flutter-daemon--device-added (gethash "params" hash-table)))
                                 ("daemon.connected" (lsp-dart-flutter-daemon--send "device.enable"))
                                 ("daemon.logMessage" (lsp-dart-flutter-daemon--log (plist-get (plist-get json-message :params) :level) (plist-get json-message :params))))
                             (with-temp-buffer
                               (jsonrpc-connection-receive conn
                                                           json-message)))))
                       )
                     it)))))))

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

(defun lsp-dart-flutter-daemon--send (method &optional params callback)
  "Build and send command with METHOD with optional PARAMS.
Call CALLBACK if provided when the receive a response with the built id
of this command."
  (unless (lsp-dart-flutter-daemon--running-p)
    (lsp-dart-flutter-daemon-start))
  (jsonrpc-async-request lsp-dart-flutter-daemon--conn
                         method params
                         :success-fn (lambda (result)
                                       (funcall callback
                                                (if (vectorp result)
                                                    (-map #'lsp-dart-flutter-daemon--plist->hash-table result)
                                                  (lsp-dart-flutter-daemon--plist->hash-table result))))))

(defun lsp-dart-flutter-daemon--device-removed (device)
  "Remove DEVICE from the devices list."
  (--> (gethash "id" device)
       (lsp-dart-remove-from-alist it lsp-dart-flutter-daemon-devices)
       (setq lsp-dart-flutter-daemon-devices it)))

(defun lsp-dart-flutter-daemon--plist->hash-table (plist &optional test)
  "Create a hash table initialized from PLIST.

Optionally use TEST to compare the hash keys."
  (let ((ht (ht-create test)))
    (dolist (pair (nreverse (-partition 2 plist)) ht)
      (let ((key (substring (symbol-name (car pair)) 1))
            (value (cadr pair)))
        (ht-set! ht key value)))))

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
       (if-let (emulator-running (-first (-lambda ((&FlutterDaemonDevice :emulator-id?)) (string= emulator-id? id)) (append devices nil)))
           (funcall callback emulator-running)
         (-let* ((params (lsp-make-flutter-daemon-emulator-launch :emulator-id id)))
           (add-to-list 'lsp-dart-flutter-daemon-device-added-listeners
                        (cons id (list :callback callback)))
           (lsp-dart-flutter-daemon--send "emulator.launch" params callback)))))))

(provide 'lsp-dart-flutter-daemon)
;;; lsp-dart-flutter-daemon.el ends here
