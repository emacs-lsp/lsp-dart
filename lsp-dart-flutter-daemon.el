;;; lsp-dart-flutter-daemon.el --- Helper for the Flutter daemon -*- lexical-binding: t; -*-
;;
;; Version: 1.8
;; Keywords: languages, extensions
;; Package-Requires: ((emacs "25.2") (lsp-mode "6.0"))
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
;;
;;; Commentary:
;;
;;  Helper for the Flutter daemon
;;
;;; Code:

(require 'comint)
(require 'lsp-mode)

(require 'lsp-dart-project)

(defconst lsp-dart-flutter-daemon-buffer-name "*LSP Dart - Flutter daemon*")
(defconst lsp-dart-flutter-daemon-name "LSP Dart - Flutter daemon")

(defun lsp-dart-flutter-daemon--generate-command-id ()
  "Generate a random command id."
  (random 10000))

(defun lsp-dart-flutter-daemon--start ()
  "Start the Flutter daemon."
  (let ((buffer (get-buffer-create lsp-dart-flutter-daemon-buffer-name)))
    (make-comint-in-buffer lsp-dart-flutter-daemon-name buffer lsp-dart-project-flutter-command nil "daemon")
    (with-current-buffer buffer
      (unless (derived-mode-p 'lsp-dart-flutter-daemon-mode)
        (lsp-dart-flutter-daemon-mode)))
    ;; We need to wait the daemon start
    (sit-for 1)))

(defun lsp-dart-flutter-daemon--build-command (id method &optional params)
  "Build a command from an ID and METHOD.
PARAMS is the optional method params."
  (let ((command (ht ("id" id)
                     ("method" method))))
    (when params
      (ht-set! command "params" params))
    (concat "["
            (lsp--json-serialize command)
            "]\n")))

(defvar lsp-dart-flutter-daemon-response nil)

(defun lsp-dart-flutter-daemon-handle-response (id event-name response)
  "Handle RESPONSE and save if it is for ID.
Wait for next response if EVENT-NAME is non null."
  (when (string-prefix-p "[" response)
    (-let* (((&hash "id" resp-id "result" "event" "params") (lsp--read-json (substring response 1 -2))))
      (if event-name
          (when (string= event event-name)
            (setq lsp-dart-flutter-daemon-response params))
          (when (= resp-id id)
            (setq lsp-dart-flutter-daemon-response result))))))

(defun lsp-dart-flutter-daemon-running-p ()
  "Return non-nil if the Flutter daemon is already running."
  (comint-check-proc lsp-dart-flutter-daemon-buffer-name))

(defun lsp-dart-flutter-daemon--send (method &optional params event-name)
  "Send a command with METHOD to a Flutter daemon and await for a response.
PARAMS is the optional method args and should be a hash-table.
If EVENT-NAME is non-nil, it will this event to return its value.
Starts the daemon if is not running yet."
  (unless (lsp-dart-flutter-daemon-running-p)
    (lsp-dart-flutter-daemon--start))
  (setq lsp-dart-flutter-daemon-response nil)
  (let* ((id (lsp-dart-flutter-daemon--generate-command-id))
         (command (lsp-dart-flutter-daemon--build-command id method params)))
    (remove-hook 'comint-output-filter-functions (-partial #'lsp-dart-flutter-daemon-handle-response id event-name) t)
    (add-hook 'comint-output-filter-functions (-partial #'lsp-dart-flutter-daemon-handle-response id event-name))
    (comint-send-string (get-buffer-process lsp-dart-flutter-daemon-buffer-name) command)
    (while (not lsp-dart-flutter-daemon-response)
      (sit-for 0.1))
    lsp-dart-flutter-daemon-response))

(defun lsp-dart-flutter-daemon-get-emulators ()
  "Return the available emulators from Flutter daemon."
  (lsp-dart-flutter-daemon--send "emulator.getEmulators"))

(defun lsp-dart-flutter-daemon-launch (device)
  "Launch emulator for DEVICE and wait for connected state."
  (-let* (((&hash "id") device)
          (params (ht ('emulatorId id))))
    (lsp-dart-flutter-daemon--send "device.enable")
    (lsp-dart-flutter-daemon--send "emulator.launch" params "device.added")))

;;;###autoload
(define-derived-mode lsp-dart-flutter-daemon-mode comint-mode lsp-dart-flutter-daemon-name
  "Major mode for `lsp-dart-flutter-daemon--start`."
  (setq comint-prompt-read-only nil)
  (setq comint-process-echoes nil)
  (setenv "PATH" (concat (lsp-dart-project-get-flutter-path) ":" (getenv "PATH"))))

(provide 'lsp-dart-flutter-daemon)
;;; lsp-dart-flutter-daemon.el ends here
