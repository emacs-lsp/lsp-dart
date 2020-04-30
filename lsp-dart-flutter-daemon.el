;;; lsp-dart-flutter-daemon.el --- Helper for the Flutter daemon -*- lexical-binding: t; -*-
;;
;; Version: 1.8
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
;;
;;; Commentary:
;;
;;  Helper for the Flutter daemon
;;
;;; Code:

(require 'comint)
(require 'dash)
(require 'ht)
(require 'lsp-mode)

(require 'lsp-dart-project)

(defconst lsp-dart-flutter-daemon-buffer-name "*LSP Dart - Flutter daemon*")
(defconst lsp-dart-flutter-daemon-name "LSP Dart - Flutter daemon")

(defvar lsp-dart-flutter-daemon-current-command nil)
(defvar lsp-dart-flutter-daemon-current-device nil)

(defun lsp-dart-flutter-daemon-log (level msg &rest args)
  "Log for LEVEL, MSG with ARGS adding lsp-dart-flutter-daemon prefix."
  (apply #'lsp-dart-project-custom-log (concat "[FLUTTER " (upcase level) "] ")
         msg
         args))

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

(defun lsp-dart-flutter-daemon-raw->response (response)
  "Parse raw RESPONSE into a list of responses."
  (when (string-prefix-p "[" (string-trim response))
    (--> response
         string-trim
         (replace-regexp-in-string (regexp-quote "\n") "" it nil 'literal)
         (replace-regexp-in-string (regexp-quote "][") "]\n[" it nil 'literal)
         (split-string it "\n")
         (-map (lambda (el) (lsp-seq-first (lsp--read-json el))) it))))

(defun lsp-dart-flutter-daemon-handle-events (raw-response)
  "Handle Flutter daemon events from RAW-RESPONSE."
  (-map (-lambda ((&hash "event" "params" (params &as &hash? "level" "message")))
          (when event
            (pcase event
              ("device.removed" (setq lsp-dart-flutter-daemon-current-device nil))

              ("device.added" (setq lsp-dart-flutter-daemon-current-device params))

              ("daemon.logMessage" (lsp-dart-flutter-daemon-log level message)))))
        (lsp-dart-flutter-daemon-raw->response raw-response)))

(defun lsp-dart-flutter-daemon-handle-response (raw-response)
  "Handle the RAW-RESPONSE from comint output."
  (when lsp-dart-flutter-daemon-current-command
    (--map
      (-let* (((&hash "id" resp-id "result" "event" "params") it)
              ((&hash "id" "callback" "event-name") lsp-dart-flutter-daemon-current-command))
        (if event-name
            (when (string= event event-name)
              (remove-hook 'comint-output-filter-functions #'lsp-dart-flutter-daemon-handle-response)
              (funcall callback params))
          (when (= resp-id id)
            (remove-hook 'comint-output-filter-functions #'lsp-dart-flutter-daemon-handle-response)
            (if result
                (funcall callback result)
              (funcall callback)))))
      (lsp-dart-flutter-daemon-raw->response raw-response))))

(defun lsp-dart-flutter-daemon-running-p ()
  "Return non-nil if the Flutter daemon is already running."
  (comint-check-proc lsp-dart-flutter-daemon-buffer-name))

(defun lsp-dart-flutter-daemon--send (method callback &optional params event-name)
  "Send a command with METHOD to the daemon and call CALLBACK with the response.
PARAMS is the optional method args and should be a hash-table.
If EVENT-NAME is non-nil, it will this event to return its value.
Starts the daemon if is not running yet."
  (unless (lsp-dart-flutter-daemon-running-p)
    (lsp-dart-flutter-daemon--start))
  (let* ((id (lsp-dart-flutter-daemon--generate-command-id))
         (command (lsp-dart-flutter-daemon--build-command id method params)))
    (setq lsp-dart-flutter-daemon-current-command (ht ("id" id)
                                                      ("callback" callback)
                                                      ("event-name" event-name)))
    (add-hook 'comint-output-filter-functions #'lsp-dart-flutter-daemon-handle-response)
    (comint-send-string (get-buffer-process lsp-dart-flutter-daemon-buffer-name) command)))

(defun lsp-dart-flutter-daemon-get-emulators (callback)
  "Call CALLBACK with the available emulators from Flutter daemon."
  (lsp-dart-flutter-daemon--send "emulator.getEmulators" callback))

(defun lsp-dart-flutter-daemon-launch (device callback)
  "Launch emulator for DEVICE and wait for connected state and call CALLBACK."
  (if lsp-dart-flutter-daemon-current-device
      (funcall callback lsp-dart-flutter-daemon-current-device)
    (-let* (((&hash "id") device)
            (params (ht ("emulatorId" id))))
      (lsp-dart-flutter-daemon--send
       "device.enable"
       (lambda ()
         (remove-hook 'comint-output-filter-functions #'lsp-dart-flutter-daemon-handle-events)
         (add-hook 'comint-output-filter-functions #'lsp-dart-flutter-daemon-handle-events)
         (lsp-dart-flutter-daemon--send "emulator.launch" callback params "device.added"))))))

;;;###autoload
(define-derived-mode lsp-dart-flutter-daemon-mode comint-mode lsp-dart-flutter-daemon-name
  "Major mode for `lsp-dart-flutter-daemon--start`."
  (setq comint-prompt-read-only nil)
  (setq comint-process-echoes nil)
  (setenv "PATH" (concat (lsp-dart-project-get-flutter-path) ":" (getenv "PATH"))))

(provide 'lsp-dart-flutter-daemon)
;;; lsp-dart-flutter-daemon.el ends here
