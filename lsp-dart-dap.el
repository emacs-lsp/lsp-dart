;;; lsp-dart-dap.el --- DAP support for lsp-dart -*- lexical-binding: t; -*-
;;
;; Version: 1.5
;; Keywords: languages, extensions
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

(defcustom lsp-dart-dap--extension-version "3.9.1"
  "The extension version."
  :group 'lsp-dart-dap
  :type 'string)

(defcustom lsp-dart-dap--debugger-path
  (expand-file-name "github/Dart-Code.Dart-Code"
                    dap-utils-extension-path)
  "The path to dart vscode extension."
  :group 'lsp-dart-dap
  :type 'string)

(defcustom lsp-dart-dap-debugger-program
  `("node" ,(f-join lsp-dart-dap--debugger-path "extension/out/src/debug/dart_debug_entry.js"))
  "The path to the dart debugger."
  :group 'lsp-dart-dap
  :type '(repeat string))

(defcustom lsp-dart-dap-executable "dart"
  "The dart executable from dart SDK dir."
  :group 'lsp-dart-dap
  :type 'string)

(defun lsp-dart-dap--get-project-root ()
  "Return the project root path."
  (file-truename (locate-dominating-file default-directory "pubspec.yaml")))

(defun lsp-dart-dap--setup-extension ()
  "Setup dart debugger extension to run `lsp-dart-dap-debugger-program`."
  (message "DAP Dart :: Setting up...")
  (lsp-async-start-process
   (lambda ()
     (lsp-async-start-process
      (lambda () (message "DAP Dart :: Setup done!"))
      (lambda () (message "DAP Dart :: Error setting up lsp-dart-dap, check if `npm` is on $PATH"))
      (f-join lsp-dart-dap--debugger-path "extension/node_modules/typescript/bin/tsc")
      "--project" (f-join lsp-dart-dap--debugger-path "extension")))
   (lambda () (message "DAP Dart :: Error setting up lsp-dart-dap, check if `npm` is on $PATH"))
   "npm" "install" "--prefix" (f-join lsp-dart-dap--debugger-path "extension")
   "--no-package-lock" "--silent" "--no-save"))

(dap-utils-github-extension-setup-function
 "dap-dart"
 "Dart-Code"
 "Dart-Code"
 lsp-dart-dap--extension-version
 lsp-dart-dap--debugger-path
 #'lsp-dart-dap--setup-extension)

(defun lsp-dart-dap--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path lsp-dart-dap-debugger-program)
      (dap--put-if-absent :type "dart")
      (dap--put-if-absent :cwd (lsp-dart-dap--get-project-root))
      (dap--put-if-absent :program (buffer-file-name))
      (dap--put-if-absent :name "Dart Debug")
      (dap--put-if-absent :dartPath lsp-dart-dap-executable)
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

(provide 'lsp-dart-dap)
;;; lsp-dart-dap.el ends here
