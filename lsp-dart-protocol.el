;;; lsp-dart-protocol.el --- lsp-dart custom protocol definitions -*- lexical-binding: t; -*-
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
;;  lsp-dart custom protocol definitions
;;
;;; Code:

(require 'lsp-protocol)

(lsp-interface
 (AnalyzerStatusNotification (:isAnalyzing) nil))

(lsp-interface
 (ClosingLabelsNotification (:uri :labels) nil)
 (ClosingLabel (:label :range) nil))

(lsp-interface
 (OutlineNotification (:uri :outline) nil)
 (Outline (:element :range :codeRange :children) nil)
 (Element (:name :range :kind) (:parameters :typeParameters :returnType)))

(lsp-interface
 (FlutterOutlineNotification (:uri :outline) nil)
 (FlutterOutline (:range :codeRange :children :kind) (:dartElement :label :className :variableName :attributes))
 (FlutterOutlineAttribute (:name :label) nil))

(lsp-interface
 (FlutterWidgetGuide ()))

(lsp-interface
 (FlutterDaemonCommand (:id :method) (:params))
 (FlutterDaemonResponse (:id) (:result :event :params))
 (FlutterDaemonResponseParams nil (:level :message))
 (FlutterDaemonDevice (:id :name :platform :category :platformType :ephemeral :emulator) (:isDevice))
 (FlutterDaemonEmulatorLaunch (:emulatorId)))

(provide 'lsp-dart-protocol)
;;; lsp-dart-protocol.el ends here
