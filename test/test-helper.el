;;; test-helper.el --- Helpers for tests

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

;; Helpers for tests

;;; Code:

(require 'f)

(add-to-list 'load-path
             (file-name-as-directory (f-parent (f-parent (f-this-file)))))

(defun lsp-dart-test-package-version (file)
  "Return the version header from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward ";; Version: \\([0-9]+.[0-9]+.[0-9]+\\)" nil t)
    (match-string 1)))

(require 'el-mock)
(require 'ht)

;;; test-helper.el ends here
