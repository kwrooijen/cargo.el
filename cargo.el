;;; cargo.el --- Emacs Minor Mode for Cargo, Rust's Package Manager.

;; Copyright (C) 2021  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen
;; Version  : 0.4.0
;; Keywords: tools
;; Package-Requires: ((emacs "24.3") (markdown-mode "2.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Cargo Minor mode.
;; Provides a number of key combinations and functions for managing Cargo.
;; Current supported Cargo Key Combinations:
;;  * C-c C-c C-e - cargo-process-bench
;;  * C-c C-c C-b - cargo-process-build
;;  * C-c C-c C-l - cargo-process-clean
;;  * C-c C-c C-d - cargo-process-doc
;;  * C-c C-c C-v - cargo-process-doc-open
;;  * C-c C-c C-n - cargo-process-new
;;  * C-c C-c C-i - cargo-process-init
;;  * C-c C-c C-r - cargo-process-run
;;  * C-c C-c C-x - cargo-process-run-example
;;  * C-c C-c C-s - cargo-process-search
;;  * C-c C-c C-t - cargo-process-test
;;  * C-c C-c C-u - cargo-process-update
;;  * C-c C-c C-c - cargo-process-repeat
;;  * C-c C-c C-f - cargo-process-current-test
;;  * C-c C-c C-o - cargo-process-current-file-tests
;;  * C-c C-c C-O - cargo-process-outdated
;;  * C-c C-c C-m - cargo-process-fmt
;;  * C-c C-c C-k - cargo-process-check
;;  * C-c C-c C-K - cargo-process-clippy
;;  * C-c C-c C-a - cargo-process-add
;;  * C-c C-c C-D - cargo-process-rm
;;  * C-c C-c C-U - cargo-process-upgrade
;;  * C-c C-c C-A - cargo-process-audit

;;
;;; Code:

(require 'cargo-process)

(defgroup cargo nil
  "Cargo group."
  :prefix "cargo-"
  :group 'tools)

(defvar cargo-minor-mode-command-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-e") 'cargo-process-bench)
    (define-key km (kbd "C-b") 'cargo-process-build)
    (define-key km (kbd "C-l") 'cargo-process-clean)
    (define-key km (kbd "C-d") 'cargo-process-doc)
    (define-key km (kbd "C-v") 'cargo-process-doc-open)
    (define-key km (kbd "C-n") 'cargo-process-new)
    (define-key km (kbd "C-i") 'cargo-process-init)
    (define-key km (kbd "C-r") 'cargo-process-run)
    (define-key km (kbd "C-x") 'cargo-process-run-example)
    (define-key km (kbd "C-s") 'cargo-process-search)
    (define-key km (kbd "C-t") 'cargo-process-test)
    (define-key km (kbd "C-u") 'cargo-process-update)
    (define-key km (kbd "C-c") 'cargo-process-repeat)
    (define-key km (kbd "C-f") 'cargo-process-current-test)
    (define-key km (kbd "C-o") 'cargo-process-current-file-tests)
    (define-key km (kbd "C-S-o") 'cargo-process-outdated)
    (define-key km (kbd "C-m") 'cargo-process-fmt)
    (define-key km (kbd "C-k") 'cargo-process-check)
    (define-key km (kbd "C-S-k") 'cargo-process-clippy)
    (define-key km (kbd "C-a") 'cargo-process-add)
    (define-key km (kbd "C-S-d") 'cargo-process-rm)
    (define-key km (kbd "C-S-u") 'cargo-process-upgrade)
    (define-key km (kbd "C-S-a") 'cargo-process-audit)
    km)
  "Keymap for Cargo mode commands after prefix.")
(fset 'cargo-minor-mode-command-map cargo-minor-mode-command-map)

(defvar cargo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'cargo-minor-mode-command-map)
    map)
  "Keymap for Cargo mode.")

(defvar cargo-minor-mode nil)

;;;###autoload
(define-minor-mode cargo-minor-mode
  "Cargo minor mode. Used to hold keybindings for cargo-mode.

\\{cargo-minor-mode-command-map}"
  :init-value nil
  :lighter " cargo"
  :keymap cargo-mode-map)

(provide 'cargo)
;;; cargo.el ends here
