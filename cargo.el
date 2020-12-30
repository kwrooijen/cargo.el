;;; cargo.el --- Emacs Minor Mode for Cargo, Rust's Package Manager.

;; Copyright (C) 2015  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
;; Version  : 0.4.0
;; Keywords: tools
;; Package-Requires: ((emacs "24.3") (rust-mode "0.2.0") (markdown-mode "2.4"))

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
;; Current supported Cargo Key Combinations (with default prefix C-c C-c):
;;  * C-e - cargo-process-bench
;;  * C-b - cargo-process-build
;;  * C-l - cargo-process-clean
;;  * C-d - cargo-process-doc
;;  * C-v - cargo-process-doc-open
;;  * C-n - cargo-process-new
;;  * C-i - cargo-process-init
;;  * C-r - cargo-process-run
;;  * C-x - cargo-process-run-example
;;  * C-s - cargo-process-search
;;  * C-t - cargo-process-test
;;  * C-u - cargo-process-update
;;  * C-c - cargo-process-repeat
;;  * C-f - cargo-process-current-test
;;  * C-o - cargo-process-current-file-tests
;;  * C-O - cargo-process-outdated
;;  * C-m - cargo-process-fmt
;;  * C-k - cargo-process-check
;;  * C-K - cargo-process-clippy
;;  * C-a - cargo-process-add
;;  * C-D - cargo-process-rm
;;  * C-U - cargo-process-upgrade
;;  * C-A - cargo-process-audit

;;
;;; Code:

(require 'cargo-process)

(defgroup cargo nil
  "Cargo group."
  :prefix "cargo-"
  :group 'tools)

(defvar cargo-minor-mode nil)
(defvar cargo-minor-mode-command-map
  (let ((map (make-sparse-keymap "cargo")))
    (define-key map (kbd "C-e")   #'cargo-process-bench)
    (define-key map (kbd "C-b")   #'cargo-process-build)
    (define-key map (kbd "C-l")   #'cargo-process-clean)
    (define-key map (kbd "C-d")   #'cargo-process-doc)
    (define-key map (kbd "C-v")   #'cargo-process-doc-open)
    (define-key map (kbd "C-n")   #'cargo-process-new)
    (define-key map (kbd "C-i")   #'cargo-process-init)
    (define-key map (kbd "C-r")   #'cargo-process-run)
    (define-key map (kbd "C-x")   #'cargo-process-run-example)
    (define-key map (kbd "C-s")   #'cargo-process-search)
    (define-key map (kbd "C-t")   #'cargo-process-test)
    (define-key map (kbd "C-u")   #'cargo-process-update)
    (define-key map (kbd "C-c")   #'cargo-process-repeat)
    (define-key map (kbd "C-f")   #'cargo-process-current-test)
    (define-key map (kbd "C-o")   #'cargo-process-current-file-tests)
    (define-key map (kbd "C-S-o") #'cargo-process-outdated)
    (define-key map (kbd "C-m")   #'cargo-process-fmt)
    (define-key map (kbd "C-k")   #'cargo-process-check)
    (define-key map (kbd "C-S-k") #'cargo-process-clippy)
    (define-key map (kbd "C-a")   #'cargo-process-add)
    (define-key map (kbd "C-S-d") #'cargo-process-rm)
    (define-key map (kbd "C-S-u") #'cargo-process-upgrade)
    (define-key map (kbd "C-S-a") #'cargo-process-audit)
    map)
  "Cargo-mode keymap")

(fset 'cargo-minor-mode-command-map cargo-minor-mode-command-map)

;;;###autoload
(define-minor-mode cargo-minor-mode
  "Cargo minor mode. Used to hold keybindings for cargo-mode.

\\{cargo-minor-mode-map}"
  :lighter " cargo"
  :keymap '("\C-c\C-c" . cargo-minor-mode-command-map))


(provide 'cargo)
;;; cargo.el ends here
