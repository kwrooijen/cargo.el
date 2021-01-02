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
;; Current supported Cargo Key Combinations:
;;  * C-c d e - cargo-process-bench
;;  * C-c d b - cargo-process-build
;;  * C-c d l - cargo-process-clean
;;  * C-c d d - cargo-process-doc
;;  * C-c d v - cargo-process-doc-open
;;  * C-c d n - cargo-process-new
;;  * C-c d i - cargo-process-init
;;  * C-c d r - cargo-process-run
;;  * C-c d x - cargo-process-run-example
;;  * C-c d s - cargo-process-search
;;  * C-c d t - cargo-process-test
;;  * C-c d u - cargo-process-update
;;  * C-c d c - cargo-process-repeat
;;  * C-c d f - cargo-process-current-test
;;  * C-c d o - cargo-process-current-file-tests
;;  * C-c d q - cargo-process-outdated
;;  * C-c d m - cargo-process-fmt
;;  * C-c d k - cargo-process-check
;;  * C-c d z - cargo-process-clippy
;;  * C-c d a - cargo-process-add
;;  * C-c d w - cargo-process-rm
;;  * C-c d g - cargo-process-upgrade
;;  * C-c d y - cargo-process-audit

;;
;;; Code:

(require 'cargo-process)

(defgroup cargo nil
  "Cargo group."
  :prefix "cargo-"
  :group 'tools)

(defvar cargo-minor-mode-map (make-keymap) "Cargo-mode keymap.")
(defvar cargo-minor-mode nil)

;;;###autoload
(define-minor-mode cargo-minor-mode
  "Cargo minor mode. Used to hold keybindings for cargo-mode.

\\{cargo-minor-mode-map}"
  nil " cargo" cargo-minor-mode-map)

(define-key cargo-minor-mode-map (kbd "C-c d e") 'cargo-process-bench)
(define-key cargo-minor-mode-map (kbd "C-c d b") 'cargo-process-build)
(define-key cargo-minor-mode-map (kbd "C-c d l") 'cargo-process-clean)
(define-key cargo-minor-mode-map (kbd "C-c d d") 'cargo-process-doc)
(define-key cargo-minor-mode-map (kbd "C-c d v") 'cargo-process-doc-open)
(define-key cargo-minor-mode-map (kbd "C-c d n") 'cargo-process-new)
(define-key cargo-minor-mode-map (kbd "C-c d i") 'cargo-process-init)
(define-key cargo-minor-mode-map (kbd "C-c d r") 'cargo-process-run)
(define-key cargo-minor-mode-map (kbd "C-c d x") 'cargo-process-run-example)
(define-key cargo-minor-mode-map (kbd "C-c d s") 'cargo-process-search)
(define-key cargo-minor-mode-map (kbd "C-c d t") 'cargo-process-test)
(define-key cargo-minor-mode-map (kbd "C-c d u") 'cargo-process-update)
(define-key cargo-minor-mode-map (kbd "C-c d c") 'cargo-process-repeat)
(define-key cargo-minor-mode-map (kbd "C-c d f") 'cargo-process-current-test)
(define-key cargo-minor-mode-map (kbd "C-c d o") 'cargo-process-current-file-tests)
(define-key cargo-minor-mode-map (kbd "C-c d q") 'cargo-process-outdated)
(define-key cargo-minor-mode-map (kbd "C-c d m") 'cargo-process-fmt)
(define-key cargo-minor-mode-map (kbd "C-c d k") 'cargo-process-check)
(define-key cargo-minor-mode-map (kbd "C-c d z") 'cargo-process-clippy)
(define-key cargo-minor-mode-map (kbd "C-c d a") 'cargo-process-add)
(define-key cargo-minor-mode-map (kbd "C-c d w") 'cargo-process-rm)
(define-key cargo-minor-mode-map (kbd "C-c d g") 'cargo-process-upgrade)
(define-key cargo-minor-mode-map (kbd "C-c d y") 'cargo-process-audit)

(provide 'cargo)
;;; cargo.el ends here
