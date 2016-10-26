;;; cargo.el --- Emacs Minor Mode for Cargo, Rust's Package Manager.

;; Copyright (C) 2015  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
;; Version  : 0.3.0
;; Keywords: tools
;; Package-Requires: ((emacs "24.3") (rust-mode "0.2.0"))

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
;;  * C-c C-c C-m - cargo-process-fmt
;;  * C-c C-c C-k - cargo-process-check
;;  * C-c C-c C-K - cargo-process-clippy

;;
;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'cargo-process)

(defgroup cargo nil
  "Cargo group."
  :prefix "cargo-"
  :group 'tools)

(defcustom cargo-prefix-key "C-c C-c"
  "Prefix key for `cargo-minor-mode-map'."
  :group 'cargo
  :type 'string)

(defvar cargo-bindings
  '(("C-e" . cargo-process-bench)
    ("C-b" . cargo-process-build)
    ("C-l" . cargo-process-clean)
    ("C-d" . cargo-process-doc)
    ("C-n" . cargo-process-new)
    ("C-i" . cargo-process-init)
    ("C-r" . cargo-process-run)
    ("C-x" . cargo-process-run-example)
    ("C-s" . cargo-process-search)
    ("C-t" . cargo-process-test)
    ("C-u" . cargo-process-update)
    ("C-c" . cargo-process-repeat)
    ("C-f" . cargo-process-current-test)
    ("C-o" . cargo-process-current-file-tests)
    ("C-m" . cargo-process-fmt)
    ("C-k" . cargo-process-check)
    ("C-S-k" . cargo-process-clippy)))

;; add bindings to keymap / create menu
(defun cargo--populate-menu (bindings)
  (let ((cargo-menu
         `("cargo"
          ,@(cl-loop for (key . fn) in bindings
               collect (vector (symbol-name fn) fn t)))))
    (define-key cargo-minor-mode-map (kbd cargo-prefix-key) nil)
    (cl-loop for (key . fn) in bindings
       do (define-key cargo-minor-mode-map
            (kbd (concat cargo-prefix-key " " key)) fn))
    (easy-menu-define nil cargo-minor-mode-map nil cargo-menu)))

(defvar cargo-minor-mode-map (make-sparse-keymap) "Cargo-mode keymap.")

;;;###autoload
(define-minor-mode cargo-minor-mode
  "Cargo minor mode. Used to hold keybindings for cargo-mode"
  nil " cargo" cargo-minor-mode-map
  (cargo--populate-menu cargo-bindings))

(provide 'cargo)
;;; cargo.el ends here
