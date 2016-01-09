;;; cargo-process.el --- Cargo Process Major Mode

;; Copyright (C) 2015  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
;; Keywords: processes, tools

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

;; Cargo Process Major mode.
;; Used to run Cargo background processes.
;; Current supported Cargo functions:
;;  * cargo-process-bench  - Run the benchmarks.
;;  * cargo-process-build  - Compile the current project.
;;  * cargo-process-clean  - Remove the target directory.
;;  * cargo-process-doc    - Build this project's and its dependencies' documentation.
;;  * cargo-process-new    - Create a new cargo project.
;;  * cargo-process-run    - Build and execute src/main.rs.
;;  * cargo-process-search - Search registry for crates.
;;  * cargo-process-test   - Run the tests.
;;  * cargo-process-update - Update dependencies listed in Cargo.lock.
;;  * cargo-process-repeat - Run the last cargo-process command.
;;  * cargo-process-current-test         - Run the current unit test.
;;  * cargo-process-current-file-tests   -  the current file unit tests.

;;
;;; Code:

(defgroup cargo-process nil
  "Cargo Process group."
  :prefix "cargo-process-"
  :group 'cargo)

(defvar cargo-process-mode-map
  (nconc (make-sparse-keymap) compilation-mode-map)
  "Keymap for Cargo major mode.")

(defvar cargo-process-last-command nil "Command used last for repeating.")

(defface cargo-process--ok-face
  '((t (:foreground "#00ff00")))
  "Ok face"
  :group 'cargo-process)

(defface cargo-process--error-face
  '((t (:foreground "#FF0000")))
  "Error face"
  :group 'cargo-process)

(defface cargo-process--warning-face
  '((t (:foreground "#eeee00")))
  "Warning face"
  :group 'cargo-process)

(defface cargo-process--pointer-face
  '((t (:foreground "#ff00ff")))
  "Pointer face"
  :group 'cargo-process)

(defface cargo-process--standard-face
  '((t (:foreground "#ffa500")))
  "Standard face"
  :group 'cargo-process)

(defconst cargo-process-test-regexp "^[[:space:]]*fn[[:space:]]*"
  "Regex to find Rust unit test function.")

(defconst cargo-process-font-lock-keywords
  '(("error\\:" . 'cargo-process--error-face)
    ("warning\\:" . 'cargo-process--warning-face)
    ("^\s*\\^\\~*\s*$" . 'cargo-process--pointer-face)
    ("^\s*Compiling.*" . 'cargo-process--standard-face)
    ("^\s*Running.*" . 'cargo-process--standard-face)
    ("^\s*Updating.*" . 'cargo-process--standard-face)
    ("test result: FAILED." . 'cargo-process--error-face)
    ("test result: ok." . 'cargo-process--ok-face)
    ("test\s.*\sFAILED" . 'cargo-process--error-face)
    ("test\s.*\sok" . 'cargo-process--ok-face))
  "Minimal highlighting expressions for cargo-process mode.")

(define-derived-mode cargo-process-mode compilation-mode "Cargo-Process."
  "Major mode for the Cargo process buffer."
  (use-local-map cargo-process-mode-map)
  (setq major-mode 'cargo-process-mode)
  (setq mode-name "Cargo-Process")
  (setq-local truncate-lines t)
  (run-hooks 'cargo-process-mode-hook)
  (font-lock-add-keywords nil cargo-process-font-lock-keywords))

(defun cargo-process--compilation-name (mode-name)
  "Name of the Cargo Process.  MODE-NAME is unused."
  "*Cargo Process*")

(defun cargo-process--finished-sentinel (process event)
  "Execute after PROCESS return and EVENT is 'finished'."
  (when (equal event "finished\n")
    (message "Cargo Process finished.")))

(defun cargo-process--cleanup (buffer)
  "Clean up the old Cargo process BUFFER when a similar process is run."
  (when (get-buffer-process (get-buffer buffer))
    (delete-process buffer))
  (when (get-buffer buffer)
    (kill-buffer buffer)))

(defun cargo-process--activate-mode (buffer)
  "Execute commands BUFFER at process start."
  (with-current-buffer buffer
    (funcall 'cargo-process-mode)
    (setq-local window-point-insertion-type t)))

(defun cargo-process--start (name command)
  "Start the Cargo process NAME with the cargo command COMMAND."
  (let ((buffer (concat "*Cargo " name "*")))
    (setq cargo-process-last-command (list name command))
    (cargo-process--cleanup buffer)
    (compilation-start command 'cargo-process-mode 'cargo-process--compilation-name)
    (with-current-buffer "*Cargo Process*"
      (rename-buffer buffer))
    (set-process-sentinel (get-buffer-process buffer) 'cargo-process--finished-sentinel)))

(defun cargo-process--get-current-test ()
  "Return the current test."
  (let ((start (point)))
    (save-excursion
      (end-of-line)
      (unless (and (search-backward-regexp cargo-process-test-regexp nil t)
                   (save-excursion (rust-end-of-defun) (< start (point))))
        (error "Unable to find test"))
      (search-forward "fn ")
      (thing-at-point 'sexp))))

;;;###autoload
(defun cargo-process-bench ()
  "Run the Cargo bench command.
Cargo: Run the benchmarks."
  (interactive)
  (cargo-process--start "Bench" "cargo bench"))

;;;###autoload
(defun cargo-process-build ()
  "Run the Cargo build command.
Cargo: Compile the current project."
  (interactive)
  (cargo-process--start "Build" "cargo build"))

;;;###autoload
(defun cargo-process-clean ()
  "Run the Cargo clean command.
Cargo: Remove the target directory."
  (interactive)
  (cargo-process--start "Clean" "cargo clean"))

;;;###autoload
(defun cargo-process-doc ()
  "Run the Cargo doc command.
Cargo: Build this project's and its dependencies' documentation."
  (interactive)
  (cargo-process--start "Doc" "cargo doc"))

;;;###autoload
(defun cargo-process-new (name &optional bin)
  "Run the Cargo new command.
Cargo: Create a new cargo project.
NAME is the name of your application.
If BIN is t then create a binary application, otherwise a library."
  (interactive "sProject Name: ")
  (let* ((bin (when (or bin (y-or-n-p "Create Bin Project? ")) "--bin")))
    (cargo-process--start "New" (concat "cargo new " name " " bin))))

;;;###autoload
(defun cargo-process-run ()
  "Run the Cargo run command.
Cargo: Build and execute src/main.rs."
  (interactive)
  (cargo-process--start "Run" "cargo run"))

;;;###autoload
(defun cargo-process-search (search-term)
  "Run the Cargo search command.
Cargo: Search registry for crates.
SEARCH-TERM is used as the search term for the Cargo registry."
  (interactive "sSearch: ")
  (cargo-process--start "Search" (concat "cargo search " search-term)))

;;;###autoload
(defun cargo-process-test ()
  "Run the Cargo test command.
Cargo: Run the tests."
  (interactive)
  (cargo-process--start "Test" "cargo test"))

;;;###autoload
(defun cargo-process-current-test ()
  "Run the Cargo test command for the current test.
Cargo: Run the tests."
  (interactive)
  (let ((name (cargo-process--get-current-test))
        (filename (file-name-base)))
    (cargo-process--start "Test" (format "cargo test --test %s %s" filename name))))

;;;###autoload
(defun cargo-process-current-file-tests ()
  "Run the Cargo test command for the current file.
Cargo: Run the tests."
  (interactive)
  (let ((filename (file-name-base)))
    (cargo-process--start "Test" (concat "cargo test --test " filename))))

;;;###autoload
(defun cargo-process-update ()
  "Run the Cargo update command.
Cargo: Update dependencies listed in Cargo.lock."
  (interactive)
  (cargo-process--start "Update" "cargo update"))

;;;###autoload
(defun cargo-process-repeat ()
  "Run the last cargo-process command."
  (interactive)
  (if cargo-process-last-command
      (apply 'cargo-process--start cargo-process-last-command)
    (message "No last Cargo command.")))

(provide 'cargo-process)
;;; cargo-process.el ends here
