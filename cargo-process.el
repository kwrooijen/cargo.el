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

(require 'compile)

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

(defun cargo-process--maybe-read-command (default)
  "Prompt to modify the DEFAULT command when the prefix argument is present.
Without the prefix argument, return DEFAULT immediately."
  (if current-prefix-arg
      (read-string "Cargo command: " default)
    default))

;;;###autoload
(defun cargo-process-bench (command)
  "Run the Cargo bench COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Run the benchmarks."
  (interactive
   (list (cargo-process--maybe-read-command "cargo bench")))
  (cargo-process--start "Bench" command))

;;;###autoload
(defun cargo-process-build (command)
  "Run the Cargo build COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Compile the current project."
  (interactive
   (list (cargo-process--maybe-read-command "cargo build")))
  (cargo-process--start "Build" command))

;;;###autoload
(defun cargo-process-clean (command)
  "Run the Cargo clean COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Remove the target directory."
  (interactive
   (list (cargo-process--maybe-read-command "cargo clean")))
  (cargo-process--start "Clean" command))

;;;###autoload
(defun cargo-process-doc (command)
  "Run the Cargo doc COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Build this project's and its dependencies' documentation."
  (interactive
   (list (cargo-process--maybe-read-command "cargo doc")))
  (cargo-process--start "Doc" command))

;;;###autoload
(defun cargo-process-new (command)
  "Run the Cargo new COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Create a new cargo project."
  (interactive
   (let ((name (read-string "Project name: "))
         (bin (if (y-or-n-p "Create Bin Project? ") " --bin" "")))
     (list (cargo-process--maybe-read-command (concat "cargo new " name bin)))))
  (cargo-process--start "New" command))

;;;###autoload
(defun cargo-process-init (command)
  "Run the Cargo init COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Create a new cargo project in current directory."
  (interactive
   (let ((directory (read-directory-name "Directory: " nil default-directory t))
         (bin (if (y-or-n-p "Create Bin Project? ") " --bin" "")))
     (list (cargo-process--maybe-read-command (concat "cargo init " directory bin)))))
  (cargo-process--start "Init" command))

;;;###autoload
(defun cargo-process-run (command)
  "Run the Cargo run COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute src/main.rs."
  (interactive
   (list (cargo-process--maybe-read-command "cargo run")))
  (cargo-process--start "Run" command))

;;;###autoload
(defun cargo-process-search (command)
  "Run the Cargo search COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Search registry for crates."
  (interactive
   (let ((search-term (read-string "Search: ")))
     (list (cargo-process--maybe-read-command (concat "cargo search " search-term)))))
  (cargo-process--start "Search" command))

;;;###autoload
(defun cargo-process-test (command)
  "Run the Cargo test COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive
   (list (cargo-process--maybe-read-command "cargo test")))
  (cargo-process--start "Test" command))

;;;###autoload
(defun cargo-process-current-test (command)
  "Run the Cargo test COMMAND for the current test.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive
   (list (cargo-process--maybe-read-command
          (format "cargo test --test %s %s"
                  (file-name-base) (cargo-process--get-current-test)))))
  (cargo-process--start "Test" command))

;;;###autoload
(defun cargo-process-current-file-tests (command)
  "Run the Cargo test COMMAND for the current file.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive
   (list (cargo-process--maybe-read-command
          (concat "cargo test --test " (file-name-base)))))
  (cargo-process--start "Test" command))

;;;###autoload
(defun cargo-process-update (command)
  "Run the Cargo update COMMAND.
With the prefix argument, modify the command's invocation.
Cargo: Update dependencies listed in Cargo.lock."
  (interactive
   (list (cargo-process--maybe-read-command "cargo update")))
  (cargo-process--start "Update" command))

;;;###autoload
(defun cargo-process-repeat ()
  "Run the last cargo-process command."
  (interactive)
  (if cargo-process-last-command
      (apply 'cargo-process--start cargo-process-last-command)
    (message "No last Cargo command.")))

(provide 'cargo-process)
;;; cargo-process.el ends here
