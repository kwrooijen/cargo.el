;;; cargo-process.el --- Cargo Process Major Mode -*-lexical-binding: t-*-

;; Copyright (C) 2021  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen
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
;;  * cargo-process-bench              - Run the benchmarks.
;;  * cargo-process-build              - Compile the current project.
;;  * cargo-process-clean              - Remove the target directory.
;;  * cargo-process-doc                - Build this project's and its dependencies' documentation.
;;  * cargo-process-doc-open           - Open this project's documentation.
;;  * cargo-process-new                - Create a new cargo project.
;;  * cargo-process-init               - Create a new cargo project inside an existing directory.
;;  * cargo-process-run                - Build and execute src/main.rs.
;;  * cargo-process-run-example        - Build and execute with --example <name>.
;;  * cargo-process-run-bin            - Build and execute a specific binary.
;;  * cargo-process-search             - Search registry for crates.
;;  * cargo-process-test               - Run all unit tests.
;;  * cargo-process-update             - Update dependencies listed in Cargo.lock.
;;  * cargo-process-repeat             - Run the last cargo-process command.
;;  * cargo-process-current-test       - Run the current unit test.
;;  * cargo-process-current-file-tests - Run the current file unit tests.
;;  * cargo-process-fmt                - Run the optional cargo command fmt.
;;  * cargo-process-check              - Run the optional cargo command check.
;;  * cargo-process-clippy             - Run the optional cargo command clippy.
;;  * cargo-process-add                - Run the optional cargo command add.
;;  * cargo-process-rm                 - Run the optional cargo command rm.
;;  * cargo-process-upgrade            - Run the optional cargo command upgrade.
;;  * cargo-process-outdated           - Run the optional cargo command outdated.
;;  * cargo-process-audit              - Run the optional cargo command audit.

;;
;;; Code:

(require 'compile)
(require 'button)
(require 'markdown-mode)
(require 'tramp)

(defgroup cargo-process nil
  "Cargo Process group."
  :prefix "cargo-process-"
  :group 'cargo)

(defcustom cargo-process--custom-path-to-bin
  (or (executable-find "cargo")
      (expand-file-name "cargo" "~/.cargo/bin")
      "/usr/local/bin/cargo")
  "Custom path to the cargo executable"
  :type 'file
  :group 'cargo-process)

(defcustom cargo-process--rustc-cmd
  (or (executable-find "rustc")
      (expand-file-name "rustc" "~/.cargo/bin")
      "/usr/local/bin/rustc")
  "Custom path to the rustc executable"
  :type 'file
  :group 'cargo-process)

(defcustom cargo-process--enable-rust-backtrace nil
  "Set RUST_BACKTRACE environment variable to 1 for tasks test and run"
  :type 'boolean
  :group 'cargo-process)

(defcustom cargo-process--command-flags ""
  "Flags to be added to every cargo command when run."
  :group 'cargo-process
  :type 'string)

(defcustom cargo-process--open-file-after-new nil
  "Open the created project file after generating a new project"
  :group 'cargo-process
  :type 'boolean)

(defvar cargo-process-mode-map
  (nconc (make-sparse-keymap) compilation-mode-map)
  "Keymap for Cargo major mode.")

(defvar cargo-process--no-manifest-commands
  '("New" "Init" "Search" "Fmt" "Audit")
  "These commands should not specify a manifest file.")

(defvar cargo-process-last-command nil "Command used last for repeating.")

(make-variable-buffer-local 'cargo-process-last-command)

(defcustom cargo-process--command-bench "bench"
  "Subcommand used by `cargo-process-bench'."
  :type 'string)

(defcustom cargo-process--command-build "build"
  "Subcommand used by `cargo-process-build'."
  :type 'string)

(defcustom cargo-process--command-clean "clean"
  "Subcommand used by `cargo-process-clean'."
  :type 'string)

(defcustom cargo-process--command-doc "doc"
  "Subcommand used by `cargo-process-doc'."
  :type 'string)

(defcustom cargo-process--command-doc-open "doc --open"
  "Subcommand used by `cargo-process-doc'."
  :type 'string)

(defcustom cargo-process--command-new "new"
  "Subcommand used by `cargo-process-new'."
  :type 'string)

(defcustom cargo-process--command-init "init"
  "Subcommand used by `cargo-process-init'."
  :type 'string)

(defcustom cargo-process--command-run "run"
  "Subcommand used by `cargo-process-run'."
  :type 'string)

(defcustom cargo-process--command-run-bin "run --bin"
  "Subcommand used by `cargo-process-run-bin'."
  :type 'string)

(defcustom cargo-process--command-run-example "run --example"
  "Subcommand used by `cargo-process-run-example'."
  :type 'string)

(defcustom cargo-process--command-search "search"
  "Subcommand used by `cargo-process-search'."
  :type 'string)

(defcustom cargo-process--command-test "test"
  "Subcommand used by `cargo-process-test'."
  :type 'string)

(defcustom cargo-process--command-current-test "test"
  "Subcommand used by `cargo-process-current-test'."
  :type 'string)

(defcustom cargo-process--command-current-file-tests "test"
  "Subcommand used by `cargo-process-current-file-tests'."
  :type 'string)

(defcustom cargo-process--command-update "update"
  "Subcommand used by `cargo-process-update'."
  :type 'string)

(defcustom cargo-process--command-fmt "fmt"
  "Subcommand used by `cargo-process-fmt'."
  :type 'string)

(defcustom cargo-process--command-outdated "outdated -R"
  "Subcommand used by `cargo-process-outdated'."
  :type 'string)

(defcustom cargo-process--command-check "check"
  "Subcommand used by `cargo-process-check'."
  :type 'string)

(defcustom cargo-process--command-clippy "clippy -Zunstable-options"
  "Subcommand used by `cargo-process-clippy'. Uses `-Zunstable-options` to work around https://github.com/rust-lang/rust-clippy/issues/4612."
  :type 'string)

(defcustom cargo-process--command-add "add"
  "Subcommand used by `cargo-process-add'."
  :type 'string)

(defcustom cargo-process--command-rm "rm"
  "Subcommand used by `cargo-process-rm'."
  :type 'string)

(defcustom cargo-process--command-upgrade "upgrade"
  "Subcommand used by `cargo-process-upgrade'."
  :type 'string)

(defcustom cargo-process--command-audit "audit -f"
  "Subcommand used by `cargo-process-audit'."
  :type 'string)

(defvar cargo-process-favorite-crates nil)

(defface cargo-process--ok-face
  '((t (:inherit success)))
  "Ok face"
  :group 'cargo-process)

(defface cargo-process--error-face
  '((t (:inherit error)))
  "Error face"
  :group 'cargo-process)

(defface cargo-process--warning-face
  '((t (:inherit warning)))
  "Warning face"
  :group 'cargo-process)

(defface cargo-process--pointer-face
  '((t (:inherit font-lock-negation-char-face)))
  "Pointer face"
  :group 'cargo-process)

(defface cargo-process--standard-face
  '((t (:inherit font-lock-comment-face)))
  "Standard face"
  :group 'cargo-process)

(defface cargo-process--errno-face
  '((t (:inherit link)))
  "Error number face"
  :group 'cargo-process)

(defconst cargo-process--rust-backtrace "RUST_BACKTRACE")

(defconst cargo-process-test-regexp "^[[:space:]]*fn[[:space:]]*"
  "Regex to find Rust unit test function.")

(defconst cargo-process-test-mod-regexp "^[[:space:]]*mod[[:space:]]+[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*[[:space:]]*{")

(defconst cargo-process-font-lock-keywords
  '(("^error\\:?" . 'cargo-process--error-face)
    ("^warning\\:?" . 'cargo-process--warning-face)
    ("^\s*\\^\\~*\s*$" . 'cargo-process--pointer-face)
    ("^\s*Compiling.*" . 'cargo-process--standard-face)
    ("^\s*Running.*" . 'cargo-process--standard-face)
    ("^\s*Updating.*" . 'cargo-process--standard-face)
    ("test result: FAILED." . 'cargo-process--error-face)
    ("test result: ok." . 'cargo-process--ok-face)
    ("test\s.*\sFAILED" . 'cargo-process--error-face)
    ("test\s.*\sok" . 'cargo-process--ok-face))
  "Minimal highlighting expressions for cargo-process mode.")

;; Bind `case-fold-search' to nil before using the regex.
(defconst cargo-process--errno-regex "\\bE[0-9]\\{4\\}\\b"
  "A regular expression to match Rust error number.")

(define-button-type 'rustc-errno
  'follow-link t
  'face 'cargo-process--errno-face
  'action #'cargo-process--explain-action)

(defun cargo-process--defun-at-point-p ()
  (string-match cargo-process-test-regexp
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))

(defun cargo-process--project-root (&optional extra)
  "Find the root of the current Cargo project."
  (let ((root (locate-dominating-file (or buffer-file-name default-directory) "Cargo.toml")))
    (when root
      (file-truename (concat root extra)))))

(define-derived-mode cargo-process-mode compilation-mode "Cargo-Process."
  "Major mode for the Cargo process buffer."
  (use-local-map cargo-process-mode-map)
  (setq major-mode 'cargo-process-mode)
  (setq mode-name "Cargo-Process")
  (setq-local truncate-lines t)
  (run-hooks 'cargo-process-mode-hook)
  (add-hook 'compilation-filter-hook #'cargo-process--add-errno-buttons)
  (font-lock-add-keywords nil cargo-process-font-lock-keywords))

(defun cargo-process--finished-sentinel (process event)
  "Execute after PROCESS return and EVENT is 'finished'."
  (compilation-sentinel process event)
  (when (equal event "finished\n")
    (message "Cargo Process finished.")))

(defun cargo-process--activate-mode (buffer)
  "Execute commands BUFFER at process start."
  (with-current-buffer buffer
    (funcall 'cargo-process-mode)
    (setq-local window-point-insertion-type t)))

(defun set-rust-backtrace (command)
  "Set RUST_BACKTRACE variable depending on the COMMAND used.
Always set to nil if cargo-process--enable-rust-backtrace is nil"
  (if (and cargo-process--enable-rust-backtrace
           (string-match "\\(test\\|run\\)" command))
      (setenv cargo-process--rust-backtrace "1")
    (setenv cargo-process--rust-backtrace nil)))

(defun cargo-json-read-from-string (text)
  "A wrapper for `json-read-from-string' with improved error reporting.

This assumes that TEXT is a human readable error, which is used
for error reporting."
  (condition-case nil
      (json-read-from-string text)
    (error (user-error text))))

(defun cargo-process--tramp-file-name-prefix (file-name)
  "Return the TRAMP prefix for FILE-NAME.
If FILE-NAME is not a TRAMP file, return an empty string."
  (if (and (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p file-name))
      (let ((tramp-file-name (tramp-dissect-file-name file-name)))
        (setf (nth 6 tramp-file-name) nil)
        (tramp-make-tramp-file-name tramp-file-name))
    ""))

(defun cargo-process--tramp-file-local-name (file-name)
  "Return the local component of FILE-NAME.
If FILE-NAME is not a TRAMP file, return it unmodified."
  (if (and (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p file-name))
      (nth 6 (tramp-dissect-file-name file-name))
    file-name))

(defun cargo-process--workspace-root ()
  "Find the workspace root using `cargo metadata`."
  (when (cargo-process--project-root)
    (let* ((metadata-text (shell-command-to-string
                           (concat (shell-quote-argument cargo-process--custom-path-to-bin)
                                   " metadata --format-version 1 --no-deps")))
           (metadata-json (cargo-json-read-from-string metadata-text))
           (tramp-prefix (cargo-process--tramp-file-name-prefix (cargo-process--project-root)))
           (workspace-root (concat tramp-prefix
                                   (cdr (assoc 'workspace_root metadata-json)))))
      workspace-root)))

(defun manifest-path-argument (name)
  (let ((manifest-filename (cargo-process--tramp-file-local-name (cargo-process--project-root "Cargo.toml"))))
    (when (and manifest-filename
               (not (member name cargo-process--no-manifest-commands)))
      (concat "--manifest-path " (shell-quote-argument manifest-filename)))))

(defun cargo-process--start (name command &optional last-cmd opens-external)
  "Start the Cargo process NAME with the cargo command COMMAND.
OPENS-EXTERNAL is non-nil if the COMMAND is expected to open an external application.
Returns the created process."
  (set-rust-backtrace command)
  (let* ((buffer (concat "*Cargo " name "*"))
         (project-root (cargo-process--project-root))
         (cmd
          (or last-cmd
              (cargo-process--maybe-read-command
               (cargo-process--augment-cmd-for-os opens-external
                                                  (mapconcat #'identity (list (shell-quote-argument cargo-process--custom-path-to-bin)
                                                                              command
                                                                              (manifest-path-argument name)
                                                                              cargo-process--command-flags)
                                                             " ")))))
         (default-directory (or project-root default-directory)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (and project-root
                              buffer-file-name
                              (string-prefix-p project-root (file-truename buffer-file-name)))))
    (setq cargo-process-last-command (list name command cmd))
    (let ((default-directory (or (cargo-process--workspace-root)
                                 default-directory)))
      (compilation-start cmd 'cargo-process-mode (lambda(_) buffer)))
    (let ((process (get-buffer-process buffer)))
      (set-process-sentinel process 'cargo-process--finished-sentinel)
      process)))

(defun cargo-process--explain-action (button)
  "Action called when the user activates Rust errno BUTTON."
  (cargo-process--explain-help (button-label button)))

(defun cargo-process--explain-help (errno)
  "Display a detailed explaination of ERRNO in a markdown buffer."
  (pop-to-buffer
   (let ((current-window (selected-window))
         (inhibit-message t))
     (with-current-buffer (get-buffer-create "*rust errno*")
       (let ((buffer-read-only nil))
         (erase-buffer)
         (insert (shell-command-to-string
                  (concat cargo-process--rustc-cmd " --explain=" errno))))
       (markdown-view-mode)
       (setq-local markdown-fontify-code-blocks-natively t)
       (setq-local markdown-fontify-code-block-default-mode 'rust-mode)
       (setq-local kill-buffer-hook (lambda ()
                                      (when (window-live-p current-window)
                                        (select-window current-window))))
       (setq
        header-line-format
        (concat (propertize " " 'display
                            `(space :align-to (- right-fringe ,(1+ (length errno)))))
                (propertize errno 'face 'error)))
       (markdown-toggle-markup-hiding 1)
       (goto-char 1)
       (current-buffer)))))

(defun cargo-process--add-errno-buttons ()
  "Turn error numbers into clickable links in Cargo process output.
Meant to be run as a `compilation-filter-hook'."
  (save-excursion
    (let ((start compilation-filter-start)
          (end (point))
          (case-fold-search nil))
      (goto-char start)
      (while (re-search-forward cargo-process--errno-regex end t)
        (make-button (match-beginning 0)
                     (match-end 0)
                     :type 'rustc-errno)))))

(defun cargo-process--get-current-test ()
  "Return the current test."
  (save-excursion
    (unless (cargo-process--defun-at-point-p)
      (cond ((fboundp 'rust-beginning-of-defun)
             (rust-beginning-of-defun))
            ((fboundp 'rustic-beginning-of-defun)
             (rustic-beginning-of-defun))
            (t (user-error "%s needs either rust-mode or rustic-mode"
                           this-command))))
    (beginning-of-line)
    (search-forward "fn ")
    (let* ((line (buffer-substring-no-properties (point)
                                                 (line-end-position)))
           (lines (split-string line "("))
           (function-name (car lines)))
      function-name)))

(defun cargo-process--get-current-mod ()
  "Return the current mod."
  (save-excursion
    (when (search-backward-regexp cargo-process-test-mod-regexp nil t)
      (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))
             (line (string-trim-left line))
             (lines (split-string line " \\|{"))
             (mod (cadr lines)))
        mod))))

(defun cargo-process--get-current-test-fullname ()
  (if (cargo-process--get-current-mod)
      (concat (cargo-process--get-current-mod)
              "::"
              (cargo-process--get-current-test))
    (cargo-process--get-current-test)))

(defun cargo-process--maybe-read-command (default)
  "Prompt to modify the DEFAULT command when the prefix argument is present.
Without the prefix argument, return DEFAULT immediately."
  (if current-prefix-arg
      (read-shell-command "Cargo command: " default)
    default))

(defun cargo-process--get-dependencies (&optional manifest)
  "Extract the list of dependencies from the
MANIFEST (i.e. Cargo.toml)."
  (with-current-buffer (find-file-noselect (or manifest
                                               (cargo-process--project-root "Cargo.toml")))
    (save-excursion
      (let ((deps-list))
        (widen)
        (goto-char (point-min))
        (search-forward "[dependencies]" nil t)
        (while (re-search-forward "^[a-zA-Z\-\_]* *=" nil t)
          (beginning-of-line)
          (setq deps-list
                (cons (thing-at-point 'sexp t) deps-list))
          (end-of-line))
        deps-list))))

(defun cargo-process--augment-cmd-for-os (opens-external cmd)
  "Augment the cargo CMD according to OS. OPENS-EXTERNAL is non-nil
if the CMD is expected to open and external application."
  (if (and opens-external
           (not (member system-type '(windows-nt ms-dos)))
           (executable-find "setsid"))
      (concat "setsid -w " cmd)
    cmd))

;;;###autoload
(defun cargo-process-bench ()
  "Run the Cargo bench command.
With the prefix argument, modify the command's invocation.
Cargo: Run the benchmarks."
  (interactive)
  (cargo-process--start "Bench" cargo-process--command-bench))

;;;###autoload
(defun cargo-process-build ()
  "Run the Cargo build command.
With the prefix argument, modify the command's invocation.
Cargo: Compile the current project."
  (interactive)
  (cargo-process--start "Build" cargo-process--command-build))

;;;###autoload
(defun cargo-process-clean ()
  "Run the Cargo clean command.
With the prefix argument, modify the command's invocation.
Cargo: Remove the target directory."
  (interactive)
  (cargo-process--start "Clean" cargo-process--command-clean))

;;;###autoload
(defun cargo-process-doc ()
  "Run the Cargo doc command.
With the prefix argument, modify the command's invocation.
Cargo: Build this project's and its dependencies' documentation."
  (interactive)
  (cargo-process--start "Doc" cargo-process--command-doc))

;;;###autoload
(defun cargo-process-doc-open ()
  "Run the Cargo doc command with the --open switch.
With the prefix argument, modify the command's invocation.
Cargo: Open this project's documentation."
  (interactive)
  (cargo-process--start "Doc" cargo-process--command-doc-open nil t))

;;;###autoload
(defun cargo-process-new (name &optional bin)
  "Run the Cargo new command.
With the prefix argument, modify the command's invocation.
NAME is the name of your application.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project."
  (interactive "sProject name: ")
  (let* ((bin (if (or bin
                      (y-or-n-p "Create Bin Project? "))
                  " --bin"
                " --lib"))
         (command
          (concat cargo-process--command-new " " name bin))
         (process
          (cargo-process--start "New" command)))
    (set-process-sentinel
     process
     (lambda (_process _event)
       (let* ((project-root (cargo-process--project-root))
              (default-directory (or project-root default-directory)))
         (cond
          ((and cargo-process--open-file-after-new
                (string= " --bin" bin))
           (find-file (format "%s/src/main.rs" name)))

          ((and cargo-process--open-file-after-new
                (string= " --lib" bin))
           (find-file (format "%s/src/lib.rs" name)))))))))

;;;###autoload
(defun cargo-process-init (directory &optional bin)
  "Run the Cargo init command.
With the prefix argument, modify the command's invocation.
DIRECTORY is the directory you want to create a cargo project in.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project in current directory.

DIRECTORY is created if necessary."
  (interactive
   (list (read-directory-name "Directory: " nil default-directory nil)))
  (let* ((bin (if (or bin (y-or-n-p "Create Bin Project? "))
                  " --bin"
                " --lib"))
         (process
          (cargo-process--start "Init" (concat cargo-process--command-init
                                               " "
                                               directory
                                               bin))))
    (set-process-sentinel
     process
     (lambda (process event)
       (cargo-process--open-manifest process
                                     event
                                     (expand-file-name "Cargo.toml" directory))))))

(defun cargo-process--open-manifest (_process event manifest-path)
  "Open the manifest file when process ends."
  (when (equal event "finished\n")
    (find-file manifest-path)))

;;;###autoload
(defun cargo-process-run ()
  "Run the Cargo run command.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute src/main.rs."
  (interactive)
  (cargo-process--start "Run" cargo-process--command-run))

;;;###autoload
(defun cargo-process-run-bin (command)
  "Run the Cargo run command --bin <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute a specific binary"
  (interactive "sBinary name: ")
  (cargo-process--start (concat "Run " command)
                        (concat cargo-process--command-run-bin " " command)))

;;;###autoload
(defun cargo-process-run-example (command)
  "Run the Cargo run command --example <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute with --example <name>."
  (interactive "sExample name: ")
  (cargo-process--start (concat "Example " command)
                        (concat cargo-process--command-run-example " " command)))

;;;###autoload
(defun cargo-process-search (search-term)
  "Run the Cargo search command.
With the prefix argument, modify the command's invocation.
SEARCH-TERM is used as the search term for the Cargo registry.
Cargo: Search registry for crates."
  (interactive
   (list (cargo-process--read-search-term)))
  (cargo-process--start "Search"
                        (concat cargo-process--command-search " " search-term)))

(defun cargo-process--read-search-term ()
  "Prompt for a search term, using the crate name at point (if
any) as the default if none is entered."
  (let* ((default-search-term (and (thing-at-point-looking-at "[[:alnum:]-_]+") (match-string-no-properties 0)))
         (default-prompt-text (if default-search-term (format " (default %s)" default-search-term) ""))
         (search-term (read-string (format "Search%s: " default-prompt-text)
                                   nil nil default-search-term)))
    search-term))

;;;###autoload
(defun cargo-process-test ()
  "Run the Cargo test command.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive)
  (cargo-process--start "Test" cargo-process--command-test))

;;;###autoload
(defun cargo-process-current-test ()
  "Run the Cargo test command for the current test.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive)
  (cargo-process--start "Test"
                        (concat cargo-process--command-current-test
                                " "
                                (cargo-process--get-current-test-fullname))))

;;;###autoload
(defun cargo-process-current-file-tests ()
  "Run the Cargo test command for the current file.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive)
  (cargo-process--start "Test" (concat cargo-process--command-current-file-tests
                                       " "
                                       (cargo-process--get-current-mod))))

;;;###autoload
(defun cargo-process-update ()
  "Run the Cargo update command.
With the prefix argument, modify the command's invocation.
Cargo: Update dependencies listed in Cargo.lock."
  (interactive)
  (cargo-process--start "Update" cargo-process--command-update))

;;;###autoload
(defun cargo-process-fmt ()
  "Run the Cargo fmt command.
With the prefix argument, modify the command's invocation.
Requires Cargo Fmt to be installed."
  (interactive)
  (cargo-process--start "Fmt" cargo-process--command-fmt))

;;;###autoload
(defun cargo-process-outdated ()
  "Run the Cargo outdated command.
With the prefix argument, modify the command's invocation.
Requires Cargo Outdated to be installed."
  (interactive)
  (cargo-process--start "Outdated" cargo-process--command-outdated))

;;;###autoload
(defun cargo-process-check ()
  "Run the Cargo check command.
With the prefix argument, modify the command's invocation.
Cargo: Check compile the current project.
Requires cargo-check to be installed."
  (interactive)
  (cargo-process--start "Check" cargo-process--command-check))

;;;###autoload
(defun cargo-process-clippy ()
  "Run the Cargo clippy command.
With the prefix argument, modify the command's invocation.
Cargo: Clippy compile the current project.
Requires Cargo clippy to be installed."
  (interactive)
  (cargo-process--start "Clippy" cargo-process--command-clippy))

;;;###autoload
(defun cargo-process-add (crate)
  "Run the Cargo add command.
With the prefix argument, modify the command's invocation.
CRATES is the name of the crate to add.
Cargo: This command allows you to add a dependency to a Cargo.toml manifest file."
  (interactive (list
                (completing-read "Crate(s) to add: " cargo-process-favorite-crates)))
  (cargo-process--start "Add" (concat cargo-process--command-add
                                      " "
                                      crate)))

;;;###autoload
(defun cargo-process-audit ()
  "Run the Cargo audit command.
With the prefix argument, modify the command's invocation.
Cargo: Audit checks the current project's Cargo.lock for security vulnerabilities.
Requires Cargo Audit to be installed."
  (interactive)
  (cargo-process--start "Audit" (concat cargo-process--command-audit
                                        " "
                                        (cargo-process--project-root "Cargo.lock"))))

;;;###autoload
(defun cargo-process-rm (crate)
  "Run the Cargo rm command.
With the prefix argument, modify the command's invocation.
CRATE is the name of the crate to remove.
Cargo: Remove a dependency from a Cargo.toml manifest file."
  (interactive
   (list
    (let ((deplist (cargo-process--get-dependencies)))
      (when deplist
        (completing-read "Crate to remove: "
                         deplist nil t)))))
  (if crate (cargo-process--start "Remove" (concat cargo-process--command-rm
                                                   " "
                                                   crate))
    (message "No crates used in current project.")))

;;;###autoload
(defun cargo-process-upgrade (&optional all crates)
  "Run the Cargo update command.
With the prefix argument, modify the command's invocation.
If ALL is t then update all crates, otherwise specify CRATES.
Cargo: Upgrade dependencies as specified in the local manifest file"
  (interactive)
  (let ((deplist (cargo-process--get-dependencies)))
    (if deplist
        (let* ((all (when (or all
                              (y-or-n-p "Upgrade all crates? "))
                      "--workspace"))
               (crates (when (not all)
                         (or crates
                             (completing-read "Crate(s) to upgrade: "
                                              deplist nil 'confirm)))))
          (cargo-process--start "Upgrade" (concat cargo-process--command-upgrade
                                                  " "
                                                  all
                                                  " "
                                                  crates)))
      (message "No crates used in current project."))))

;;;###autoload
(defun cargo-process-repeat ()
  "Run the last cargo-process command."
  (interactive)
  (if cargo-process-last-command
      (apply 'cargo-process--start cargo-process-last-command)
    (message "No last Cargo command.")))

(define-key cargo-process-mode-map (kbd "n") 'forward-button)
(define-key cargo-process-mode-map (kbd "p") 'backward-button)

(provide 'cargo-process)
;;; cargo-process.el ends here
