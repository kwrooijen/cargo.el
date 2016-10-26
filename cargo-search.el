;;; cargo-search --- search and add cargo dependencies

;;; Commentary:

;; Provides an interactive tabulated list interface to cargo
;; search results. Dependencies can be added to Cargo.toml
;; by selecting items from list.

;; Only two functions need be bound:
;; Main entry point is just `cargo-search-crates', and
;; in `cargo-search-mode', `cargo-search-add-crate' is
;; by default bound to return.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;;###autoload
(defun cargo-search-crates (query)
  "Start 'cargo search QUERY' process in the background.  When it 
completes, the result is converted into a tabulated list in 
`cargo-search-mode' and that buffer is brought into focus."
  (interactive (list (read-string "Search for: ")))
  (with-current-buffer (get-buffer-create cargo-search-buffer)
    (let ((inhibit-read-only t)) (erase-buffer)))
  (set-process-sentinel
   (start-process "cargo-search" cargo-search-buffer "cargo"
                  "search" query)
   'cargo-search-process-result))

;; ------------------------------------------------------------

;; process buffer for cargo search
(defvar cargo-search-buffer "*cargo-search*")

;; regexp to match results
(defvar cargo-search-re
  "\\(^[a-zA-Z][^ ]+\\)\\s-*(\\([.0-9]+\\))\\s-*\\([^\n]*\\)")

;; structure to hold crate info
(cl-defstruct (cargo-search-crate
               (:constructor cargo-search-crate--create))
  name version desc)

;; create entries for crates in table
(defun cargo-search--table-entry (item)
  (cl-destructuring-bind (name . crate) item
    (list name (vector (cargo-search-crate-name crate)
                       (cargo-search-crate-version crate)
                       (cargo-search-crate-desc crate)))))

(defun cargo-search--table-entries (crates)
  (nreverse (mapcar 'cargo-search--table-entry crates)))

;; munge cargo search results
(defun cargo-search-process-result (p m)
  (when (zerop (process-exit-status p))
    (let ((inhibit-read-only t)
          crates)
      (with-current-buffer cargo-search-buffer
        (goto-char (point-min))
        (while (not (looking-at-p "^[^ \t]"))
          (forward-line 1))
        ;; make crates
        (while (looking-at cargo-search-re)
          (push (cons (match-string 1)
                      (cargo-search-crate--create
                       :name (match-string 1)
                       :version (match-string 2)
                       :desc (match-string 3)))
                crates)
          (forward-line 1))
        ;; create tabulated list interface
        (setq tabulated-list-entries (cargo-search--table-entries crates))
        (erase-buffer)
        (cargo-search-mode)
        (tabulated-list-print)
        (pop-to-buffer (current-buffer))))))

;;; Cargo search mode functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; last known project root
(defvar cargo-search-project-root nil)

;; find project's Cargo.toml, or prompt if unable
(defun cargo-search-root ()
  (if-let ((root (locate-dominating-file
                    (or default-directory buffer-file-name)
                    "Cargo.toml")))
      (expand-file-name "Cargo.toml" (setq cargo-search-project-root
                                           root))
    (let ((default-directory
            (read-directory-name
             (format "Can't find Cargo.toml, %s: "
                     (if cargo-search-project-root
                         "use previous or reselect"
                       "choose directory"))
             (or cargo-search-project-root default-directory))))
      (cargo-search-root))))

;; check if dependency is already listed, return version
;; and line position if so
(defun cargo-search-check-dep (name)
  (goto-char (point-min))
  (when (search-forward name nil t)
    (and (looking-at ".*\"\\([0-9.]+\\)\"")
         (cons (match-string 1) (line-beginning-position)))))

;; open Cargo.toml, insert dependency
(defun cargo-search-add-crate (crate)
  (interactive (list (ignore-errors (tabulated-list-get-entry))))
  (unless crate (user-error "No crate at point."))
  ;; flash selected line
  (cargo-search-blink-region (line-beginning-position)
                             (line-end-position))
  (when-let ((toml (cargo-search-root))
             (do-it t))
    (cl-destructuring-bind (name version desc) (append crate ())
      (and (y-or-n-p (format "Add dependency for %s %s? " name version))
           (with-current-buffer (find-file-noselect toml)
             (when-let ((ver (cargo-search-check-dep name)))
               (setq do-it
                     (y-or-n-p
                      (format "%s was found with version %s. Overwrite? "
                              name (car ver))))
               (when do-it
                 ;; remove offending line
                 (goto-char (cdr ver))
                 (delete-region (1- (point)) (line-end-position))))
             (if (not do-it)
                 (message "No dependency added.")
               ;; find location to insert new crate
               (goto-char (point-min))
               (unless (search-forward "[dependencies]" nil t)
                 (goto-char (point-max))
                 (insert "\n[dependencies]"))
               ;; insert dependency
               (insert (format "\n%s = \"%s\"" name version))))))))

;;; Indicator: blink crate when adding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cargo-search-blink-delay 0.4)

(defvar cargo-search-current-region-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face 'highlight)
    overlay))

(defun cargo-search-blink-region (start end)
  (move-overlay cargo-search-current-region-overlay start end)
  (run-with-timer cargo-search-blink-delay nil
                  #'(lambda ()
                      (delete-overlay
                       cargo-search-current-region-overlay))))

;;; Cargo search mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cargo-search-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "RET") 'cargo-search-add-crate)
    km))

(define-derived-mode cargo-search-mode tabulated-list-mode
  "Cargo Search"
  "Crates found by cargo search.\n
Commands:\n
\\{cargo-search-mode-map}"
  (setq tabulated-list-format [("name" 15 nil)
                               ("version" 7 nil)
                               ("description" 60 nil)])
  (tabulated-list-init-header))

(provide 'cargo-search)
;;; cargo-search.el ends here
