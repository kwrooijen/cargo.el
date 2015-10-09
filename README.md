# cargo.el

Cargo mode for Emacs. This package gives you a set of key combinations to perform Cargo tasks within your Rust projects.

## Installation

This package can be installed through [melpa](http://melpa.milkbox.net/):

    M-x package-install cargo

## Usage

Add cargo-minor-mode to your rust-mode-hook

    (add-hook 'rust-mode-hook 'cargo-minor-mode)

You will now have the following key combinations at your disposal:

    C-c C-c C-e - cargo-process-bench
    C-c C-c C-b - cargo-process-build
    C-c C-c C-l - cargo-process-clean
    C-c C-c C-d - cargo-process-doc
    C-c C-c C-n - cargo-process-new
    C-c C-c C-r - cargo-process-run
    C-c C-c C-s - cargo-process-search
    C-c C-c C-t - cargo-process-test
    C-c C-c C-u - cargo-process-update
    C-c C-c C-c - cargo-process-repeat
