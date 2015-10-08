# cargo.el

Cargo mode for Emacs. This package gives you a set of key combinations to perform Cargo tasks within your Rust projects.

## Usage

First make sure to require cargo.el

    (require 'cargo)

Then add it to your rust-mode-hook

    (add-hook 'rust-mode-hook 'cargo-mode)

You will now have the following key combinations at your disposal:

    C-c C-c C-e - cargo-process-bench
    C-c C-c C-b - cargo-process-build
    C-c C-c C-c - cargo-process-clean
    C-c C-c C-d - cargo-process-doc
    C-c C-c C-n - cargo-process-new
    C-c C-c C-r - cargo-process-run
    C-c C-c C-s - cargo-process-search
    C-c C-c C-t - cargo-process-test
    C-c C-c C-u - cargo-process-update
