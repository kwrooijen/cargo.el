# cargo.el

Cargo mode for Emacs. This package gives you a set of key combinations to perform Cargo tasks within your Rust projects.

## Installation

This package can be installed through [melpa](https://melpa.org/):

```
M-x package-install cargo
```

## Usage

Add cargo-minor-mode to your rust-mode-hook

```el
(add-hook 'rust-mode-hook 'cargo-minor-mode)
```

You will now have the following key combinations at your disposal:

 Keybinding             | Command
------------------------|----------------------
 <kbd>C-c C-c C-e</kbd> | cargo-process-bench
 <kbd>C-c C-c C-b</kbd> | cargo-process-build
 <kbd>C-c C-c C-l</kbd> | cargo-process-clean
 <kbd>C-c C-c C-d</kbd> | cargo-process-doc
 <kbd>C-c C-c C-n</kbd> | cargo-process-new
 <kbd>C-c C-c C-r</kbd> | cargo-process-run
 <kbd>C-c C-c C-s</kbd> | cargo-process-search
 <kbd>C-c C-c C-t</kbd> | cargo-process-test
 <kbd>C-c C-c C-u</kbd> | cargo-process-update
 <kbd>C-c C-c C-c</kbd> | cargo-process-repeat
 <kbd>C-c C-c C-f</kbd> | cargo-process-current-test
 <kbd>C-c C-c C-o</kbd> | cargo-process-current-file-tests
