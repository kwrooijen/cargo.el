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
 <kbd>C-c C-c C-i</kbd> | cargo-process-init
 <kbd>C-c C-c C-r</kbd> | cargo-process-run
 <kbd>C-c C-c C-x</kbd> | cargo-process-run-example
 <kbd>C-c C-c C-s</kbd> | cargo-process-search
 <kbd>C-c C-c C-t</kbd> | cargo-process-test
 <kbd>C-c C-c C-u</kbd> | cargo-process-update
 <kbd>C-c C-c C-c</kbd> | cargo-process-repeat
 <kbd>C-c C-c C-f</kbd> | cargo-process-current-test
 <kbd>C-c C-c C-o</kbd> | cargo-process-current-file-tests
 <kbd>C-c C-c C-m</kbd> | cargo-process-fmt
 <kbd>C-c C-c C-k</kbd> | cargo-process-check
 <kbd>C-c C-c C-K</kbd> | cargo-process-clippy

Before executing the task, Emacs will prompt you to save any modified buffers
associated with the current Cargo project. Setting `compilation-ask-about-save`
to `nil` makes Emacs save modified buffers without asking.


## Notes

In order to run `cargo-process-fmt` you need to have the `rustfmt` package installed.

```
cargo install rustfmt
```

In order to run `cargo-process-check` you need to have the `cargo-check` package installed.

```
cargo install cargo-check
```

In order to run `cargo-process-clippy` you need to have the `clippy` package installed.

```
cargo install clippy
```
