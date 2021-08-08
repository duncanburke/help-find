# help-find.el

Additional help functions for working with keymaps.

This is especially useful when working with a highly customized set of keybindings. It is common both in Emacs itself and in external packages for assumptions to be made about keybinding defaults. This package provides functions to find which keymaps contain bindings for a particular key sequence or to a particular function.

For example, if you want to change your `mode-specific-command-prefix` from `C-c` to something else, you can use `help-find-keybinding` to find all keymaps which bind a prefix to `C-c` so that you can change them as well.

## Installation

### With straight.el

```emacs-lisp
(straight-use-package `(help-find :type git :repo "https://github.com/duncanburke/help-find"))
```

## Setup

Add keybindings of your choice to `help-map`. For example:

```emacs-lisp
(define-key help-map (kbd "uu") #'help-find-function)
(define-key help-map (kbd "ui") #'help-find-keybinding)
```

## Functions

### `help-find-keybinding`

Display all keymaps containing a binding for the key sequence, in `kbd` format. This searches all
keymaps in the global `obarray`.  Note that this will also find prefix keymaps, as there is no way
for it to know which keymaps will result in top-level bindings.

### `help-find-function`

Display all keymaps containing a binding to the function.
This searches all keymaps in the global `obarray`.
