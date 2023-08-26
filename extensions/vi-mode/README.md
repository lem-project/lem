# vi-mode

## Usage

To enable, add the following code to `~/.lem/init.lisp`:

```common-lisp
(lem-vi-mode:vi-mode)
```

## Defining keymaps

```common-lisp
;; NORMAL mode
(define-key lem-vi-mode:*normal-keymap* "q" 'quit-window)
(define-key lem-vi-mode:*normal-keymap* "Space @" 'paredit-splice)

;; INSERT mode
(define-key lem-vi-mode:*insert-keymap* "(" 'paredit-insert-paren)
(define-key lem-vi-mode:*insert-keymap* ")" 'paredit-close-parenthesis)
```

## Options

Vi-mode options are global settings, similarly to Vim.

They can be set with `:set` command, or a function `vi-option-value` in `~/.lem/init.lisp`, like:

```common-lisp
(setf (lem-vi-mode:vi-option-value "autochdir") t)
```

Here's a list of all options currently implemented:

* `autochdir`: Boolean to change the current directory to the buffer's directory automatically.
  * Default: `nil` (don't change the current directory)
  * Aliases: `acd`
* `number`: Boolean to show the line number.
  * Default: `nil` (don't show)
  * Aliases: `nu`
