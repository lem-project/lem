
- <2026-04-24> added `lem-transient`.
  - define transients with `lem/transient:define-transient`
  - use `*transient-always-show*` to show keybindings.
  - small incompatible changes:
    - `make-keymap`'s `:name` argument was changed to `:description`.
    - so `keymap-name` is now `keymap-description`
    - *`keymaps`* is no longer relevant. we use *`root-keymap*` which maintains the full structure at any point in time.
    - keymap's :`parent` arg became :`base`, so `make-keymap` and
        `make-vi-keymap` now take :`base` instead of :`parent`.
    - `keymap-find-keybind` was renamed to `keymap-find` (and modified).
    - `other-keymaps` now serves a similar job to what `all-keymaps` did before the changes (but very different). maybe i should reconsider the rename, but the issue is that it doesnt really collect 'all' keymaps.
    - defining keys remains the same, but the internal code in `keymap.lisp` and `input.lisp` was heavily modified. the diff shows many edited files but most just correspond to renames.

[many things happened]
