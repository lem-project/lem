
- <2026-11-18> ncurses color fixes:
  - fix crash on 17-255-color terminals (e.g. rxvt-unicode-88color): populate the rxvt/xterm 88-color grid and bound palette scans by the filled prefix.
  - direct-color mode paints cleared regions with the theme background (setcchar/wbkgrndset — the chtype path cannot carry pairs above 255). Without a theme background, pair 0 keeps the terminal default, so background-transparent terminals stay transparent.
  - init-pair owns the pair bound and caches pair 0 on exhaustion/ERR; get-color-rgb degrades to the terminal default before term-init; term-set-color bounds its write.
- <2026-11-14> ncurses: attribute caches are generation-tagged and invalidated when color pair numbers are renumbered (direct<->non-direct switches); palette slots are handed out monotonically — theme loads no longer reset the OSC-4 allocator, so a mid-redisplay reset can never invalidate slot numbers that earlier lookups hold; get-color-pair returns the raw pair number, transported via wattr_set (valid above 255).
- <2026-11-12> ncurses: fix default-pair color clobber on theme load (the background allocation no longer clobbers the foreground's slot); the palette is reset only when the OSC-4 allocator is exhausted, not on every theme load; wattr_set FFI fusion — pair and attributes applied in a single call.
- <2026-11-11> ncurses: lem no longer overwrites the terminal's color palette at startup — the user's scheme colors show through and are restored on exit (OSC 104); theme colors render exactly on 256-color redefinable terminals via OSC 4 (init_color), with CIELAB nearest-match quantization as fallback; direct-color (truecolor) support for terminals with the terminfo "RGB" capability.
- <2026-09-23> directory mode: added `reverse` key, sort with `sb-unicode` (for uppercase and accents)
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
