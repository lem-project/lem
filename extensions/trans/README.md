# lem-trans

Translate text inside Lem with [translate-shell](https://github.com/soimort/translate-shell)
(`trans`). Results are shown in a popup next to the cursor, and translation
runs in the background so the editor stays responsive.

## Requirements

Install translate-shell and make sure `trans` is on `PATH`:

```sh
# Debian/Ubuntu
sudo apt install translate-shell

# macOS
brew install translate-shell
```

This extension is part of the default Lem build, so no extra installation is
needed.

## Commands

| Command | Description |
|---------|-------------|
| `trans-translate-region` | Translate the active region. |
| `C-u trans-translate-region` | Replace the active region with its translation. |
| `trans-translate-symbol` | Translate the symbol at point. |
| `trans-translate-string` | Prompt for text and translate it. |
| `trans-set-target-language` | Change the target language for this session. |

The default target language is English (`en`).

## Configuration

```lisp
;; Translate into Chinese by default.
(setf lem-trans:*trans-target-language* "zh")

;; Fix the source language instead of auto-detecting it.
(setf lem-trans:*trans-source-language* "en")

;; Pass extra options to trans (for example an engine or a proxy).
(setf lem-trans:*trans-extra-arguments*
      '("-b" "-no-ansi" "-no-theme" "-x" "127.0.0.1:7890"))

;; Keep the popup until the next message, or change the timeout in seconds.
(setf lem-trans:*trans-popup-timeout* nil)
(setf lem-trans:*trans-popup-timeout* 10)

;; Change where the popup appears (any style accepted by `show-message`).
(setf lem-trans:*trans-popup-style* '(:gravity :cursor))
```

## Suggested key bindings

```lisp
(define-key *global-keymap* "C-c t" 'lem-trans:trans-translate-region)
(define-key *global-keymap* "C-c T" 'lem-trans:trans-translate-symbol)
```
