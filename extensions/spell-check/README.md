# lem-spell-check

Buffer-local spell checking for [Lem](https://github.com/lem-project/lem)
using [Enchant](https://rrthomas.github.io/enchant/) through
[cl-enchant](https://github.com/tlikonen/cl-enchant).

Misspelled words are underlined with a curly red underline. The buffer
is rescanned shortly after you stop typing, and `M-$` offers spelling
suggestions for the word at point.

## Requirements

The extension binds to the Enchant C library at load time, so
`libenchant-2` must be installed:

| Platform | Package |
|----------|---------|
| Arch Linux | `enchant` |
| Debian/Ubuntu | `libenchant-2-2` plus a dictionary such as `hunspell-en-us` or `aspell-en` |
| Fedora | `enchant2` plus `hunspell-en-US` |
| macOS (Homebrew) | `brew install enchant` |

At least one Enchant dictionary must be available. Check the available
languages with:

```lisp
(lem-spell-check:spell-check-available-dictionaries)
```

## Loading and enabling

The extension is not part of the default `lem/extensions` build because
that would make libenchant a hard dependency of every Lem build and of
the documentation CI. Load it explicitly from your init file:

```lisp
(lem:maybe-load-systems "lem-spell-check")
```

Then enable it per buffer:

```lisp
(spell-check-mode t)
```

or interactively with `M-x spell-check-mode`. To enable it
automatically for particular major modes:

```lisp
(add-hook *markdown-mode-hook*
          (lambda () (spell-check-mode t)))
```

## Usage

| Key / command | Description |
|---------------|-------------|
| `M-$` (`spell-check-word`) | Check the word at point and prompt for a replacement |
| `M-x spell-check-buffer` | Enable the mode if needed and check the whole buffer |
| `M-x spell-check-next-error` | Move to the next misspelled word |
| `M-x spell-check-previous-error` | Move to the previous misspelled word |
| `M-x spell-check-add-word` | Add the word at point to the personal dictionary |
| `M-x spell-check-ignore-word` | Ignore the word for this session only |
| `M-x spell-check-change-dictionary` | Choose the dictionary for this buffer |
| `M-x spell-check-clear` | Remove the highlights until the next edit |

## Configuration

All three variables are editor variables, so they can be set globally
or per buffer.

```lisp
;; Default language for all buffers.
(setf (variable-value 'lem-spell-check:spell-check-language :global) "en_GB")

;; Or per buffer.
(setf (variable-value 'lem-spell-check:spell-check-language
                      :buffer (current-buffer))
      "de_DE")
```

- `spell-check-language` (`nil` by default) — Enchant language code.
  When `nil`, the language is derived from `LC_ALL`, `LC_MESSAGES` or
  `LANG` (for example `en_US.UTF-8` becomes `en_US`), falling back to
  the first installed dictionary. An explicit language that Enchant
  does not provide raises an error.
- `spell-check-delay` (`400`) — idle delay in milliseconds before
  rescanning after an edit.
- `spell-check-min-word-length` (`2`) — words shorter than this are
  ignored.

Tokens containing digits are not checked, and words shorter than
`spell-check-min-word-length` are skipped. Apostrophes and hyphens are
kept inside words when followed by a letter or digit, so `don't` and
`well-known` are checked as single words.

## Limitations

- The whole buffer is scanned synchronously on the editor thread. This
  is fast for normal text (tens of milliseconds for thousands of
  words) but is not incremental.
- Code, URLs and markup are tokenized like prose, so identifiers and
  other non-words may be flagged. There is no per-major-mode filtering
  yet.
- Dictionaries and session-ignored words are shared per Enchant
  dictionary, so ignoring a word in one buffer affects every buffer
  using the same language.
