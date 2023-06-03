## Move
| Command                                                                                                          | Key bindings  | Documentation                                         |
|------------------------------------------------------------------------------------------------------------------|---------------|-------------------------------------------------------|
| [next-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L62)                          | C-n, Down     | Move the cursor to next line.                         |
| [next-logical-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L69)                  |               | Move the cursor to the next logical line.             |
| [previous-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L76)                      | C-p, Up       | Move the cursor to the previous line.                 |
| [previous-logical-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L80)              |               | Move the cursor to the previous logical line.         |
| [forward-char](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L84)                       | C-f, Right    | Move the cursor to the next character.                |
| [backward-char](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L90)                      | C-b, Left     | Move the cursor to the previous character.            |
| [move-to-beginning-of-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L95)        | M-<           | Move the cursor to the beginning of the buffer.       |
| [move-to-end-of-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L100)             | M->           | Move the cursor to the end of the buffer.             |
| [move-to-beginning-of-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L105)         | C-a, Home     | Move the cursor to the beginning of the line.         |
| [move-to-beginning-of-logical-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L121) |               | Move the cursor to the beginning of the logical line. |
| [move-to-end-of-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L125)               | C-e, End      | Move the cursor to the end of the line.               |
| [move-to-end-of-logical-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L131)       |               | Move the cursor to the end of the logical line.       |
| [next-page](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L135)                         | C-v, PageDown | Move the cursor to the next page by one page.         |
| [previous-page](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L143)                     | M-v, PageUp   | Move the cursor to the previous page by one page.     |
| [next-page-char](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L151)                    | C-x ]         | Move the cursor to the next page character (^L).      |
| [previous-page-char](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L161)                | C-x [         | Move the cursor to the previous page character (^L).  |
| [goto-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/move.lisp#L165)                         | M-g           | Move the cursor to the specified line number.         |

## Edit
| Command                                                                                                   | Key bindings   | Documentation                                                                        |
|-----------------------------------------------------------------------------------------------------------|----------------|--------------------------------------------------------------------------------------|
| [self-insert](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L70)                 |                | Insert the input character.                                                          |
| [newline](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L83)                     | Return         | Insert a new line.                                                                   |
| [open-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L87)                   | C-o            | Insert a new line without moving the cursor position.                                |
| [quoted-insert](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L91)               | C-q            | Insert the next entered key (including control characters).                          |
| [delete-next-char](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L106)           | C-d, Delete    | Delete the next character.                                                           |
| [delete-previous-char](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L129)       | C-h, Backspace | Delete the previous character.                                                       |
| [copy-region](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L143)                | M-w            | Copy the text of region.                                                             |
| [copy-region-to-clipboard](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L149)   |                | Copy the selected text to the clipboard.                                             |
| [kill-region](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L161)                | C-w            | Kill the text of region.                                                             |
| [kill-region-to-clipboard](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L170)   |                | Kill the text of region and copy to the clipboard.                                   |
| [kill-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L175)                  | C-k            | Kill from the current cursor position to the end of the line.                        |
| [yank](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L205)                       | C-y            | Paste the copied text.                                                               |
| [yank-pop](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L209)                   | M-y            | Replaces the immediately pasted text with the next text in the killring.             |
| [yank-pop-next](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L222)              |                | Replaces the immediately preceding yank-pop text with the text before the kill ring. |
| [yank-to-clipboard](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L235)          |                | Copy the text of the killring to the clipboard.                                      |
| [paste-from-clipboard](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L242)       |                | Inserts text from the clipboard.                                                     |
| [entab-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L260)                 |                | Replaces the indent of the current line from space to tab.                           |
| [detab-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L266)                 |                | Replaces the indent of the current line from tab to space.                           |
| [delete-blank-lines](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L273)         | C-x C-o        | Delete blank lines before and after the cursor.                                      |
| [just-one-space](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L297)             | M-Space        | Combines consecutive whitespace before and after the cursor into one.                |
| [delete-indentation](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L303)         | M-^            | Merge the current line with the previous line.                                       |
| [transpose-characters](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L323)       | C-t            | Swaps the characters before and after the cursor.                                    |
| [undo](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L340)                       | C-\            | Undo.                                                                                |
| [redo](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L347)                       | C-_, C-/       | Redo.                                                                                |
| [delete-trailing-whitespace](https://github.com/lem-project/lem/blob/1b1f693/src/commands/edit.lisp#L374) |                | Removes all end-of-line and end-of-buffer whitespace from the current buffer.        |

## Mark
| Command                                                                                             | Key bindings | Documentation                                                  |
|-----------------------------------------------------------------------------------------------------|--------------|----------------------------------------------------------------|
| [mark-set](https://github.com/lem-project/lem/blob/1b1f693/src/commands/mark.lisp#L13)              | C-@, C-Space | Sets a mark at the current cursor position.                    |
| [exchange-point-mark](https://github.com/lem-project/lem/blob/1b1f693/src/commands/mark.lisp#L19)   | C-x C-x      | Exchange the current cursor position with the marked position. |
| [mark-set-whole-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/mark.lisp#L27) | C-x h        | Select the whole buffer as a region.                           |

## Word
| Command                                                                                             | Key bindings                    | Documentation                                             |
|-----------------------------------------------------------------------------------------------------|---------------------------------|-----------------------------------------------------------|
| [forward-word](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L82)          | M-f, C-Right                    | Move to cursor to next word.                              |
| [previous-word](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L86)         | M-b, C-Left                     | Move to cursor to previous word                           |
| [delete-word](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L90)           | M-d, C-Delete                   | Delete the next word.                                     |
| [backward-delete-word](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L104) | M-C-h, M-Backspace, C-Backspace | Delete the previous word.                                 |
| [downcase-region](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L135)      | C-x C-l                         | Replaces the selected region with a downcase.             |
| [uppercase-region](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L139)     | C-x C-u                         | Replaces the selected region with a uppercase.            |
| [capitalize-word](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L160)      | M-c                             | Replace the following word with capital-case.             |
| [lowercase-word](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L164)       | M-l                             | Replace the following word with lowercase.                |
| [uppercase-word](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L168)       | M-u                             | Replace the following word with uppercase.                |
| [forward-paragraph](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L172)    | M-}                             | Move cursor to forward paragraph.                         |
| [backward-paragraph](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L185)   | M-{                             | Move cursor to backward paragraph.                        |
| [kill-paragraph](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L189)       | M-k                             | Kill the forward paragraph.                               |
| [count-words](https://github.com/lem-project/lem/blob/1b1f693/src/commands/word.lisp#L205)          | M-=                             | Count the number of lines/words/characters in the buffer. |

## S-Expression
| Command                                                                                                | Key bindings     | Documentation                                     |
|--------------------------------------------------------------------------------------------------------|------------------|---------------------------------------------------|
| [forward-sexp](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L28)     | M-C-f            | Move the cursor to the forward expression.        |
| [backward-sexp](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L39)    | M-C-b            | Move the cursor to the backward expression.       |
| [forward-list](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L43)     | M-C-n            | Move the cursor to the forward list.              |
| [backward-list](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L47)    | M-C-p            | Move the cursor to the backward list.             |
| [down-list](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L51)        | M-C-d            | Move the cursor to the inner expression.          |
| [backward-up-list](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L55) | M-C-u            | Move the cursor to the outer expression.          |
| [mark-sexp](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L60)        | M-C-@, M-C-Space | Select the forward expression as a region.        |
| [kill-sexp](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L70)        | M-C-k            | Kill the forward expression as a region.          |
| [transpose-sexps](https://github.com/lem-project/lem/blob/1b1f693/src/commands/s-expression.lisp#L79)  | M-C-t            | Swaps the expression before and after the cursor. |

## File
| Command                                                                                            | Key bindings | Documentation                                                                                                 |
|----------------------------------------------------------------------------------------------------|--------------|---------------------------------------------------------------------------------------------------------------|
| [find-file](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L49)            | C-x C-f      | Open the file.                                                                                                |
| [read-file](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L95)            | C-x C-r      | Open the file as a read-only.                                                                                 |
| [save-current-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L133) | C-x C-s      | Saves the current buffer text to a file                                                                       |
| [write-file](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L139)          | C-x C-w      | Saves the text in the current buffer to the specified file                                                    |
| [write-region-file](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L160)   |              | Saves the region of text to the specified file                                                                |
| [insert-file](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L168)         | C-x Tab      | Inserts the contents of the file into the current buffer.                                                     |
| [save-some-buffers](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L174)   | C-x s        | Save some files in the open buffer.                                                                           |
| [revert-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L206)       |              | Restores the buffer. Normally this command will cause the contents of the file to be reflected in the buffer. |
| [change-directory](https://github.com/lem-project/lem/blob/1b1f693/src/commands/file.lisp#L240)    |              | Change directories associated with the buffer.                                                                |

## Buffer
| Command                                                                                          | Key bindings | Documentation                                 |
|--------------------------------------------------------------------------------------------------|--------------|-----------------------------------------------|
| [toggle-read-only](https://github.com/lem-project/lem/blob/1b1f693/src/commands/buffer.lisp#L14) | C-x C-q      | Toggle the buffer read-only.                  |
| [rename-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/buffer.lisp#L22)    |              | Rename the buffer.                            |
| [unmark-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/buffer.lisp#L26)    | M-~          | Remove the mark where the buffer was changed. |

## Window
| Command                                                                                                           | Key bindings   | Documentation                                                     |
|-------------------------------------------------------------------------------------------------------------------|----------------|-------------------------------------------------------------------|
| [select-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L62)                     | C-x b          | Switches to the selected buffer.                                  |
| [kill-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L100)                      | C-x k          | Delete buffer.                                                    |
| [previous-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L109)                  | C-x Left       | Switches to the previous buffer.                                  |
| [next-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L119)                      | C-x Right      | Switches to the next buffer.                                      |
| [recenter](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L125)                         | C-l            | Scroll so that the cursor is in the middle.                       |
| [split-active-window-vertically](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L132)   | C-x 2          | Split the current window vertically.                              |
| [split-active-window-horizontally](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L138) | C-x 3          | Split the current window horizontally.                            |
| [other-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L149)                     | C-x o, M-o     | Go to the next window.                                            |
| [switch-to-last-focused-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L162)    |                | Go to the window that was last in focus.                          |
| [window-move-down](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L171)                 |                | Go to the window on the down.                                     |
| [window-move-up](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L176)                   |                | Go to the window on the up.                                       |
| [window-move-right](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L181)                |                | Go to the window on the right.                                    |
| [window-move-left](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L186)                 |                | Go to the window on the left.                                     |
| [delete-other-windows](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L191)             | C-x 1          | Delete all other windows.                                         |
| [delete-active-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L204)             | C-x 0          | Delete the active window.                                         |
| [quit-active-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L209)               |                | Quit the active window. This is a command for a popped-up window. |
| [grow-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L214)                      | C-x ^          |                                                                   |
| [shrink-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L221)                    | C-x C-z        |                                                                   |
| [grow-window-horizontally](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L228)         | C-x }          |                                                                   |
| [shrink-window-horizontally](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L235)       | C-x {          |                                                                   |
| [scroll-down](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L247)                      | C-Down, M-Down | Scroll down.                                                      |
| [scroll-up](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L259)                        | C-Up, M-Up     | Scroll up.                                                        |
| [find-file-other-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L269)           | C-x 4 f        |                                                                   |
| [read-file-other-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L270)           | C-x 4 r        |                                                                   |
| [select-buffer-other-window](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L271)       | C-x 4 b        |                                                                   |
| [compare-windows](https://github.com/lem-project/lem/blob/1b1f693/src/commands/window.lisp#L273)                  |                |                                                                   |

## Multiple-Cursors
| Command                                                                                                           | Key bindings | Documentation                                               |
|-------------------------------------------------------------------------------------------------------------------|--------------|-------------------------------------------------------------|
| [add-cursors-to-next-line](https://github.com/lem-project/lem/blob/1b1f693/src/commands/multiple-cursors.lisp#L8) | M-C          | Duplicates the cursor under the currently existing cursors. |

## Process
| Command                                                                                        | Key bindings | Documentation                                                                         |
|------------------------------------------------------------------------------------------------|--------------|---------------------------------------------------------------------------------------|
| [filter-buffer](https://github.com/lem-project/lem/blob/1b1f693/src/commands/process.lisp#L10) | C-x #        | Replaces the contents of the buffer with the result of executing the command entered. |
| [pipe-command](https://github.com/lem-project/lem/blob/1b1f693/src/commands/process.lisp#L43)  | C-x @        | Run a command and displays the output.                                                |

## Help
| Command                                                                                         | Key bindings | Documentation |
|-------------------------------------------------------------------------------------------------|--------------|---------------|
| [describe-key](https://github.com/lem-project/lem/blob/1b1f693/src/commands/help.lisp#L11)      | C-x ?        |               |
| [describe-bindings](https://github.com/lem-project/lem/blob/1b1f693/src/commands/help.lisp#L39) |              |               |
| [apropos-command](https://github.com/lem-project/lem/blob/1b1f693/src/commands/help.lisp#L97)   |              |               |
| [lem-version](https://github.com/lem-project/lem/blob/1b1f693/src/commands/help.lisp#L103)      |              |               |

## Font
| Command                                                                                          | Key bindings | Documentation                                                        |
|--------------------------------------------------------------------------------------------------|--------------|----------------------------------------------------------------------|
| [font-size-increase](https://github.com/lem-project/lem/blob/1b1f693/src/commands/font.lisp#L10) | C-+          | Make the font larger (this currently only works with SDL2 frontend)  |
| [font-size-decrease](https://github.com/lem-project/lem/blob/1b1f693/src/commands/font.lisp#L14) | C--          | Make the font smaller (this currently only works with SDL2 frontend) |

## Other
| Command                                                                                          | Key bindings | Documentation                                                |
|--------------------------------------------------------------------------------------------------|--------------|--------------------------------------------------------------|
| [nop-command](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L21)       | NopKey       |                                                              |
| [undefined-key](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L24)     |              | Signal undefined key error.                                  |
| [keyboard-quit](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L28)     | C-g          | Signal a `quit` condition.                                   |
| [escape](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L32)            | Escape       | Signal a `quit` condition silently.                          |
| [exit-lem](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L36)          | C-x C-c      | Ask for modified buffers before exiting lem.                 |
| [quick-exit](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L43)        |              | Exit the lem job and kill it.                                |
| [execute-command](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L48)   | M-x          | Read a command name, then read the ARG and call the command. |
| [show-context-menu](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L67) | Shift-F10    |                                                              |
| [load-library](https://github.com/lem-project/lem/blob/1b1f693/src/commands/other.lisp#L72)      |              | Load the Lisp library named NAME.                            |

