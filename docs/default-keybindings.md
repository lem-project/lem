## Move
| Command                                                                                                       | Key bindings  | Documentation                                                                                                                                                                                                                                                        |
|---------------------------------------------------------------------------------------------------------------|---------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [next-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L64)                          | C-n, Down     | Move the cursor to next line.
Underlying function for the `NEXT-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [next-logical-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L71)                  |               | Move the cursor to the next logical line.
Underlying function for the `NEXT-LOGICAL-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [previous-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L78)                      | C-p, Up       | Move the cursor to the previous line.
Underlying function for the `PREVIOUS-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [previous-logical-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L82)              |               | Move the cursor to the previous logical line.
Underlying function for the `PREVIOUS-LOGICAL-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [forward-char](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L86)                       | C-f, Right    | Move the cursor to the next character.
Underlying function for the `FORWARD-CHAR` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [backward-char](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L92)                      | C-b, Left     | Move the cursor to the previous character.
Underlying function for the `BACKWARD-CHAR` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [move-to-beginning-of-buffer](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L97)        | M-<, C-Home   | Move the cursor to the beginning of the buffer.
Underlying function for the `MOVE-TO-BEGINNING-OF-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [move-to-end-of-buffer](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L102)             | M->, C-End    | Move the cursor to the end of the buffer.
Underlying function for the `MOVE-TO-END-OF-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [move-to-beginning-of-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L107)         | C-a, Home     | Move the cursor to the beginning of the line.
Underlying function for the `MOVE-TO-BEGINNING-OF-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [move-to-beginning-of-logical-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L123) |               | Move the cursor to the beginning of the logical line.
Underlying function for the `MOVE-TO-BEGINNING-OF-LOGICAL-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [move-to-end-of-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L127)               | C-e, End      | Move the cursor to the end of the line.
Underlying function for the `MOVE-TO-END-OF-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [move-to-end-of-logical-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L133)       |               | Move the cursor to the end of the logical line.
Underlying function for the `MOVE-TO-END-OF-LOGICAL-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [next-page](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L137)                         | C-v, PageDown | Move the cursor to the next page by one page.
Underlying function for the `NEXT-PAGE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [previous-page](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L145)                     | M-v, PageUp   | Move the cursor to the previous page by one page.
Underlying function for the `PREVIOUS-PAGE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [next-page-char](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L153)                    | C-x ]         | Move the cursor to the next page character (^L).
Underlying function for the `NEXT-PAGE-CHAR` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [previous-page-char](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L163)                | C-x [         | Move the cursor to the previous page character (^L).
Underlying function for the `PREVIOUS-PAGE-CHAR` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |
| [goto-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L167)                         | M-g           | Move the cursor to the specified line number.
Underlying function for the `GOTO-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                |

## Edit
| Command                                                                                                | Key bindings      | Documentation                                                                                                                                                                                                                                                                         |
|--------------------------------------------------------------------------------------------------------|-------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [self-insert](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L71)                 |                   | Processes the key entered.
Underlying function for the `SELF-INSERT` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [newline](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L89)                     | Return            | Insert a new line.
Underlying function for the `NEWLINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [open-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L93)                   | C-o               | Insert a new line without moving the cursor position.
Underlying function for the `OPEN-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [quoted-insert](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L97)               | C-q               | Insert the next entered key (including control characters).
Underlying function for the `QUOTED-INSERT` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [delete-next-char](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L112)           | C-d, Delete       | Delete the next character.
Underlying function for the `DELETE-NEXT-CHAR` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [delete-previous-char](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L135)       | C-h, Backspace    | Delete the previous character.
Underlying function for the `DELETE-PREVIOUS-CHAR` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [copy-region](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L149)                | M-w               | Copy the text of region.
Underlying function for the `COPY-REGION` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [copy-region-to-clipboard](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L155)   |                   | Copy the selected text to the clipboard.
Underlying function for the `COPY-REGION-TO-CLIPBOARD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [kill-region](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L167)                | C-w               | Kill the text of region.
Underlying function for the `KILL-REGION` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [kill-region-to-clipboard](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L176)   |                   | Kill the text of region and copy to the clipboard.
Underlying function for the `KILL-REGION-TO-CLIPBOARD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [kill-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L181)                  | C-k               | Kill from the current cursor position to the end of the line.
Underlying function for the `KILL-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [kill-whole-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L213)            | C-Shift-Backspace | Kill the entire line and the remaining whitespace
Underlying function for the `KILL-WHOLE-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [yank](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L232)                       | C-y               | Paste the copied text.
Underlying function for the `YANK` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [yank-pop](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L236)                   | M-y               | Replaces the immediately pasted text with the next text in the killring.
Underlying function for the `YANK-POP` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [yank-pop-next](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L249)              |                   | Replaces the immediately preceding yank-pop text with the text before the kill ring.
Underlying function for the `YANK-POP-NEXT` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [yank-to-clipboard](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L262)          |                   | Copy the text of the killring to the clipboard.
Underlying function for the `YANK-TO-CLIPBOARD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [paste-from-clipboard](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L269)       |                   | Inserts text from the clipboard.
Underlying function for the `PASTE-FROM-CLIPBOARD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [entab-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L287)                 |                   | Replaces the indent of the current line from space to tab.
Underlying function for the `ENTAB-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [detab-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L293)                 |                   | Replaces the indent of the current line from tab to space.
Underlying function for the `DETAB-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [delete-blank-lines](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L300)         | C-x C-o           | Delete blank lines before and after the cursor.
Underlying function for the `DELETE-BLANK-LINES` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [just-one-space](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L324)             | M-Space           |                                                                                                                                                                                                                                                                                       |
| [delete-indentation](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L330)         | M-^               | Merge the current line with the previous line.
Underlying function for the `DELETE-INDENTATION` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [transpose-characters](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L350)       | C-t               | Swaps the characters before and after the cursor.
Underlying function for the `TRANSPOSE-CHARACTERS` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [undo](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L367)                       | C-\               | Undo.
Underlying function for the `UNDO` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [redo](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L374)                       | C-_, C-/          | Redo.
Underlying function for the `REDO` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |
| [delete-trailing-whitespace](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L401) |                   | Removes all end-of-line and end-of-buffer whitespace from the current buffer.
Underlying function for the `DELETE-TRAILING-WHITESPACE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                              |

## Mark
| Command                                                                                          | Key bindings | Documentation                                                                                                                                                                                                                                                   |
|--------------------------------------------------------------------------------------------------|--------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [mark-set](https://github.com/lem-project/lem/blob/main/src/commands/mark.lisp#L13)              | C-@, C-Space | Sets a mark at the current cursor position.
Underlying function for the `MARK-SET` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                             |
| [exchange-point-mark](https://github.com/lem-project/lem/blob/main/src/commands/mark.lisp#L19)   | C-x C-x      | Exchange the current cursor position with the marked position.
Underlying function for the `EXCHANGE-POINT-MARK` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                             |
| [mark-set-whole-buffer](https://github.com/lem-project/lem/blob/main/src/commands/mark.lisp#L27) | C-x h        | Select the whole buffer as a region.
Underlying function for the `MARK-SET-WHOLE-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                             |

## Word
| Command                                                                                          | Key bindings                    | Documentation                                                                                                                                                                                                                                      |
|--------------------------------------------------------------------------------------------------|---------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [forward-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L82)          | M-f, C-Right                    | Move to cursor to next word.
Underlying function for the `FORWARD-WORD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [previous-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L86)         | M-b, C-Left                     | Move to cursor to previous word
Underlying function for the `PREVIOUS-WORD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [delete-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L90)           | M-d, C-Delete                   | Delete the next word.
Underlying function for the `DELETE-WORD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [backward-delete-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L104) | M-C-h, M-Backspace, C-Backspace | Delete the previous word.
Underlying function for the `BACKWARD-DELETE-WORD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [downcase-region](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L135)      | C-x C-l                         | Replaces the selected region with a downcase.
Underlying function for the `DOWNCASE-REGION` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [uppercase-region](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L139)     | C-x C-u                         | Replaces the selected region with a uppercase.
Underlying function for the `UPPERCASE-REGION` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [capitalize-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L160)      | M-c                             | Replace the following word with capital-case.
Underlying function for the `CAPITALIZE-WORD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [lowercase-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L164)       | M-l                             | Replace the following word with lowercase.
Underlying function for the `LOWERCASE-WORD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [uppercase-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L168)       | M-u                             | Replace the following word with uppercase.
Underlying function for the `UPPERCASE-WORD` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [forward-paragraph](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L172)    | M-}                             | Move cursor to forward paragraph.
Underlying function for the `FORWARD-PARAGRAPH` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [backward-paragraph](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L185)   | M-{                             | Move cursor to backward paragraph.
Underlying function for the `BACKWARD-PARAGRAPH` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [kill-paragraph](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L189)       | M-k                             | Kill the forward paragraph.
Underlying function for the `KILL-PARAGRAPH` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [count-words](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L205)          | M-=                             | Count the number of lines/words/characters in the buffer.
Underlying function for the `COUNT-WORDS` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |

## S-Expression
| Command                                                                                            | Key bindings     | Documentation                                                                                                                                                                                                                                  |
|----------------------------------------------------------------------------------------------------|------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [forward-sexp](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L28)    | M-C-f            | Move the cursor to the forward expression.
Underlying function for the `FORWARD-SEXP` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |
| [backward-sexp](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L39)   | M-C-b            | Move the cursor to the backward expression.
Underlying function for the `BACKWARD-SEXP` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |
| [forward-list](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L43)    | M-C-n            | Move the cursor to the forward list.
Underlying function for the `FORWARD-LIST` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |
| [backward-list](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L47)   | M-C-p            | Move the cursor to the backward list.
Underlying function for the `BACKWARD-LIST` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |
| [down-list](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L51)       | M-C-d            | Move the cursor to the inner expression.
Underlying function for the `DOWN-LIST` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |
| [up-list](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L55)         | M-C-u            | Move the cursor to the outer expression.
Underlying function for the `UP-LIST` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |
| [mark-sexp](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L60)       | M-C-@, M-C-Space | Select the forward expression as a region.
Underlying function for the `MARK-SEXP` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |
| [kill-sexp](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L70)       | M-C-k            | Kill the forward expression as a region.
Underlying function for the `KILL-SEXP` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |
| [transpose-sexps](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L79) | M-C-t            | Swaps the expression before and after the cursor.
Underlying function for the `TRANSPOSE-SEXPS` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                  |

## File
| Command                                                                                           | Key bindings | Documentation                                                                                                                                                                                                                                                                                                                                                                                                          |
|---------------------------------------------------------------------------------------------------|--------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [find-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L67)              | C-x C-f      | Open the file.
Underlying function for the `FIND-FILE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [find-file-recursively](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L210) |              | Open a file, from the list of all files present under the buffer's directory, recursively.
Underlying function for the `FIND-FILE-RECURSIVELY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [read-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L226)             | C-x C-r      | Open the file as a read-only.
Underlying function for the `READ-FILE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [save-current-buffer](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L264)   | C-x C-s      | Saves the current buffer text to a file
Underlying function for the `SAVE-CURRENT-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [write-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L270)            | C-x C-w      | Saves the text in the current buffer to the specified file
Underlying function for the `WRITE-FILE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [write-region-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L291)     |              | Saves the region of text to the specified file
Underlying function for the `WRITE-REGION-FILE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [insert-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L299)           | C-x Tab      | Inserts the contents of the file into the current buffer.
Underlying function for the `INSERT-FILE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [save-some-buffers](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L305)     | C-x s        | Save some files in the open buffer.
Underlying function for the `SAVE-SOME-BUFFERS` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [revert-buffer](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L337)         |              | Restores the buffer. Normally this command will cause the contents of the file to be reflected in the buffer.
Underlying function for the `REVERT-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [change-directory](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L370)      |              |                                                                                                                                                                                                                                                                                                                                                                                                                        |
| [current-directory](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L379)     |              | Display the directory of the active buffer.
With prefix argument INSERT, insert the directory of the active buffer at point.
Underlying function for the `CURRENT-DIRECTORY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [format-current-buffer](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L387) |              | Save changes and try to format the current buffer.

Supported modes include: c-mode with clang-format, go-mode with gofmt, js-mode and json-mode with prettier, and lisp-mode. Additionally rust-mode uses rustfmt.
Underlying function for the `FORMAT-CURRENT-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |

## Project
| Command                                                                                               | Key bindings | Documentation                                                                                                                                                                                                                                                                                                                                                                                                              |
|-------------------------------------------------------------------------------------------------------|--------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [project-find-file](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L132)      | C-x p f      | Open a file, from the list of all files in this project.
Underlying function for the `PROJECT-FIND-FILE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| [project-root](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L149)           |              | Display this buffer's project directory.
Underlying function for the `PROJECT-ROOT` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| [project-root-directory](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L156) | C-x p d      | Open this project's root directory.
Underlying function for the `PROJECT-ROOT-DIRECTORY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| [project-kill-buffers](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L216)   | C-x p K      | Delete all this project's buffers, except:

  - if *delete-repl-buffer* is non t, we don't delete the REPL buffer.
  - if *delete-last-buffer* is non nil, we will delete the last buffer. This would cause Lem to exit.
Underlying function for the `PROJECT-KILL-BUFFERS` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |

## Buffer
| Command                                                                                            | Key bindings | Documentation                                                                                                                                                                                                                            |
|----------------------------------------------------------------------------------------------------|--------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [indent-current-buffer](https://github.com/lem-project/lem/blob/main/src/commands/buffer.lisp#L15) |              | Indent the current buffer.
Underlying function for the `INDENT-CURRENT-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                        |
| [toggle-read-only](https://github.com/lem-project/lem/blob/main/src/commands/buffer.lisp#L19)      | C-x C-q      | Toggle the buffer read-only.
Underlying function for the `TOGGLE-READ-ONLY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                        |
| [rename-buffer](https://github.com/lem-project/lem/blob/main/src/commands/buffer.lisp#L27)         |              | Rename the buffer.
Underlying function for the `RENAME-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                        |
| [unmark-buffer](https://github.com/lem-project/lem/blob/main/src/commands/buffer.lisp#L31)         | M-~          | Remove the mark where the buffer was changed.
Underlying function for the `UNMARK-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                        |

## Window
| Command                                                                                                        | Key bindings   | Documentation                                                                                                                                                                                                                                                            |
|----------------------------------------------------------------------------------------------------------------|----------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [select-buffer](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L66)                     | C-x b          | Switches to the selected buffer.
Underlying function for the `SELECT-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [kill-buffer](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L93)                       | C-x k          | Delete buffer.
Underlying function for the `KILL-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [previous-buffer](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L102)                  | C-x Left       | Switches to the previous buffer.
Underlying function for the `PREVIOUS-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [next-buffer](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L112)                      | C-x Right      | Switches to the next buffer.
Underlying function for the `NEXT-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [recenter](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L118)                         | C-l            | Scroll so that the cursor is in the middle.
Underlying function for the `RECENTER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [split-active-window-vertically](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L125)   | C-x 2          | Split the current window vertically.
Underlying function for the `SPLIT-ACTIVE-WINDOW-VERTICALLY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [split-active-window-horizontally](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L131) | C-x 3          | Split the current window horizontally.
Underlying function for the `SPLIT-ACTIVE-WINDOW-HORIZONTALLY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [next-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L137)                      | C-x o, M-o     | Go to the next window.
Underlying function for the `NEXT-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [previous-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L149)                  | M-O            | NIL
Underlying function for the `PREVIOUS-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [switch-to-last-focused-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L152)    |                | Go to the window that was last in focus.
Underlying function for the `SWITCH-TO-LAST-FOCUSED-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [window-move-down](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L160)                 |                | Go to the window on the down.
Underlying function for the `WINDOW-MOVE-DOWN` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [window-move-up](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L165)                   |                | Go to the window on the up.
Underlying function for the `WINDOW-MOVE-UP` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [window-move-right](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L170)                |                | Go to the window on the right.
Underlying function for the `WINDOW-MOVE-RIGHT` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [window-move-left](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L175)                 |                | Go to the window on the left.
Underlying function for the `WINDOW-MOVE-LEFT` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [delete-other-windows](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L180)             | C-x 1          | Delete all other windows.
Underlying function for the `DELETE-OTHER-WINDOWS` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [delete-active-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L193)             | C-x 0, M-q     | Delete the active window.
Underlying function for the `DELETE-ACTIVE-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [quit-active-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L198)               |                | Quit the active window. This is a command for a popped-up window.
Underlying function for the `QUIT-ACTIVE-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [grow-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L203)                      | C-x ^          | Grow the window's height.
Underlying function for the `GROW-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [shrink-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L211)                    | C-x C-z        | Shrink the window's height.
Underlying function for the `SHRINK-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [grow-window-horizontally](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L219)         | C-x }          | Grow the window's width.
Underlying function for the `GROW-WINDOW-HORIZONTALLY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [shrink-window-horizontally](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L227)       | C-x {          | Shrink the window's width.
Underlying function for the `SHRINK-WINDOW-HORIZONTALLY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [scroll-down](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L238)                      | C-Down, M-Down | Scroll down.
Underlying function for the `SCROLL-DOWN` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [scroll-up](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L252)                        | C-Up, M-Up     | Scroll up.
Underlying function for the `SCROLL-UP` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [find-file-next-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L264)            | C-x 4 f        | Open a file in another window. Split the screen vertically if needed.
Underlying function for the `FIND-FILE-NEXT-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [read-file-next-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L265)            | C-x 4 r        | Read a file in another window.
Underlying function for the `READ-FILE-NEXT-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [select-buffer-next-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L266)        | C-x 4 b        | Select a buffer in another window.
Underlying function for the `SELECT-BUFFER-NEXT-WINDOW` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |
| [compare-windows](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L270)                  |                | NIL
Underlying function for the `COMPARE-WINDOWS` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                                      |

## Multiple-Cursors
| Command                                                                                                        | Key bindings | Documentation                                                                                                                                                                                                                                                     |
|----------------------------------------------------------------------------------------------------------------|--------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [add-cursors-to-next-line](https://github.com/lem-project/lem/blob/main/src/commands/multiple-cursors.lisp#L8) | M-C          | Duplicates the cursor under the currently existing cursors.
Underlying function for the `ADD-CURSORS-TO-NEXT-LINE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                             |

## Process
| Command                                                                                     | Key bindings | Documentation                                                                                                                                                                                                                                                                    |
|---------------------------------------------------------------------------------------------|--------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [filter-buffer](https://github.com/lem-project/lem/blob/main/src/commands/process.lisp#L10) | C-x #        | Replaces the contents of the buffer with the result of executing the command entered.
Underlying function for the `FILTER-BUFFER` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                         |
| [pipe-command](https://github.com/lem-project/lem/blob/main/src/commands/process.lisp#L43)  | C-x @        | Run a command and displays the output.
Underlying function for the `PIPE-COMMAND` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                         |

## Help
| Command                                                                                      | Key bindings | Documentation                                                                                                                                                                                                                                                             |
|----------------------------------------------------------------------------------------------|--------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [describe-key](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L12)      | C-x ?        | Tell what is the command associated to a keybinding.
Underlying function for the `DESCRIBE-KEY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [describe-bindings](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L41) |              | Describe the bindings of the buffer's current major mode.
Underlying function for the `DESCRIBE-BINDINGS` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [list-modes](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L63)        |              | Output all available major and minor modes.
Underlying function for the `LIST-MODES` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [apropos-command](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L110)  |              | Find all symbols in the running Lisp image whose names match a given string.
Underlying function for the `APROPOS-COMMAND` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |
| [lem-version](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L121)      |              | Display Lem's version.
Underlying function for the `LEM-VERSION` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                                   |

## Font
| Command                                                                                       | Key bindings | Documentation                                                                                                                                                                                                                                                        |
|-----------------------------------------------------------------------------------------------|--------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [font-size-increase](https://github.com/lem-project/lem/blob/main/src/commands/font.lisp#L10) | C-+          | Make the font larger (this currently only works with SDL2 frontend)
Underlying function for the `FONT-SIZE-INCREASE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                               |
| [font-size-decrease](https://github.com/lem-project/lem/blob/main/src/commands/font.lisp#L14) | C--          | Make the font smaller (this currently only works with SDL2 frontend)
Underlying function for the `FONT-SIZE-DECREASE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                               |

## Other
| Command                                                                                       | Key bindings   | Documentation                                                                                                                                                                                                                                             |
|-----------------------------------------------------------------------------------------------|----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [nop-command](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L22)       | NopKey         | NIL
Underlying function for the `NOP-COMMAND` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |
| [undefined-key](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L25)     |                | Signal undefined key error.
Underlying function for the `UNDEFINED-KEY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |
| [keyboard-quit](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L29)     | C-g            | Signal a `quit` condition.
Underlying function for the `KEYBOARD-QUIT` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |
| [escape](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L33)            | Escape         | Signal a `quit` condition silently.
Underlying function for the `ESCAPE` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |
| [exit-lem](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L37)          | C-x C-c        | Ask for modified buffers before exiting lem.
Underlying function for the `EXIT-LEM` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |
| [quick-exit](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L50)        |                | Exit the lem job and kill it.
Underlying function for the `QUICK-EXIT` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |
| [execute-command](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L55)   | M-x            | Read a command name, then read the ARG and call the command.
Underlying function for the `EXECUTE-COMMAND` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |
| [show-context-menu](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L74) | Shift-F10, M-h | NIL
Underlying function for the `SHOW-CONTEXT-MENU` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |
| [load-library](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L80)      |                | Load the Lisp library named NAME.
Underlying function for the `LOAD-LIBRARY` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                                                      |

## Frame
| Command                                                                                            | Key bindings | Documentation                                                                                                                                                                                                            |
|----------------------------------------------------------------------------------------------------|--------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [toggle-frame-fullscreen](https://github.com/lem-project/lem/blob/main/src/commands/frame.lisp#L6) |              | Toggles fullscreen.
Underlying function for the `TOGGLE-FRAME-FULLSCREEN` command.
If you call it directly instead of using `call-command`:
  - *this-command* will not be bound
  - the execute-hook will not be called                                                                                                                                                                                                                                                                                                        |

