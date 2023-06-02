## Move
| Command                           | Key bindings  | Documentation                                         |
|-----------------------------------|---------------|-------------------------------------------------------|
| NEXT-LINE                         | C-n, Down     | Move the cursor to next line.                         |
| NEXT-LOGICAL-LINE                 |               | Move the cursor to the next logical line.             |
| PREVIOUS-LINE                     | C-p, Up       | Move the cursor to the previous line.                 |
| PREVIOUS-LOGICAL-LINE             |               | Move the cursor to the previous logical line.         |
| FORWARD-CHAR                      | C-f, Right    | Move the cursor to the next character.                |
| BACKWARD-CHAR                     | C-b, Left     | Move the cursor to the previous character.            |
| MOVE-TO-BEGINNING-OF-BUFFER       | M-<           | Move the cursor to the beginning of the buffer.       |
| MOVE-TO-END-OF-BUFFER             | M->           | Move the cursor to the end of the buffer.             |
| MOVE-TO-BEGINNING-OF-LINE         | C-a, Home     | Move the cursor to the beginning of the line.         |
| MOVE-TO-BEGINNING-OF-LOGICAL-LINE |               | Move the cursor to the beginning of the logical line. |
| MOVE-TO-END-OF-LINE               | C-e, End      | Move the cursor to the end of the line.               |
| MOVE-TO-END-OF-LOGICAL-LINE       |               | Move the cursor to the end of the logical line.       |
| NEXT-PAGE                         | C-v, PageDown | Move the cursor to the next page by one page.         |
| PREVIOUS-PAGE                     | M-v, PageUp   | Move the cursor to the previous page by one page.     |
| NEXT-PAGE-CHAR                    | C-x ]         | Move the cursor to the next page character (^L).      |
| PREVIOUS-PAGE-CHAR                | C-x [         | Move the cursor to the previous page character (^L).  |
| GOTO-LINE                         | M-g           | Move the cursor to the specified line number.         |

## Edit
| Command                    | Key bindings   | Documentation                                                                        |
|----------------------------|----------------|--------------------------------------------------------------------------------------|
| SELF-INSERT                |                | Insert the input character.                                                          |
| NEWLINE                    | Return         | Insert a new line.                                                                   |
| OPEN-LINE                  | C-o            | Insert a new line without moving the cursor position.                                |
| QUOTED-INSERT              | C-q            | Insert the next entered key (including control characters).                          |
| DELETE-NEXT-CHAR           | C-d, Delete    | Delete the next character.                                                           |
| DELETE-PREVIOUS-CHAR       | C-h, Backspace | Delete the previous character.                                                       |
| COPY-REGION                | M-w            | Copy the text of region.                                                             |
| COPY-REGION-TO-CLIPBOARD   |                | Copy the selected text to the clipboard.                                             |
| KILL-REGION                | C-w            | Kill the text of region.                                                             |
| KILL-REGION-TO-CLIPBOARD   |                | Kill the text of region and copy to the clipboard.                                   |
| KILL-LINE                  | C-k            | Kill from the current cursor position to the end of the line.                        |
| YANK                       | C-y            | Paste the copied text.                                                               |
| YANK-POP                   | M-y            | Replaces the immediately pasted text with the next text in the killring.             |
| YANK-POP-NEXT              |                | Replaces the immediately preceding yank-pop text with the text before the kill ring. |
| YANK-TO-CLIPBOARD          |                | Copy the text of the killring to the clipboard.                                      |
| PASTE-FROM-CLIPBOARD       |                | Inserts text from the clipboard.                                                     |
| ENTAB-LINE                 |                | Replaces the indent of the current line from space to tab.                           |
| DETAB-LINE                 |                | Replaces the indent of the current line from tab to space.                           |
| DELETE-BLANK-LINES         | C-x C-o        | Delete blank lines before and after the cursor.                                      |
| JUST-ONE-SPACE             | M-Space        | Combines consecutive whitespace before and after the cursor into one.                |
| DELETE-INDENTATION         | M-^            | Merge the current line with the previous line.                                       |
| TRANSPOSE-CHARACTERS       | C-t            | Swaps the characters before and after the cursor.                                    |
| UNDO                       | C-\            | Undo.                                                                                |
| REDO                       | C-_, C-/       | Redo.                                                                                |
| DELETE-TRAILING-WHITESPACE |                | Removes all end-of-line and end-of-buffer whitespace from the current buffer.        |

## Mark
| Command               | Key bindings | Documentation                                                  |
|-----------------------|--------------|----------------------------------------------------------------|
| MARK-SET              | C-@, C-Space | Sets a mark at the current cursor position.                    |
| EXCHANGE-POINT-MARK   | C-x C-x      | Exchange the current cursor position with the marked position. |
| MARK-SET-WHOLE-BUFFER | C-x h        | Select the whole buffer as a region.                           |

## Word
| Command              | Key bindings                    | Documentation                                             |
|----------------------|---------------------------------|-----------------------------------------------------------|
| FORWARD-WORD         | M-f, C-Right                    | Move to cursor to next word.                              |
| PREVIOUS-WORD        | M-b, C-Left                     | Move to cursor to previous word                           |
| DELETE-WORD          | M-d, C-Delete                   | Delete the next word.                                     |
| BACKWARD-DELETE-WORD | M-C-h, M-Backspace, C-Backspace | Delete the previous word.                                 |
| DOWNCASE-REGION      | C-x C-l                         | Replaces the selected region with a downcase.             |
| UPPERCASE-REGION     | C-x C-u                         | Replaces the selected region with a uppercase.            |
| CAPITALIZE-WORD      | M-c                             | Replace the following word with capital-case.             |
| LOWERCASE-WORD       | M-l                             | Replace the following word with lowercase.                |
| UPPERCASE-WORD       | M-u                             | Replace the following word with uppercase.                |
| FORWARD-PARAGRAPH    | M-}                             | Move cursor to forward paragraph.                         |
| BACKWARD-PARAGRAPH   | M-{                             | Move cursor to backward paragraph.                        |
| KILL-PARAGRAPH       | M-k                             | Kill the forward paragraph.                               |
| COUNT-WORDS          | M-=                             | Count the number of lines/words/characters in the buffer. |

## S-Expression
| Command          | Key bindings     | Documentation                                     |
|------------------|------------------|---------------------------------------------------|
| FORWARD-SEXP     | M-C-f            | Move the cursor to the forward expression.        |
| BACKWARD-SEXP    | M-C-b            | Move the cursor to the backward expression.       |
| FORWARD-LIST     | M-C-n            | Move the cursor to the forward list.              |
| BACKWARD-LIST    | M-C-p            | Move the cursor to the backward list.             |
| DOWN-LIST        | M-C-d            | Move the cursor to the inner expression.          |
| BACKWARD-UP-LIST | M-C-u            | Move the cursor to the outer expression.          |
| MARK-SEXP        | M-C-@, M-C-Space | Select the forward expression as a region.        |
| KILL-SEXP        | M-C-k            | Kill the forward expression as a region.          |
| TRANSPOSE-SEXPS  | M-C-t            | Swaps the expression before and after the cursor. |

## File
| Command             | Key bindings | Documentation                                                                                                 |
|---------------------|--------------|---------------------------------------------------------------------------------------------------------------|
| FIND-FILE           | C-x C-f      | Open the file.                                                                                                |
| READ-FILE           | C-x C-r      | Open the file as a read-only.                                                                                 |
| SAVE-CURRENT-BUFFER | C-x C-s      | Saves the current buffer text to a file                                                                       |
| WRITE-FILE          | C-x C-w      | Saves the text in the current buffer to the specified file                                                    |
| WRITE-REGION-FILE   |              | Saves the region of text to the specified file                                                                |
| INSERT-FILE         | C-x Tab      | Inserts the contents of the file into the current buffer.                                                     |
| SAVE-SOME-BUFFERS   | C-x s        | Save some files in the open buffer.                                                                           |
| REVERT-BUFFER       |              | Restores the buffer. Normally this command will cause the contents of the file to be reflected in the buffer. |
| CHANGE-DIRECTORY    |              | Change directories associated with the buffer.                                                                |

## Buffer
| Command          | Key bindings | Documentation                                 |
|------------------|--------------|-----------------------------------------------|
| TOGGLE-READ-ONLY | C-x C-q      | Toggle the buffer read-only.                  |
| RENAME-BUFFER    |              | Rename the buffer.                            |
| UNMARK-BUFFER    | M-~          | Remove the mark where the buffer was changed. |

## Window
| Command                          | Key bindings   | Documentation                                                     |
|----------------------------------|----------------|-------------------------------------------------------------------|
| SELECT-BUFFER                    | C-x b          | Switches to the selected buffer.                                  |
| KILL-BUFFER                      | C-x k          | Delete buffer.                                                    |
| PREVIOUS-BUFFER                  | C-x Left       | Switches to the previous buffer.                                  |
| NEXT-BUFFER                      | C-x Right      | Switches to the next buffer.                                      |
| RECENTER                         | C-l            | Scroll so that the cursor is in the middle.                       |
| SPLIT-ACTIVE-WINDOW-VERTICALLY   | C-x 2          | Split the current window vertically.                              |
| SPLIT-ACTIVE-WINDOW-HORIZONTALLY | C-x 3          | Split the current window horizontally.                            |
| OTHER-WINDOW                     | C-x o, M-o     | Go to the next window.                                            |
| SWITCH-TO-LAST-FOCUSED-WINDOW    |                | Go to the window that was last in focus.                          |
| WINDOW-MOVE-DOWN                 |                | Go to the window on the down.                                     |
| WINDOW-MOVE-UP                   |                | Go to the window on the up.                                       |
| WINDOW-MOVE-RIGHT                |                | Go to the window on the right.                                    |
| WINDOW-MOVE-LEFT                 |                | Go to the window on the left.                                     |
| DELETE-OTHER-WINDOWS             | C-x 1          | Delete all other windows.                                         |
| DELETE-ACTIVE-WINDOW             | C-x 0          | Delete the active window.                                         |
| QUIT-ACTIVE-WINDOW               |                | Quit the active window. This is a command for a popped-up window. |
| GROW-WINDOW                      | C-x ^          |                                                                   |
| SHRINK-WINDOW                    | C-x C-z        |                                                                   |
| GROW-WINDOW-HORIZONTALLY         | C-x }          |                                                                   |
| SHRINK-WINDOW-HORIZONTALLY       | C-x {          |                                                                   |
| SCROLL-DOWN                      | C-Down, M-Down | Scroll down.                                                      |
| SCROLL-UP                        | C-Up, M-Up     | Scroll up.                                                        |
| FIND-FILE-OTHER-WINDOW           | C-x 4 f        |                                                                   |
| READ-FILE-OTHER-WINDOW           | C-x 4 r        |                                                                   |
| SELECT-BUFFER-OTHER-WINDOW       | C-x 4 b        |                                                                   |
| COMPARE-WINDOWS                  |                |                                                                   |

## Multiple-Cursors
| Command                  | Key bindings | Documentation                                               |
|--------------------------|--------------|-------------------------------------------------------------|
| ADD-CURSORS-TO-NEXT-LINE | M-C          | Duplicates the cursor under the currently existing cursors. |

## Process
| Command       | Key bindings | Documentation                                                                         |
|---------------|--------------|---------------------------------------------------------------------------------------|
| FILTER-BUFFER | C-x #        | Replaces the contents of the buffer with the result of executing the command entered. |
| PIPE-COMMAND  | C-x @        | Run a command and displays the output.                                                |

## Help
| Command           | Key bindings | Documentation |
|-------------------|--------------|---------------|
| DESCRIBE-KEY      | C-x ?        |               |
| DESCRIBE-BINDINGS |              |               |
| APROPOS-COMMAND   |              |               |
| LEM-VERSION       |              |               |

## Font
| Command            | Key bindings | Documentation                                                        |
|--------------------|--------------|----------------------------------------------------------------------|
| FONT-SIZE-INCREASE | C-+          | Make the font larger (this currently only works with SDL2 frontend)  |
| FONT-SIZE-DECREASE | C--          | Make the font smaller (this currently only works with SDL2 frontend) |

## Other
| Command           | Key bindings | Documentation |
|-------------------|--------------|---------------|
| NOP-COMMAND       | NopKey       |               |
| UNDEFINED-KEY     |              |               |
| KEYBOARD-QUIT     | C-g          |               |
| ESCAPE            | Escape       |               |
| EXIT-LEM          | C-x C-c      |               |
| QUICK-EXIT        |              |               |
| EXECUTE-COMMAND   | M-x          |               |
| SHOW-CONTEXT-MENU | Shift-F10    |               |
| LOAD-LIBRARY      |              |               |

