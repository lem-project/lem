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
| Command                    | Key bindings   | Documentation |
|----------------------------|----------------|---------------|
| SELF-INSERT                |                |               |
| NEWLINE                    | Return         |               |
| OPEN-LINE                  | C-o            |               |
| QUOTED-INSERT              | C-q            |               |
| DELETE-NEXT-CHAR           | C-d, Delete    |               |
| DELETE-PREVIOUS-CHAR       | C-h, Backspace |               |
| COPY-REGION                | M-w            |               |
| COPY-REGION-TO-CLIPBOARD   |                |               |
| KILL-REGION                | C-w            |               |
| KILL-REGION-TO-CLIPBOARD   |                |               |
| KILL-LINE                  | C-k            |               |
| YANK                       | C-y            |               |
| YANK-POP                   | M-y            |               |
| YANK-POP-NEXT              |                |               |
| YANK-TO-CLIPBOARD          |                |               |
| PASTE-FROM-CLIPBOARD       |                |               |
| ENTAB-LINE                 |                |               |
| DETAB-LINE                 |                |               |
| DELETE-BLANK-LINES         | C-x C-o        |               |
| JUST-ONE-SPACE             | M-Space        |               |
| DELETE-INDENTATION         | M-^            |               |
| TRANSPOSE-CHARACTERS       | C-t            |               |
| UNDO                       | C-\            |               |
| REDO                       | C-_, C-/       |               |
| DELETE-TRAILING-WHITESPACE |                |               |

## Mark
| Command               | Key bindings | Documentation |
|-----------------------|--------------|---------------|
| MARK-SET              | C-@, C-Space |               |
| EXCHANGE-POINT-MARK   | C-x C-x      |               |
| MARK-SET-WHOLE-BUFFER | C-x h        |               |

## Word
| Command              | Key bindings                    | Documentation |
|----------------------|---------------------------------|---------------|
| FORWARD-WORD         | M-f, C-Right                    |               |
| PREVIOUS-WORD        | M-b, C-Left                     |               |
| DELETE-WORD          | M-d, C-Delete                   |               |
| BACKWARD-DELETE-WORD | M-C-h, M-Backspace, C-Backspace |               |
| DOWNCASE-REGION      | C-x C-l                         |               |
| UPPERCASE-REGION     | C-x C-u                         |               |
| CAPITALIZE-WORD      | M-c                             |               |
| LOWERCASE-WORD       | M-l                             |               |
| UPPERCASE-WORD       | M-u                             |               |
| FORWARD-PARAGRAPH    | M-}                             |               |
| BACKWARD-PARAGRAPH   | M-{                             |               |
| KILL-PARAGRAPH       | M-k                             |               |
| COUNT-WORDS          | M-=                             |               |

## S-Expression
| Command          | Key bindings     | Documentation |
|------------------|------------------|---------------|
| FORWARD-SEXP     | M-C-f            |               |
| BACKWARD-SEXP    | M-C-b            |               |
| FORWARD-LIST     | M-C-n            |               |
| BACKWARD-LIST    | M-C-p            |               |
| DOWN-LIST        | M-C-d            |               |
| BACKWARD-UP-LIST | M-C-u            |               |
| MARK-SEXP        | M-C-@, M-C-Space |               |
| KILL-SEXP        | M-C-k            |               |
| TRANSPOSE-SEXPS  | M-C-t            |               |

## File
| Command             | Key bindings | Documentation |
|---------------------|--------------|---------------|
| FIND-FILE           | C-x C-f      |               |
| READ-FILE           | C-x C-r      |               |
| SAVE-CURRENT-BUFFER | C-x C-s      |               |
| WRITE-FILE          | C-x C-w      |               |
| WRITE-REGION-FILE   |              |               |
| INSERT-FILE         | C-x Tab      |               |
| SAVE-SOME-BUFFERS   | C-x s        |               |
| REVERT-BUFFER       |              |               |
| CHANGE-DIRECTORY    |              |               |

## Buffer
| Command          | Key bindings | Documentation |
|------------------|--------------|---------------|
| TOGGLE-READ-ONLY | C-x C-q      |               |
| RENAME-BUFFER    |              |               |
| UNMARK-BUFFER    | M-~          |               |

## Window
| Command                          | Key bindings   | Documentation |
|----------------------------------|----------------|---------------|
| SELECT-BUFFER                    | C-x b          |               |
| KILL-BUFFER                      | C-x k          |               |
| PREVIOUS-BUFFER                  | C-x Left       |               |
| NEXT-BUFFER                      | C-x Right      |               |
| RECENTER                         | C-l            |               |
| SPLIT-ACTIVE-WINDOW-VERTICALLY   | C-x 2          |               |
| SPLIT-ACTIVE-WINDOW-HORIZONTALLY | C-x 3          |               |
| OTHER-WINDOW                     | C-x o, M-o     |               |
| SWITCH-TO-LAST-FOCUSED-WINDOW    |                |               |
| WINDOW-MOVE-DOWN                 |                |               |
| WINDOW-MOVE-UP                   |                |               |
| WINDOW-MOVE-RIGHT                |                |               |
| WINDOW-MOVE-LEFT                 |                |               |
| DELETE-OTHER-WINDOWS             | C-x 1          |               |
| DELETE-ACTIVE-WINDOW             | C-x 0          |               |
| QUIT-ACTIVE-WINDOW               |                |               |
| GROW-WINDOW                      | C-x ^          |               |
| SHRINK-WINDOW                    | C-x C-z        |               |
| GROW-WINDOW-HORIZONTALLY         | C-x }          |               |
| SHRINK-WINDOW-HORIZONTALLY       | C-x {          |               |
| SCROLL-DOWN                      | C-Down, M-Down |               |
| SCROLL-UP                        | C-Up, M-Up     |               |
| FIND-FILE-OTHER-WINDOW           | C-x 4 f        |               |
| READ-FILE-OTHER-WINDOW           | C-x 4 r        |               |
| SELECT-BUFFER-OTHER-WINDOW       | C-x 4 b        |               |
| COMPARE-WINDOWS                  |                |               |

## Multiple-Cursors
| Command                  | Key bindings | Documentation |
|--------------------------|--------------|---------------|
| ADD-CURSORS-TO-NEXT-LINE | M-C          |               |

## Process
| Command       | Key bindings | Documentation |
|---------------|--------------|---------------|
| FILTER-BUFFER | C-x #        |               |
| PIPE-COMMAND  | C-x @        |               |

## Help
| Command           | Key bindings | Documentation |
|-------------------|--------------|---------------|
| DESCRIBE-KEY      | C-x ?        |               |
| DESCRIBE-BINDINGS |              |               |
| APROPOS-COMMAND   |              |               |
| LEM-VERSION       |              |               |

## Font
| Command            | Key bindings | Documentation |
|--------------------|--------------|---------------|
| FONT-SIZE-INCREASE | C-+          |               |
| FONT-SIZE-DECREASE | C--          |               |

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

