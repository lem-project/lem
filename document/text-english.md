# Buffer
* TYPE * `lem: buffer`
`buffer` contains buffer name, file name, text, position pointing to text,
Object to manage the document.
Multiple `buffer`s are managed in a list.

* FUNCTION * `lem: current-buffer ()`
Return the current `buffer`.

* SETF * `(setf lem: current-buffer) (buffer)`
Change the current `buffer`.

* FUNCTION * `lem: make - buffer (name & key temporary read - only - p (enable - undo - p t) (syntax - table
                                                                                (fundamental-syntax-table))) `
If the buffer whose buffer name is `name` is included in the buffer list
It returns that buffer, if it does not exist, it creates it.
Is `read-only-p` read-only?
Does `enable-undo-p` enable undo?
`syntax-table` specifies the syntax table for that buffer.
If `temporary` is non-NIL, create a buffer that is not in the buffer list.
Options that can be specified with arguments are ignored if `temporary` is already NIL and the buffer already exists.


* FUNCTION * `lem: bufferp (x)`
If `x` is` buffer` it returns T, otherwise it returns NIL.

* FUNCTION * `lem: buffer-modified-p (& optional (buffer (current-buffer)))`
It returns T if `buffer` has been changed, NIL otherwise.

* FUNCTION * `lem: buffer-enable-undo-p (& optional (buffer (current-buffer)))`
`buffer` will return T if undo is enabled, NIL otherwise.

* FUNCTION * `lem: buffer-enable-undo (buffer)`
Enable undo of `buffer`.

* FUNCTION * `lem: buffer-disable-undo (buffer)`
Disable the undo of `buffer` and empty the information for undo.

* FUNCTION * `lem: buffer-name (& optional (buffer (current-buffer)))`
Returns the name of `buffer`.

* FUNCTION * `lem: buffer-filename (& optional (buffer (current-buffer)))`
Return the filename of `buffer`.

* FUNCTION * `lem: buffer-directory (& optional (buffer (current-buffer)))`
Return the directory of `buffer`.

* FUNCTION * `lem: buffer-unmark (buffer)`
Lower the change flag of `buffer`.

* FUNCTION * `lem: buffer-rename (buffer name)`
Change the name of `buffer` to` name`.

* FUNCTION * `lem: buffer-list ()`
Return a list of `buffer`.

* FUNCTION * `lem: get-buffer (buffer-or-name)`
If `buffer-or-name` is a buffer it returns as is,
If it is a string, it returns a buffer of that name.

* FUNCTION * `lem: delete-buffer (buffer)`
Delete `buffer` from the list of buffers.
The editor variable `kill-buffer-hook` is executed before the buffer is deleted.

* FUNCTION * `lem: get-next-buffer (buffer)`
Returns the buffer next to `buffer` in the buffer list.

* FUNCTION * `lem: get-previous-buffer (buffer)`
Return the buffer before `buffer` in the buffer list.

* FUNCTION * `lem: bury-buffer (buffer)`
Move `buffer` to the end of the buffer list and return the beginning of the buffer list.

* FUNCTION * `lem: get-file-buffer (filename)`
It returns the buffer corresponding to `filename`.
If not found, it returns NIL.


# Point
* TYPE * `lem: point`
`point` is an object that points to the position of the text in the buffer.
It has `buffer`, the line at that position, and an offset` charpos` at the beginning of the line beginning with 0.
`point` has` kind`, and its position after insertion and deletion in the buffer depends on the value of `kind`.
If `kind` is`: temporary`, use `point` for temporary reading.
Although the overhead at creation and deletion is low, there is no need to explicitly delete it,
After editing the position before that point `point` can not be used correctly.
When `kind` is`: left-inserting` or `: right-inserting`, when editing the position before it,
Adjust the position by the edited length.
When inserted in the same position as `point`
`: right-inserting` will remain in its original position and`: left-inserting` will move.
In case of `: left-inserting` or`: right-inserting`, explicitly delete it with `delete-point` after use,
You need to use `with-point`.


* FUNCTION * `lem: point-buffer (object)`
Return `buffer` pointed to by` point`.

* FUNCTION * `lem: point-kind (object)`
Return `kind` type (`: temporary`, `: left-inserting` or`: right-inserting`).

* FUNCTION * `lem: current-point ()`
Return the current `point`.

* FUNCTION * `lem: pointp (x)`
It returns T if `x` is` point`, NIL otherwise.

* FUNCTION * `lem: copy-point (point & optional kind)`
Make a copy of `point` and return it.
`kind` is`: temporary`, `: left-inserting` or` right-inserting`.
If omitted it is the same as `point`.

* FUNCTION * `lem: delete-point (point)`
Delete `point`.
If `point-kind` is: temporary, you do not need to use this function.

* FUNCTION * `lem: point = (point 1 point 2)`
Returns T if `point1` and` point2` are in the same position, otherwise it returns NIL.

* FUNCTION * `lem: point / = (point 1 point 2)`
Returns T if `point1` and` point2` are not in the same position, otherwise it returns NIL.

* FUNCTION * `lem: point <(point 1 point 2)`
It returns T if `point1` is before` point2`, otherwise it returns NIL.

* FUNCTION * `lem: point <= (point 1 point 2)`
If `point1` is at the same position as or before` point2`, T is returned, otherwise NIL is returned.

* FUNCTION * `lem: point> (point 1 point 2)`
If `point1` is after` point2`, it returns T, otherwise it returns NIL.

* FUNCTION * `lem: point> = (point 1 point 2)`
If `point1` is at the same position as or after` point2`, it returns T, otherwise it returns NIL.


* FUNCTION * `lem: set-current-mark (point)`
Set `point` to the current mark.


* FUNCTION * `lem: character-at (point & optional (offset 0))`
It returns the character at the position shifted from `point` by` offset`.
If it is out of the buffer range, NIL is returned.

* FUNCTION * `lem: line-string (point)`
Returns the string in the `point` line.


* FUNCTION * `lem: line-number-at-point (point)`
Returns the line number of `point`.

* FUNCTION * `lem: point-column (point)`
Returns the width of the column from the beginning of the line `point`.

* FUNCTION * `lem: position-at-point (point)`
Returns the offset of the beginning of 1 from the beginning of the `point` buffer.


* MACRO * `lem: with-point`
This macro creates each `point` used in` body` with `bindings`,
Exiting `body` removes each` point` and returns the value of `body`.
Even if `body` has an error, each` point` is deleted.
The format of `bindings` is a list of (` var` `point` & optional` kind`).
`kind` is optional and defaults to`: temporary`.
`` `
An example
(with-point ((p3 expr1)
             (p1 expr2: left-inserting)
             (p2 expr3: right-inserting))
  ...)
`` `


* MACRO * `lem: save-excursion`
We save the current `point` and` mark`, restore it after evaluating `body` and return the result of` body`.
Even if there is an error in `body`, it will be restored.


# # Acquire points
* FUNCTION * `lem: buffer-point (object)`
Return the current `point` of` buffer`.

* FUNCTION * `lem: buffer-mark (object)`
Return `point` of the current mark of` buffer`.

* FUNCTION * `lem: buffer-start-point (object)`
Return `point` at the beginning of` buffer`.

* FUNCTION * `lem: buffer-end-point (object)`
Return `point` at the end of` buffer`.


## Inspect point position
* FUNCTION * `lem: first-line-p (point)`
If `point` is the first row, it returns T, otherwise it returns NIL.

* FUNCTION * `lem: last-line-p (point)`
If `point` is the last line, it returns T, otherwise it returns NIL.

* FUNCTION * `lem: start-line-p (point)`
If `point` is at the beginning of a line, it returns T, otherwise it returns NIL.

* FUNCTION * `lem: end-line-p (point)`
If `point` is the end of the line, it returns T, otherwise it returns NIL.

* FUNCTION * `lem: start-buffer-p (point)`
If `point` is the first position in the buffer, it returns T, otherwise it returns NIL.

* FUNCTION * `lem: end-buffer-p (point)`
If `point` is the last position in the buffer, T is returned, otherwise NIL is returned.

* FUNCTION * `lem: same-line-p (point 1 point 2)`
Returns T if `point1` and` point2` are at the same position, NIL otherwise.


## Point movement
* FUNCTION * `lem: move-point (point new-point)`
Move `point` to the` new-point` position.

* FUNCTION * `lem: line-start (point)`
Move `point` to the beginning of the line.

* FUNCTION * `lem: line-end (point)`
Move `point` to the end of the line.

* FUNCTION * `lem: buffer-start (point)`
Move `point` to the first position in the buffer.

* FUNCTION * `lem: buffer-end (point)`
Move `point` to the last position in the buffer.

* FUNCTION * `lem: line-offset (point n & optional (charpos 0))`
`point` is moved down if` n` is a positive number, upward if negative, and `point` after the move.
`n` If there is no line in the destination, it returns NIL with the position of` point` unchanged.
`charpos` is the offset from the beginning of the line after the move.


* FUNCTION * `lem: character-offset (point n)`
`point` will be moved if` n` is a positive number, if it is a negative number, move `point` after the move.
If the `n` character is out of the buffer range, it will return NIL with the` point` position intact.

* FUNCTION * `lem: move-to-column (point column & optional force)`
Move `point` from the start of the line to the column width` column` and return `point` after the move.
If `force` is non-NIL, if the length of the line is less than` column`, insert a space and move it,
If `force` is NIL, it moves to the end of the line and returns` point` after the move.

* FUNCTION * `lem: move-to-position (point position)`
`point` is moved to the offset` position` of the beginning of the buffer from the beginning of the buffer and its position is returned.
If `position` is out of the buffer,` point` does not move and returns NIL.

* FUNCTION * `lem: move-to-line (point line-number)`
Move `point` to the line number` line-number` and return the position after the move.
If `line-number` is out of the buffer,` point` does not move and returns NIL.

* FUNCTION * `lem: skip-chars-forward (point test)`
`point` evaluates the character at that position with` test` and moves it in the backward direction during non-NIL.
If `test` is a list of characters, the character at that position is included in the list of` test`
If `test` is a function, take a character at that position as an argument and check if the return value is non-NIL


* FUNCTION * `lem: skip-chars-backward (point test)`
`point` evaluates the character before that position with` test` and moves it in the previous direction during non-NIL.
If `test` is a list of characters, the character before that position is included in the list of` test`
If `test` is a function, take one character as the argument before the position and check if the return value is non-NIL



## region
* FUNCTION * `lem: region-beginning (& optional (buffer (current-buffer)))`
Return `point` at the beginning of the region in` buffer`.

* FUNCTION * `lem: region-end (& optional (buffer (current-buffer)))`
Return `point` at the end of the region in` buffer`.

* FUNCTION * `lem: points-to-string (start-point end-point)`
Returns a string in the range from `start-point` to` end-point`.

* FUNCTION * `lem: count-characters (start-point end-point)`
Returns the length of the string from `start-point` to` end-point`.

* FUNCTION * `lem: count-lines (start-point end-point)`
Returns the number of lines from `start-point` to` end-point`.

* FUNCTION * `lem: apply-region-lines (start-point end-point function)`
For each line from `start-point` to` end-point`
Apply `function` which takes a point as an argument.


# Edit the text
* FUNCTION * `lem: insert-character (point char & optional (n 1))`
Insert `char` character` n` times `point`.

* FUNCTION * `lem: insert-string (point string & rest plist)`
Insert the string `string` in` point`.
When `plist` is specified, text property is set in the range where` string` is inserted.

* FUNCTION * `lem: delete-character (point & optional (n 1))`
Deletes `n` characters from` point` and returns the deleted character string.
If it reaches the end of the buffer before deleting `n` characters it returns NIL.

* FUNCTION * `lem: erase-buffer (& optional (buffer (current-buffer)))`
Delete all text of `buffer`.

* FUNCTION * `lem: delete - between - points (start - point end - point)`
Deletes the range from `start-point` to` end-point` and returns the deleted character string.


## Text properties
* FUNCTION * `lem: text-property-at (point prop & optional (offset 0))`
It returns the property of `prop` at the position offset from` point` by `offset`.

* FUNCTION * `lem: put-text-property (start-point end-point prop value)`
Set the text property `prop` between` start-point` and `end-point` to` value`.

* FUNCTION * `lem: remove-text-property (start-point end-point prop)`
Delete the text property `prop` from` start-point` to `end-point`.

* FUNCTION * `lem: next-single-property-change (point prop & optional limit-point)`
Move in the backward direction from `point` to a position where the value of the text property` prop` differs,
Return `point` after the move.
Scanning does not stop to the end of the buffer, or if it exceeds `limit-point`, scanning is interrupted and NIL is returned.

* FUNCTION * `lem: previous-single-property-change (point prop & optional limit-point)`
Move in the previous direction from `point` to a position where the value of the text property` prop` differs,
Return `point` after the move.
Scanning does not stop until the first position in the buffer, or `limit-point` is exceeded, scanning is interrupted and NIL is returned.


## buffer variable
* FUNCTION * `lem: buffer-value (buffer name & optional default)`
It returns the value bound to the buffer variable `name` of` buffer`.
The type of `buffer` is` buffer` or `point`.
If no variable is set, it returns `default`.

* SETF * `(setf lem: buffer-value) (value buffer name & optional default)`
Bind `value` to` buffer` buffer variable `name`.
The type of `buffer` is` buffer` or `point`.

* FUNCTION * `lem: buffer-unbound (buffer name)`
Delete the binding of `buffer` buffer variable` name`.

* FUNCTION * `lem: clear-buffer-variables (& key (buffer (current-buffer)))`
Erases all buffer variables bound to `buffer`.


# Editor variable
* TYPE * `lem: editor-variable`
`editor-variable` is a variable used in the editor.
Used to manage buffer-local variables and global values.

* MACRO * `lem: define-editor-variable`
Define the editor variable `var`.
`value` is a global value bound to that editor variable.
`documentation` is the document string of the editor variable.
`change-value-hook` is called when the global value of the editor variable is changed
This is a function.


* FUNCTION * `lem: clear-editor-local-variables (buffer)`
Make all buffer-local editor variables `buffer` unbound.

* FUNCTION * `lem: variable-value (symbol & optional (kind default) (where nil
                                                                 wherep)) `
Returns the value of the `symbol` editor variable.

`where` is a buffer, if unspecified it will be` current-buffer`.

If `kind` is`: default`, it returns it if the buffer-local editor variable of `where` is bound,
If not, it returns the value of the global editor variable.

If `kind` is`: buffer`, it returns buffer-local editor variable of `where` if it is bound,
If not, NIL is returned.

If `kind` is`: global`, it returns the global editor variable.


* SETF * `(setf lem: variable-value) (value symbol & optional (kind default) (where
                                                                          nil
                                                                          wherep)) `
Bind `value` to the value of the` symbol` editor variable.

`where` is a buffer, if unspecified it will be` current-buffer`.

If `kind` is` default` or `buffer`, bind` value` to the buffer-local editor variable of `where`.

If `kind` is` global`, bind `value` to the global editor variable.
If there is `change-value-hook` in the editor variable it will be called with` value` as argument before binding the value.