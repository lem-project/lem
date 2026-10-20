# winner-mode

Undo and redo changes to the window layout, like Emacs' `winner-mode`.

Every command that changes the window configuration — splitting or deleting
windows, switching the buffer of a window, changing the layout of a frame —
is remembered.  `winner-undo` brings back the layout from before the change,
and `winner-redo` applies it again.

## Usage

Enable the mode in `~/.lem/init.lisp`:

```common-lisp
(lem-winner-mode:winner-mode)
```

It is a global minor mode, so it can also be toggled at any time with
`M-x winner-mode`.

| Command       | Key       | Description                                      |
|:--------------|:----------|:-------------------------------------------------|
| `winner-undo` | `C-c Left`  | Restore the window layout from before the last change. |
| `winner-redo` | `C-c Right` | Reapply the layout that `winner-undo` undid.     |

## What is restored

A saved configuration keeps everything needed to rebuild the layout:

- the window tree structure and the geometry of every window
- the buffer displayed in each window
- the point and the view point the window was showing

The layout is only recorded when its structure actually changes, so moving the
cursor or editing a buffer never adds a history entry.

## Notes

- The history is kept per frame.
- Calling `winner-undo` repeatedly steps further back through the history.
  Layouts already shown during the run are skipped, so the same layout is not
  restored twice.
- Layouts referring to buffers that have since been killed or made
  unswitchable are skipped as well.
- Any layout change made after an undo discards the redo history.
- If the frame has been resized since a configuration was saved, the windows
  are balanced instead of being restored to the old geometry.

## Customization

### History size

`lem-winner-mode:*winner-max-configurations*` is the maximum number of
configurations remembered per frame (default `200`).  The oldest entries are
dropped first.

```common-lisp
(setf lem-winner-mode:*winner-max-configurations* 500)
```
