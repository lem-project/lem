# lem-mpd

An [MPD](https://www.musicpd.org/) client for Lem.  It presents the MPD
database as a directory-mode style buffer: directories are opened with
`Return`, songs are played with `Return`, and the current queue, stored
playlists and outputs are available as additional views.

## Setup

Load the extension:

```lisp
(ql:quickload :lem-mpd)
```

It is also part of `lem/extensions`, so it is included in normal Lem
builds.

Connection settings can be customized before connecting:

```lisp
(setf lem/mpd:*mpd-host* "localhost"
      lem/mpd:*mpd-port* 6600
      lem/mpd:*mpd-password* nil)
```

`MPD_HOST` and `MPD_PORT` environment variables are used as defaults when
present.  As with other MPD clients, `MPD_HOST` may be written as
`password@host`.

Run `M-x mpd` (or `C-c m`) to open the browser.  Use `M-x mpd-connect` to
connect to a different server.

## Keys

| Key       | Action                                         |
|-----------|------------------------------------------------|
| `Return`  | Open a directory, play a song, open a playlist |
| `^`       | Parent directory or view                       |
| `n` / `p` | Next / previous line                           |
| `g`       | Refresh                                        |
| `a`       | Add the entry at point to the queue            |
| `Space`   | Play / pause                                   |
| `s`       | Stop                                           |
| `<` / `>` | Previous / next song                           |
| `+` / `-` | Volume up / down                               |
| `d`       | Delete a queue item or stored playlist entry   |
| `C`       | Clear the queue                                |
| `S`       | Save the queue as a stored playlist            |
| `q`       | Close the MPD window                           |
| `?`       | Show keybindings                               |

Press `Return` on `[Queue]`, `[Playlists]` or `[Outputs]` at the root to
switch views.  In the outputs view, `Return` toggles an output.
