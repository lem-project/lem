# LEM-XCB

This is an experimental front end for Lem.  Interesting features:

- Custom XCB bindings - for better threading than XLIB
- Freetype2 bindings - for beautiful subpixel antialiasing

## Requirements

In order for LEM-XCB to work, your system must have XWindows/XCB libraries and Freetype2 libraries installed.  Most Linux distributions come with these pre-installed.

## Quick Start

After installing Lem editor with `roswell`, at the command line type
```
$ lem-xcb
```

The editor will terminate when you close the window or <C-x C-c>.

## Configuring with .lemrc
```
(setf xcb:*font-path-normal* ...system-relative-pathname...)
(setf xcb:*font-path-bold* ...system-relative-pathname...)
```
## STATUS 

Stable, but early.

Japanese font and input support is very limited right now.

## Notes:

### Hacking

You can use Emacs to develop lem-xcb (xcb, unlike ncurses, does not require a real terminal).  Make sure lem directory is visible to quicklisp (either symlink or add to your `.sbclrc` or whatever, something like this: `(pushnew (truename "...your lem directory...") ql:*local-project-directories* )`.  Now you can start Emacs/slime, and `(ql:quickload :lem-xcb`, and start Lem with:
```
> (ql:quickload :lem-xcb)
> (lem:lem)
```
You can run Lem in a separate thread to keep the SLIME REPL:
```
> (bt:make-thread #'lem:lem)
```
### Debug information

See xcb/global.lisp.
Set `*xbug*` t and recompile for debug output.
Set `*compile-checked*` and recompile for checked XCB calls.

Sequencing of events is a little tricky.

1) at compile/load-time, `lem::*implementation*` is set to :xcb. 

2) interface-invoke creates an X environment (todo: split to global init)

3) input-loop is invoked, eventually processing %on-expose.  This installs handlers for expose,keypress and resize x events.  There may be a better place to do this.

4) Eventually, `*exit-editor-hook*` destroys the x window.

