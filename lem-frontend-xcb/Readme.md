# LEM-XCB

This is an experimental front end for Lem.  Interesting features:

- Custom XCB bindings - for better threading than XLIB
- Freetype2 bindings - for beautiful subpixel antialiasing

## Quick Start

Clone the repo (make sure to clone the XCB branch until it's merged).  From emacs:

```
> (ql:quickload :lem-xcb)
> (lem:lem)
```

You can also run it in a separate thread so your REPL is still available with
```
> (bt:make-thread (lambda () (lem:lem)))
```

The editor will terminate when you close the window or <C-x C-c>.

## STATUS

Just got it working - expecting issues.


## Notes:

### Debug information

See xcb/global.lisp.
Set `*xbug*` t and recompile for debug output.
Set `*compile-checked*` and recompile for checked XCB calls.

Sequencing of events is a little tricky.

1) at compile/load-time, `lem::*implementation*` is set to :xcb. 

2) interface-invoke creates an X environment (todo: split to global init)

3) input-loop is invoked, eventually processing %on-expose.  This installs handlers for expose,keypress and resize x events.  There may be a better place to do this.

4) Eventually, `*exit-editor-hook*` destroys the x window.

