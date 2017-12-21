#LEM-XCB

This is an experimental front end for Lem.  Interesting features:

- Custom XCB bindings - for better threading than XLIB
- Freetype2 bindings - for beautiful subpixel antialiasing

## STATUS

Just got it working - expecting issues.


## Notes:

Sequencing of events is a little tricky.

1) at compile/load-time, `lem::*implementation*` is set to :xcb. 

2) interface-invoke creates an X environment (todo: split to global init)

3) input-loop is invoked, eventually processing %on-expose.  This installs handlers for expose,keypress and resize x events.  There may be a better place to do this.

4) Eventually, `*exit-editor-hook*` destroys the x window.

