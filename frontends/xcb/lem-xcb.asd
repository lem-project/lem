(asdf:defsystem "lem-xcb"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "MIT"
  :description "xcb frontend for the Lem editor"
  :depends-on ("cffi" "cffi-libffi" "cl-freetype2" "lem")
  :serial t
  :components ((:file "package")
	       (:file "global")
	       (:file "xcb/libs")
	       (:file "xcb/xcb")
	       (:file "xcb/icccm")
	       (:file "xcb/events")
	       (:file "xcb/xcb-xrender")
	       (:file "xcb/keys")
	       
	       (:file "xcb-app/ft2")
	       (:file "xcb-app/event-handling")
	       (:file "xcb-app/xcb-system")
	       (:file "xcb-app/window-direct")
	       (:file "xcb-app/window-double")
	       (:file "xcb-app/attributes")
	       (:file "xcb-app/geometry")
	       (:file "xcb-app/lemwin")
	 

	       ;;	       (:file "xcb/util")

	       ;;(:file "window/xcb-system")
	       ;;(:file "window/event-handling")
	       ))
