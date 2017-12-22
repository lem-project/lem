(asdf:defsystem "lem-xcb"
  :depends-on ("cffi" "cffi-libffi" "cl-freetype2" "lem")
  :serial t
  :components ((:file "package")
	       (:file "xcb/global")
	       (:file "xcb/libs")
	       (:file "xcb/xcb")
	       (:file "xcb/icccm")
	       (:file "xcb/events")
	       (:file "xcb/xcb-xrender")
	       (:file "xcb/keys")
	       
	       (:file "xcb-app/ft2")
	       (:file "xcb-app/event-handling")
	       (:file "xcb-app/xcb-system")
	       (:file "xcb-app/attributes")
	       (:file "xcb-app/win")
	       (:file "xcb-app/geometry")
	       (:file "xcb-app/textwin")
	 

	       ;;	       (:file "xcb/util")

	       ;;(:file "window/xcb-system")
	       ;;(:file "window/event-handling")
	       ))
