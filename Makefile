LISP ?= ${shell which sbcl}

build-ncurses:
	$(LISP) --load patch-build-ncurses.lisp	

build-sdl2:
	$(LISP) --load patch-build-sdl2.lisp	

update-submodules:
	git submodule update --remote
