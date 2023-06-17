LISP ?= ${shell which sbcl}

build-ncurses:
	$(LISP) --load scripts/patch-build-ncurses.lisp	

build-sdl2:
	$(LISP) --load scripts/patch-build-sdl2.lisp	

update-submodules:
	git submodule update --remote

update:
	git pull
	git submodule update --init --recursive
