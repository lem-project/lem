LISP ?= ${shell which sbcl}

build-ncurses:
	$(LISP) --load scripts/patch-build-ncurses.lisp	

build-sdl2:
	$(LISP) --load scripts/patch-build-sdl2.lisp	

test:
	$(LISP) --load scripts/launch-tests.lisp

generate-doc:
	$(LISP) --load scripts/generate-documentation-tests.lisp --eval '(progn (lem-documentation-mode/tests::generate-markdown-file "/tmp/test.md" :test) (quit))'

update-submodules:
	git submodule update --remote

update:
	git pull
	git submodule update --init --recursive
