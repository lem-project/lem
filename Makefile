LISP ?= ${shell which sbcl}

ncurses:
	qlot install
	qlot exec $(LISP) --load scripts/build-ncurses.lisp

sdl2:
	qlot install
	qlot exec $(LISP) --load scripts/build-sdl2.lisp

test:
	qlot install
	qlot exec .qlot/bin/rove lem-tests.asd

doc:
	qlot install
	qlot exec $(LISP) --load scripts/generate-documentation-tests.lisp --eval '(progn (lem-documentation-mode/tests::generate-markdown-file "test.md" :test) (quit))'

update:
	git pull
	qlot install
