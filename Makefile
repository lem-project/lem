LISP ?= ${shell which sbcl}

ncurses:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/build-ncurses.lisp

sdl2:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/build-sdl2.lisp

sdl2-ncurses:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/build-sdl2-ncurses.lisp

install:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/build-sdl2-ncurses.lisp
	sudo install -m 755 lem /usr/local/bin/
	sudo install -m 644 scripts/install/lem.svg /usr/share/icons/hicolor/scalable/apps/
	sudo gtk-update-icon-cache /usr/share/icons/hicolor
	sudo desktop-file-install --dir=/usr/share/applications scripts/install/lem.desktop
	@echo "+--------------------------------+"
	@echo "|   Lem installation complete!   |"
	@echo "+--------------------------------+"

test:
	qlot install
	.qlot/bin/rove lem-tests.asd

doc:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/generate-documentation-tests.lisp --eval '(progn (lem-documentation-mode/tests::generate-markdown-file "test.md" :test) (quit))'

update:
	git pull
	qlot install

ext:
# linux is hardcoded because I tried copy-pasting other detection
# stuff from StackOverflow and somehow it didn't work.
	LEM_OS=linux make -C extensions/

clean:
	make -C extensions/ clean

lint:
	.qlot/bin/sblint lem.asd
	make -C extensions/ lint
