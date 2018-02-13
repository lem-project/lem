bin_SCRIPTS = bin/${LEM_LAUNCHER} bin/lem-ncurses$(EXEEXT)

%$(EXEEXT): %.ros
	ros build $<

bin/%.ros: roswell/%.ros
	cat $< | sed -e "4 s/sbcl-bin/${LISP_IMPL}/g" > $@

bin/${LEM_LAUNCHER}: roswell/lem
	${MKDIR_P} bin
	cp roswell/lem $@

CLEANFILES = ${bin_SCRIPTS}
