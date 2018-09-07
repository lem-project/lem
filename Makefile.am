bin_SCRIPTS = bin/${LEM_LAUNCHER}
EXTRA_DIST =  bin/${LEM_LAUNCHER}

if GENERATE_NCURSES
 bin_SCRIPTS += bin/lem-ncurses$(EXEEXT)
 EXTRA_DIST += bin/lem-ncurses$(EXEEXT)
endif

if GENERATE_RPC
 bin_SCRIPTS += bin/lem-rpc$(EXEEXT)
 EXTRA_DIST += bin/lem-rpc$(EXEEXT)
endif

if GENERATE_XCB
 bin_SCRIPTS += bin/lem-xcb$(EXEEXT)
 EXTRA_DIST += bin/lem-xcb$(EXEEXT)
endif

if GENERATE_NCURSES_CCL
 bin_SCRIPTS += bin/lem-ncurses-ccl$(EXEEXT)
 EXTRA_DIST += bin/lem-ncurses-ccl$(EXEEXT)
endif

%$(EXEEXT): %.ros
	ros build $<

bin/%.ros: roswell/%.ros
	cp $< $@

bin/${LEM_LAUNCHER}:
	${MKDIR_P} bin
	cat roswell/lem | sed -e "s/ncurses/${DEFAULT_FRONTEND}/g" > $@

CLEANFILES = ${bin_SCRIPTS}

depend:
	ros make-depends lem-ncurses bin/lem-ncurses > bin/lem-ncurses.depend

-include bin/lem-ncurses.depend
