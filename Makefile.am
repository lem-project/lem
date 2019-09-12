SUBDIRS = scripts/gen-contrib contrib

export lem_contrib_prefix = ${datadir}/lem

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

lem-icon.svg: scripts/gen-desktop.ros
	cd $(PWD);ros scripts/gen-desktop.ros --prefix ${prefix} --icon blue

lem-ncurses.desktop: scripts/gen-desktop.ros
	cd $(PWD);ros scripts/gen-desktop.ros --prefix ${prefix}

%$(EXEEXT): %.ros
	env CL_SOURCE_REGISTRY=$(PWD)// ros -L ${LISP_IMPL} -l $< dump output $(basename $(notdir $<))
	env CL_SOURCE_REGISTRY=$(PWD)// ros -L ${LISP_IMPL} -l $< dump executable $<

bin/%.ros: roswell/%.ros
	cp $< $@

bin/${LEM_LAUNCHER}:
	${MKDIR_P} bin
	cat roswell/lem | sed -e "s/ncurses/${DEFAULT_FRONTEND}/g" > $@
	chmod +x $@

CLEANFILES = ${bin_SCRIPTS}
CLEANFILES += lem-icon.svg lem-ncurses.desktop

depend:
	env CL_SOURCE_REGISTRY=$(PWD)// ros make-depends lem-ncurses bin/lem-ncurses > bin/lem-ncurses.depend

-include bin/lem-ncurses.depend

clean-local:
	ros -L ${LISP_IMPL} delete dump lem-ncurses|true

all-sbcl:
    sbcl --eval '(asdf:make :lem/executable)' --quit
