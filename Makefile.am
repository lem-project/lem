SUBDIRS = scripts/gen-contrib contrib

export lem_contrib_prefix = ${datadir}/lem

lemdlldir= $(libdir)
dist_lemdll_DATA= libasyncprocess.$(SOEXT)

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
	echo "#!/bin/sh" > $@
	echo "SBCL_HOME=\"$(lem_contrib_prefix)/$(PACKAGE_VERSION)/SBCL_HOME\"" >> $@
	echo "export SBCL_HOME" >> $@
	cat roswell/lem | sed -e "s/ncurses/${DEFAULT_FRONTEND}/g"| sed -e '1d' >> $@
	chmod +x $@

libasyncprocess.$(SOEXT):
	cd $(PWD);ros scripts/copy-shared.ros

CLEANFILES = ${bin_SCRIPTS}
CLEANFILES += lem-icon.svg lem-ncurses.desktop

depend:
	env CL_SOURCE_REGISTRY=$(PWD)// ros make-depends lem-ncurses bin/lem-ncurses > bin/lem-ncurses.depend

-include bin/lem-ncurses.depend

clean-local:
	ros -L ${LISP_IMPL} delete dump lem-ncurses|true

# [tbd] need roswell yet. edit configure.ac
all-sbcl:
	sbcl --eval '(asdf:make :lem/executable)' --quit
