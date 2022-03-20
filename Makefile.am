-include .env
export $(shell sed 's/=.*//' .env)

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
CLEANFILES += lem-icon.svg lem-ncurses.desktop libasyncprocess.$(SOEXT)

depend:
	env CL_SOURCE_REGISTRY=$(PWD)// ros make-depends lem-ncurses bin/lem-ncurses > bin/lem-ncurses.depend

-include bin/lem-ncurses.depend

clean-local:
	ros -L ${LISP_IMPL} delete dump lem-ncurses|true

# [tbd] need roswell yet. edit configure.ac
all-sbcl:
	sbcl --eval '(asdf:make :lem/executable)' --quit

PACK = $(PACKAGE)-$(VERSION)-$(shell uname -s | tr '[A-Z]' '[a-z]' | sed -E 's/([^-]*).*/\1/')-$(shell uname -m)$(LEM_VARIANT)
packdir = $(PACK)

$(packdir)/Makefile:
	rm -rf $(packdir)
	$(MKDIR_P) $(packdir)
	echo "prefix = /usr/local" > $@
	echo "all: bin/lem$(EXEEXT)" >> $@
	echo "install:" >> $@
	echo "	mkdir -p $$""(prefix)/bin" >> $@
	echo "	mkdir -p $$""(prefix)/lib" >> $@
	echo "	mkdir -p $$""(prefix)/share/$(PACKAGE)/$(PACKAGE_VERSION)/site-lisp" >> $@
	echo "	install -c $(bin_SCRIPTS) $$""(prefix)/bin" >> $@
	echo "	install -c lib/libasyncprocess.$(SOEXT) $$""(prefix)/lib" >> $@
	echo "	cp -r contrib $$""(prefix)/share/$(PACKAGE)/$(PACKAGE_VERSION)/site-lisp" >> $@
	echo "uninstall:" >> $@
	echo "	cd $$""(prefix); rm -f $(bin_SCRIPTS)" >> $@
	echo "	cd $$""(prefix); rm -f lib/libasyncprocess.$(SOEXT)" >> $@
	echo "	rm -rf $$""(prefix)/share/$(PACKAGE)/$(PACKAGE_VERSION)/site-lisp" >> $@

$(PACK).tar.bz2: $(packdir)/Makefile $(bin_SCRIPTS)
	$(MKDIR_P) $(packdir)/bin
	cp -f $(bin_SCRIPTS) $(packdir)/bin
	$(MKDIR_P) $(packdir)/lib
	cp -f $(dist_lemdll_DATA) $(packdir)/lib
	cp -r contrib $(packdir)
	tar jcvf $(PACK).tar.bz2 $(packdir)

pack: $(PACK).tar.bz2

upload:
	echo upload!!! $(FILE)
	ros install roswell/sbcl_bin
	ros -s roswell.github.utils -e "(mapc (lambda (x) (print x) (roswell.github.utils:github x \"v$(VERSION)\" \"lem-project\" \"lem\")) (directory \"$(FILE)\"))"

build-docker:
	docker build -t docker.pkg.github.com/lem-project/lem/$$(cat ./scripts/$(IMAGE)/Name) ./scripts/$(IMAGE)
push-docker:
	docker push docker.pkg.github.com/lem-project/lem/$$(cat ./scripts/$(IMAGE)/Name);
pull-docker:
	docker pull docker.pkg.github.com/lem-project/lem/$$(cat ./scripts/$(IMAGE)/Name);
docker:
	docker run \
		-v `pwd`:/tmp2 \
		-e ARCH=$(ARCH) \
		docker.pkg.github.com/lem-project/lem/$$(cat ./scripts/$(IMAGE)/Name) \
		bash \
		-c "cd /tmp2;./bootstrap;./configure;make; make pack"
