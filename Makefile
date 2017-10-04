-include roswell/.deps/lem
# not supporting windows yet so far. don't use autotools.
EXEEXT = 
INSTALL = /usr/bin/install -c
MKDIR_P = /bin/mkdir -p

bindir = ${exec_prefix}/bin
exec_prefix = ${prefix}
prefix = /usr/local
bin_PROGRAMS = roswell/lem$(EXEEXT)

all:
	make $(bin_PROGRAMS)

%$(EXEEXT): %.ros
	ros build $<

install:
	$(INSTALL) $(bin_PROGRAMS) $(DESTDIR)$(bindir)

uninstall:
	@list='$(bin_PROGRAMS)'; test -n "$(bindir)" || list=; \
	files=`for p in $$list; do echo "$$p"; done | \
	  sed -e 'h;s,^.*/,,;s/$(EXEEXT)$$//;$(transform)' \
	      -e 's/$$/$(EXEEXT)/' \
	`; \
	test -n "$$list" || exit 0; \
	echo " ( cd '$(DESTDIR)$(bindir)' && rm -f" $$files ")"; \
	cd "$(DESTDIR)$(bindir)" && rm -f $$files

.PHONY: all install uninstall

