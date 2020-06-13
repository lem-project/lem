pkglib_LTLIBRARIES = libasyncprocess.la
libasyncprocess_la_LDFLAGS = -module -avoid-version -no-undefined
libasyncprocess_la_SOURCES = src/async-process.c src/async-process_windows.c

copy:
	$(MKDIR_P) static/`uname -m`/`uname`
	cp -f .libs/libasyncprocess.so static/`uname -m`/`uname`/libasyncprocess.so|true
	cp -f .libs/libasyncprocess.dylib static/`uname -m`/`uname`/libasyncprocess.dylib|true
	case "$(MSYSTEM)" in \
	  MINGW32)           \
            $(MKDIR_P) static/x86/windows ;\
	    cp -f .libs/libasyncprocess.dll static/x86/windows/libasyncprocess.dll|true;; \
	  *)                 \
            $(MKDIR_P) static/`uname -m`/windows ;\
	    cp -f .libs/libasyncprocess.dll static/`uname -m`/windows/libasyncprocess.dll|true;; \
	esac
