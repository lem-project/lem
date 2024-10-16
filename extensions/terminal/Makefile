OUTPUT_FILE = "terminal.so"

default:
	make ${LEM_OS}

linux: terminal.c
	OUT_DIR = "lib/linux/x64"
	mkdir -p ${OUT_DIR}
	gcc terminal.c -Wl,-Bstatic -lvterm -Wl,-Bdynamic -o ${OUTPUT_FILE} -shared -fPIC
	mv -f ${OUTPUT_FILE} ${OUT_DIR}

osx: terminal.c
	OUT_DIR = "lib/macosx/arm64"
	mkdir -p ${OUT_DIR}
	gcc terminal.c -I/opt/homebrew/include -L/opt/homebrew/lib -lvterm -o ${OUTPUT_FILE} -shared -fPIC
	mv -f ${OUTPUT_FILE} ${OUT_DIR}

clean:
	rm -rf lib
