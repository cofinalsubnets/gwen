G ?= /home/gwen/g

EMCC ?= /usr/lib/emscripten/emcc
CFLAGS = -std=gnu2x -Os -g0 -Dg_tco=0 \
  -Wall -Wno-unused-parameter -Wno-shift-negative-value \
  -fno-exceptions \
  -I. -I../g -I ../b

EMFLAGS = \
  -sMODULARIZE=1 -sEXPORT_NAME=GwenModule \
	-sASSERTIONS -sSINGLE_FILE=1 \
  -sINITIAL_MEMORY=16MB -sALLOW_MEMORY_GROWTH=1 \
  -sEXPORTED_FUNCTIONS=_gwen_init,_gwen_eval,_gwen_out_ptr,_gwen_out_len,_gwen_out_reset,_malloc,_free \
  -sEXPORTED_RUNTIME_METHODS=ccall,cwrap,UTF8ToString,stringToUTF8,lengthBytesUTF8,HEAPU8,HEAPU32

all: gwen.js


../b/boot.h:
	make -C .. b/boot.h

gwen.js: host.c ../b/boot.h ../g/g.c ../g/g.h
	$(EMCC) $(CFLAGS) $(EMFLAGS) host.c ../g/g.c -o $@

serve: all
	python3 -m http.server 8000

clean:
	rm -f gwen.js gwen.wasm

.PHONY: all serve clean
