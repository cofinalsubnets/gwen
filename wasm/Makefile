EMCC ?= /usr/lib/emscripten/emcc
# ccache wraps emcc for the per-object compiles below; override with
# `make CCACHE=` to disable. Caching only works because each source is
# compiled separately (-c) -- ccache passes through multi-source links.
CCACHE ?= $(shell command -v ccache 2>/dev/null)
emcc = $(CCACHE) $(EMCC)

# This frontend's build output directory under the top-level out/.
o = ../out/wasm

# g.c was split into per-concern translation units (kernel/*.c); build them all.
g_c = ../love.c ../crew/moon/lib/math/am.c
g_h = $(wildcard ../*.h)

# One object per source so an edit recompiles just that TU (and so ccache
# can cache each compile). Local host.c stays rooted in cwd; shared kernel/*.c
# carry the ../ prefix, mapped into $(o)/ by the patsubst.
host_o = $(o)/host.o
core_o = $(g_c:../%.c=$(o)/%.o)
objs = $(host_o) $(core_o)

CFLAGS = -std=gnu2x -O2 -g0 -Dai_tco=0 \
  -Wall -Wno-unused-parameter -Wno-shift-negative-value \
  -fno-exceptions \
  -I.. -I../out/lib

EMFLAGS = \
  -sMODULARIZE=1 -sEXPORT_NAME=Love \
	-sASSERTIONS -sSINGLE_FILE=1 \
  -sINITIAL_MEMORY=16MB -sALLOW_MEMORY_GROWTH=1 \
  -sEXPORTED_FUNCTIONS=_ai_init,_ai_eval,_ai_out_ptr,_ai_out_len,_ai_out_reset,_malloc,_free \
  -sEXPORTED_RUNTIME_METHODS=ccall,cwrap,UTF8ToString,stringToUTF8,lengthBytesUTF8,HEAPU8,HEAPU32

all: love.js

# prel.h/ev.h are serialized from lib/*.g by the top-level lcat rule;
# host.c assembles them into the bootstrap via G_EGG_PRE/POST (no repl here --
# the browser feeds the REPL through ai_eval).
../out/lib/egg.h: ../love/egg.l
../out/lib/p1.h: ../love/p1.l
../out/lib/prel.h: ../love/prel.l
../out/lib/ev.h: ../love/ev.l
../out/lib/uu.h: ../love/uu.l
../out/lib/coin.h: ../love/coin.l
../out/lib/rng.h: ../love/rng.l
../out/lib/q.h: ../love/q.l
../out/lib/kanren.h: ../love/kanren.l
../out/lib/egg.h ../out/lib/p1.h ../out/lib/prel.h ../out/lib/ev.h ../out/lib/uu.h ../out/lib/coin.h ../out/lib/rng.h ../out/lib/q.h ../out/lib/kanren.h ../out/lib/love_version.h:
	$(MAKE) -C .. out/lib/$(notdir $@)

# love.o bakes the version string (love_version.h, via love.c's __has_include), so
# regenerate it through the parent (force_version) and recompile love.o when the
# id changes -- mirrors the host build's l.o relink. Without this the browser
# banner (love-version) drifts to whatever revision love.o was last built at.
#
# The committed love.js is ALWAYS built from a tree that love.js itself dirties, so
# `git describe --dirty` would brand the page banner "-dirty" in perpetuity --
# cosmetic, but it reads as broken. Map it to "-wasm" in a wasm-local copy (same
# revision, honest "this is the browser build"); a clean tree just keeps the bare
# id. Compile love.o against it -- -I$(o) wins the "love_version.h" quote-include
# over ../out/lib, so the host build's honest -dirty is untouched.
$(o)/love_version.h: ../out/lib/love_version.h
	@mkdir -p $(dir $@)
	@sed 's/-dirty"/-wasm"/' $< > $@

$(o)/love.o: ../love.c $(g_h) $(o)/love_version.h
	@mkdir -p $(dir $@)
	$(emcc) -I$(o) $(CFLAGS) -c $< -o $@

# Link step: emcc's link is multi-input, so ccache can't cache it -- use
# bare $(EMCC). EMFLAGS (MODULARIZE, exports, memory) only apply here.
love.js: $(objs)
	$(EMCC) $(CFLAGS) $(EMFLAGS) $(objs) -o $@

$(o)/host.o: host.c ../out/lib/egg.h ../out/lib/p1.h ../out/lib/prel.h ../out/lib/ev.h ../out/lib/uu.h ../out/lib/coin.h ../out/lib/rng.h ../out/lib/q.h ../out/lib/kanren.h $(g_h)
	@mkdir -p $(dir $@)
	$(emcc) $(CFLAGS) -c $< -o $@

$(o)/%.o: ../%.c $(g_h)
	@mkdir -p $(dir $@)
	$(emcc) $(CFLAGS) -c $< -o $@

serve: all
	python3 -m http.server 8000

# clean only the build intermediates ($(o) = ../out/wasm). love.js is a TRACKED,
# committed artifact (the .githooks pre-commit keeps it fresh), so clean must
# NOT delete it -- doing so left a spurious `D wasm/love.js` after every top-level
# `make clean`. (l.wasm was a dead pre-rename name; SINGLE_FILE=1 embeds the
# wasm into love.js, so no standalone .wasm is ever emitted.)
clean:
	rm -rf $(o)

.PHONY: all serve clean
