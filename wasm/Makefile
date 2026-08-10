EMCC ?= /usr/lib/emscripten/emcc
# ccache wraps emcc for the per-object compiles below (`make CCACHE=` disables). It only
# caches because each source compiles separately -- ccache passes multi-source links through.
CCACHE ?= $(shell command -v ccache 2>/dev/null)
emcc = $(CCACHE) $(EMCC)

# This frontend's build output directory under the top-level out/.
o = ../out/wasm

# g.c was split into per-concern translation units (kernel/*.c); build them all.
g_c = ../love.c ../crew/moon/lib/math/am.c
g_h = $(wildcard ../*.h)

# One object per source, so an edit recompiles just that TU and ccache can cache each.
# Local host.c stays rooted in cwd; shared kernel/*.c carry ../, mapped in by the patsubst.
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

# a failed recipe takes its half-written target with it -- else a 0-byte artifact carries a
# fresh mtime and the next make calls it up to date. the root does not include this file,
# so it is said here. see ../Makefile.
.DELETE_ON_ERROR:

all: love.js

# these are serialized from love/*.l by the top-level lcat rule; host.c assembles them into
# the bootstrap via G_EGG_PRE/POST -- no repl here, the browser feeds it through ai_eval.
../out/lib/egg.h: ../love/egg.l
../out/lib/post.h: ../love/post.l
../out/lib/p1.h: ../love/p1.l
../out/lib/prel.h: ../love/prel.l
../out/lib/ev.h: ../love/ev.l
../out/lib/uu.h: ../love/uu.l
../out/lib/coin.h: ../love/coin.l
../out/lib/rng.h: ../love/rng.l
../out/lib/q.h: ../love/q.l
../out/lib/kanren.h: ../love/kanren.l
../out/lib/egg.h ../out/lib/post.h ../out/lib/p1.h ../out/lib/prel.h ../out/lib/ev.h ../out/lib/uu.h ../out/lib/coin.h ../out/lib/rng.h ../out/lib/q.h ../out/lib/kanren.h ../out/lib/love_version.h:
	$(MAKE) -C .. out/lib/$(notdir $@)

# love.o bakes the version string, so regenerate it through the parent and recompile when
# the id changes -- else the browser banner drifts to whatever revision love.o last saw.
# ⚠ the committed love.js is always built from a tree love.js itself dirties, so
# `git describe --dirty` would brand the banner "-dirty" in perpetuity. Map it to "-wasm"
# in a wasm-local copy: same revision, honest about being the browser build. -I$(o) wins
# the quote-include over ../out/lib, so the host build's honest -dirty is untouched.
$(o)/love_version.h: ../out/lib/love_version.h
	@mkdir -p $(dir $@)
	@sed 's/-dirty"/-wasm"/' $< > $@

$(o)/love.o: ../love.c $(g_h) $(o)/love_version.h
	@mkdir -p $(dir $@)
	$(emcc) -I$(o) $(CFLAGS) -c $< -o $@

# the link is multi-input, so ccache cannot cache it -- bare $(EMCC), and EMFLAGS apply
# only here. Spelled ONCE for two places: `love.js` is the COMMITTED artifact refreshed by
# hand, $(o)/love.js the same bytes under out/, which is what test_wasm runs. ⚠ a gate must
# not rewrite the working tree. ../Makefile says why a cmp-against-committed gate cannot work.
emlink = $(EMCC) $(CFLAGS) $(EMFLAGS) $(objs) -o $@

love.js: $(objs)
	$(emlink)

$(o)/love.js: $(objs)
	@mkdir -p $(dir $@)
	$(emlink)

# what test_wasm asks for: the link, out of tree.
.PHONY: gate
gate: $(o)/love.js

$(o)/host.o: host.c ../out/lib/egg.h ../out/lib/post.h ../out/lib/p1.h ../out/lib/prel.h ../out/lib/ev.h ../out/lib/uu.h ../out/lib/coin.h ../out/lib/rng.h ../out/lib/q.h ../out/lib/kanren.h $(g_h)
	@mkdir -p $(dir $@)
	$(emcc) $(CFLAGS) -c $< -o $@

$(o)/%.o: ../%.c $(g_h)
	@mkdir -p $(dir $@)
	$(emcc) $(CFLAGS) -c $< -o $@

serve: all
	python3 -m http.server 8000

# ⚠ clean takes only the build intermediates: love.js is a TRACKED, committed artifact
# refreshed BY HAND (`make wasm` -- there is no pre-commit hook; see ../Makefile), so
# deleting it here would leave a spurious `D wasm/love.js` after every top-level clean.
# SINGLE_FILE=1 embeds the wasm into love.js, so no standalone .wasm is ever emitted.
clean:
	rm -rf $(o)

.PHONY: all serve clean
