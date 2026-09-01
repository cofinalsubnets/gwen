R ?= .
include $(R)/mk/common.mk

CCACHE ?= $(shell command -v ccache 2>/dev/null)

cc_named := $(filter command line environment override,$(origin CC))
ifneq ($(words $(CC)),1)
CCACHE :=
endif

# bootstrap interpreter
love0 = out/host/love0

.PHONY: all install uninstall clean distclean host kernel wasm love0 site site-serve test test_host \
  test_hdiff test_slow test_extra test_tools test_love0 test_wasm test_proof test_gen test_uugen \
  test_gc test_gcheck test_gcstress test_hostegg test_hostnif test_doc test_glaze test_hook test_sat test_cli \
  test_holo test_as test_elf32 test_objcopy test_holofuzz test_glazefuzz test_encver test_lux \
  test_extract test_big test_mx test_clay test_moonfuzz test_thumb1 test_thumb2 \
  test_virt test_wake test_rp2040 valg disasm flame cat cata catav perf repl gdb \
  vmret waits bench nettest lint ccdb ulp

# an unpacked release builds the product; a checkout keeps the fast gate
ifeq ($(in_git),)
.DEFAULT_GOAL := dist
else
.DEFAULT_GOAL := test
endif

# avoid creating empty artifacts with fresh mtime
.DELETE_ON_ERROR:

lib_h = $(patsubst love/%.l,out/lib/%.h,$(wildcard love/*.l))
holo_h = out/lib/holo.h  out/lib/amd64.h  out/lib/arm64.h  out/lib/rv64.h
asm0_h = out/lib/holo0.h out/lib/amd640.h out/lib/arm640.h
glaze_h = out/lib/emit.h out/lib/auto.h out/lib/hook.h out/lib/walk.h
sed_lit = sed \
  -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/"/' -e 's/$$/\\n"/'
boot_h = out/lib/cli0.h out/lib/egg0.h out/lib/post0.h out/lib/p10.h out/lib/prel0.h out/lib/ev0.h out/lib/bao0.h out/lib/uu0.h out/lib/coin0.h out/lib/rng0.h out/lib/q0.h out/lib/glob0.h out/lib/kanren0.h out/lib/overlay0.h out/lib/peg0.h out/lib/verbs0.h $(asm0_h)
.PHONY: lib
lib: $(lib_h) $(boot_h)
lcat_love = $(love0) -l love/prel.l
# same bytes: stamp anyway, or the target stays older than its inputs forever
publish = if cmp -s $$t $@ 2>/dev/null; then rm -f $$t; touch $@; else mv -f $$t $@; echo "$$tag	$@"; fi
# a forced witness keeps its mtime -- that is what stops the cascade
note = if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
lcat_h = @mkdir -p out/lib; tag=LOVE; t=$@.$$$$.tmp; \
  $(lcat_love) tools/lcat.l $< > $$t && test -s $$t \
    || { rm -f $$t; echo "FAIL: $@ empty (lcat failed -- broken bootstrap?)"; exit 1; }; \
  $(publish)
$(lib_h): out/lib/%.h: love/%.l tools/lcat.l   # + $(love0), stated below
	$(lcat_h)
sed_h = @mkdir -p out/lib; tag=SED; t=$@.$$$$.tmp; LOVE_NO_IMAGE= $(sed_lit) $< > $$t; \
  $(publish)
$(holo_h): out/lib/%.h: crew/holo/%.l tools/lcat.l
	$(lcat_h)
out/lib/rune.h: crew/rune/rune.l tools/lcat.l
	$(lcat_h)
$(asm0_h): out/lib/%0.h: crew/holo/%.l
	$(sed_h)
out/lib/%0.h: love/%.l
	$(sed_h)
$(glaze_h): out/lib/%.h: love/glaze/%.l
	$(lcat_h)
glaze_items = "(use 'holo)(module 'glaze " @out/lib/emit.h @out/lib/auto.h ")" \
  "(: ev (from 'glaze 'ev) member? (from 'glaze 'member?))" \
  @out/lib/hook.h @out/lib/walk.h @out/lib/holo.h @out/lib/amd64.h @out/lib/arm64.h
out/lib/glaze_z.h: $(glaze_h) $(holo_h) tools/mkgz.l $(love0)
	@mkdir -p out/lib; tag=MKGZ; t=$@.$$$$.tmp; \
	  $(lcat_love) tools/mkgz.l src_glaze_z $(glaze_items) > $$t && test -s $$t \
	    || { rm -f $$t; echo "FAIL: $@ empty (mkgz failed?)"; exit 1; }; \
	  $(publish)
cats_egg_items   = @out/lib/egg.h
cats_p1_items    = @out/lib/p1.h
cats_prel_items  = @out/lib/prel.h " " @out/lib/ev.h
cats_post_items  = @out/lib/post.h
cats_modsa_items = @out/lib/coin.h @out/lib/rng.h @out/lib/q.h @out/lib/glob.h \
  @out/lib/kanren.h @out/lib/overlay.h @out/lib/uu.h
cats_modsb_items = @out/lib/bao.h @out/lib/verbs.h @out/lib/scan.h @out/lib/re.h @out/lib/peg.h
cats_z = out/lib/cat_egg_z.h out/lib/cat_p1_z.h out/lib/cat_prel_z.h out/lib/cat_post_z.h \
  out/lib/cat_modsa_z.h out/lib/cat_modsb_z.h \
  out/lib/cat_mods_amd64_z.h out/lib/cat_mods_arm64_z.h out/lib/cat_mods_rv64_z.h
out/lib/cat_egg_z.h: out/lib/egg.h tools/mkgz.l $(love0)
	@mkdir -p out/lib; tag=MKGZ; t=$@.$$$$.tmp; \
	  $(lcat_love) tools/mkgz.l ai_cat_egg_z $(cats_egg_items) > $$t && test -s $$t \
	    || { rm -f $$t; echo "FAIL: $@ empty (mkgz failed?)"; exit 1; }; \
	  $(publish)
out/lib/cat_p1_z.h: out/lib/p1.h tools/mkgz.l $(love0)
	@mkdir -p out/lib; tag=MKGZ; t=$@.$$$$.tmp; \
	  $(lcat_love) tools/mkgz.l ai_cat_p1_z $(cats_p1_items) > $$t && test -s $$t \
	    || { rm -f $$t; echo "FAIL: $@ empty (mkgz failed?)"; exit 1; }; \
	  $(publish)
out/lib/cat_prel_z.h: out/lib/prel.h out/lib/ev.h tools/mkgz.l $(love0)
	@mkdir -p out/lib; tag=MKGZ; t=$@.$$$$.tmp; \
	  $(lcat_love) tools/mkgz.l ai_cat_prel_z $(cats_prel_items) > $$t && test -s $$t \
	    || { rm -f $$t; echo "FAIL: $@ empty (mkgz failed?)"; exit 1; }; \
	  $(publish)
out/lib/cat_post_z.h: out/lib/post.h tools/mkgz.l $(love0)
	@mkdir -p out/lib; tag=MKGZ; t=$@.$$$$.tmp; \
	  $(lcat_love) tools/mkgz.l ai_cat_post_z $(cats_post_items) > $$t && test -s $$t \
	    || { rm -f $$t; echo "FAIL: $@ empty (mkgz failed?)"; exit 1; }; \
	  $(publish)
out/lib/cat_modsa_z.h: out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/glob.h out/lib/kanren.h out/lib/overlay.h out/lib/uu.h tools/mkgz.l $(love0)
	@mkdir -p out/lib; tag=MKGZ; t=$@.$$$$.tmp; \
	  $(lcat_love) tools/mkgz.l ai_cat_mods_a_z $(cats_modsa_items) > $$t && test -s $$t \
	    || { rm -f $$t; echo "FAIL: $@ empty (mkgz failed?)"; exit 1; }; \
	  $(publish)
out/lib/cat_modsb_z.h: out/lib/bao.h out/lib/verbs.h out/lib/scan.h out/lib/re.h out/lib/peg.h tools/mkgz.l $(love0)
	@mkdir -p out/lib; tag=MKGZ; t=$@.$$$$.tmp; \
	  $(lcat_love) tools/mkgz.l ai_cat_mods_b_z $(cats_modsb_items) > $$t && test -s $$t \
	    || { rm -f $$t; echo "FAIL: $@ empty (mkgz failed?)"; exit 1; }; \
	  $(publish)
out/lib/cat_mods_%_z.h: out/lib/holo.h out/lib/%.h tools/mkgz.l $(love0)
	@mkdir -p out/lib; tag=MKGZ; t=$@.$$$$.tmp; \
	  $(lcat_love) tools/mkgz.l ai_cat_mods_h_z @out/lib/holo.h @out/lib/$*.h > $$t && test -s $$t \
	    || { rm -f $$t; echo "FAIL: $@ empty (mkgz failed?)"; exit 1; }; \
	  $(publish)
.PHONY: force_corpus_list
force_corpus_list: ;
out/lib/corpus.list: force_corpus_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$t' > $$tf; \
	 $(note)

out/lib/love_version.h: $(R)/VERSION
	@mkdir -p out/lib
	@printf '#define AiVersion "%s"\n' "$$(cat $(R)/VERSION)" > $@
	@echo 'SH	'$@

out/lib/readme.bin: $(love0) $(R)/love/cli.l $(R)/VERSION
	@mkdir -p out/lib
	@printf 'love %s\n' "$$(cat $(R)/VERSION)" > $@
	@$(love0) -h </dev/null >> $@
	@echo 'LOVE	'$@

$(lib_h) $(holo_h) out/lib/rune.h: $(love0)
ho = out/host$(hsuf)
h_o = $(love_c:$(R)/%.c=$(ho)/%.o)
host_o = $(host_c:$(R)/%.c=$(ho)/%.o)
host_cc = $(CC)
hcc = LOVE_NO_IMAGE= $(host_cc) $(ai_cflags) $(GCDBG) -Dai_tco=$(tco) -fpic -I$(ho) -I. -Isrc -Iout/lib
image_ldflags = -Wl,--section-start=.love.image=0x2000000
.PHONY: force_hostcc
force_hostcc: ;
$(ho)/.hostcc: force_hostcc
	@mkdir -p $(ho)
	@tf=$@.$$$$.tmp; printf '%s\n' '$(host_cc) $(image_ldflags)' > $$tf; \
	 $(note)
host: $(ho)/love $(ho)/love.baked $(ho)/love.1 $(ho)/cook.1
love0: $(love0)

$(ho)/love.baked $(ho)/love.cand.baked: %.baked: % $(ho)/.dist-cat.l
	@echo 'LOVE	'$< "(bake)"
	@$< bake -l $(ho)/.dist-cat.l
	@touch $@


.PHONY: candidate
candidate: $(ho)/love.cand.baked


$(ho)/liblove.a: $(h_o)
	@echo 'AR	'$@
	@mkdir -p $(dir $@)
	@rm -f $@; ar rcs $@ $^

love0_host_o = $(patsubst $(R)/%.c,out/host/0/%.o,$(filter-out $(R)/src/cats.c,$(host_c)))
love0_o = $(love0_host_o) $(love_c:$(R)/%.c=out/host/0/%.o)   # pinned (not $(ho)/0)
out/host/0/src/main.o: $(boot_h)
out/host/0/src/cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
boot_cc = $(CCACHE) $(CC) $(ai_cflags) -fPIE -DLoveBoot -Dai_tco=0 -Dai_data_section=0 -DAiVersion='"$(love_base)+bootstrap"' -I. -Isrc -Iout/lib
.PHONY: force_love0cc
force_love0cc: ;
out/host/0/.love0cc: force_love0cc
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; printf '%s\n' '$(boot_cc)' > $$tf; \
	 $(note)
out/host/0/%.o: $(R)/%.c $(love_h) out/host/0/.love0cc
	@echo 'CC	'$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(boot_cc) -c $< -o $@
$(love0): $(love0_o)
	@echo 'LD	'$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(CC) $(ai_cflags) -pie -o $@ $(love0_o)

# src/love.c -> out/host/*.o
$(ho)/%.o: $(R)/%.c $(love_h) $(ho)/.hostcc
	@echo 'CC	'$@
	@mkdir -p $(dir $@)
	@$(hcc) -c $< -o $@

# l.o carries the version string; recompile it when the id changes. love0's twin is
# deliberately not here -- see the -DAiVersion note on boot_cc.
$(ho)/love.o: out/lib/love_version.h
# the lcat'd headers the frontends bake inline -- src/cats.c takes the egg and the module
# set, src/main.c the CLI and the glaze. one roster for both: the mooncc twin and the HCC
# link below read the same name, and three spellings is how they drift.
baked_h = out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/cli.h out/lib/bao.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/glob.h out/lib/kanren.h out/lib/overlay.h out/lib/scan.h out/lib/re.h out/lib/peg.h out/lib/uu.h out/lib/verbs.h out/lib/distlist.h $(holo_h) $(glaze_h)
$(ho)/src/main.o $(ho)/src/cats.o: $(baked_h)
$(ho)/src/cats.o: $(cats_z)
# the carried-blob reader both the first boot and the kernel's ram fs decode with
$(ho)/src/main.o $(ho)/src/ustar.o: $(R)/src/ustar.h
# src/cb.c rides the crew/quay sources by unity include -- recompile when they move.
$(ho)/src/cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h

boot_love = $(love0)
moon0 = $(love0) wake out/host/mooncc0.image mooncc $(GCDBG)
moon0_dep = out/host/mooncc0.image
moon_d = $(ho)/moon
moon_host_o = $(host_c:$(R)/src/%.c=$(moon_d)/host_%.o)
moon_math_o = $(patsubst crew/moon/lib/math/%.c,$(moon_d)/m_%.o,$(wildcard crew/moon/lib/math/*.c))
moon_love_o = $(love_tu:%.c=$(moon_d)/%.o)
moon_o = $(moon_love_o) $(moon_host_o) $(moon_math_o) $(moon_d)/sys.o
$(moon_love_o): $(moon_d)/%.o: $(R)/src/%.c $(love_h) $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moon0) -D ai_tco=$(tco) -D AiHaveVersionH -I$(ho) -I. -Isrc -Iout/lib -c $< $@
$(moon_d)/love.o: out/lib/love_version.h        # only this TU carries the version id
$(moon_d)/host_%.o: $(R)/src/%.c $(love_h) $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moon0) -D ai_tco=$(tco) -I$(ho) -I. -Isrc -Iout/lib -c $< $@
$(moon_d)/host_main.o $(moon_d)/host_cats.o: $(baked_h)
$(moon_d)/host_cats.o: $(cats_z)
$(moon_d)/host_cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
$(moon_d)/m_%.o: crew/moon/lib/math/%.c $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moon0) -Icrew/moon/lib/math -Icrew/moon/include -c $< $@
ifeq ($(hosta),aarch64)
mksys_e = mksys-arm64
else
mksys_e = mksys
endif
mksys_l = crew/kore/text.l crew/kore/u.l crew/kore/asbook.l \
          crew/holo/amd64.l crew/holo/arm64.l crew/holo/rv64.l \
          crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
out/host/.mksys-cat.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(mksys_l)' > $$tf; \
	 $(note)
out/host/.mksys-cat.l: $(mksys_l) out/host/.mksys-cat.list
	@echo 'CAT	'$@
	@mkdir -p $(dir $@)
	@cat $(mksys_l) > $@
$(moon_d)/sys.o: out/host/.mksys-cat.l $(love0)
	@echo 'HOLO	'$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(boot_love) -l out/host/.mksys-cat.l -q -e "((from 'moon '$(mksys_e)) \"$@\")" && test -s $@
ifneq ($(HCC),)
$(ho)/love $(ho)/love.cand: $(host_o) $(ho)/liblove.a $(ho)/.hostcc $(R)/src/love_data.ld $(baked_h)
	@echo 'LD	'$@
	@mkdir -p $(dir $@)
	@$(hcc) -o $@ $(host_o) $(ho)/liblove.a $(image_ldflags) $(data_ld)
else
nolibc_src = $(wildcard crew/moon/lib/nolibc/*.c crew/moon/lib/nolibc/*.h \
                        crew/moon/lib/nolibc/*/*.c crew/moon/lib/nolibc/*/*.h)
$(ho)/love $(ho)/love.cand: $(moon_o) out/host/src.o out/host/rt.o out/lib/readme.bin $(nolibc_src)
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moon0) -pie $(moon_o) $(kart_o) out/host/src.o out/host/rt.o -freadme=out/lib/readme.bin -o $@
endif

$(ho)/love.1 $(ho)/cook.1 $(ho)/lush.1: $(ho)/%.1: doc/%.md tools/mkman.l crew/lapiz/lapiz.l out/lib/love_version.h $(ho)/love
	@echo 'LOVE	'$@
	@mkdir -p $(dir $@)
	@$(ho)/love tools/mkman.l doc/$*.md out/lib/love_version.h > $@

lushfiles = crew/lush/job.l crew/lush/lex.l crew/lush/gram.l crew/lush/glob.l crew/lush/word.l crew/lush/eval.l crew/lush/line.l crew/lush/main.l
korefiles =crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/fs.l crew/kore/sum.l crew/kore/re.l crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/bc.l crew/kore/proc.l crew/kore/less.l lib/lint.l crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l crew/kore/patch.l lib/dns.l tools/ain.l $(lushfiles) crew/kore/find.l crew/cook/cook.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/holo/copy.l crew/kore/kore.l
moonfiles = crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/amd64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/rv64.l crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/moon/floor.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l crew/moon/val.l crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l
$(ho)/.mooncc-cat.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(moonfiles)' > $$tf; \
	 $(note)
$(ho)/.mooncc-cat.l: $(moonfiles) $(ho)/.mooncc-cat.list
	@echo 'CAT	'$@
	@mkdir -p $(dir $@)
	@cat $(filter %.l,$^) > $@
sbfiles = crew/kore/text.l crew/kore/diff.l lib/dns.l crew/sb/merge.l crew/sb/http.l crew/sb/sb.l
$(ho)/sb: $(sbfiles)
$(ho)/lush: $(lushfiles)
$(ho)/sb $(ho)/lush:
	@echo 'CAT	'$@
	@mkdir -p $(dir $@)
	@{ echo '#!/usr/bin/env -S love'; cat $^; } > $@
	@chmod 755 $@
out/host/mooncc0.image: out/host/.mooncc-cat.l $(love0)
	@echo 'LOVE	'$@
	@$(love0) -l out/host/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'

distfiles = crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/fs.l crew/kore/sum.l crew/kore/re.l \
            crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/bc.l crew/kore/proc.l crew/kore/less.l lib/lint.l crew/vi/config.l crew/vi/hue.l \
            crew/vi/core.l crew/vi/vi.l \
            crew/kore/diff.l crew/kore/patch.l lib/dns.l tools/ain.l $(lushfiles) crew/kore/find.l \
            crew/cook/cook.l crew/kore/asbook.l \
            crew/holo/amd64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/rv64.l \
            crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l \
            crew/holo/link.l crew/holo/copy.l crew/moon/floor.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l \
            crew/moon/val.l crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l crew/kore/kore.l crew/sb/merge.l \
            crew/sb/http.l crew/sb/sb.l crew/kiosko/kiosko.l \
            lib/gz.l lib/tar.l crew/tar/tarcmd.l crew/gz/gzcmd.l lib/cpio.l \
            crew/cpio/cpiocmd.l crew/source/source.l crew/lapiz/lapiz.l \
            lib/salt.l crew/libra/libra.l lib/hueweb.l lib/serve.l
.PHONY: force_dist_list
force_dist_list: ;
$(ho)/.dist.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(distfiles)' > $$tf; \
	 $(note)
$(ho)/.dist-cat.l: $(distfiles) $(ho)/.dist.list
	@echo 'CAT	'$@
	@mkdir -p $(dir $@)
	@cat $(distfiles) > $@
out/lib/distlist.h: Makefile
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; printf '"%s"\n' '$(distfiles)' > $$tf; \
	 $(note)
.PHONY: dist dist-source dist-seed

dist_ver  := $(love_base)
dist_stamp ?= 0
dist_source = out/dist/love-$(dist_ver).tar.gz
dist-source: $(dist_source)
ifneq ($(HCC),)
dist-seed:
	$(error dist: the HCC flavor is a differential, not the artifact -- drop HCC=)
else
dist-seed: $(ho)/love.baked
endif
dist: dist-source dist-seed   # a release is both

dist_drop = bench port wasm
.PHONY: force_src
force_src: ;
$(dist_source): force_src $(love0)
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= LOVE_BUDGET_MB=256 $(boot_love) tools/selfpack.l $@ love-$(dist_ver) $(dist_stamp) $(dist_drop)

src_arch = amd64
ifeq ($(hosta),aarch64)
src_arch = arm64
endif
ifeq ($(hosta),riscv64)
src_arch = rv64
endif
out/host/src.o: $(dist_source) tools/mksrc.l out/host/.mksys-cat.l $(love0)
	@$(boot_love) -l out/host/.mksys-cat.l tools/mksrc.l $(dist_source) $@ $(src_arch)

rt_slice = $(wildcard crew/moon/include/*.h crew/moon/include/*/*.h \
                      crew/moon/lib/*.l \
                      crew/moon/lib/nolibc/*.c crew/moon/lib/nolibc/*.h \
                      crew/moon/lib/nolibc/*/*.c crew/moon/lib/nolibc/*/*.h \
                      crew/moon/lib/math/*.c)
out/host/rt.o: $(rt_slice) tools/mkrt.l out/host/mooncc0.image $(love0)
	@$(love0) wake out/host/mooncc0.image tools/mkrt.l $@ $(src_arch)

xtgt_x86_64   = amd64
xtgt_aarch64  = arm64
xtgt_riscv64  = rv64
xqemu_x86_64  = qemu-x86_64
xqemu_aarch64 = qemu-aarch64
xqemu_riscv64 = qemu-riscv64
xmksys_x86_64  = mksys
xmksys_aarch64 = mksys-arm64
xmksys_riscv64 = mksys-riscv
xa ?= $(if $(filter aarch64,$a),x86_64,aarch64)
xtgt   = $(xtgt_$(xa))
xqemu  = $(xqemu_$(xa))
xmksys = $(xmksys_$(xa))
ifeq ($(xtgt),)
$(error x-lane: no such arch `$(xa)' -- the roster carries x86_64 aarch64 riscv64)
endif
xd = out/x-$(xa)
moonx = $(moon0) -t $(xtgt)
xhost_o = $(host_c:$(R)/src/%.c=$(xd)/host_%.o)
xmath_o = $(patsubst crew/moon/lib/math/%.c,$(xd)/m_%.o,$(wildcard crew/moon/lib/math/*.c))
xlove_o = $(love_tu:%.c=$(xd)/%.o)
xobjs = $(xlove_o) $(xhost_o) $(xmath_o) $(xd)/sys.o
$(xlove_o): $(xd)/%.o: $(R)/src/%.c $(love_h) out/host/mooncc0.image
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -D AiHaveVersionH -I$(ho) -I. -Isrc -Iout/lib -c $< $@
$(xd)/love.o: out/lib/love_version.h            # only this TU carries the version id
$(xd)/host_main.o $(xd)/host_cats.o: $(baked_h)
$(xd)/host_cats.o: $(cats_z)
$(xd)/host_%.o: $(R)/src/%.c $(love_h) out/host/mooncc0.image
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -I$(ho) -I. -Isrc -Iout/lib -c $< $@
$(xd)/host_main.o: $(baked_h)
$(moon_d)/host_main.o $(ho)/src/main.o $(xd)/host_main.o: out/lib/glaze_z.h
$(xd)/host_cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
$(xd)/m_%.o: crew/moon/lib/math/%.c out/host/mooncc0.image
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moonx) -Icrew/moon/lib/math -Icrew/moon/include -c $< $@
$(xd)/sys.o: out/host/.mksys-cat.l $(love0)
	@echo 'HOLO	'$@
	@mkdir -p $(dir $@)
	@$(love0) -l out/host/.mksys-cat.l -q -e "((from 'moon '$(xmksys)) \"$@\")" && test -s $@

$(xd)/src.o: $(dist_source) tools/mksrc.l out/host/.mksys-cat.l $(love0)
	@$(boot_love) -l out/host/.mksys-cat.l tools/mksrc.l $(dist_source) $@ $(xtgt)
$(xd)/rt.o: $(rt_slice) tools/mkrt.l out/host/mooncc0.image $(love0)
	@$(love0) wake out/host/mooncc0.image tools/mkrt.l $@ $(xtgt)
$(xd)/love: $(xobjs) $(xd)/src.o $(xd)/rt.o out/lib/readme.bin
	@echo 'MOON	'$@
	@$(moonx) -pie $(xobjs) $(xkart_o) $(xd)/src.o $(xd)/rt.o -freadme=out/lib/readme.bin -o $@
fat = out/dist/love-fat
.PHONY: dist-fat
dist-fat: $(ho)/love.baked $(xd)/love tools/fatpack.l
	@mkdir -p out/dist
	@$(boot_love) tools/fatpack.l $(fat) $a $(ho)/love $(xa) $(xd)/love
	@chmod +x $(fat)

huefiles = crew/vi/config.l crew/vi/hue.l tools/hue2vim.l
$(ho)/syntax.vim: $(huefiles) $(m)
	@echo 'HUE	'$@
	@mkdir -p $(dir $@); t=$@.$$$$.tmp; \
	  cat $(huefiles) | env -u LOVE_NO_IMAGE $(m) > $$t && test -s $$t && mv -f $$t $@ \
	    || { rm -f $$t; echo "FAIL: $@ empty (hue2vim.l failed)"; exit 1; }
.PHONY: syntax
syntax: $(ho)/syntax.vim
include $(R)/mk/distro.mk

ko = out/free
dl = dl

# every gate and verb below is phony: one roster, so adding one is one line and not two.
.PHONY: force_kfs_list kmain_o run run-$a run-sh run-headless init-container \
  uefi test_disk test_uefi test_uefi_arm64 test_kboot test_kverb test_kernel_arm64 \
  test_inle test_wasm

ifdef K_TEST
ksuf := -test
endif

KCC ?= LOVE_NO_IMAGE= $(ho)/love mooncc

k_arch_c = $(wildcard $(R)/src/$a_*.c)
k_free_c = $R/src/kmain.c $R/src/blk.c $R/src/sys.c
k_host_c = $(patsubst %,$R/src/%.c,main cats cb image mem hash sock tls deflate inflate src posix seat ustar)
k_quay_c = $R/crew/quay/cga_8x8.c $R/crew/quay/moderndos_8x16.c $R/crew/quay/paint.c
k_shared_c = $(love_c) $(k_quay_c) $(c_c)
k_h = $(love_h) $(R)/src/k.h $(R)/src/ustar.h $(wildcard *.h $(R)/src/$a_*.h)

k_odir = $(ko)/$a$(ksuf)
k_elf = $(ko)/love-$a$(ksuf).elf
k_pie = $(k_odir)/love.pie

k_shared_o = $(k_shared_c:$(R)/%.c=$(k_odir)/%.o)
k_arch_o = $(k_arch_c:$(R)/%.c=$(k_odir)/%.o)
k_free_o = $(k_free_c:$(R)/%.c=$(k_odir)/%.o)
k_host_o = $(k_host_c:$(R)/%.c=$(k_odir)/%.o)
k_lay_o = $(k_odir)/$a/vec.o
k_boot_o = $(k_odir)/$a/boot.o
k_tail_o = $(k_odir)/$a/sys.o
k_rt_o = $(k_odir)/rt.o
ifndef K_TEST
k_src_o = $(k_odir)/src.o
endif
k_o = $(k_shared_o) $(k_arch_o) $(k_free_o) $(k_host_o) $(k_lay_o) $(k_tail_o) $(k_rt_o) $(k_src_o) $(k_doom_o)

kcflags =
kcppflags := \
  -I$(k_odir) \
  -I. -Isrc -I$(R)/out/host -Iout/lib -I$(R)/crew/quay -I$(R) \
  -I$(R)/crew/moon/include \
  $(kcppflags)
ifdef K_TEST
# tail-threaded, matching the real kernel and the host; love0 stays the trampoline lane.
kcppflags += -DK_TEST -Dai_tco=1
endif
kcc = $(KCC) $(kcflags) $(kcppflags) -t $(k_be_$a)
# ours has to exist before it can compile anything.
kcc_dep = $(ho)/love.baked

kernel: $(k_elf)

$(k_odir)/src/cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
$(k_odir)/rt.o: $(rt_slice) tools/mkrt.l $m
	@echo 'LOVE	'$@
	@mkdir -p "$(dir $@)"
	@$m tools/mkrt.l $@ $(k_be_$a)
$(k_odir)/src.o: $(dist_source) tools/mksrc.l out/host/.mksys-cat.l $m
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $m -l out/host/.mksys-cat.l tools/mksrc.l $(dist_source) $@ $(k_be_$a)
$(k_pie): $(k_o) $m
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(KCC) -pie -t $(k_be_$a) $(k_o) -o $@
kproject_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/elf.l $R/crew/holo/obj.l $R/crew/holo/link.l $R/tools/kproject.l
$(k_odir)/kproject.list: force_dist_list
	@mkdir -p "$(dir $@)"
	@tf=$@.$$$$.tmp; echo '$(kproject_l)' > $$tf; \
	 $(note)
$(k_odir)/kproject.l: $(kproject_l) $(k_odir)/kproject.list
	@echo 'CAT	'$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $R/crew/kore/text.l $R/crew/kore/u.l; \
	   echo "(use 'kore)"; cat $(filter-out $R/crew/kore/text.l $R/crew/kore/u.l,$(kproject_l)); } > $@

k_pie_in = $(k_pie)
k_pie_dep =
ifndef K_TEST
ifeq ($a,$(hosta))
k_pie_in = $(ho)/love
# the projection carries the baked image, so it must follow the in-place bake
k_pie_dep = $(kcc_dep)
endif
endif
$(k_elf): $(k_odir)/kproject.l $(k_pie_in) $(k_pie_dep) $(k_boot_o) $m
	@echo 'KPROJ	'$@
	@mkdir -p "$(dir $@)"
	@$m $(k_odir)/kproject.l $(k_pie_in) $(k_boot_o) $@ $a && test -s $@

kfs = $(sort $(wildcard $R/lib/*.l))
force_kfs_list: ;
out/lib/kfs.list: force_kfs_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$(kfs)' > $$tf; \
	 $(note)
out/lib/kfs.h: $(kfs) out/lib/kfs.list $(love0) tools/lcatfs.l love/prel.l
	@mkdir -p out/lib
	@echo 'LOVE	'$@
	@$(love0) -l love/prel.l tools/lcatfs.l $(kfs:$R/%=%) > $@

out/lib/korelist.h: Makefile
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; printf '"%s"\n' '$(korefiles)' > $$tf; \
	 $(note)

# Shared C sources (src/love.c, crew/quay/, nolibc's six) + per-arch free/<a>/.
# Under K_TEST kmain.c #includes the baked corpus out/lib/ktests.h.
$(k_odir)/%.o: $(R)/%.c $(k_h) $(kcc_dep) $(baked_h) $(cats_z) $(if $(K_TEST),out/lib/kfs.h out/lib/ktests.h,out/lib/korelist.h)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

# kmain_o -- the kernel frontend, COMPILED AND NOTHING MORE, at whatever arch and face the
# caller's `a=` / `K_TEST=` say; the odir is spelled here so a caller never re-derives it.
kmain_o: $(k_free_o)

kart_inc = -I$(ho) -I. -Isrc -Iout/lib -I$R \
  -I$R/crew/quay -I$R/crew/moon/include
kart_h = $(love_h) $(R)/src/k.h $(R)/src/ustar.h $(wildcard $(R)/src/$(hosta)_*.h)
# kmain.c's own bake is the kore ROSTER now; the egg and the module set are src/cats.c's,
# and that object rides the host lane above.
kart_bake = out/lib/korelist.h
kart_arch_o = $(patsubst $R/src/%.c,$(moon_d)/k_%.o,$(wildcard $R/src/$(hosta)_*.c))
# the console's painter and its fonts: kernel-only draws the host link never had
kart_quay_o = $(patsubst %,$(moon_d)/k_q_%.o,paint cga_8x8 moderndos_8x16)
# the twin link takes the same set at $(xa) -- $(xkart_o), below the lays
kart_o = $(moon_d)/k_kmain.o $(moon_d)/k_blk.o $(moon_d)/k_sys.o $(kart_arch_o) $(kart_quay_o) $(moon_d)/kvec.o

ifdef DOOM
doom_d = $R/dl/doomgeneric/doomgeneric
doom_drop = $(wildcard $(doom_d)/doomgeneric_*.c $(doom_d)/i_allegro*.c $(doom_d)/i_sdl*.c)
doom_c = $(filter-out $(doom_drop),$(wildcard $(doom_d)/*.c))
k_doom_o = $(patsubst $(doom_d)/%.c,$(k_odir)/doom/%.o,$(doom_c)) $(k_odir)/doom/wad.o
k_free_c += $R/src/doom.c
kcppflags += -I$(doom_d)
$(k_odir)/doom/%.o: $(doom_d)/%.c $(kcc_dep)
	@echo 'DOOM	'$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@
$(k_odir)/doom/wad.o: $R/dl/doom1.wad tools/mkblob.l out/host/.mksys-cat.l $m
	@echo 'MKBLOB	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $m -l out/host/.mksys-cat.l tools/mkblob.l $< $@ doom_wad $(k_be_$a)
# and the same set on the KART lane, which is where the host's own kernel is
# built (plan C2: the artifact carries it) -- so `make kernel DOOM=1` at $(hosta)
# rides these and the cross odir rides the rows above.
kart_inc += -I$(doom_d)
kart_doom_o = $(patsubst $(doom_d)/%.c,$(moon_d)/kd_%.o,$(doom_c)) \
  $(moon_d)/kd_wad.o $(moon_d)/k_doom.o
kart_o += $(kart_doom_o)
$(moon_d)/kd_%.o: $(doom_d)/%.c $(moon0_dep)
	@echo 'DOOM	'$@
	@mkdir -p "$(dir $@)"
	@$(moon0) $(kart_inc) -c $< $@
$(moon_d)/kd_wad.o: $R/dl/doom1.wad tools/mkblob.l out/host/.mksys-cat.l $(love0)
	@echo 'MKBLOB	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $(boot_love) -l out/host/.mksys-cat.l tools/mkblob.l $< $@ doom_wad $(k_be_$(hosta))
endif

$(moon_d)/k_%.o: $R/src/%.c $(kart_h) $(kart_bake) $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(moon0) $(kart_inc) -c $< $@
$(moon_d)/k_q_%.o: $R/crew/quay/%.c $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(moon0) $(kart_inc) -c $< $@
# the vector lay, under whatever love a fresh tree has (mksys's own idiom)
$(moon_d)/kvec.o: $(ko)/$(hosta)/mkvec.l $(love0)
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $(boot_love) -l $< -q -e '(lay-vec "$@" "$(hosta)")' && test -s $@
$(ho)/love $(ho)/love.cand: $(kart_o)

$(k_odir)/src/love.o: out/lib/love_version.h
$(k_odir)/src/love.o: kcppflags += -DAiHaveVersionH

k_be_x86_64 = amd64
k_be_aarch64 = arm64
klay_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/$(k_be_$a).l $R/crew/holo/elf.l $R/crew/holo/obj.l
$(k_odir)/mkvec.l $(k_odir)/mkboot.l: $(k_odir)/%.l: $R/src/%.l $(klay_l)
	@echo 'CAT	'$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $R/crew/kore/text.l $R/crew/kore/u.l; \
	   echo "(use 'kore)"; cat $(filter-out $R/crew/kore/text.l $R/crew/kore/u.l,$(klay_l)) $<; } > $@

xkart_inc = -I$(ho) -I. -Isrc -Iout/lib -I$R \
  -I$R/crew/quay -I$R/crew/moon/include
xkart_h = $(love_h) $(R)/src/k.h $(wildcard $(R)/src/$(xa)_*.h)
xkart_arch_o = $(patsubst $R/src/%.c,$(xd)/k_%.o,$(wildcard $R/src/$(xa)_*.c))
xkart_quay_o = $(patsubst %,$(xd)/k_q_%.o,paint cga_8x8 moderndos_8x16)
xkart_o = $(if $(xkart_arch_o),$(xd)/k_kmain.o $(xd)/k_blk.o $(xd)/k_sys.o $(xkart_arch_o) $(xkart_quay_o) $(xd)/kvec.o,)
$(xd)/k_%.o: $R/src/%.c $(xkart_h) $(kart_bake) $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(moonx) $(xkart_inc) -c $< $@
$(xd)/k_q_%.o: $R/crew/quay/%.c $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(moonx) $(xkart_inc) -c $< $@
# the twin's own cat, the shape above worn at $(xa): the kernel's is cut at $a
# and this is the other machine. one target, so an ordinary rule serves.
xklay_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/$(k_be_$(xa)).l $R/crew/holo/elf.l $R/crew/holo/obj.l
$(xd)/mkvec.l: $R/src/mkvec.l $(xklay_l)
	@echo 'CAT	'$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $R/crew/kore/text.l $R/crew/kore/u.l; \
	   echo "(use 'kore)"; cat $(filter-out $R/crew/kore/text.l $R/crew/kore/u.l,$(xklay_l)) $<; } > $@
$(xd)/kvec.o: $(xd)/mkvec.l $(love0)
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $(boot_love) -l $< -q -e '(lay-vec "$@" "$(xa)")' && test -s $@
$(xd)/love: $(xkart_o)

# `test -s`: an empty object is the failure this build cannot see -- it links, and the
# kernel boots into nothing.
$(k_lay_o) $(k_boot_o): $(k_odir)/$a/%.o: $(k_odir)/mk%.l $m
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@$m -l $< -q -e '(lay-$* "$@" "$a")' && test -s $@

# the machine tail rides the host's own cat (flavour-neutral, one cut for every
# consumer); only the entry names the arch.
k_mksys_x86_64 = mksys
k_mksys_aarch64 = mksys-arm64
$(k_tail_o): out/host/.mksys-cat.l $m
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@$m -l out/host/.mksys-cat.l -q -e "((from 'moon '$(k_mksys_$a)) \"$@\")" && test -s $@

k_kvm = $(if $(and $(wildcard /dev/kvm),$(filter x86_64,$a),$(filter x86_64,$(shell uname -m))),-enable-kvm -cpu host,)
k_qemu_x86_64 = -M q35 -serial stdio
k_qemu_risc = -device ramfb -device qemu-xhci -device usb-kbd -device usb-mouse
k_qemu_aarch64 = -M virt,gic-version=2 -cpu cortex-a72 -serial stdio -semihosting $(k_qemu_risc)
k_qemu = qemu-system-$a -m 256M $(k_qemu_$a) $(k_kvm)
k_fw = -drive if=pflash,unit=0,format=raw,file=$(dl)/edk2-ovmf/ovmf-code-$a.fd,readonly=on

ifeq ($a,x86_64)
run: run-$a
run-$a: $(ko)/esp-$a/EFI/BOOT/$(k_efiname) $(ko)/esp-$a/love.elf $(dl)/edk2-ovmf/ovmf-code-$a.fd
	exec $(k_qemu) $(k_fw) -drive format=raw,file=fat:rw:$(ko)/esp-$a
else
run: run-$a
run-$a: $(k_elf)
	exec $(k_qemu) -kernel $<
endif
# the serial doors: no firmware, nothing downloaded, and a command line.
run-sh: $(k_elf)
	exec $(k_qemu) -kernel $< -append "sh"
run-headless: $(k_elf)
	exec $(k_qemu) -kernel $< -display none -no-reboot

init-container: host
	@command -v unshare >/dev/null || { echo "init-container: needs unshare (util-linux)"; exit 1; }
	@echo "-- love as PID 1 in a pid+user+mount namespace --"
	unshare --pid --fork --mount-proc --user --map-root-user -- $m -l crew/init/init.l -e "(pid1 0)"

kt = $(filter-out %/run.l %/bell.l %/zz-fin.l,$t) \
  $R/test/kernel/ramfs.l $R/test/kernel/fs.l $R/test/kernel/wfs.l \
  $R/test/kernel/kore0.l $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/core.l $R/crew/kore/fs.l \
  $R/test/kernel/kore.l $R/test/kernel/pipe.l $R/test/kernel/sys.l \
  $R/test/kernel/sh0.l $R/crew/lush/job.l $R/crew/lush/lex.l $R/crew/lush/gram.l \
  $R/crew/lush/glob.l $R/crew/lush/word.l $R/crew/lush/eval.l $R/test/kernel/sh.l \
  $R/test/kernel/disk.l $R/test/kernel/svm.l $R/test/kernel/vmx.l \
  $R/test/zz-fin.l
out/lib/ktests.list: force_dist_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$(kt)' > $$tf; \
	 $(note)
out/lib/ktests.l: $(kt) out/lib/corpus.list out/lib/ktests.list
	@echo 'CAT	'$@
	@mkdir -p out/lib
	@cat $(kt) > $@
out/lib/ktests.h: out/lib/%.h: out/lib/%.l $(love0) tools/lcatv.l love/prel.l
	@echo 'LOVE	'$@
	@$(love0) -l love/prel.l tools/lcatv.l $< > $@


ifeq ($a,x86_64)

test_disk: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/love-$a-test.elf
	@rm -f $(ko)/love-$a-test.elf.disk
	@echo TEST $(ko)/love-$a-test.elf "(two boots, one disk: the reset-persistence gate)"
	@$m $(R)/tools/ktest.l $(ko)/love-$a-test.elf - $a
	@$m $(R)/tools/ktest.l $(ko)/love-$a-test.elf - $a "disk: fat kept across the reset"
	@echo "test_disk: the machine remembered"

test_kverb: host
	@$(MAKE) -s $(k_elf)
	@echo TEST love kernel "(the projection verb; byte-identical to make's)"
	@rm -f $(ko)/.kverb.elf
	@cd $(ko) && $(abspath $m) kernel .kverb.elf > /dev/null
	@cmp $(ko)/.kverb.elf $(k_elf)
	@rm -f $(ko)/.kverb.elf

test_kboot: host $(R)/tools/kboot.l
	@$(MAKE) -s $(k_elf)
	@echo TEST $(k_elf) "(the kore cat off cmdline; 4 boots, ceiling 420s each)"
	@$m $(R)/tools/kboot.l $(k_elf) "kore ls lib" "json.l"
	@$m $(R)/tools/kboot.l $(k_elf) "kore wc lib/json.l" "lib/json.l" $$(wc -c < $(R)/lib/json.l)
	@$m $(R)/tools/kboot.l $(k_elf) "sh -c \"cd lib; pwd\"" "/lib"
	@$m $(R)/tools/kboot.l $(k_elf) "sh -c \"kore ls lib | kore wc -l\"" $$(ls $(R)/lib | wc -l)
else
test_disk test_kboot:
	@echo "$@: skipped (host arch $a is not x86_64)"
endif

uefi_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/elf.l $R/crew/holo/obj.l $R/crew/holo/link.l $R/crew/holo/pe.l \
  $R/src/uefi_mkefi.l
# the removable-media path firmware looks for, per arch -- it is the FILENAME that
# picks the loader, so the two ESPs differ in nothing else.
k_efiname_x86_64 = BOOTX64.EFI
k_efiname_aarch64 = BOOTAA64.EFI
k_efiname = $(k_efiname_$a)
k_uefid = $(ko)/uefi-$a$(ksuf)
k_espd = $(ko)/esp-$a$(ksuf)
$(k_uefid)/loader.o: $R/src/uefi_loader.c $(ho)/love.baked
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(ho)/love mooncc -t $(k_be_$a) -c $< $@
$(k_uefid)/$(k_efiname): $(k_uefid)/loader.o $(uefi_l) $m
	@echo 'HOLO	'$@
	@mkdir -p $(dir $@)
	@{ echo "(use 'holo)"; cat $(uefi_l); echo '(mkboot "$@" "$a" (list "$<"))'; } | $m
# the ESP: the loader at that path, and the kernel beside it (the loader opens
# "love.elf" on its own volume).
$(k_espd)/EFI/BOOT/$(k_efiname): $(k_uefid)/$(k_efiname)
$(k_espd)/love.elf: $(ko)/love-$a$(ksuf).elf
$(k_espd)/EFI/BOOT/$(k_efiname) $(k_espd)/love.elf:
	@echo 'CP	'$@
	@mkdir -p $(dir $@)
	@cp $< $@
uefi: $(ko)/esp-$a/EFI/BOOT/$(k_efiname) $(ko)/esp-$a/love.elf
	@echo "uefi: $(ko)/esp-$a is an ESP -- copy it to a FAT32 partition, or"
	@echo "      qemu-system-$a -drive format=raw,file=fat:rw:$(ko)/esp-$a ..."

OVMF_X64 := $(wildcard $(dl)/edk2-ovmf/ovmf-code-x86_64.fd)
ifeq ($(and $(filter x86_64,$a),$(OVMF_X64)),)
test_uefi:
	@echo "test_uefi: skipped (x86_64 + $(dl)/edk2-ovmf/ovmf-code-x86_64.fd needed)"
else
test_uefi: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/esp-x86_64-test/EFI/BOOT/BOOTX64.EFI $(ko)/esp-x86_64-test/love.elf
	@echo TEST $(ko)/esp-x86_64-test "(serial, headless, our own BOOTX64.EFI; ~64s, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/esp-x86_64-test $(OVMF_X64) x86_64
endif

OVMF_A64 := $(wildcard $(dl)/edk2-ovmf/ovmf-code-aarch64.fd)
QEMU_A64U ?= $(shell command -v qemu-system-aarch64 2>/dev/null)
ifeq ($(and $(OVMF_A64),$(QEMU_A64U)),)
test_uefi_arm64:
	@echo "test_uefi_arm64: skipped (qemu-system-aarch64 + $(dl)/edk2-ovmf/ovmf-code-aarch64.fd needed)"
else
test_uefi_arm64: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 a=aarch64 $(ko)/esp-aarch64-test/EFI/BOOT/BOOTAA64.EFI $(ko)/esp-aarch64-test/love.elf
	@echo TEST $(ko)/esp-aarch64-test "(serial, headless, our own BOOTAA64.EFI; TCG, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/esp-aarch64-test $(OVMF_A64) aarch64
endif

test_inle:
	@$(MAKE) -s test_disk
	@$(MAKE) -s test_uefi
	@$(MAKE) -s test_kboot
	@$(MAKE) -s test_kverb
	@$(MAKE) -s test_kernel_arm64
	@$(MAKE) -s test_uefi_arm64
	@echo "test_inle: boot, disk, command line, firmware -- both arches"

QEMU_A64 ?= $(shell command -v qemu-system-aarch64 2>/dev/null)
ifeq ($(QEMU_A64),)
test_kernel_arm64:
	@echo "test_kernel_arm64: skipped (need qemu-system-aarch64)"
else
test_kernel_arm64: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 a=aarch64 $(ko)/love-aarch64-test.elf
	@echo TEST $(ko)/love-aarch64-test.elf "(serial, headless, TCG, -kernel; ~90s, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/love-aarch64-test.elf - aarch64
endif

NODE ?= $(shell command -v node 2>/dev/null)
EMCC ?= $(or $(shell command -v emcc 2>/dev/null),/usr/lib/emscripten/emcc)
ifeq ($(and $(NODE),$(wildcard $(EMCC))),)
test_wasm:
	@echo "test_wasm: skipped (needs emcc + node)"
else
test_wasm:
	@$(MAKE) -s -C $(R)/wasm gate
	@echo TEST out/wasm/love.js "(node)"
	@$(NODE) $(R)/wasm/test.mjs --love $(R)/out/wasm/love.js $t
endif

# --- downloads -------------------------------------------------------
$(dl)/edk2-ovmf/ovmf-code-%.fd:
	@echo 'MK	'ovmf
	@mkdir -p $(dl)
	@curl -L https://github.com/osdev0/edk2-ovmf-nightly/releases/latest/download/edk2-ovmf.tar.gz | gunzip | tar -C $(dl) -xf -
	@case "$a" in \
		aarch64) dd if=/dev/zero of=$@ bs=1 count=0 seek=67108864 2>/dev/null;; \
	esac
include $(R)/test/test.mk
include $(R)/mk/install.mk

JOBS  ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
osync := $(if $(filter output-sync,$(.FEATURES)),--output-sync=target,)
test_phases = test_host test_love0
# fast gate
test:
	@$(MAKE) --no-print-directory $(test_phases)

# slow gate
test_slow: test_host test_love0 vmret test_bakerep test_stdinbuf test_stdincorpus test_seat test_cli test_cookdiff test_dist test_seed


# really slow gate
test_extra: test_filemode waits test_front test_proof test_gen test_uugen test_uulean test_uuwm \
	test_uukind test_gc test_gcheck test_gcstress test_extract test_big test_mx \
	test_tools test_hostnif test_doc test_glaze test_hook test_sat test_holo test_as \
	test_holofuzz test_glazefuzz test_encver test_lux test_kore test_refuzz test_sb test_vi \
	test_moon test_clay test_moonfuzz test_forge \
	test_cts test_libc test_ulp test_raw \
	test_drv test_hdiff test_tco0 nettest test_wake test_gz test_cpio \
	test_uuhomgen test_uusplgen test_uumx test_uuvallaw \
	test_fixpoint test_xfixpoint test_raw_bake test_drat test_vec \
	test_asmops test_elf32 test_objcopy test_distboot test_fat test_wasm \
	test_riscv test_ccarm64 test_ccriscv test_cts_arm64 test_cts_riscv \
	test_raw_arm64 test_raw_riscv \
	test_virt test_thumb1 test_thumb2 test_thumb2sp \
	test_mps2 test_mps2_t1 test_mps2_wake test_nucleo446 test_nucleo446_smoke \
	test_playdate test_rp2040 test_teensy41 \
	test_freebsd test_netbsd test_freebsd_arm64 test_netbsd_arm64 \
	test_inle

all: host kernel wasm dist

lint: $(ho)/love
	@$(ho)/love $R/crew/libra/libra.l $$(git ls-files '*.l') && echo "lint: parens balance"

ccdb: $(ho)/love
	@$(ho)/love $R/tools/ccdb.l

crewtools = $(foreach d,$(wildcard crew/*),$(wildcard $d/$(notdir $d).l))
sitetools = $(foreach f,$(crewtools),\
  $(if $(wildcard doc/$(notdir $(basename $f)).md doc/misc/$(notdir $(basename $f)).md),,$f))
out/toolmd.stamp: $(sitetools) crew/libra/libra.l $(ho)/love
	@rm -rf out/toolmd && mkdir -p out/toolmd
	@for f in $(sitetools); do n=$${f##*/}; n=$${n%.l}; \
	   { $(ho)/love $R/crew/libra/libra.l doc $$f && echo && echo "[the source]($$n.src.html)"; } \
	     > out/toolmd/$$n.md || exit 1; done
	@echo "  toolmd: $(words $(sitetools)) crew headers -> out/toolmd/"
	@touch $@
# the source pages and their stylesheet, written into the site papel just built
huesrc = $(crewtools) crew/vi/hue.l crew/vi/config.l tools/hue2web.l $(ho)/love
site: host out/toolmd.stamp
	@$(ho)/love -l crew/papel/papel.l -t love -o out/site README.md doc out/toolmd
	@$(MAKE) --no-print-directory out/site/hue.css
out/site/hue.css: $(huesrc)
	@env -u LOVE_NO_IMAGE $(ho)/love $R/tools/hue2web.l css > $@
	@for f in $(crewtools); do n=$${f##*/}; n=$${n%.l}; \
	   env -u LOVE_NO_IMAGE $(ho)/love $R/tools/hue2web.l src $$f > out/site/$$n.src.html \
	     || exit 1; done
	@echo "  hue2web: $(words $(crewtools)) sources painted -> out/site/*.src.html"
SITEPORT ?= 8080
site-serve: host out/toolmd.stamp
	@$(ho)/love -l crew/papel/papel.l -t love -o out/site -s $(SITEPORT) README.md doc out/toolmd

wasm:
	@$(MAKE) -C wasm

clean:
	rm -rf out
	@rm -f test/proof/rocq/*.vo test/proof/rocq/*.vok test/proof/rocq/*.vos test/proof/rocq/*.glob test/proof/rocq/.*.aux
	@[ -d wasm ] && $(MAKE) -C wasm clean || :
distclean: clean
	rm -rf dl
valg: host
	@cat $t > $(ho)/.valg-corpus.l
	valgrind --error-exitcode=1 --suppressions=$R/tools/valgrind.supp $m $(ho)/.valg-corpus.l </dev/null
.PHONY: ulp
ulp:
	@mkdir -p out/host
	@$(CC) -O2 -o out/host/ulp $R/tools/ulp.c $R/crew/moon/lib/math/am.c -lm
	@out/host/ulp
out/host/perf.data: host
	cat $t | perf record -o $@ $m
perf: out/host/perf.data
	exec perf report -i $<
out/host/flamegraph.svg: out/host/perf.data
	flamegraph -o $@ --perfdata $<
repl: host
	@exec $m
cloc:
	cloc --by-file love src/love.c src/love.h main.c port tools test vim crew
cat: clean all test
cata: clean all test_slow
# full clean rebuild, every frontend, all tests, then the corpus under valgrind
catav: clean all test_slow valg

disasm: host
	exec rizin -A $m
gdb: host
	exec gdb $m
OBJDUMP_ANY := $(shell command -v objdump 2>/dev/null || command -v llvm-objdump 2>/dev/null)
ifeq ($(OBJDUMP_ANY),)
vmret: host
	@echo "vmret: skipped (needs objdump or llvm-objdump)"
else
vmret: host
	@$m tools/vmret.l $m
endif

WAITS_C := $(shell git ls-files '*.c' 2>/dev/null)
ifeq ($(WAITS_C),)
waits: host
	@echo "waits: skipped (needs a git checkout to enumerate the .c files)"
else
waits: host
	@$m tools/waits.l $(WAITS_C)
endif

bench: host
	$(MAKE) -C bench bench

