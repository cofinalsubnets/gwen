# widening the charm in strings -- the census

what it would take for `(s i)` to answer a codepoint instead of a byte, so the
byte law (`text + charm` appends one, `net` sums the charms) keeps its shape but
stops capping at 255. the motive is CJK: byte semantics means `.` matches a third
of a kanji, and the cap leaks into user code forever. ⚠ the site counts below
drift with the tree -- re-run the sweep (`txt()` / `len()` / `str()` / `ai_str` /
`EmptyString` / `ai_strp`) before leaning on a number.

the representation assumed throughout is **flexible width per string** (python's
PEP 393): one byte if every charm < 256, else four. not UTF-32 everywhere -- 4x on
an all-ASCII image and on the teensy/rp2040 targets. not UTF-16 -- surrogate pairs
break above U+FFFF, which is exactly where CJK ext-B (the rare personal-name hanzi)
lives. flexible width is also the only option that keeps `(s i)` O(1): under plain
UTF-8-with-character-indices mooncc's lexer loop goes quadratic.

## the count

318 sites touch `txt()` / `len()` / `str()` / `ai_str` / `EmptyString` / `ai_strp`:
`love.c` 215, `love.h` 11, `host/*.c` 91 over twelve files, `port/inle/kmain.c` 1.

love.c's 210 attributable sites split three ways:

| class | sites | what it means |
|---|---|---|
| BYTES -- io + casks | 64 | must stay octets; served by the same type today |
| TEXT -- must become width-aware | 56 | the actual design work |
| ASCII-only / audit | 90 | semantically unaffected, but each uses `txt()`/`len()` and must be re-read once `ai_str` grows a width tag |

TEXT is `lvm_string` `lvm_snip` `lvm_peep` `lvm_index` `data_string_apply`
`lvm_add_string` `lvm_add_seq` `add_emit` `lvm_mul_rep` `ai_net` `ai_count`
`stringlen` `mint_cmp` `cmp3` `eqv_at` `ini_str` `str0` `copy_str` `evac_str`
`image_objsize`. BYTES is the whole `io*` family plus the cask backing
(`lvm_casknew` `lvm_bcopy` `cask_str` `bytes_of` `grbufg`). `host/*.c` is nifs over
syscalls -- near-100% BYTES (`main.c` 26, `init.c` 17, `cb.c` 10, `net.c` 9,
`haven.c` 7, `pty.c` 6).

## what actually sets the cost

`struct ai_str { lvm_t *ap; uintptr_t len; char bytes[]; }` is the text type AND
the byte container, in three load-bearing places that fail SILENTLY:

1. `struct ai_cask` wraps an `ai_str` -- a cask's backing. `host/cb.c` rides that to
   hand a C struct's bytes to love. a cask is bytes by definition.
2. every port read/write is an `ai_str`. `slurp` of a binary file answers one.
3. `crew/holo/elf.l` -- `(write-bytes path bs)` was `(say q (string bs))`, so every
   ELF mooncc and holo emit went to disk THROUGH A STRING. under codepoint
   semantics `(string bs)` on a byte >= 128 re-encodes it as two bytes and the
   binary is quietly corrupt. `crew/holo/obj.l`'s `o-strbytes` ran the same
   conversion inbound.

no compiler finds these. silent corruption of build output is the risk profile,
and it is what makes the arc expensive -- not the 318 sites.

## the love layer

mostly insulated: if `tally`/`snip`/index all switch together, code that slices at
boundaries it found by scanning keeps working. the 207 `snip` sites are all text
(`snip` on a list answers 0). `tally` appears 1052 times but is generic over lists;
only the string subset moves. eighteen `("" + n)` byte-builds sit in
`crew/quay/{berth,ink,rove}.l`, `crew/vi/core.l`, `love/bao.l`,
`crew/manifest/`, and three tests.

two bonuses: `crew/quay/psf.l`'s `psf-cp` and `crew/quay/berth.l`'s `utf8` are
hand-rolled UTF-8 decode/encode and become redundant -- delete them. and
`crew/moon/lex.l`'s hot `(src i)` loop stays O(1), since ASCII source picks the
narrow kind.

## laws and proofs

`test/spec.l`: the byte law (146, 151), the NUL laws (300-301), the cask lockstep
(338-342), and the strings & mints section. `proof/rocq/spec.v` carries 83
string/byte mentions. `test/gen.v` regenerates through `tools/spec2coq.l`.

## the split: 2a lands alone

**2a -- evacuate the byte users, strings unchanged.** the OUTBOUND half is done.
the enabling hole was that the only BULK byte path in the system ran through
a string: `(string <charlist>)` had a C lane, `(cask ...)` took a count only, and
filling a cask by `pin` per byte measures 20x slower (20 ms per 200 KB -- ~200 ms
on a real link). so `cask` carries the charlist lane its sibling already had
(`lvm_casknew`), and the two whole-binary writers say a cask:
`crew/holo/elf.l`'s `write-bytes` and `crew/holo/link.l`'s `ld-write` -- the second
one carries the linker's output and does NOT go through `write-bytes`, so a grep
for that name alone misses it. the same lane serves two hot paths that would
otherwise hand-roll a pin-fill loop: `love/glaze/emit.l`'s `emit` (the JIT) and
`crew/sat/flat.l`'s `fnif` (the native CDCL kernels). strings stay bytes.
`o-strbytes` in `crew/holo/obj.l` STAYS -- an ELF string table holds names, which
are text, so that one is a real encode and becomes an explicit utf-8 encode at 2b.

**2a, the INBOUND leg -- still open.** reading a binary file still lands in a
string: `crew/quay/psf.l` slurps a font blob and indexes it. there is no
`read-bytes`; `slurp` answers a string and the port surface has no byte-read, so
closing this wants a new port primitive (a slurp-to-cask), not just a call-site
swap. lower stakes than the outbound half -- a mis-decoded font is visible, a
mis-encoded ELF is not -- but it is the rest of 2a.

**2b -- widen the charm.** with the byte users evacuated, the 56 TEXT sites and the
90 audit sites are a bounded, compiler-checkable change plus a law rewrite.

do 2a whichever way 2b goes. if 2b never happens, 2a still made the ELF path honest
about what it emits.
