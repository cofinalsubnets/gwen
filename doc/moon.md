# moon (mooncc) -- rungs 3-4 of the distro, the C compiler + gcc-free seam: THE PLAN

the plan of record for the chibicc-class C compiler, written in love, emitting
through the holo books. drafted 2026-07-06; stage 0 LANDED the same day
(crew/moon/{lex,parse,gen,cc}.l + law.l, `make test_moon` -- the gcc
differential is born). trued up as stages land. cc is its OWN app `mooncc`
(a catted `#!/usr/bin/env -S love -l` script, NOT baked into the kore cat -- so a
cc edit rebuilds only mooncc, never kore, and no kore rebuild in another session can
tear the compiler mid-run); it was `kore cc` through 7c-iii part 1.

## the goal, and the fence

`cc` compiles **love.c** -- and eventually the whole out/host build (love.h's
inline world, host/*.c) -- into objects the system linker accepts, on x86-64
first, arm64 by parity. the closure it buys: love-in-love compiles the compiler
that compiles love.c, so userland is fully self-hosting and the KERNEL is the
one imported artifact.

**RUNG 2 LANDED 2026-07-12: EVERY out/host object compiles under mooncc.** love.c
plus all thirteen host/*.c (cb/cuda/drm/fs/haven/image/image_baked/init/main/
net/lux/pty) go through `mooncc -c`, and clang links the bag -- gcc appears
ONLY as the linker. The all-mooncc binary boots and passes the whole 2831-test
corpus. Delivered: our own POSIX headers under crew/moon/include (errno, string,
stdio, stdlib, fcntl, time, poll, termios, dirent, link, sys/{types,stat,wait,
socket,un,ioctl,mount,signalfd}, netinet/in, arpa/inet, netdb, sched,
stdnoreturn) -- glibc-x86-64 ABI-faithful, NOT glibc's headers; plus the cc
gaps they exposed: top-level `static` fns/globals stay LOCAL symbols, an
AI_NIF's `__attribute__((section("ai_nifs")))` global lays in a real ai_nifs
ELF section (obj.l grew the 10th section + its bracket symbols so main()'s
`__start_/__stop_ai_nifs` drain finds the entries), leading `__attribute__`
before/after `static` skips, a lone global/typedef/proto-list at EOF parses
(the `want ";"`-vs-refusal conflation), struct-by-value locals copy their
FULL byte width (was a bogus 16-byte SSE-only copy), an integer-pair struct
returns/receives in rax:rdx (the SSE2 pair's twin), and -- the boot-blocker
-- pointer subtraction over a struct now divides by the real pointee size
(`lg2` was capped at 3, so every >8-byte struct-pointer difference divided by
8; non-power-of-two sizes take a real idiv).

**LADDER RUNG 3 LANDED 2026-07-12: our own static linker.** crew/holo/link.l
(~200 lines, in the mooncc cat after obj.l) reads the relocatable objects obj.l
lays -- .text/.data/ai_nifs + symtab + RELA, name-driven off the section
headers -- resolves every symbol across the set (strong over weak, two strongs
scare 'link-dup, `__start_/__stop_ai_nifs` synthesized over the nif region:
16-aligned once and packed tight across objects, so the drain reads ONE
array), lays the sections at the classic ET_EXEC base, applies the
relocations (R_64 = S+A; PC32/PLT32 both = S+A-P once the link is static,
range-checked), and wraps elf.l's one-segment executable header. crt0 rides
in as a synthetic FIRST object built by objelf itself (call main; exit), so
`main` resolves through the same reloc path as everything else. the driver:
a .o input or several inputs without -c is a LINK -- `mooncc a.o b.c -o prog`,
`a.out` bare; one .c stays the direct cc-exe path, -c and the legacy pair
unchanged. a foreign .o (gcc's .comment/.eh_frame zoo) scares off honestly
('link-section) -- our own objects only, by design: linking glibc's .a is the
ifunc/TLS tarpit and stays OFF the ladder. the whole 84-program battery
passes linked through it (compile -c, link, run, gcc differential), weak
default/override and the nif bracket walk are gated in test_moon, and the full
14-object selfhost set (585KB love.o included) links in under a second --
refusing at `sqrt`, the first libm undef, which is exactly rung 4's seam:
the raw-syscall host + in-tree sin/cos/log, and gcc is gone entirely.

**LADDER RUNG 4 LANDED 2026-07-12: THE GCC-FREE love.** the standalone chain is
now entirely ours -- mooncc compiles love.c + host/*.c + our own libc, mksys lays
the machine tail, and crew/holo/link.l binds them: no gcc, no glibc, no ld.
three new pieces under crew/moon/lib/:
  * `nolibc.c` -- the raw libc. ~90 wrappers straight off the x86-64 syscall
    table (through the one `__ai_sys` trampoline), a mini stdio (a FILE is a
    fd plus a flush buffer; stdout line-buffers 8KB, the one formatter speaks
    %s/%d/%u/%x/%c/%p with the l/z widths -- no floats, love prints its own),
    a K&R first-fit malloc over 1MB mmap arenas, dirent over getdents64 (the
    kernel record IS `struct dirent`), the glibc-152B-to-kernel-32B sigaction
    fold with our own restorer, a numeric getaddrinfo (localhost + dotted
    quad), env/exec/termios/pty. single-threaded like love: errno is one int,
    no locks. strtol/strtod keep libc/str.c's bodies (the rung-4 differential
    measured them corpus-green against glibc's).
  * `mksys.l` -- lays sys.o, the four things C cannot say: the 7-slot syscall
    trampoline, `__sigsetjmp`/`siglongjmp` over our own layout INSIDE the
    glibc-sized 25-long buffer (the signal mask in buf[8], saved/restored by
    rt_sigprocmask -- love.c's fault barrier is `sigsetjmp(env,1)`, so the mask
    is load-bearing), and `__ai_sigret` (the SA_RESTORER tail). every encoding
    objdump-checked, the holo house rule.
  * `math/` -- am.c, OUR transcendentals (fdlibm retired 2026-07-13: one 400-line file, sqrt exact / the seven <= a few ulp, `make ulp` is the differential gate; -lm is gone from every link).
the crt0 switch is one weak symbol: `__ai_start` is defined WEAK in the crt0
object (the bare call-main tail every small link gets), and nolibc overrides
it STRONG to unpack argv/envp/auxv before main -- no link-time flag anywhere,
the weak machinery IS the switch. `make test_raw` (opt-in, x86-64) runs the
whole corpus over the fresh egg: 2899 tests green. two cc gaps surfaced and
were closed on the way: cpp now predefines `__linux__` (host/posix.c's signalfd
source was silently compiling OUT of the selfhost binary, a latent rung-2
wart), and `open()` is variadic in unistd.h (it clashed with fcntl.h's shape).
one gap noted, not closed (nothing needs it yet): an image-time `&arr[i]`
global initializer still refuses -- dodge it with separate globals.

the one firm fence: NOT C++, ever.

## what love.c actually demands (the census, 2026-07-06)

the subset is not "C11-ish" by taste -- it is measured off the target:

* the whole statement/expression core; switch (138 cases), goto (plain
  labels only -- NO computed goto), do/while/for, the comma operator.
* typedefs, structs, unions, enums (30+/12 uses), nested aggregates,
  a flexible array member (`struct env { ... word end[]; }`), designated
  initializers (`{.x = putcharm(2)}` -- the nif tables), ANONYMOUS unions
  and structs (love.h:152,204 -- confirmed in), one compound literal.
* function pointers as first-class citizens -- the lvm dispatch tables ARE
  the program. pointer arithmetic throughout.
* varargs: va_list x4, variadic prototypes x24 (ai_push, the printers).
* the preprocessor in anger: object + function-like macros, variadic macros,
  ## token paste (17 uses -- the _(nif_x, "name", ...) tables), #if/#ifdef
  trees (AI_STAT, ai_tco, arch selection), #include.
* doubles (the gems: math.h's sin/cos/log/atan2/fmod...), NO long double.
* _Static_assert x4; __attribute__ x10 but ALL behind ai_ macros (ai_inline,
  ai_noinline) -- love.h grows a plain-C branch under `__mooncc__` and the
  attributes vanish. NO bitfields, NO VLAs, NO statement expressions.
* setjmp/sigsetjmp + signal handlers: LIBC's problem, not the compiler's --
  cc only needs the calls and the volatile discipline around them.
* the tail-threaded VM: `return Continue()` everywhere. gcc keeps the stack
  flat via sibling-call optimization (ai_tco=1). BUT love0 already builds and
  passes the whole corpus with -Dai_tco=0 -- the return-based mode -- so
  guaranteed sibcalls are a PERFORMANCE stage, not a correctness gate. the
  first cc-built love runs ai_tco=0; a later stage adds guaranteed tail calls
  (direct + indirect, the lvm shape) and flips ai_tco=1.

## the architecture (crew/moon/, every piece pure and lawed)

the kore/vi discipline: pure engines with law files, thin drivers, one
gate per stage. the pipeline, each its own file:

* **lex.l** -- text -> token list (pure). tokens carry file/line for
  diagnostics. lawed directly.
* **cpp.l** -- token list -> token list (pure!). macro tables as tablets,
  expansion + ## + #if evaluation + #include (the reader hands in file
  texts through a hook, so the laws feed includes as data). the census says
  this is a QUARTER of the work; it gets its own fuzz (macro expansion vs
  gcc -E, token-for-token).
* **parse.l** -- tokens -> AST + the type layer (one file to start; split
  types out if it swells). C's declarator grammar is the dragon; chibicc's
  incremental grammar is the map. AST as tagged lists, house-style.
* **gen.l** -- AST -> holo IR forms (pure). stack-machine codegen, chibicc
  style: every expression computes into r0, locals at frame offsets off
  holo's sp idiom. correct and dumb; the glaze is the fast path.
* **moon.l** -- the driver: files -> cpp -> parse -> gen -> holo assemble ->
  elf .o; then the system linker (`mooncc -c` first; `mooncc` calling ld is a
  convenience wrapper). the surface is
  `mooncc [-c] [-I dir] [-D name[=val]] [-o out] in.c [in2.c ..]` -- several
  inputs need -c and land each in the cwd as x.o (gcc-shaped); the old
  positional pair `mooncc [-c] IN OUT` still reads (two bare args, the second
  no .c); -I dirs search before the system pair on both include forms; a -D
  prepends a `#define` line to the source text before the one lex (so
  function-like -DF(x)=.. rides the normal macro path, and diagnostics under
  -D skew by the define count). the driver wears the cc CONVENTIONS
  (2026-07-28, gwen's call -- `CC=mooncc` drives a gcc-shaped recipe
  unchanged): the advisory families (-W.. -O.. -g.. -std= -f.. -pipe
  -static) ride through ignored, and an exe link still owing strong symbols
  pulls the runtime BY NEED, archive-fashion -- nolibc + the am math + the
  mksys sys leaf, compiled from the TOOLCHAIN ROOT (below; a set carrying its
  own am.o never meets a twin). -nostdlib/-ffreestanding turn the pull off;
  the SEMANTIC refusals stay loud (-shared, -Wl,'s payload, -m..) because an
  ignored one would be the silent-no-op trap in a cc suit. gate:
  test/gate/drv.sh (test_drv, test_all). its tail SEAT fires moon-main when `mooncc` is the
  program on the command line (the same trick as ain/cook), so mooncc stands
  alone as its own catted script -- it does NOT ride the kore multi-call
  dispatcher.

### the toolchain root (2026-07-28)

mooncc's own files -- our headers (crew/moon/include/, glibc-ABI-faithful but
NOT glibc's) and the runtime sources the implicit link pulls (nolibc.c + the am
math) -- are found through a two-rung walk, tried in order:

1. **the dev tree**, `crew/moon/` off the cwd;
2. **the installed nest**, `<seat>/../lib/love/moon/` -- the loader's own seat
   walk (love/prel.l), readlink /proc/self/exe. so ~/.love/bin/love finds
   ~/.love/lib/love/moon/, and a distro's /usr/bin/love finds
   /usr/lib/love/moon/. mk/install.mk lays them there (~300K).

before this every path in the driver was cwd-relative, so an installed mooncc
outside a source tree could not compile hello-world at all: `<stdio.h>` fell
through to /usr/include -- *glibc's*, whose stdio.h wants the compiler's own
stddef.h -- and the link found no libc. a nest that ships no C sources made it
structural. gate: test/gate/nest.sh compiles and links from a scratch cwd
against the installed nest (`cd` matters: from the repo root rung 1 serves and
rung 2 is never exercised).

⚠ the root is READ AT EACH CALL, never bound. mooncc rides a BAKED image, and a
captured seat folds the build tree's path into that image and rides it forever
-- prel.l carries the same warning, having learned it the hard way.

owing symbols with NO root in reach is its own diagnostic, naming the owed
symbols and the roots searched -- not a bare link-undef on whichever symbol
came first. an absent toolchain and an incomplete link are different
conditions and must not wear the same face.

### the seams to grow (owned by their threads)

* **holo**: inventoried 2026-07-06 -- it already carries div/rem,
  immediate shifts, setcc (`set cond d`), call/ret/sys, indirect jmp,
  scaled-index addressing, and an SSE2 double lane (x64). C still needs:
  SIZED loads/stores (ld1/ld2/ld4 + sign/zero extends; ld/st are 64-bit
  today, LANDED), shifts by REGISTER count (LANDED), an indirect CALL
  (callr -- function pointers are the lvm dispatch; LANDED 2026-07-06, x64
  FF /2 + arm64 BLR, holotest 177), and-address-of-label (la; LANDED).
  still open: double->int cvt, unsigned div/compare lanes,
  rip-relative/global addressing for the .o world, and the arm64 float lane
  for parity. each lands with holotest.l's objdump goldens.
* **elf.l**: today it lays runnable ELFs, now assembling at ORG = the load
  vaddr so an abs64 data pointer resolves (cc stage 4d -- static char*/fn-ptr
  tables). the STILL-open .o world needs RELOCATABLE output -- symbol table,
  .text/.data/.rodata/.bss, RELA relocations (PC32/PLT32/64/GOTPCREL minimum);
  a well-fenced extension with its own laws (readelf as oracle).
* **love.h**: an `__mooncc__` branch making ai_inline/ai_noinline/attributes
  plain no-ops. a core edit -- small, coordinated.
* **headers**: our own include/ for the freestanding subset (stdint stddef
  stdbool stdarg + declarations for the libc calls love.c/host make: math,
  mman, unistd, signal, setjmp, stdio slice). dodges parsing glibc entirely;
  stage 7 decides header-by-header what the host files still miss.

## the basement, and the go borrowings (gwen's direction, 2026-07-06)

two ideas to keep warm as the stages climb, neither committed yet:

* **a basement language under C, in the spirit of historical B.** the
  observation that seeds it: this compiler's core IS B-shaped already --
  one word type, typeless 64-bit registers, C's types a checking-and-
  conversion layer the stage-2b work laid ON TOP of the word core (sized
  memory ops at the edges, words in the middle). love itself is B-kin the
  same way (the word is the one basic type). so the basement may want to
  become a real, nameable layer: the typeless word language cc's gen
  already speaks internally, possibly with its own thin surface syntax --
  useful for runtime shims, the crt0, compiler self-tests, and as the
  honest semantic floor the C dialect desugars onto.
* **mooncc-specific syntactic refinements borrowed from go.** C is almost
  perfect; a few go conveniences might make the dialect nicer to live in
  without leaving C's semantics. candidates to weigh when the grammar is
  fuller (gwen picks): unparenthesized conditions with mandatory braces,
  := style short declarations, cleaner declarator spellings for the
  gnarly cases (function-pointer types especially). the fence stands:
  extensions, opt-in, never needed to compile plain C -- love.c stays the
  gate and it is written in C.

## the ladder (each stage lands green and useful on its own)

0. **the spike** -- LANDED 2026-07-06: `int main(){return 42;}` -> tokens ->
   AST -> holo -> elf64 -> runs, exit 42, matching gcc's exit. one true-up
   against the draft: stage 0 lays a RUNNABLE static ELF straight through
   the existing elf64 (the crt0 stub -- call main, r0 to the exit syscall --
   emitted inline), no .o and no system ld touched; the relocatable-.o seam
   moves wholly into stage 3 where globals make it real. the lexer landed
   fuller than the stage needs (all keywords, maximal-munch punctuators,
   both comments, line numbers): stage 1 eats it as-is.
1. **integer expressions + statements** -- LANDED 2026-07-06: the whole C
   int expression ladder (precedence climbing; && || short-circuit
   normalizing to 0/1; ternary, comma; compound assigns and ++/-- desugared
   at parse, only ('post ..) surviving), decls with initializers and block
   scoping, if/else, while, for (all clauses optional, C99 decl-init),
   break/continue. codegen chibicc-plain: r0 the value, binaries push-left/
   pop-r1/compute-into-r1/mov (holo's two-address lowering allows d = a,
   never d = b uncommuted -- the alias-dst scare taught this), locals off
   the r5 frame, the frame sized after the body and rounded to 16. ints are
   8 BYTES until stage 2's types; shifts take CONSTANT counts (holo's
   register-count shift is stage-2 seam work) -- both guarded by gen
   refusals in the laws. the battery lives in test/cc/*.c -- 15 programs,
   compile-run-compare vs gcc -O0 in test_moon, growing every stage. one core
   wrinkle found and dodged: a value binding woven after a lambda that
   transitively forward-references its consumer draws a load-time book read
   (a benign ;; missing scare) -- op tables sit above the lambdas now.
2. **functions** -- the call half LANDED 2026-07-06: definitions with
   parameters (spilled off the SysV registers rdi rsi rdx rcx r8 r9 = holo
   r6 r5 r2 r1 r7 r8 into frame slots), calls with args evaluated left to
   right / pushed / popped reversed into the registers, recursion and
   mutual recursion through prototypes (parsed, recorded, skipped by gen --
   holo's two-pass labels do the real linking), the frame register moved to
   r4/rbp (r5 is rsi, ARG 2 -- the map bites the unwary). fenced and lawed:
   at most six arguments AND six parameters (the stack tail is later seam
   work); stack alignment at calls is ours alone until libc (stage 6).
   fibonacci runs -- fib(10) through real recursion, in the battery. the
   battery lesson worth keeping: differential programs must be UB-FREE --
   pick(++i,++i,++i) is unsequenced, and gcc legitimately disagrees.
   the types half LANDED the same day: char/short/long/int with real
   memory widths -- holo grew the whole sized family first (ld1/ld2/ld4
   sign-extending, st1/st2/st4 truncating, sx1/sx2/sx4 in-register, and
   shlv/shrv/sarv register-count shifts with the count PINNED to r1, x86's
   CL making the contract for every backend), all 24 encodings verified
   against objdump/llvm-objdump on BOTH arches and frozen as holotest
   goldens (172 pass). in cc: types ride declarations and params, casts
   and sizeof(TYPE) fold through parse, char literals lex with escapes,
   variable shifts unfenced. the ALU stays 64-bit -- sound because signed
   overflow is UB and the battery is UB-free; the OBSERVABLE width effects
   (stores truncate, loads extend, casts convert, and an assignment's
   VALUE is the converted one) all ride the sized ops. sizeof(expr) stays
   out until expressions carry types. locals still take 8-byte slots --
   width matters at the memory op, not the offset.
3. **pointers, arrays, strings, globals** -- LANDED 2026-07-06, and with a
   smaller footprint than the draft planned: instead of opening the .o/ld
   seam, holo grew ONE op -- (la d label), pc-relative address-of-label
   riding the EXISTING rel32 fixup on x64 (lea rip-relative) and a new
   byte-granular adr21 kind on arm64 (adr), objdump-verified both arches
   (holotest 174) -- and elf64's one load segment went RWX, so globals and
   string literals live as LABELED RAW DATA after the code, no binutils
   touched. the relocatable-.o seam moves to where libc forces it (stage
   6/7). in cc: cgexpr got TYPED -- every expression answers (type forms),
   so pointer arithmetic scales by pointee size (ptr-ptr differences
   divide back), dereferences load by pointee width, arrays decay to
   addresses, and lvalues have one door (clval: address in r0 + pointee
   type) through which x, *p, and a[i] all assign. string literals lex
   with escapes and land NUL-terminated in the data tail; globals are
   collected in a first pass so use may precede declaration order within
   the file. fenced and refused with laws: ptr+ptr, deref of a non-
   pointer, assigning arrays, star-array mixed declarators, array-local
   and non-constant/string global initializers. the battery reaches 33:
   double indirection, *(a+i) loops, string char reads, global mutation
   across calls, swap through pointers, pointer difference and compare,
   a strcpy-shaped buffer walk.
4. **aggregates** -- the semantic half (4a) LANDED 2026-07-06: struct and
   union with the real C layout rules (members aligned to their own
   alignment, the whole rounded to the widest; unions flatten offsets to
   zero), nested aggregates, arrays of structs and struct members that are
   arrays, ANONYMOUS struct/union members splicing their fields into the
   parent (love.h's habit), self-referential struct pointers (linked lists
   walk), enum with values (constants FOLD AT PARSE), typedef (top-level),
   `.` and `->` (arrow desugars to (dot (deref ..)) and one lvalue door
   handles both), sizeof over every aggregate, switch with fallthrough as
   a flat marker list + compare-chain dispatch, do-while, goto/labels
   (function-scoped name mangling). the deep change: the PARSER CARRIES
   STATE now -- typedef names, enum constants, and the struct tag table,
   which rides out with the AST for gen's sizing (C cannot be parsed
   stateless; the lexer-hack wrinkle, met on schedule).
   the trap that cost the debugging hour: (two? x) tests PAIRHOOD -- both
   () and a text fail it; presence wants truthiness (texts net positive).
   the DECLARATOR DRAGON (4b) LANDED 2026-07-06: function pointers as a
   real type ('ptr ('fn)), the dispatch-table declarator T (*t[n])(..)
   (parameters BALANCE-SKIPPED -- gen calls untyped), a bare function name
   decaying to its address (holo (la)), a generalized call whose HEAD is any
   postfix expression ('call headexpr args) reaching the callee through the
   NEW holo (callr) -- indirect call, x64 FF /2, arm64 BLR, both objdump/
   llvm-mc frozen (holotest 177) -- after parking the pointer in callee-saved
   r3; and WHOLE-STRUCT ASSIGNMENT as a word-then-byte block copy (bcopy),
   so a = b and *p = *q copy structs. THE love.c dispatch tables now compile.
   battery at 46 (fn-pointer call, dispatch table, struct copy by value and
   through pointers, fn-pointer parameter).
   INITIALIZERS (4c) LANDED 2026-07-06: brace and designated initializers
   ('init (item..), item plain | ('dfield name val) | ('didx n val)),
   nesting for arrays of structs; a LOCAL zeroes the slot then fills (cgfill,
   runtime values allowed), a GLOBAL bakes a constant byte image (cgimage +
   imgbytes, string-into-char-array and the flexible-array member []); [] size
   is inferred from the initializer (string bytes+NUL, or the highest brace
   index). battery at 52.
   LABEL-VALUED GLOBALS (4d) LANDED 2026-07-06: a global initializer whose
   value is a LABEL bakes an abs64 fixup ('fix 8 abs64 label 0) -- the label's
   ABSOLUTE load address -- so char *s = "hi" (a rodata string), &global, a
   decayed array, and THE love.c PATTERN, a static (name, fn-ptr) table like
   struct Nif tab[] = {{"inc", inc}, ..}, all bake correctly. the mechanism:
   holo gained an abs64 relocation kind (x64 + arm64, both le64 of the target),
   and elf64 now assembles at ORG = the load vaddr (base + header) so labels
   carry absolute addresses -- pc-relative fixups are unchanged (the org
   cancels in target-site), so as/kernel/arm64 output is byte-identical; only
   abs64 reads the absolute address. gen's imgbytes emits the fixup for a
   pointer global (imlabel resolves the string/function/global/&global label),
   and the byte image mixes bytes and fixups so lengths go through imbytelen +
   a placement-flatten (imflat/imsort) instead of byte-indexed splice. the
   star-array fence lifted too: char *m[n] is now an array of pointers, so a
   string table char *m[] = {..} parses and bakes. holotest 179. still fenced
   for 4e: struct params/returns BY VALUE (assignment walks); local typedefs;
   case-label expressions; sizeof of an EXPRESSION (only sizeof(TYPE) today);
   a bare TOP-LEVEL fn-pointer array int (*t[n])(..) = {..} (ptop lays its own
   declarators, doesn't route through pdtor -- the struct-wrapped table works,
   which is what love.c uses). battery at 56.
   ENV TRAP paid: a catted app is `#!/usr/bin/env -S love` + the cat, so bare
   `mooncc` runs on the PATH `love` -- a STALE install mis-runs it (missing baked
   core like holo callr) and the heap grows unboundedly; the make gate is safe
   (m defaults to ./out/host/love). probe cc with `./out/host/love out/host/mooncc`,
   never a bare `mooncc`, until `make install` refreshes the PATH binary.
5. **the preprocessor** LANDED 2026-07-06: crew/moon/cpp.l, TOKEN-BASED (it sits
   between clex and cparse, so an identifier inside a string/char literal --
   already one opaque token -- is never mistaken for a macro). object +
   function-like macros; rescan to a fixpoint under Prosser's HIDESETS (the
   blue-paint 4th token element, so FOO->FOO stops); # stringize; ## paste
   (fold + relex); ... variadics with __VA_ARGS__; #define/#undef;
   #if/#ifdef/#ifndef/#elif/#else/#endif over a precedence-climbing integer
   const-expr evaluator (defined X resolved first, undefined ids -> 0);
   #include through an incf hook (moon.l's incload searches the source dir for
   "quoted", crew/moon/include + /usr/include for <sys>; guards work because the
   included file shares the macro tablet); adjacent-string concatenation;
   __LINE__/__STDC__. #error fails the compile; #pragma/#line ignored. fenced:
   _Pragma, __COUNTER__, the GNU ,##__VA_ARGS__ comma elision. moon.l now rides
   lex -> CPP -> parse -> gen. battery 56->59 (macros, conditionals, a guarded
   #include header). laws: 18 pps/token goldens in law.l.
   TRAP PAID (the whole stage's hard bug): a lambda PARAMETER named after a
   sibling :-local gets shadowed by boxfix's "capture by location" shared cell
   (immune to shadowing -- the :-local owns the pin, the param reads the stale
   cell), so subst's macro-body arg silently read cppgo's whole define line.
   also the function NAME `adj##` (a `##` in a nom) fed the confusion. fix:
   give the param a nom bound NOWHERE else (`mb`) and inline the sibling helper
   as a closure-local. the tell was `;; missing <nom>` weave-scares at LOAD.
   NEXT gate to add (stage 6+): gcc -E vs cc -E token streams on love.c's real
   headers + our include/.
6. **the long tail love.c names**: varargs (the SysV register-save dance --
   the hairiest single item in the plan), doubles through the xmm ABI,
   _Static_assert, anonymous members (LANDED 4a), the one compound literal.
   gate: every host/*.c compiles; love.c compiles.
   THE GEM LANE (6a) LANDED 2026-07-06: the `double` type as a first-class
   scalar riding the xmm register file. float literals lex (12.5, .5, 2., 1e9,
   3.14e-2, an f/F/l/L suffix consumed) into a 'flo token carrying a love gem;
   the value lives in f0 (its TYPE is the flag -- 'double vs the integer lane's
   r0), materialized by fbits, the compile-time IEEE-754 encoder ported from the
   glaze (the 52-bit mantissa makes frac*2^52 exact). the whole value path:
   double locals + globals (an 8-byte image via fbits), + - * / (f1=left/f0=
   right through the gp stack, the scalar-double ops), the six comparisons
   (ucomisd + the UNSIGNED setcc -- lt->below, gt->above, le->be, ge->ae),
   unary minus (0.0 - d) and !d (d == 0.0), int<->double conversion (explicit
   casts AND implicit in mixed arithmetic / assignment / init -- asflo/asint
   bridge the lanes), doubles in arrays and structs (ldsd/stsd at the element),
   ternary and pointer walks. holo grew ONE op -- cvttsd2si (x64 F2 REX.W 0F 2C,
   arm64 fcvtzs toward zero), both llvm-mc-verified (holotest 181). fenced for
   6b (they need the xmm CALLING convention): a double PARAMETER, a double
   RETURN (a bare `return <double>` refuses -- write `return (int)d`), a double
   CALL ARGUMENT; also `float` (4-byte, needs movss/cvtss) and ~ on a double.
   battery 59->63 (arithmetic, the six compares, a loop accumulator + negate +
   ternary, doubles in arrays/structs/globals with pointer walks); law.l gains
   the float-literal/type/value-path/fence goldens. all match gcc -O0.
   THE CALLING CONVENTION (6b) LANDED 2026-07-06: the xmm ABI, so double params,
   returns, and arguments compile -- what 6a fenced. the parser now threads a
   SIGNATURE table (ps 'sigs: name -> (rettype (paramty..)) from every fn
   definition and prototype), returned as a 4th value from cparse and handed to
   cgen; the AST shape is unchanged (no golden churn), and a function's own
   return type is read back through the name already in `g 'fn`. cgfn spills each
   parameter from its SysV register by type -- integers from r6 r5 r2 r1 r7 r8
   (<=6), doubles from f0..f7 (<=8), SEPARATE counters. a call places each
   argument by the callee's PARAM type (converting the arg to it -- an int into a
   double slot cvtsi2sd's, a double into an int slot cvttsd2si's), or by the
   arg's own type when the callee is unknown (an indirect call); the result rides
   f0 for a double-returning callee, r0 otherwise. a `return <double>` in a
   double function leaves f0; in an int function it truncates. the conversion
   fixes rippled to every scalar-store site (a double value into an int local /
   assignment / call arg goes through asint), and to every CONDITION site
   (if/while/for/do/ternary/&&/|| fold a double to `d != 0.0` via ctest, since a
   bare double left its bits in f0 where `cmp r0 0` reads garbage). the classify
   bug paid: a scalar param type is an ATOM, so `(two? pty)` is false -- test
   presence with `pty` itself (a type is truthy, () absent). battery 63->65
   (double add/scale/poly with int->double and double->int arg conversion, a
   double-returning recursion + a 6-double fan). still deferred to 6c: DEFINING
   variadic functions (va_list/va_start/va_arg -- the register save area),
   calling variadic functions (al = the vector-register count), _Static_assert,
   the one compound literal, and `float` (4-byte).
   VARARGS + _Static_assert (6c) LANDED 2026-07-07: variadic functions define,
   call, and walk their extra arguments, and _Static_assert folds at parse.
   the `...` becomes the signature's third slot -- a variadic BIT (ps 'sigs is
   now name -> (rettype (paramty..) variadic?)); pparams returns `(params rest
   1)` when it meets `...` after a named param. cc uses a SIMPLIFIED all-stack
   variadic ABI, doc'd AS SUCH (not SysV interop -- that waits for linking in the
   gate): a variadic CALL pushes every argument as an 8-byte stack slot (arg0 at
   the lowest address, a pad word when the count is odd so the callee sees a
   16-aligned frame), and pops them after; the variadic CALLEE reads its named
   params straight from positive frame offsets ([rbp+16+i*8], via `place`, no
   register spill and no save area), and va_arg walks a plain char* cursor 8
   bytes at a step. so <stdarg.h> (crew/moon/include/, ours -- gcc uses its own in
   the differential, both self-consistent) is three one-line macros over the
   builtins: va_start -> (vastart v) seeds the cursor at the first vararg slot,
   va_arg -> (vaarg v type) reads the slot and steps, va_end -> (vaend v) is a
   no-op. _Static_assert(const, "msg") folds its constant through `cfold` at
   parse: a true one vanishes to a (tdef) no-op marker (block scope handled by a
   (= k 'tdef) () arm in cgstmt), a false one REFUSES the whole compile. the va
   builtins are recognized in pprim (__builtin_va_start/_arg/_end); va_arg parses
   its type argument through pcty. battery 65->68 (int & pointer varargs with the
   align-pad path, double varargs behind a named double param, a tag-selected
   int/double mix, file- and block-scope _Static_assert with sizeof folds); law.l
   gains the signature-bit / va-form / const-fold-or-refuse goldens. STILL
   deferred (6d, small): the one compound literal and `float` (4-byte). the
   simplified ABI is a KNOWN gap: it does not interoperate with libc's printf
   until the gate wires real SysV varargs -- that's stage 7's linking moment.
   THE `float` SCALAR + THE COMPOUND LITERAL (6d) LANDED 2026-07-07: the last two
   items love.c needs (typedef float ai_flo_t; the `mm` macro's &(struct ai_r){..}).
   float is 4 bytes in MEMORY but shares the double register lane -- the core
   invariant is that a value in an xmm register is ALWAYS a 64-bit double, so a
   float LOAD widens (ldss + cvtss2sd) and a float STORE narrows (cvtsd2ss + stss);
   every loaded float types 'double, and only the DECLARED type (float vs double,
   read at the store/spill/global-image sites) drives the 4-vs-8-byte choice.
   fldf/fstf pick the lane; a global float bakes a single-precision image (fbits32,
   the 23-bit-mantissa cousin of fbits, + le4s). the ABI keeps floats as doubles in
   the xmm registers (self-consistent within cc; the callee narrows on spill),
   which is NOT gcc-SysV-compatible (deferred with varargs to stage 7's linker) but
   matches gcc -O0 exactly for values representable in single AND double -- the
   battery holds to those, so double intermediates round identically. holo grew
   FOUR ops (x64 + arm64, both llvm-mc-verified, holotest 181->189): cvtss2sd,
   cvtsd2ss (F3/F2 0F 5A; arm64 FCVT), ldss, stss (F3 0F 10/11 movss; arm64 LDR/STR
   Sd, disp scaled by 4). the COMPOUND LITERAL (type){init} parses to ('clit ty
   init): an unnamed object filled into a fresh frame TEMP (ntmp bumps the frame
   high-water like a local), whose ADDRESS is the lvalue -- so clval handles it
   (&(struct S){..}, (&(struct P){..})->a) and cgexpr loads it as a value (loadval,
   the shared deref/decay tail); an aggregate zeroes-then-fills (cgfill), a scalar
   unwraps its one-element brace. battery 68->70 (69-float: the whole value path +
   ABI + a float global/struct; 70-compound-literal: struct + scalar literals
   address-taken and read); law.l gains the float / clit goldens. RUNG 3 IS NOW
   FEATURE-COMPLETE for love.c's C subset; stage 7 (the linker + real SysV) is next.
7. **THE GATE**: cc-built love.c (+ host/*.c, system ld, ai_tco=0) boots the
   egg and runs `make test` green -- the corpus under a cc-built binary.
   this is the rung's kore-feature-complete moment. a multi-part integration
   gate, landing sub-rung by sub-rung:
   7a THE RELOCATABLE OBJECT (crew/holo/obj.l) LANDED 2026-07-07: `mooncc -c IN OUT`
   lays an ELF `.o` the SYSTEM linker consumes, so cc code links against gcc-built
   host/*.o + libc. where elf.l wraps assembled bytes in a static EXECUTABLE (one
   RWX segment, every label resolved), obj.l emits SEPARATE .text / .data sections,
   a symbol table (each function a global STT_FUNC, each global var a global
   STT_OBJECT, each referenced-but-undefined label an UNDEF), and RELA relocations
   for what the assembler cannot resolve within a section: an la into .data ->
   R_X86_64_PC32 via the .data section symbol (addend = data offset - 4), a call to
   an external symbol -> R_X86_64_PLT32, a data pointer to a label -> R_X86_64_64
   (via the target's section symbol + offset, or an UNDEF). intra-.text branches
   and calls (and a function-address la within the TU) stay pc-relative and resolve
   internally -- both ends ride .text and shift together, so the field is invariant.
   the split comes from cgen-obj: cgen-core now builds TEXT (functions, NO _start)
   and DATA (globals + strings) separately, and the `main` requirement moved to the
   EXECUTABLE path (a library TU with no main is a valid object). obj.l reaches the
   assembler internals through the `holo` book (o-walk reuses (holo 'lay) then
   patches text-local fixups / records the rest as relocations). x86-64 only for
   now (the arm64 reloc kinds differ; cc emits x64). GATE: the make .o smoke
   cc-compiles a lib + a main TU, links them with a gcc-built third TU via the
   system linker (-no-pie), runs, and matches an all-gcc build; probed live: cc<->
   gcc interop both directions, cc calling libc (abs/strlen), the love.c static
   (name, fn-ptr) table across an R_64 link.
   7b REAL SysV VARARGS LANDED 2026-07-07: cc's variadic ABI is now gcc-compatible,
   so ai_push (a variadic function DEFINED in love.c but CALLED from the gcc-built
   host objects -- love.h declares it) works across the toolchain seam. the va_list
   is the real 24-byte struct { int gp_offset; int fp_offset; void
   *overflow_arg_area; void *reg_save_area; } (crew/moon/include/stdarg.h), a typedef
   to __va_list_tag[1] so it DECAYS to a pointer when handed to another function
   (love.c's gvzprintf/ai_pushr take a va_list). the CALLEE prologue lays a 176-byte
   register save area (6 gp @ +0 step 8, 8 xmm @ +48 step 16) and addresses its
   named register-passed params INSIDE it; va_start seeds gp_offset = 8*named-gp,
   fp_offset = 48 + 16*named-xmm, overflow = rbp+16, reg_save = &area; va_arg walks
   the register area until its offset passes the limit (48 gp / 176 fp), then the
   overflow area, stepping. the xmm registers are saved unconditionally (reading an
   xmm never faults), so the callee needs no `al` guard; the CALLER sets al = #xmm
   args before the call (its variadic callee -- or libc's -- reads it). every call
   now routes through call-fixed (the variadic difference is just al), and va_start/
   va_arg use cgexpr (not clval) to get the struct pointer UNIFORMLY -- a va_list
   LOCAL (array) decays to its address, a va_list PARAM (a decayed pointer) loads
   its value, both landing the struct pointer. a parameter of ARRAY type now decays
   to a pointer (pparams -- the C rule, which the va_list param needs). probed live:
   cc<->gcc variadic BOTH directions, integer AND double varargs, va_list handed to
   a helper, array-param decay; battery 70->71 (71-varargs-sysv), the make gate
   links a cc-compiled variadic function against a gcc main. THE PAID BUG: a
   caller-side stack-OVERFLOW rewrite (for >6 gp / >8 xmm args) tripped the boxfix
   cell trap (a helper's params shadowing sibling :-locals) into a load-time
   `;; missing`-scare block (0% CPU, looks like a hang) -- reverted to register-only,
   so >6-arg calls still refuse (a shared limitation with fixed calls, a later
   rung). float at the ABI boundary stays cc's double-in-xmm (self-consistent,
   matches gcc for representable values); real single-precision is deferred -- it is
   off the 64-bit critical path (ai_flo_t is `double`; the `float` typedef is the
   32-bit variant).
   7c-i THE FREESTANDING HEADERS + IGNORABLE QUALIFIER/ATTRIBUTE SURFACE LANDED
   2026-07-07: crew/moon/include/ now carries stdint stddef stdbool math signal
   setjmp sys/mman unistd (they WIN over /usr/include for <> includes -- incload's
   sys? path), each minimal and freestanding so love.c never reaches into a glibc
   header. cc keeps NO linkage/qualifier state, so the parser gained `pquals`: a
   run of storage/qualifier keywords (const volatile restrict register auto inline
   extern static) and __attribute__((..)) / _Alignas(..) / _Noreturn / __inline
   (their parens balance-skipped) is SKIPPED wherever a type may begin (pbty) or a
   declarator carry it -- at each `*` in the star loops (so `void const *` keeps its
   pointer), before the name (attrs like the AI_NIF `__attribute__((section))`),
   before the tkbty? decl-vs-stmt gate in blocks (so `register int x;` reads as a
   declaration), and in pparams (so `volatile int b` is a parameter). unnamed
   prototype parameters now parse as ABSTRACT declarators (`int f(int, long);` --
   the form every system-header proto uses). the headers ride cc's SIGNED integer
   model for now: the uintN_t names typedef to signed bases of the right WIDTH (no
   `unsigned` keyword yet), and `signed`/hex/suffix literals stay out -- correct for
   this cc, refined in 7c-ii. gate: 72-quals.c (the qualifier + header-parse
   program, gcc=cc=37) folds into the auto-globbed battery; law.l proves the
   ignored-qualifier equivalences + the abstract-declarator proto. love.c itself still
   stops at a LEX error (hex/unsigned/inline-asm -- the later sub-rungs).
   7c-ii UNSIGNED + HEX LANDED 2026-07-07: the lexer gained hex literals (0x..),
   integer suffixes (u/U/l/L, consumed), and the missing char/string escapes (\a \b
   \f \v \? \e, via a shared `escv`) -- love.c now lexes end to end. the parser reads a
   type-specifier RUN and canonicalizes it (`unsigned long` -> 'ulong, `signed char`
   -> 'char, `long long` -> 'long, `unsigned` -> 'uint) into the new unsigned scalars
   'uchar 'ushort 'uint 'ulong. gen treats them by WIDTH like their signed twins but
   with unsigned semantics: a narrow load ZERO-extends (ldu1/ldu2/ldu4 + zx1/zx2/zx4
   in holo), `>>` on an unsigned left operand is LOGICAL (shr/shrv, not sar/sarv), a
   compare with either operand unsigned uses the below/above/be/ae condition codes,
   and `/`/`%` go unsigned (udiv/urem = xor rdx + F7 /6 div, added beside the signed
   cqo+idiv). a binary op is unsigned if EITHER operand is (simplified usual
   conversions), so the signedness threads through chains. holo grew ldu1/2/4, zx1/2/4,
   udiv/urem (x64 + a mirror on arm64: ldrb/ldrh zero-extend, ubfx, udiv+msub), all
   objdump/llvm-mc-checked (holotest 189 -> 205). the headers now typedef the uintN_t names to REAL
   `unsigned` bases. gate: 73-unsigned.c (a FNV-style unsigned hash + logical shifts +
   unsigned compare/div/mod + zero-extend, gcc = cc = 177) in the battery; law.l goldens
   the hex/suffix/escape lexing and the specifier-soup canonicalizer. love.c now advances
   PAST lexing to a parse error (7c-iii).
   7c-iii PART 1 (THE PARSE TAIL) LANDED 2026-07-07: the accumulated ~a-dozen parse/cpp
   gaps that love.c hits once it lexes, found by grinding cc through love.c form by form.
   cpp learned the OBJECT-vs-FUNCTION macro distinction: a macro `(` is a function macro
   ONLY when GLUED to the name (no space) -- the lexer now flags a glued `(` with a 4th
   token field (set when the prior char is an idchar), so `#define EOF (-1)` is an object
   macro whose body is `(-1)` while `#define M(x) ..` is a function macro; plus GNU
   `, ##__VA_ARGS__` comma-elision at zero variadic args. the parser gained: multi-
   declarator typedefs (`typedef long a, b;` binds BOTH via a top-level `tdeflist`),
   function-type typedefs (`typedef int F(int);`), a comma-list of function prototypes
   (`int f(int), g(long);` via a top-level `mproto`), sizeof of a TYPE (folds to a `num`)
   vs sizeof of an EXPR (a `szof` node gen sizes from the operand's type), a bare `void`
   return, a folded/EOF case label, the function-pointer cast type `(int (*)(int))`, an
   incomplete array `extern int v[];`, and a stray top-level `;`. gen grew the `szof`
   handler (clval-or-cgexpr for the type, then `li (tsz type)`). the mproto/tdeflist
   helpers are TOP-LEVEL siblings taking explicit params -- NOT nested -- to dodge the
   boxfix capture trap (a deep-nested recursive helper captures outer `:`-locals as
   unfilled cells -> `;; missing <name>` at runtime). gate: 74-ctail.c (sizeof expr/type,
   void return, folded case, fn-ptr cast, function-type + multi-decl typedef, object-vs-
   function macro, gcc = cc = 50) in the battery; law.l goldens each. love.c now parses
   through ~5000 lines and stops at the first 2D array (`lvm_t *const tbl[KN][KN]`).
   7c-iii PART 2 (MULTIDIMENSIONAL ARRAYS + THE mooncc BREAKOUT) LANDED 2026-07-07:
   the declarator now reads a RUN of `[n]`/`[]` bracket suffixes (a top-level `adims`
   helper) and folds them outer..inner into a nested arr type -- `int a[3][4]` is
   `(arr (arr int 4) 3)` (`mkarr`, a top-level sibling both pdtor and the global-decl
   `one` share). indexing needs NO new gen: `a[i][j]` desugars to the plain
   `*(*(a+i)+j)`, and gen's type-directed pointer arithmetic already scales `a+i` by
   the ROW size while a deref whose result type is still an array preserves the address
   -- so the layout and the access fall out of the existing generic machinery. a global
   2D table takes a nested-brace initializer; only the OUTERMOST dim may be `[]`, its
   count inferred from the init's top level (`int g[][3] = {{..},{..}}`). an array
   dimension may be an ENUM CONSTANT (`int m[KN][KN]`), folded through ps 'enums (the
   `dimval` helper threaded into `adims`/`pdtor` -- love.h's kind matrices are `[KN][KN]`).
   gate: 75-arr2d.c (row-major layout, `[i][j]` access, nested-brace + `[]`-inferred
   globals, gcc = cc = 18); law.l goldens the nested + enum-dim types. AND cc SPLIT OUT OF kore INTO ITS OWN
   APP `mooncc`: a catted `#!/usr/bin/env -S love -l` script (u-floor + asbook + elf/obj +
   crew/moon/{lex,cpp,parse,gen,cc}.l) whose tail SEAT in moon.l fires moon-main -- so a cc
   edit rebuilds only mooncc, never the whole kore cat, and a parallel kore rebuild can no
   longer tear the compiler mid-run. `make test_moon` and `make install` both target mooncc.
   7c-iii PART 3 (THE LAST OF THE PARSE TAIL) LANDED 2026-07-07 -- love.c NOW PARSES END
   TO END (all 925 top-level forms; localized form by form with a ptop-loop driver over
   the preprocessed token stream, since macro-expanded tokens carry the macro-SITE line).
   four gaps, each found by grinding: (1) DESIGNATED array initializers `[i]=v` were
   already parsed (`didx`) and gen'd -- only the index was num-only; now it folds a
   const-expr index so enum `[KMint]=..` works (the add/mul kind matrices). (2) HEX FLOAT
   literals `0x1.0p-53` (a base-2 exponent after `p`; a `hexlit` lexer helper mirroring
   `flolit`, the plain-hex-int path folded in). (3) array DIMENSIONS are now any integer
   CONSTANT EXPRESSION -- `adims` parses the dim with pexpr + cfold (subsuming the
   enum-constant case), so `ai_limb limb[64 / limb_bits]` folds. (4) BLOCK-SCOPED TYPEDEF
   SHADOWING: love.h makes `num`/`word` typedefs, and love.c uses them as local variable
   names; a local whose name is a typedef now HIDES it for the rest of the block (pblock
   pins the name to not-a-type on the declaration, restores at `}`), so `num + num` reads
   as an expression, not a cast -- and a sibling function still sees the type. gate:
   76-ctail3.c (hex float, const-expr dim, nested enum-indexed designated init, a shadowed
   local, gcc = cc = 41); law.l goldens each. the next wall is CODEGEN (`mooncc -c love.c`
   parses, then errors in gen) -- 7d territory.
   THE GEN CHOKE LIST (measured 2026-07-07 by a full-corpus instrumented run --
   log-and-continue wrappers on cgfn/cgexpr/cgstmt/cgdecl/cgimage): 15 of love.c's 611
   functions refused, plus the data tail. THE GEN TAIL, FIRST CUT (landed 2026-07-08,
   15 -> 10): (a) mproto SIBLINGS -- `double sin(double), cos(double), ..;` registered
   cos only in the sig table, so a fn-as-VALUE use (`Ap(.., ai_cos)`) refused; gen's
   pass 0 now sweeps the sig keys into the fns table. (b) &f -- address-of a function
   is the bare use's address, in code (the addr lane falls back to the fns table;
   locals still win) and in a global image (imlabel takes `&f`). (c) INDIRECT DOUBLE
   CALLS -- a call through a fn pointer typed its result 'long, so a double result
   read r0 not f0 (a SILENT miscompile, exactly love.c's lvm_math1 shape); the
   declarator dragon now carries the base type as the pointee's return --
   ('ptr ('fn ret)) through params/locals/typedefs/members/casts -- fn-name values
   get ret from the sig, and callr's result types by it. gate: 77-protoaddr.c
   (fn-ptr table image, &f/bare-f agreement, double through a pointer, gcc = cc = 42)
   + law goldens.
   THE LIST CLEARED (2026-07-08, same day, commit by commit) -- **`mooncc -c love.c`
   COMPILES END TO END: all 611 functions + the data tail, a ~514KB relocatable
   love.o.** the rest of the tail as it fell: ENUM-CONSTANT SHADOWING (a local named
   `N` was folded to `enum { N = 13 }`'s value -- a silent read miscompile; locals
   now shadow enum constants like the typedef shadow, the constant PULLED for the
   block and re-pinned at `}`); FOLDING IMAGES (a scalar initializer that cfolds is
   its number -- `(Bits>>3)` macro math; cfold learned value-preserving casts, so
   NULL and putcharm-word-math image; a label under a word-sized integer cast --
   `(word)nif_absent` -- is an abs64 fixup, so the nif/def tables lay); HONEST
   EXTERN (extern data is ('xdecl ..): addressable, NO storage, the reference an
   UNDEF symbol -- probed (word)&ai_stdin resolving to gcc's definition across the
   seam); >6-ARG CALLS (args 7+ ride the caller stack, arg7 shallowest, caller
   cleans, odd counts padded for 16-alignment; callee reads rbp+16+8k; probed
   cross-toolchain both directions); ALL the gcc BUILTINS as raw-x64 splices (the
   overflow trio via seto, clzll via bsr^63, isinf via bits<<1, inf/nanf constants,
   expect, trap = ud2, clear_cache a no-op) -- surfaced by tightening `direct` to
   KNOWN callees, each previously a silent undefined-symbol landmine; STRUCT BY
   VALUE (struct ai_zn as the SysV two-SSE-eightbyte xmm pair -- the cgexpr rep of
   a struct value is its ADDRESS in r0; returns load the pair, calls materialize a
   frame temp, args ride an x2 register-pair class, params spill 16 bytes; any
   other by-value struct still refuses) -- which surfaced the MEMBER-ORDER ABI BUG:
   `double re, im;` laid im-then-re (a double rev; only multi-declarator member
   lines bit, 4a's battery had none); and FUNCTION-TYPE TYPEDEF DECLARATIONS
   (`lvm_t lvm_ret0, lvm_cur;` registers sigs, lays NO storage -- it laid duplicate
   .data symbols, the 7d link's first wall). gates: 78-enumshadow 79-tables
   80-manyargs 81-builtins 82-znvalue (gcc = cc = 42 each) + laws.
   **7d LANDED (2026-07-08): the cc-built love BOOTS the egg and passes the WHOLE
   CORPUS -- 2831 tests green.** the recipe: `mooncc -c love.c love.o` (with
   `#define ai_tco 0` prepended -- cc emits no sibcalls, so the VM takes the
   trampoline dispatch, the same lane love0 exercises every gate) + gcc host
   objects built `-Dai_tco=0` + libc. the corpus was the differential oracle
   that named every remaining miscompile, each now a law + battery program:
   CALLEE-SAVED rbx (holo r3; the callr park / overflow builtins / va_arg walk
   clobbered it, and gcc -O2's main kept its argv-fold BOUND in ebx -- the boot
   spun forever, popped past the stack top, and wore an oom face; every fn now
   owns frame slot -8 for it, and the gate links a cc callee against an -O2
   caller holding its loop bound live across the call); cfold folds 64-bit
   MACHINE PATTERNS (an unsigned cast masks to width, >> under an unsigned left
   is LOGICAL -- the arithmetic fold pinned max-charm -1 and flipped spec.l's
   `word`); the conditional CONVERTS its arms (one flo arm makes the result flo;
   the int arm rode into ucomisd unconverted -- the eq lane's charm×twin face);
   NaN-HONEST float compares (unordered answers false everywhere but !=; < and
   <= compare swapped onto above/ae, ==/!= carry the parity bit -- holo grew
   set p/np); negative float literals image (the neg folds into the literal;
   fbits answers the SIGNED i64 pattern so the sign bit doesn't big); and cc now
   PREDEFINES `__x86_64__` (love-arch read the fallback "other" and the glaze
   handed holo an unknown target) while `__SIZEOF_INT128__` stays undefined --
   the bignum limbs keep the portable 32-bit branch, the lone `divq` asm stays
   out of reach. found on the way, in the CORE: a tco=0 host with the glaze
   baked segfaulted on its first hot loop (glazed code Continues by tail-jump --
   threaded-VM only); love.c now pins `love-tco` and auto.l keeps the pure
   interpreter on a trampoline build. and the house speed-law paid off:
   `mooncc -c love.c` took ~13 MINUTES -- holo's lay appended each lowered chunk to
   the tail of the item stream, a re-copy per instruction -- accumulating
   reversed made it linear: **4.3 seconds**. gates: 83-ternflo 84-nan + the
   rbx interop check. unions by value stay refused; running the cc build at
   ai_tco=1 (`make vmret`-honest sibcalls) is stage 8's flat-stack rung.
8. **the fixpoint + the flat stack** (2026-07-08):
   (a) THE FIXPOINT LANDED -- `cc(cc(love))` is BYTE-IDENTICAL to `cc(love)`. the
   cc-built love (tco=0), running mooncc, compiles love.c to an object bit-for-bit
   the same as the host-built mooncc does. self-hosting is a closed loop:
   `cmp aiA.o aiB.o` where aiB.o = love-cc0 compiling love.c. (the self-hosted
   compile is ~6x slower -- 24s vs 4s -- because the cc build runs the pure
   interpreter, no glaze; correctness, not speed, is the fixpoint's claim.)
   (b) GUARANTEED SIBCALLS LANDED -- a RET-position call tail-JUMPS: the
   epilogue reloads rbx and `jmp F` replaces `call F`, so deep tail recursion
   runs flat (the 50M-frame battery would need ~2GB of stack as plain calls;
   flat, it's a loop) and the lvm shape Continues by jmp. the rewrite is a
   LOCAL peephole (call immediately followed by the exact epilogue, or by a
   join label leading to it -- the nested-ternary return), gated per fn by an
   ESCAPE analysis: a frame address that becomes a VALUE (the & lane, a local
   array decaying, a struct-value rep, va_start's save area) pins `fesc` and
   the fn keeps all its calls; an lvalue's own load/store rides `lean` (lea's
   bytes, invisible to the gate) and stays eligible. holo grew `lean`; the
   overflow builtins' `&t` arg is argpos-exempt like a call arg. `make vmret`
   on the cc build drops from ~all-lvm-suspects to the tail (the gcc host has
   1). battery: the flat-recursion gate (self + mutual + fn-ptr) + gen laws
   (a self-recursive tail is `jmp rec` not `call rec`; a frame-escaping fn
   keeps its call).
   (c) THE GLAZE RUNS AT ai_tco=1. the cc build's first crash was NOT Sp
   threading at all -- cc predefined `__STDC_HOSTED__` = 0, so love.c compiled
   its FREESTANDING lanes: `toast`/`nif` copied native code into the plain
   heap (NX) instead of the hosted W^X mmap+mprotect arena, and a glazed
   native body jmped into non-executable heap and faulted (rip in the heap,
   the cell's value[0]/value[4] a heap addr not the W^X code). a cc-built
   object links against libc into a HOSTED program, so cc now predefines
   `__STDC_HOSTED__` = 1. with it: the glaze self-test passes under the
   cc-built binary (`test/glaze-x86.l` -- cc-compiled code running the
   glaze's OWN x86 native codegen), hot loops native-compile correctly
   (`(loop ...)` = 4999950000), and the 5 glaze-heavy corpus files that
   crashed now pass. the whole tco=0 corpus MISSED this because the glaze is
   the only executable-arena user and it's gated off on trampoline builds.
   (d) 16-BYTE STACK ALIGNMENT. `(run (list "true"))` crashed the FORKED
   child (SIGSEGV, SI_KERNEL, si_addr NULL -- a non-canonical control
   transfer) after glibc's fork child-cleanup, BEFORE execvp. NOT the fault
   barrier and NOT the `run` nif: the child faults INSIDE `__libc_fork`, on
   `movaps %xmm1,-0x140(%rbp)` in glibc's child-ONLY path (the parent
   branches away). `movaps` #GPs on a misaligned operand, so the ABI's
   16-byte stack alignment was violated -- and it was OURS. cc spilled an
   8-byte temporary with a bare `push` and HELD it across a nested call:
   the assignment target `&g` across `g = <rhs-with-calls>` (ai_evals_),
   binop operands, argument marshalling. rsp sat at 8 mod 16 for the inner
   call, so glibc's fork (the first callee that stores aligned SSE to
   rbp-relative slots) crashed. INVISIBLE to love.c's own code (it never
   movaps'es a frame slot) and to the -O0 gcc differential (both align
   correctly), so the whole corpus rode a skewed stack that only libc's
   fork child ever tripped. the fix (`gen.l`): a spill that outlives a
   nested call reserves a 16-byte cell (`spush`/`spop` = sub/st .. ld/add
   16), not an 8-byte push -- rsp stays 16-aligned at every call. register
   call-args ride 16-byte cells too (a later arg may hold a call);
   overflow args keep their 8-byte packing (the callee reads them packed,
   and their targets are love-internal, never a libc movaps path). WITH IT:
   the cc-built love runs `run`/`fork`, and the whole 2831-test corpus passes
   at ai_tco=1 with the glaze live. `test_moon` guards it with a
   `g=id(fork())` program whose child dies iff the stack is skewed. benches
   vs gcc are the flat-stack tail.
9. **stretch, as appetite allows**: arm64 parity through the same holo
   seam (the IR is neutral; gen.l shouldn't care), our own static linker
   (elf.l already lays executables -- close the last binutils door), -g
   line tables.

## inline asm (landed 2026-07-16 -- the minimal skeleton)

the GNU statement form, with ONE deliberate twist: the template is holo's
neutral TEXT (crew/holo/text.l's `asm-text` parses it, the baked assembler
encodes it), not AT&T -- so one template rides both targets wherever it
sticks to the neutral surface, and no new assembler exists anywhere.

    asm [volatile] ("li %0, 40" : "=r"(v) : "r"(x), "i"(3) : "memory");

* registers are the neutral file: x64 r0=rax r1=rcx r2=rdx r3=rbx
  r4=rbp(the frame) r5=rsi r6=rdi r7..r14=r8..r15; arm64 rN=xN. so a raw
  x64 syscall is `asm("sys" : : "r0"(nr), "r6"(a0), "r5"(a1), "r2"(a2))`.
* constraints: `"r"`/`"=r"`/`"+r"` pick a register, `"rN"` forms pin one,
  `"i"` an immediate (parse-time constant). `%0..%9` substitute (outputs
  first), `%%` a literal `%`. adjacent template strings concatenate.
* the body assembles AT CODEGEN into one opaque `('raw bytes)`: the IR
  passes barrier on raw, labels inside a template stay LOCAL to it, and no
  pass (nor arm64's r4->fp retarget) ever rewrites user instructions.
  external symbols can't be named in a template -- reach values through
  operands ("r"(&x) works; the address-taken local also fences deadst).
* operands stage through the machine stack, so calls inside operand
  expressions are safe, and any scalar lvalue output works (*p, a[i]).
  float/struct/bitfield operands refuse.
* allowed registers (operands + clobbers): x64 r0-r3 + r5-r10 (r3 rides
  every prologue's -8 slot; r4 is the frame and refuses), arm64 adds r4
  (x4, an argument register there -- 5+-arg syscalls need it). sp and the
  callee-saved r11-r14 refuse. clobbers (`"memory"`/`"cc"`/reg names) are
  validated but need no action yet: an asm-containing fn turns register
  HOMING off (gen.l's g 'hasasm, the 'sys-gate precedent), so nothing
  lives in a register across any statement. the allocator arc (the alive
  pass: per-call wrap sets + zero-crossing locals homing through calls,
  landed 2026-07-16 on BOTH targets -- arm64 homes ride r10-r14, one
  prologue mov each, wraps say fp) KEEPS this gate --
  asm-as-precolor+clobber-node waits for a consumer that needs homes in
  an asm fn.
* deferred until a consumer demands them: an AT&T template front-end (the
  Linux floor), "f" float operands, asm goto (Linux x86 requires it),
  named [sym] operands, top-level asm.

## testing (the house discipline, restated for C)

* every pure piece lawed in crew/moon/law.l from day 0 (lexer goldens, cpp
  expansions, parser ASTs printed and compared, layout/alignment tables).
* the differential oracle is gcc -O0: same source, run both, compare
  stdout + exit code -- the GNU-byte-identical spirit, one binary deeper.
  the battery starts at stage 1 and ONLY grows; every bug fixed adds its
  regression.
* a seeded expression fuzz vs gcc (the Brzozowski precedent): generate
  random well-typed int expressions/statements, both compilers, compare.
  csmith-class whole-program fuzz is a stretch goal.
* `make test_moon` gates laws + differential battery; stage 7 adds the
  corpus-under-cc-love run to test_all.

## size and pacing

chibicc is ~8k lines of C with tests; in love, with the assembler/ELF layers
already standing in holo, the compiler proper should land around 4-6k lines
(cpp ~1k, parse+types ~2k, gen ~1k, lex+driver ~500). the memory's estimate
stands: a year of evenings at full scope -- but stage 7 is reachable well
before that, and every stage before it is independently alive (stage 1 is
already a lawed calculator-to-ELF; stage 4 compiles real single-file C).

## the installed shape (2026-07-13): a wake shim over a baked image

`make install` no longer ships the cat as `bin/mooncc`. The compiler bakes WARM
into `lib/love/mooncc.image` (the live bake nif, doc/snapshot.md) and `bin/mooncc`
is a three-line sh shim: `love --wake mooncc.image -e "(moon-main (cuup (cup
cmdline)))" "$@"`. The whole-cat re-eval that every compile used to pay
(~1.5 s wall) is paid once, at bake: `mooncc -c love.c` 4.3 → 2.8 s wall, and a
small-file compile drops 0.77 s → 0.02 s -- gcc-class invocation latency.
The image is binary-specific (anchor-checked) and installs from the same
build as `bin/love` (strip keeps vaddrs, so the stripped install wakes it);
a mismatched pair falls back to a fresh boot with no moon-main, so never mix
builds by hand. The repo cat `out/host/mooncc` is unchanged -- probe with
`./out/host/love out/host/mooncc`, as ever.

## naming

crew `moon` (🍄 the glowing mycelium on holo's cave walls, a sibling to inle)
and the binary `mooncc` — chosen 2026-07-15, all-the-way-down from the `aicc` /
crew/cc/ working names. `mooncc` slots into the cc/gcc/tcc tradition and clears
the `moon`(MoonBit)/`moonc`(MoonScript) collision (the naming-lore memory holds
the sources). Like everything here, revisable.
