```love
; love -- a fully-curried language with an infix, low-paren surface that factors down to a tiny
; lisp core: `map (+ 1)` and `(3 = 1 + 2)` desugar through opfix to plain parens (one source of
; truth, both compilers). the core rides a tiny generic C runtime (love.c + love.h) plus a
; self-hosting compiler in love (love/{prel,ev,bao}.l). source is .l; the host binary is `love`.
; see README.md.
;
; this file ORIENTS -- how to work here, the traps, the vocabulary. the LAWS live in test/spec.l
; (the executable spec, each section's laws in the comment over the asserts that prove them, green
; on every target) and proof/rocq/spec.v (machine-checked axiom-free; test_proof + test_gen).
; settle any doubt by PROBING THE BINARY: never trust a prior over a one-line experiment. demos
; here show their result inline (`expr ; value`).

; --- how to work here (read this first) ---
; * `make test` is the DEV GATE -- every edit, ~20s: host + bootstrap love0 must BOTH print the
;   zz-fin "tests pass" summary (love0 EXACTLY twice -- a silent reader stop exits 0, so the exit
;   code alone proves nothing), plus vmret and waits (tools/waits.l: the only code that BLOCKS is
;   the scheduler, as a roster of every wait and the sentence that earns it; a new one reddens the
;   gate). `make test_slow` is the MERGE GATE -- before publishing, not per edit: the proofs,
;   gc/glaze/sat/holo/lux, tool diffs, arm64 + the qemu kernel + wasm. BETWEEN THE TWO run the
;   test_* targets covering what you touched -- most are subsecond (they wake the baked image,
;   test/test.mk; the EGG gates test_host/test_love0/test_gcheck and test_sat stay cold).
;   `make valg` for memory. one file: `out/host/love test/x.l` -- but the corpus runs CONCATENATED
;   in one global scope, so keep helpers local (give `:` a body).
; * SPEED IS A SIGNAL: every test runs in a second or two. a test that HANGS or crawls is a bug
;   announcing itself, never a slow bench to wait out -- the classic is a 0-byte lcat header make
;   thinks fresh, leaving a native lane unbound: one bug, two faces (infinite loop or crash).
; * the kernel has THREE boot doors, one ELF per arch: -kernel (x86_64 PVH stub, aarch64 EL1 MMU
;   stub -- test_kernel + test_kernel_arm64, nothing downloaded), UEFI (port/inle/uefi/, our own
;   BOOTX64.EFI; `make uefi`, test_uefi; doc/uefi.md), limine (iso/hdd + the run-* lanes; out/dl
;   feeds these two, `make clean` nukes it -- stash it if you use them). the LINK is ours on every
;   door (holo's ldkern lane, driven by port/inle/klink.l; `KLINK=lld` is the comparison), the
;   ASSEMBLY is ours (no .S anywhere: mkboot.l lays the bring-up, mkvec.l the interrupt tail;
;   `make test_vec` faults on purpose -- the one way to reach a stub), and the COMPILER is ours
;   (`KCC ?= mooncc`, doc/moon-kernel.md; `KCC=clang` + test_kdiff the differential, ~45s/arch).
;   ⚠ each KCC variant has its OWN odir and ELF (kccsuf/klsuf). ⚠ mooncc REFUSES -m flags rather
;   than ignoring them. editing love.h needs no clean (every object deps on $(love_h)).
; * CHECK A .l EDIT for balance before trusting it: `out/host/love crew/libra/libra.l <file>` (or
;   `make lint`) -- LIBRA ⚖ (crew/libra/, doc/libra.md), the .l-aware paren/bracket/brace +
;   unclosed-string scan. SILENT means clean; warnings point at the OPENER of an unclosed form.
;   `-w` also strips trailing whitespace. not in the test gate. balance is the default verb;
;   `libra fmt` reindents (not adopted), `libra serve` speaks lsp -- one scanner (lib/lint.l)
;   under all three. the `singleton` and `deprecated` rules ride a config and are OFF until it
;   asks; they only speak unless `(strict <rule>)` promotes one.
; * CONFIG IS SALT (lib/salt.l): (salt 'app) answers a settings tablet from ~/.love/etc/<app>.l
;   then ./.<app>.l over it -- any crew app, same door, the project file speaks last. a setting is
;   one form, head names it, tail is the value. ⚠ read as DATA with `sound`, never evaled.
;   ⚠ PRESENCE IS THE OPEN -- ask whether it OPENED, not whether it had bytes. ⚠ a non-setting
;   form is skipped in silence, and a dropped paren ends the read -- half a config beats none.
;   HOME is the one env var here. doc/libra.md.
; * C and docs EMBED love the .l sweeps miss -- grep on every rename: host/main.c,
;   port/inle/kmain.c, port/rp2040/main.c, port/playdate/ (main.c + cas.l), wasm/; and
;   index.html, whose examples + "; answers" are PROBED against out/host/love, never written
;   from memory -- re-probe on every rename or semantic change.
; * a bare all-punct symbol mid-list captures its left operand when code compiles (opfix) -- escape
;   in parens ((+) is + as a value); GLUED to a datum it is monadic instead (the valence law: space
;   your dyadics); quoted lists are data, operators plain.
; * arithmetic operators are DYADIC: `(+ a b c)` is `((+ a b) c)` -- APPLICATION, not a 3-way sum,
;   so it church-EXPONENTIATES ((+ 192 40 5) = (232 5) = 5^232, a bignum).
; * ⚠ `(f)` IS NOT A CALL -- (f) == f at zero operands, so a NULLARY helper hands its closure back
;   UNRUN and NOTHING ERRORS: `(go)` `(loop)` `(step)` never fire -- a silent no-op that keeps
;   biting loops and named-lets. fire every thunk WITH AN OPERAND, and let it be the UNIT:
;   `(go ())` -- () not 0. a 0 in a do-nothing slot is a 0-FOSSIL (sibling to the nil-tail
;   fossil): the ignored slot is the unit, not a number.
; * ⚠ NEVER PUT SCRATCH IN AN lvm_ -- no stack buffer, no address-taken local. the VM is
;   TAIL-THREADED; a frame forces the tail Continue() into a `ret` and the stack grows EVERY step,
;   so the fault is a stack overflow deep in some unrelated test, never a wrong answer. put the
;   body in an `ai_noinline static` helper taking `g` (rng_canon and host_cwd are the models;
;   Have first, the helper only bumps g->hp). `make vmret` catches this: the default love is
;   MOONCC-BUILT (test_fixpoint rebuilds it to the byte; test_raw the from-scratch cross-check),
;   so the fast gate disassembles mooncc's own emission.
; * ⚠ PRESENCE IS THE WRAPPER, NEVER THE NET -- the costliest recurring bug in this tree. every
;   nothing is nil by design ((), 0, "", @()), so ABSENCE AND EMPTINESS ARE INDISTINGUISHABLE by
;   the value alone: a "do I have one?" test written `(! x)` fails silently on every legitimate
;   empty (a 0-byte file, an empty blob, ""). carry presence OUT OF BAND: the `(1 x)` wrapper
;   tested with `two?`, or a separate predicate (`(two? (stat p))`, not `(! (fsize p))`). ⚠ and
;   `two?` is FALSE for a STRING -- it tests cons pairs -- the same trap wearing its other face.
; * quasiquote is GONE: the ` list ctor EVALUATES every element, so QUOTE the literal positions
;   (numbers self-eval, so bare). a lisp-primed hand writes the inverse on reflex -- check every
;   ` twice.
; * a corpus test that twirls a task must (catch p) it: an orphan stalls the kernel runner.
; * the repl reads each LINE as one expression (1 = 1 answers 1); files read forms. the shell
;   installs a default help (bao's shell-help): a scare prints `;; a b` and answers the zero
;   point, so the session survives every raise. FILE MODE's file-help prints the same face and
;   quits 1 on EVERY scare -- a missing name included, since the point it would answer is a fake
;   result wearing a real one's face. so ask presence OUT OF BAND ((member? 'x (names ())), never
;   `(lit? x)`, which reads x). a help is INSTALLED, not bound: (hear f)
;   writes the hot_help slot, (hear ()) uninstalls, (heard ()) reads -- `help` is not a book name.
; * python \b-sweeps treat - as a boundary: kebab names with capital segments mangle.
; * the CREW (crew/, the apps) rides over the core, each owning NON-OVERLAPPING files so a session
;   can take one in parallel: lux (the X11 window manager), inle (the freestanding kernel,
;   port/inle/), seed (the patch-set vcs, doc/seed.md; `make dist` bakes out/dist/love-x86_64,
;   the one-file download door -- `love up` defaults CC to the artifact's own mooncc verb, no
;   ambient toolchain needed), moon (the C compiler in love -- compiles love.c + host/*.c, holo
;   links, no gcc/glibc/ld: test_raw; `CC=mooncc` drives gcc-shaped recipes unchanged, test_drv),
;   rune (symbolic algebra on the q coin; its gate verifies the 2026 jacobian-conjecture disproof),
;   lush (the command shell 🐚, the distro's console shell AND its /bin/sh; gate test/host/sh.l,
;   doc/lush.md), and the DOCUMENT chain: lapiz (the markdown/html/roff lens, writes the man
;   pages) -> papel (the static site, `make site`) -> kiosko (the static web server; `papel -s
;   PORT` is a dev server). apps add nifs through the host/*.c glob + AI_NIF (no core edit);
;   love.c/love.h/host/main.c are CORE -- an app session needing a core change stops and asks the
;   core thread, never reaches in. the runnable ones install on PATH via `make install`.
; * MODULES: a baked service keeps its names off the global book, and the LAYERS ARE THE
;   RUNTIME'S -- no user-facing enter/leave. the chain: TOP is the defglob target, under it a
;   use-stack, orth last and read-only; run_program pushes the SESSION layer, one load = one
;   layer. `use` IS the loader: (use 'x) splices a registered module just below the top (bare
;   names resolve on the walk, never shadow yours), and on a MISS loads x from the SOURCE LIBRARY
;   (the frontend's static ai_libs table, .rodata) or the filesystem WALK: lib/<x>.l off the cwd, then
;   <seat>/../lib/<x>.l (the seat = the binary's home via readlink /proc/self/exe), then its
;   love/ subfolder. PATH picks the love, the love carries its library -- no env vars anywhere.
;   a STRING is an explicit path; a SLASHED name ('holo/x64, one symbol) INCLUDES lib/x/y.l into
;   the current head -- no layer, no registration. module files carry NO brackets; their macros
;   land in their own book's macro slot and RIDE THE SPLICE (macroget walks the chain). the boot
;   splices keep rng/kanren/bao ambient so the corpus reads rand/unify/reads bare; holo registers
;   NON-ambient. (from 'holo 'assemble) is the OPAQUE accessor: currying reaches a member, 'keys
;   introspects, a missing module answers () (the presence guard), (from ()) lists the registry.
;   baked consumers FOLD their bare refs at their own compile, so LOAD ORDER IS THE SCOPE: a use
;   must precede its readers' compiles. laws: spec.l's modules section + test/host/loader.l.
;   ⚠ a body-less-`:` binding whose name COLLIDES with a module nom leaks and CLOBBERS the binding.

; --- vocabulary & house style --- the words here are a VOCABULARY -- never "terminology" or
; "nomenclature"; a vocabulary is living and chosen, warm not clinical. the style is COZY:
; lowercase, plain, a touch playful, names that earn their keep. we frame in the GREEN -- name
; what a value IS and KEEPS, not what it lacks. the surface stays small ((names ()) is the whole
; vocabulary). the celestial numerics: a CHARM is a fixnum (a word); a SUN a full-word integer
; (the charm's overflow, one rung under big); a STAR is a self-netting scalar
; (charm/sun/big/gem/twin-gem); a GALAXY is a tray of stars; a CONSTELLATION is any numeric -- a
; charm is a star is a constellation. a GEM is a float, a TWIN GEM a complex ((re,im); `twin`
; pairs them). the global env is the `book` -- *the* book, the outermost one; a map is a book, a
; tablet a little book, a global "pinned in the book under <name>".
;
; COMMENTS earn their keep the same way, in C and .l alike: one or two lines, inline where
; possible; paragraph blocks near-forbidden. a comment states what the code CANNOT -- a ⚠
; invariant, a wire format, a contract -- one breath each. never what the next line does, why an
; edit was correct, or the bug it once fixed (a fixed bug's story is git log; delete such
; comments on sight, and delete any orphan whose code is gone).

; --- the shape of it --- one cell is one word: a fixnum is a tagged odd word, anything else a heap
; object whose first word is its hot -- a live external reference, the wire out of the heap to the
; ap that runs it. every operation is *fully generic*: it dispatches on a value's *kind*, and the
; kinds form a lattice that is literally the diagonal of the dispatch tables. that ONE lattice is
; read FOUR ways: dispatch (the enum), order (the true-blue bands), ALGEBRA (each band carries the
; strongest +/* theory its rep affords, net the lone measure-hom threading them), and ABSTRACT
; DOMAIN (a compile pass climbs it by join to a fixpoint -- doc/proto/kinds.l). the VM is
; tail-threaded (aps tail-jump, never return -- `make vmret` checks it) over a two-space copying
; heap. the C core is tiny; most of the language is love closures installed reflectively from the
; prel, then laid into a heap image (the *egg*) at compile time.

; --- everything is a function --- (f x y) == ((f x) y) and (f) == f, so application is just
; left-to-right currying. numbers are church numerals, a list of numbers an exponential tower, and
; data self-applies (indexes). asserts in spec.l read INFIX -- (3 = 1 + 2) is ((= 3 (+ 1 2))),
; folding by GRIP and, at equal grip, by the operator's HAND -- arithmetic is LEFT-handed
; ((1 - 2 - 3) is -4), everything else right -- sound by (f) == f. the two pillars:
; demo:
(0 5)                ; 1       0 is const-1
(1 5)                ; 5       1 is the identity
(0 0 x)              ; x       const of const is id: (0 0 x) = ((0 0) x) = (1 x) = x
(3 2)                ; 8       (n x) = x**n
(2 3 4)              ; 262144  the tower 4**(3**2)
(map (+ 1) '(1 2 3)) ; (2 3 4) currying *is* partial application
((/ 1 2) 9)          ; 3.0     (1/2 x) = sqrt x

; --- three special forms --- `:` is letrec*/sequence, `?` is cond, `\` is lambda (and, with one
; operand, quote). everything else is a function call.
; demo:
(: a 1 b 2 (a + b))          ; 3     : binds in source order; the last form is the result
((\ x y (x + y)) 3 4)        ; 7     \ args.. body -- a lambda, auto-curried
(\ (1 2))                    ; (1 2) one operand: \ is quote, so 'x is just (\ x)
(? 0 'a (1 < 2) 'big 'else)  ; big   ? -- test/result pairs, then a final else
; `:` doubles as sequencing (bind `_` for effect); `(f x)` on the left is define-sugar; a body-less
; top-level `:` leaks its bindings to the global scope (how tests share helpers). a body-having `:`
; is ONE SCOPE: every name binds over the whole form, so a read before the pin is the MISSING
; condition carrying the binding site's nom -- no read escapes to an outer binding of the same
; name. rebinding a name still reads the previous value (the sequence law); recursion among lambda
; bindings resolves lazily. an EMPTY form is its head's value -- so (:) (?) (\) all read (), and a
; nullary helper call is the same law and the same trap (the ⚠ bullet up top).
; demo:
(: (twice f x) (f (f x)))    ; defines twice; (twice (+ 1) 10) ; 12
(: x 1 x (x + 1) x)          ; 2     the sequence law: a rebind reads the previous value
(:)                          ; ()    an empty special form reads its head: the zero point
(: (go x) (+ x 1) (go ()))   ; 1     fire a thunk with the UNIT; (go) ALONE is go UNRUN, never 1

; --- true and false, in one breath --- false is *nothing*: whatever NETS <= 0. the net is the
; complex-valued measure -- a number its own value, text the sum of its charms, a symbol its
; spelling, a list or array the SUM of its elements' nets (spine only, recursive, unclamped).
; `!` (nil?) reads the net's sign, `$` (sat) is the ONE saturating clamp onto the green charms
; (!x == (0 = $x)), and `tally` is the COUNT -- how many, not how much. every value wears a COLOR
; by its net-sign: GREEN nonnegative, RED negative, BLUE the zero floor -- true iff POSITIVE
; green; every nothing ((), 0, "", @(), #(), ~(0 0)) is blue. a TWIN GEM splits the question: the
; REAL PART gates, the MAGNITUDE measures, so a PURE PHASE is blue. ⚠ the ONE place TRUTH AND THE
; TOTAL ORDER PART: `i` sorts above 0 but nets nothing (C admits no order compatible with its
; arithmetic -- truth cannot rest on which root we named). no "truthy"/"falsy": true and false are
; the bits of `!!$`. spec.l's true-and-false section, doc/measures.md.
; demo:
!0  !""  !()  !-5  !'(-2 1)   ; nothing -> false: zero, empty, red at any rank
$'(1 2 3)            ; 6       $ sums the nets, then clamps once
(!"" = 0 = $"")      ; true    the invariant !x == (0 = $x)

; --- the reference --- test/spec.l carries the whole surface, section by section -- types &
; predicates, arithmetic (an undefined op answers (), the unit rides through every lane), order &
; equality, comparing functions, + and * generic, numeric functions (sine/cosine/log the only
; transcendental nifs -- power IS application), identities, complex, arrays, chains & lists,
; strings & mints, hashes (peep the TOTAL presence test), casks, reader operators, macros,
; control (help/welp, missing, apcap), i/o & ports (sound takes TEXT -- a string or a charlist --
; and its return IS the read protocol: (datum . rest) with the rest ALWAYS a charlist, () at a
; clean end, the SYMBOL 'torn mid-shape -- spelled at each site, not a book name), bootstrapping.
; each law lives in
; its section comment; the asserts below keep it honest. deep dives: doc/measures.md,
; test/operator.l + test/infixop.l, test/help.l test/missing.l test/apcap.l, proof/rocq/spec.v.

; --- bootstrapping --- the C core is minimal; the key semantics are l closures installed from the
; prel and shared by both compilers:
;   numap -- number application (x**n, or compose ($ n) times).
;   add/mul -- `+` and `*` of functions are church add and compose, so numerals agree.
;   opfix -- the operator factor pass: sigil surface -> core source, run FIRST.
;   boxfix -- the letrec* capture-by-location rewrite: a forward reference indirects through a
;     CELL keyed by the binding site's nom; a pre-fill read IS the missing condition.
;   wev -- the pre-pass: expand macros, apply boxfix, fold pure globals, mark apply strategy.
;   maps -- #(..)/map expand to nested pins.
; the *egg* (love/egg.l): warm the egg and the evaluator SITS on it twice -- compile the compiler
; with the C bootstrap, recompile the corpus through itself -- then the hatchling installs as `ev`
; in the image at C compile time; `born` records the hatch time. just before birth the egg MOPS UP
; every runtime-internal nom: the raw cell nifs (peek poke seek spin -- spin stays, it's
; ultimate), the compiler's machinery, every hot lvm_* pointer, the `book` itself. compiled
; references were folded, so only the noms die; noms the printer/reader/expanders EMIT stay, as do
; the C-resolved hooks (num-ap add mul). the shell core (love/bao.l) is a REGISTERED MODULE: the
; user verbs (read reads welp wrap) re-pin at its foot, the plumbing stays sealed -- frontends
; eval ((from 'bao 'bao) 0) / ((from 'bao 'shell) 0), (from 'bao 'keys) is the manifest.
; demo:
(lit? ev)            ; true    ev is installed in the image
born                 ; a fixnum (the hatch time) post-egg; unbound pre-egg
macros               ; ()      mopped up after birth -- off the book, so the nom reads nothing

; --- under the hood --- an op at two is an NxN table indexed by the two kinds; at one, its
; diagonal; the three core tables are + , * , and apply. a both-fixnum fast path skips the table;
; otherwise one indexed jump picks a lane that widens only as far as the operands need. the love/
; layer (prel ev bao cli egg) drips into every frontend: host, freestanding kernel
; (x86_64/aarch64), wasm. PREL IS THE LANGUAGE AND STAYS MINIMAL -- a library is its OWN love/*.l;
; mk/lib.mk lcats it to out/lib/<name>.h automatically, and EVERY post-egg layer is a MODULE
; (coin, rng, q, kanren, overlay, uu, bao, holo, glaze, rune ..): the frontend registers the lcat'd
; constant (a row in the frontend's ai_libs table) and its boot text says (use 'x); the boot rebinds the one-name surfaces there
; too, ⚠ ALWAYS UNDER THE MODULE'S OWN NAME ((: uu (from 'uu)), (: overlay (from 'overlay))) -- an
; accessor bound under some other name means one word for both a book and whatever else wears it,
; and the lane that skips the rebind then hands out the other thing in SILENCE (`parse` did exactly
; that to holo/text.l for as long as inline asm existed). overlay's ev hook must land in ORTH,
; which only the boot can write.
; the embed sites, SEVEN: host/main.c (twice -- love0's sed-wrapped <name>0.h twins need a gl0_h
; entry), wasm/host.c, port/inle/kmain.c, port/playdate/main.c, port/mps2/main.c,
; port/teensy41/main.c; each wants a header dep in its build file. the EGG's own door takes THREE
; texts -- `ai_egg_(g, egg, p1, corpus)`, TEN call sites, `corpus` being prel.h and ev.h JUXTAPOSED
; (p1text mints a fresh list where p0onto extends the one on the stack, so the corpus is ONE p1
; read) -- and STITCHES the corpus (p0 reads egg + p1, p1 reads the rest), so the C reader's sigil
; half is off the boot path and P1.L IS THE ONLY .l HELD TO THE PURE LISP SUBSET (egg.l too, via
; applyq's driver door). ⚠ prel rides p1: an `ai_evals_` bundling p1's text WITH a later one puts
; both on p0, because readtext picks its reader once per call. coin/rng/q/kanren/uu ride host, love0, wasm and the K_TEST kernel; the playdate workbench
; takes q + kanren + rune; a shipped kernel takes uu + bao. EVERY frontend opens its session with
; ai_layer_ after boot (bakers never push; wakers always do). ⚠ a layer leans only on what
; SURVIVES BIRTH -- wrapping a mopped nif means taking it off egg.l's mop list. ⚠ a post-egg
; layer cannot add an OPERATOR: `operators` is mopped, the grammar closes at the hatch; `fixity`
; is the one door left onto the table (it answers the row it replaced, and a refused shape rolls
; back and SCARES). build codegen lives in love under tools/; the C is freestanding,
; -Wall -Wextra -Werror.
```
