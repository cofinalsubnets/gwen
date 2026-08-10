```love
; love -- a fully-curried language with an infix, low-paren surface that factors down to a tiny
; lisp core: `map (+ 1)` and `(3 = 1 + 2)` desugar through opfix to plain parens (one source of
; truth, both compilers). the core rides a tiny generic C runtime (love.c + love.h) plus a
; self-hosting compiler in love (love/{prel,ev,bao}.l). source is .l; the host binary is `love`.
; see README.md.
;
; this file orients -- how to work here, the traps, the vocabulary. the laws live in test/spec.l
; (the executable spec, each section's laws in the comment over the asserts that prove them, green
; on every target) and proof/rocq/spec.v (machine-checked axiom-free; test_proof + test_gen).
; settle any doubt by probing the binary: never trust a prior over a one-line experiment. demos
; here show their result inline (`expr ; value`).

; --- how to work here (read this first) ---
; * `make test` is the fast default gate, run it as a quick check. `make test_slow` is the slow gate,
;   run it before committing. `make test_extra` is the really slow gate, run it before merging to main.
;   ⚠ read the summary, not the exit code: host and love0 must each print the zz-fin "tests pass"
;   line (love0 twice) -- a silent reader stop exits 0, so green proves nothing on its own.
;   ⚠ never `| tail` a gate: each target prints its summary as it finishes, and the dot stream
;   buries them. `make test 2>&1 | grep -aE "tests pass|FAIL|Error [0-9]|No rule"` is the reading.
;   between the tiers run the test_* targets covering what you touched (test/test.mk; most are
;   subsecond off the baked image, the egg gates test_host/test_love0/test_gcheck + test_sat stay
;   cold). `make valg` for memory, `make vmret` for the tail-jump law, `make waits` for the blocking
;   roster. one file: `out/host/love test/x.l` -- but the corpus runs concatenated in one global
;   scope, so keep helpers local (give `:` a body).
; * speed is a signal: every test runs in a second or two. a test that hangs or crawls is a bug
;   announcing itself, never a slow bench to wait out -- the classic is a 0-byte lcat header make
;   thinks fresh, leaving a native lane unbound: one bug, two faces (infinite loop or crash).
; * the kernel has three boot doors, one ELF per arch: -kernel (x86_64 PVH stub, aarch64 EL1 MMU
;   stub -- test_kernel + test_kernel_arm64, nothing downloaded), UEFI (port/inle/uefi/, our own
;   BOOTX64.EFI; `make uefi`, test_uefi; doc/uefi.md), limine (iso/hdd + the run-* lanes; dl/
;   feeds these two and survives `make clean` -- `make distclean` is what asks the network
;   again). the link is ours on every
;   door (holo's ldkern lane, driven by port/inle/klink.l; `KLINK=lld` is the comparison), the
;   assembly is ours (no .S anywhere: mkboot.l lays the bring-up, mkvec.l the interrupt tail;
;   `make test_vec` faults on purpose -- the one way to reach a stub), and the compiler is ours
;   (`KCC ?= mooncc`, doc/moon-kernel.md; `KCC=clang` + test_kdiff the differential, ~45s/arch).
;   ⚠ each KCC variant has its own odir and ELF (kccsuf/klsuf). ⚠ mooncc refuses -m flags rather
;   than ignoring them. editing love.h needs no clean (every object deps on $(love_h)).
; * check a .l edit for balance before trusting it: `out/host/love crew/libra/libra.l <file>` (or
;   `make lint`) -- libra ⚖ (crew/libra/, doc/libra.md), the .l-aware paren/bracket/brace +
;   unclosed-string scan. silent means clean; warnings point at the opener of an unclosed form.
;   `-w` also strips trailing whitespace. not in the test gate. balance is the default verb;
;   `libra fmt` reindents (not adopted), `libra serve` speaks lsp -- one scanner (lib/lint.l)
;   under all three. the `singleton`, `shadow` and `deprecated` rules ride a config and are off
;   until it asks; they only speak unless `(strict <rule>)` promotes one.
;   `libra infix` / `libra unfix` are the two directions of the factor pass: unfix IS opfix
;   printed, infix is its right inverse (lib/unfix.l, gated by test/host/unfix.l -- prefix is
;   a fixed point, so every proposal is checked and the parenthesized spelling always wins a
;   tie). ⚠ both print from the DATUM: comments are not carried, which is why neither is a
;   mode of fmt and neither has `-w`.
; * config is salt (lib/salt.l): (salt 'app) answers a settings tablet from ~/.love/etc/<app>.l
;   then ./.<app>.l over it -- any crew app, same door, the project file speaks last. a setting is
;   one form, head names it, tail is the value. ⚠ read as data with `sound`, never evaled.
;   ⚠ presence is the open -- ask whether it opened, not whether it had bytes. ⚠ a non-setting
;   form is skipped in silence, and a dropped paren ends the read -- half a config beats none.
;   HOME is the one env var here. doc/libra.md.
; * C and docs embed love the .l sweeps miss -- grep on every rename: host/main.c,
;   port/inle/kmain.c, port/rp2040/main.c, port/playdate/ (main.c + cas.l), wasm/; and
;   index.html, whose examples + "; answers" are probed against out/host/love, never written
;   from memory -- re-probe on every rename or semantic change.
; * a bare all-punct symbol mid-list captures its left operand when code compiles (opfix) -- escape
;   in parens ((+) is + as a value); glued to a datum it is monadic instead (the valence law: space
;   your dyadics); quoted lists are data, operators plain.
; * arithmetic operators are dyadic: `(+ a b c)` is `((+ a b) c)` -- application, not a 3-way sum,
;   so it church-exponentiates ((+ 192 40 5) = (232 5) = 5^232, a bignum).
; * ⚠ `(f)` is not a call -- (f) == f at zero operands, so a nullary helper hands its closure back
;   unrun and nothing errors: `(go)` `(loop)` `(step)` never fire -- a silent no-op that keeps
;   biting loops and named-lets. fire every thunk with an operand, and let it be the unit:
;   `(go ())` -- () not 0. a 0 in a do-nothing slot is a 0-fossil (sibling to the nil-tail
;   fossil): the ignored slot is the unit, not a number.
; * ⚠ scratch in an lvm_ may not be LIVE AT THE TAIL. the VM is tail-threaded; anything the
;   frame still owes at the jump turns the tail Continue() into a `ret` and the stack grows
;   every step, so the fault is a stack overflow deep in some unrelated test, never a wrong
;   answer. a local read for the last time BEFORE the jump is fine and costs nothing --
;   lvm_udprecv's datagram buffer and lvm_accept's fd are the models; the frame is torn down
;   and then jumped from (`add $N,%rsp; jmp`). what is barred is a local whose ADDRESS outlives
;   the body -- handed to the callee, or read after. when the body genuinely needs to survive
;   the jump, put it in an `ai_noinline static` helper taking `g` (rng_canon and host_cwd are
;   the models; Have first, the helper only bumps g->hp). two instruments, not one: `ai_musttail`
;   is owed rather than opportunistic, so a shape that cannot jump REFUSES at compile; `make
;   vmret` then disassembles what was emitted. the default love is mooncc-built (test_fixpoint
;   rebuilds it to the byte; test_raw the from-scratch cross-check), so the fast gate reads
;   mooncc's own emission. ⚠ vmret reads the x86-64 host binary only -- a cross lane is on you.
; * ⚠ love has no global state -- a mutable global in C is a bug, never a shortcut, and adding one
;   is FORBIDDEN. state rides `g` (a field) or a parameter, a buffer rides `g->hp` or the caller,
;   a table that never changes is `const`. a global sits outside the heap: the collector cannot
;   trace it, the image cannot bake it, and two tasks share it without asking. the standing
;   exceptions are the immortals the image codec locates BY ADDRESS (ai_stdin/out/err, the port
;   vtables, the ai_baked_image slot) -- those are load-bearing; every other one is owed a fix.
;   temporary INSTRUMENTATION is the one licence: a counter or a probe while you hunt something.
;   it leaves with the hunt -- a probe still in the tree at commit is the bug it was chasing.
; * ⚠ never call malloc/free (calloc/realloc too) directly -- the allocator is one door on `g`:
;   `g->alloc(g, p, n)`, n>0 reserves n bytes, n==0 frees p, answers the block or NULL. it is a
;   HOOK so each seat supplies its own -- the freestanding kernel and a device heap have no libc
;   malloc at all, which is why ai_image_load_m takes the allocator as a parameter. love.c's
;   ai_libc_alloc (with the two decls it needs) is the one site that may name them: it IS the hook.
; * ⚠ presence is the wrapper, never the net -- the costliest recurring bug in this tree. every
;   nothing is nil by design ((), 0, "", @()), so absence and emptiness are indistinguishable by
;   the value alone: a "do I have one?" test written `(! x)` fails silently on every legitimate
;   empty (a 0-byte file, an empty blob, ""). carry presence out of band: the `(1 x)` wrapper
;   tested with `two?`, or a separate predicate (`(two? (stat p))`, not `(! (fsize p))`). ⚠ and
;   `two?` is false for a string -- it tests cons pairs -- the same trap wearing its other face.
; * quasiquote is gone: the ` list ctor evaluates every element, so quote the literal positions
;   (numbers self-eval, so bare). a lisp-primed hand writes the inverse on reflex -- check every
;   ` twice.
; * a corpus test that twirls a task must (catch p) it: an orphan stalls the kernel runner.
; * the repl reads each line as one expression (1 = 1 answers 1); files read forms. the shell
;   installs a default help (bao's shell-help): a scare prints `;; a b` and answers the zero
;   point, so the session survives every raise. file mode's file-help prints the same face and
;   quits 1 on every scare -- a missing name included, since the point it would answer is a fake
;   result wearing a real one's face. so ask presence out of band ((member? 'x (names ())), never
;   `(lit? x)`, which reads x). a help is installed, not bound: (hear f)
;   writes the hot_help slot, (hear ()) uninstalls, (heard ()) reads -- `help` is not a book name.
;   ⚠ a help takes (a b), the condition and nothing else, and THE ARITY IS THE PROTOCOL: a stale
;   3-arg one under-applies to a closure, which is truthy, and the raise site takes it as a value.
; * python \b-sweeps treat - as a boundary: kebab names with capital segments mangle.
; * the crew (crew/, the apps) rides over the core, each owning non-overlapping files so a session
;   can take one in parallel: lux (the X11 window manager), inle (the freestanding kernel,
;   port/inle/), seed (the patch-set vcs, doc/seed.md; `make dist` bakes out/dist/love-x86_64,
;   the one-file download door -- `love up` defaults CC to the artifact's own mooncc verb, no
;   ambient toolchain needed), moon (the C compiler in love -- compiles love.c + host/*.c, holo
;   links, no gcc/glibc/ld: test_raw; `CC=mooncc` drives gcc-shaped recipes unchanged, test_drv),
;   rune (symbolic algebra on the q coin; its gate verifies the 2026 jacobian-conjecture disproof),
;   lush (the command shell 🐚, the distro's console shell and its /bin/sh; gate test/host/sh.l,
;   doc/lush.md), and the document chain: lapiz (the markdown/html/roff lens, writes the man
;   pages) -> papel (the static site, `make site`) -> kiosko (the static web server; `papel -s
;   port` is a dev server). apps add nifs through the host/*.c glob + AI_NIF (no core edit);
;   love.c/love.h/host/main.c are core -- an app session needing a core change stops and asks the
;   core thread, never reaches in. the runnable ones install on PATH via `make install`.
; * modules: a baked service keeps its names off the global book, and the layers are the
;   runtime'S -- no user-facing enter/leave. the chain: top is the defglob target, under it a
;   use-stack, orth last and read-only; run_program pushes the session layer, one load = one
;   layer. `use` is the loader: (use 'x) splices a registered module just below the top (bare
;   names resolve on the walk, never shadow yours), and on a miss loads x from the source library
;   (the frontend's static ai_libs table, .rodata) or the filesystem walk: lib/<x>.l off the cwd, then
;   <seat>/../lib/<x>.l (the seat = the binary's home via readlink /proc/self/exe), then its
;   love/ subfolder. PATH picks the love, the love carries its library -- no env vars anywhere.
;   a string is an explicit path; a slashed name ('holo/x64, one symbol) includes lib/x/y.l into
;   the current head -- no layer, no registration. module files carry no brackets; their macros
;   land in their own book's macro slot and ride the splice (macroget walks the chain). the boot
;   splices keep rng/kanren/bao ambient so the corpus reads rand/unify/reads bare; holo registers
;   non-ambient. (from 'holo 'assemble) is the opaque accessor: currying reaches a member, 'keys
;   introspects, a missing module answers () (the presence guard), (from ()) lists the registry.
;   baked consumers fold their bare refs at their own compile, so load order is the scope: a use
;   must precede its readers' compiles. laws: spec.l's modules section + test/host/loader.l.
;   ⚠ a body-less-`:` binding whose name collides with a module nom leaks and clobbers the binding.

; --- vocabulary & house style --- the words here are a vocabulary -- never "terminology" or
; "nomenclature"; a vocabulary is living and chosen, warm not clinical. the style is cozy:
; lowercase, plain, a touch playful, names that earn their keep. we frame in the green -- name
; what a value is and keeps, not what it lacks. the surface stays small ((names ()) is the whole
; vocabulary). the celestial numerics: a charm is a fixnum (a word); a sun a full-word integer
; (the charm's overflow, one rung under big); a star is a self-netting scalar
; (charm/sun/big/gem/twin-gem); a galaxy is a tray of stars; a constellation is any numeric -- a
; charm is a star is a constellation. a gem is a float, a twin gem a complex ((re,im); `twin`
; pairs them). the global env is the `book` -- *the* book, the outermost one; a map is a book, a
; tablet a little book, a global "pinned in the book under <name>".
;
; comments earn their keep the same way, in C and .l alike: one or two lines, inline where
; possible; paragraph blocks near-forbidden. a comment states what the code cannot -- a ⚠
; invariant, a wire format, a contract -- one breath each. never what the next line does, why an
; edit was correct, or the bug it once fixed (a fixed bug's story is git log; delete such
; comments on sight, and delete any orphan whose code is gone).

; --- the shape of it --- one cell is one word: a fixnum is a tagged odd word, anything else a heap
; object whose first word is its hot -- a live external reference, the wire out of the heap to the
; ap that runs it. every operation is *fully generic*: it dispatches on a value's *kind*, and the
; kinds form a lattice that is literally the diagonal of the dispatch tables. that one lattice is
; read four ways: dispatch (the enum), order (the true-blue bands), algebra (each band carries the
; strongest +/* theory its rep affords, net the lone measure-hom threading them), and abstract
; domain (a compile pass climbs it by join to a fixpoint -- doc/proto/kinds.l). the VM is
; tail-threaded (aps tail-jump, never return -- `make vmret` checks it) over a two-space copying
; heap. the C core is tiny; most of the language is love closures installed reflectively from the
; prel, then laid into a heap image (the *egg*) at compile time.

; --- everything is a function --- (f x y) == ((f x) y) and (f) == f, so application is just
; left-to-right currying. numbers are church numerals, a list of numbers an exponential tower, and
; data self-applies (indexes). asserts in spec.l read infix -- (3 = 1 + 2) is ((= 3 (+ 1 2))),
; folding by grip and, at equal grip, by the operator's hand -- arithmetic is left-handed
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
; is one scope: every name binds over the whole form, so a read before the pin is the missing
; condition carrying the binding site's nom -- no read escapes to an outer binding of the same
; name. rebinding a name still reads the previous value (the sequence law); recursion among lambda
; bindings resolves lazily. an empty form is its head's value -- so (:) (?) (\) all read (), and a
; nullary helper call is the same law and the same trap (the ⚠ bullet up top).
; demo:
(: (twice f x) (f (f x)))    ; defines twice; (twice (+ 1) 10) ; 12
(: x 1 x (x + 1) x)          ; 2     the sequence law: a rebind reads the previous value
(:)                          ; ()    an empty special form reads its head: the zero point
(: (go x) (+ x 1) (go ()))   ; 1     fire a thunk with the unit; (go) alone is go unrun, never 1

; --- true and false, in one breath --- false is *nothing*: whatever nets <= 0. the net is the
; complex-valued measure -- a number its own value, text the sum of its charms, a symbol its
; spelling, a list or array the sum of its elements' nets (spine only, recursive, unclamped).
; `!` (nil?) reads the net's sign, `$` (sat) is the one saturating clamp onto the green charms
; (!x == (0 = $x)), and `tally` is the count -- how many, not how much. every value wears a color
; by its net-sign: green nonnegative, red negative, blue the zero floor -- true iff positive
; green; every nothing ((), 0, "", @(), #(), ~(0 0)) is blue. a twin gem reads by its real part
; alone -- gate and clamp both, the magnitude is abs's -- so a pure phase is blue. ⚠ the one place truth and the
; total order part: `i` sorts above 0 but nets nothing (C admits no order compatible with its
; arithmetic -- truth cannot rest on which root we named). no "truthy"/"falsy": true and false are
; the bits of `!!$`. spec.l's true-and-false section, doc/measures.md.
; demo:
!0  !""  !()  !-5  !'(-2 1)   ; nothing -> false: zero, empty, red at any rank
$'(1 2 3)            ; 6       $ sums the nets, then clamps once
(!"" = 0 = $"")      ; true    the invariant !x == (0 = $x)

; --- the reference --- test/spec.l carries the whole surface, section by section -- types &
; predicates, arithmetic (an undefined op answers (), the unit rides through every lane), order &
; equality, comparing functions, + and * generic, numeric functions (sine/cosine/log the only
; transcendental nifs -- power is application), identities, complex, arrays, chains & lists,
; strings & mints, hashes (peep the total presence test), casks, reader operators, macros,
; control (help/welp, trap, missing, apcap), i/o & ports (sound takes text -- a port, a string or a charlist --
; and its return is the read protocol: (datum . rest) with the rest always a charlist, () at a
; clean end, the symbol 'torn mid-shape -- spelled at each site, not a book name), bootstrapping.
; each law lives in
; its section comment; the asserts below keep it honest. deep dives: doc/measures.md,
; test/operator.l + test/infixop.l, test/help.l test/missing.l test/apcap.l, proof/rocq/spec.v.

; --- bootstrapping --- the C core is minimal; the key semantics are l closures installed from the
; prel and shared by both compilers:
;   numap -- number application (x**n, or compose ($ n) times).
;   add/mul -- `+` and `*` of functions are church add and compose, so numerals agree.
;   opfix -- the operator factor pass: sigil surface -> core source, run first.
;   boxfix -- the letrec* capture-by-location rewrite: a forward reference indirects through a
;     cell keyed by the binding site's nom; a pre-fill read is the missing condition.
;   wev -- the pre-pass: expand macros, apply boxfix, fold pure globals, mark apply strategy.
;   maps -- #(..)/map expand to nested pins.
; the *egg* (love/egg.l): warm the egg and the evaluator sits on it twice -- compile the compiler
; with the C bootstrap, recompile the corpus through itself -- then the hatchling installs as `ev`
; in the image at C compile time; `born` records the hatch time. just before birth the egg mops up
; every runtime-internal nom: the raw cell nifs (peek poke seek spin -- spin stays, it's
; ultimate), the compiler's machinery, every hot lvm_* pointer, the `book` itself. compiled
; references were folded, so only the noms die; noms the printer/reader/expanders emit stay, as do
; the C-resolved hooks (num-ap add mul). the shell core (love/bao.l) is a registered module: the
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
; (x86_64/aarch64), wasm. prel is the language and stays minimal -- a library is its own love/*.l;
; mk/lib.mk lcats it to out/lib/<name>.h automatically, and every post-egg layer is a module
; (coin, rng, q, kanren, overlay, uu, bao, holo, glaze, rune ..): the frontend registers the lcat'd
; constant (a row in the frontend's ai_libs table) and its boot text says (use 'x); the boot rebinds the one-name surfaces there
; too, ⚠ always under the module'S own name ((: uu (from 'uu)), (: overlay (from 'overlay))) -- an
; accessor bound under some other name means one word for both a book and whatever else wears it,
; and the lane that skips the rebind then hands out the other thing in silence (`parse` did exactly
; that to holo/text.l for as long as inline asm existed). overlay's ev hook must land in orth,
; which only the boot can write.
; the embed sites, seven: host/main.c (twice -- love0's sed-wrapped <name>0.h twins need a gl0_h
; entry), wasm/host.c, port/inle/kmain.c, port/playdate/main.c, port/mps2/main.c,
; port/teensy41/main.c; each wants a header dep in its build file. the egg's own door takes three
; texts -- `ai_egg_(g, egg, p1, corpus)`, ten call sites, `corpus` being prel.h and ev.h juxtaposed
; (p1text mints a fresh list where p0onto extends the one on the stack, so the corpus is one p1
; read) -- and stitches the corpus (p0 reads egg + p1, p1 reads the rest), so the C reader's sigil
; half is off the boot path and p1.l is the only .l held to the pure lisp subset (egg.l too, via
; applyq's driver door). ⚠ prel rides p1: an `ai_evals_` bundling p1's text with a later one puts
; both on p0, because readtext picks its reader once per call. coin/rng/q/kanren/uu ride host, love0, wasm and the K_TEST kernel; the playdate workbench
; takes q + kanren + rune; a shipped kernel takes uu + bao + holo + peg and the whole kore cat
; (inle rung 3: the boot cmdline's program seat dispatches it). every frontend opens its session with
; ai_layer_ after boot (bakers never push; wakers always do). ⚠ a layer leans only on what
; survives birth -- wrapping a mopped nif means taking it off egg.l's mop list. ⚠ a post-egg
; layer cannot add an operator: `dyadics` is mopped, the grammar closes at the hatch; `fixity`
; is the one door left onto the table (it answers the row it replaced, and a refused shape rolls
; back and scares). build codegen lives in love under tools/; the C is freestanding,
; -Wall -Wextra -Werror.
```
