```love
; love -- one file carrying a language, its toolchain, its userland and its own source, that
; rebuilds itself from them byte for byte (`love source`, then `love seed`, ambient cc or ours).
; the SEED IS THE PRODUCT, not a way of shipping one: out/dist/love-<arch>, ~12 MB, doc/dist.md.
;
; THE LANGUAGE: an infix, low-paren surface factoring through opfix to a tiny lisp core
; (`map (+ 1)` and `(3 = 1 + 2)` are plain parens underneath -- one truth, both compilers), fully
; curried, every op generic on a value's KIND. most of the tree reads as data, not control flow.
;
; the core is a tiny generic C runtime (love.c + love.h) under a self-hosting compiler in love
; (love/{prel,ev,bao}.l). source is .l; the host binary is `love`. see README.md.
;
; THE C COMPILER closes the loop: mooncc + holo compile and link love.c for six machines with no
; gcc, no glibc, no ld, kernel included. full C11 is the aim; doc/moon-c-gaps.md is the ledger.
;
; this file orients -- how to work here, the traps, the vocabulary. the laws live in test/spec.l
; (the executable spec, green on every target) and proof/rocq/spec.v (machine-checked, axiom-free;
; test_proof + test_gen). settle any doubt by probing the binary; demos show theirs (`expr ; value`).

; --- how to work here (read this first) ---
; * ⚠ TWO PRIMARY TARGETS, and the seam is `main ~ kmain` with the love core beneath: host/ is
;   the hosted frontend, free/ the freestanding one (the inle kernel, x86_64 + aarch64 + riscv),
;   and they are PEERS over love.c + love.h -- neither is the other's port.
; * everything under port/ is a real target and a SECONDARY one (mps2, teensy41, nucleo446,
;   playdate, rp2040): it rides test_extra and cannot hold a commit. a board that cannot follow
;   the primaries adapts or skips -- it never narrows them.
; * THE GATES: `make test` fast, `make test_slow` before committing, `make test_extra` before
;   merging to main. between them run the test_* targets covering what you touched (test/test.mk;
;   most subsecond off the baked image, the egg gates + test_sat stay cold).
; * ⚠ PIPE THE READING, KEEP THE EXIT: a pipeline's status is the LAST command's, so
;   `make test | grep ..` reports GREP and a red gate reads green. `set -o pipefail; make test
;   2>&1 | grep -aE "tests pass|FAIL|Error [0-9]|No rule"` is the reading, the prefix not optional.
; * ⚠ never `| tail` a gate -- each target prints its summary as it finishes and the dots bury it.
;   every lane holds itself to exit 0 AND its sentinel (run.sh reads the recorded X too, a failed
;   assert carrying on), so a lane that stops silently fails the recipe rather than passing quietly.
; * other lanes: `make valg` memory, `make vmret` the tail-jump law, `make waits` the blocking
;   roster. one file is `out/host/love test/x.l` -- but the corpus runs concatenated in ONE
;   global scope, so keep helpers local (give `:` a body).
; * ⚠ the natjit lanes are EXPERIMENTS, NOT CONSTRAINTS -- the interpreter is the semantics and
;   every native tier under it answers to evidence: `glaze` specializes, `amble` translates from
;   the SOURCE, `splice` from the COMPILED THREAD. a design that breaks if one vanishes is wrong.
; * speed is a signal: every test runs in a second or two, so one that hangs or crawls is a bug
;   announcing itself, never a slow bench to wait out. the classic is a 0-byte lcat header make
;   thinks fresh, leaving a native lane unbound -- one bug, two faces (infinite loop or crash).
; * the kernel has three boot doors, one ELF per arch: -kernel (x86_64 PVH stub, aarch64 EL1 MMU
;   stub; test_kernel + test_kernel_arm64, nothing downloaded), UEFI (free/uefi/, our own
;   BOOTX64.EFI; doc/uefi.md), limine (iso/hdd + the run-* lanes; dl/ survives `make clean`).
; * the kernel is OURS end to end: the link (holo's ldkern via free/klink.l, `KLINK=lld` to
;   compare), the assembly (no .S -- mkboot.l lays bring-up, mkvec.l the interrupt tail), the
;   compiler (`KCC ?= mooncc`, doc/moon-kernel.md; `KCC=clang` + test_kdiff the differential).
; * ⚠ each KCC variant has its own odir and ELF (kccsuf/klsuf), and mooncc REFUSES a -m flag ;   rather than ignoring it. `make test_vec` faults on purpose -- the one way to reach a stub.
;   editing love.h needs no clean (every object deps on $(love_h)).
; * check a .l edit for balance before trusting it: `out/host/love crew/libra/libra.l <file>`, or
;   `make lint` -- libra ⚖ (crew/libra/, doc/libra.md), the .l-aware paren/bracket/brace +
;   unclosed-string scan. silent is clean, a warning points at the opener; not in the test gate.
; * libra's verbs ride one scanner (lib/lint.l): `fmt` reindents (not adopted), `serve` speaks
;   lsp, `infix`/`unfix` are the two directions of the factor pass (unfix IS opfix printed). the
;   singleton/shadow/deprecated rules ride a config, off until it asks, and only `strict` fails.
; * ⚠ infix/unfix print from the DATUM: comments are not carried, which is why neither is a mode
;   of fmt and neither has `-w`.
; * config is salt (lib/salt.l): (salt 'app) answers a settings tablet from ~/.love/etc/<app>.l
;   then ./.<app>.l over it, the project file speaking last; a setting is one form, head names it.
;   HOME is the one env var here. doc/libra.md.
; * ⚠ salt reads as data with `sound`, never evaled; presence is the OPEN, not the byte count; a
;   non-setting form is skipped in silence and a dropped paren ends the read -- half a config
;   beats none.
; * C and docs embed love the .l sweeps miss -- grep on every rename: host/main.c,
;   free/kmain.c, port/rp2040/main.c, port/playdate/ (main.c + cas.l), wasm/, and index.html,
;   whose examples + "; answers" are probed against out/host/love, never written from memory.
; * a bare all-punct symbol mid-list captures its left operand when code compiles (opfix) --
;   escape in parens ((+) is + as a value); glued to a datum it is monadic instead (the valence
;   law: space your dyadics). quoted lists are data, operators plain. doc/precedence.md.
; * a sigil's GRIP is three words: the LANE (`operators`' arity key -- 0 glued, 2 spaced, -1
;   spaced with no bound), the BAND (higher binds tighter) and the HAND (which way a same-band run
;   folds). a row IS a signed band and the MINUS is the left hand -- arithmetic is the exception.
; * ⚠ that makes a left-handed row RED, so every test on one is by kind or by identity: (nil? -60)
;   is true, and a truth test reads every left-handed row as absent. `(grip ar nm v)` is the door,
;   one signature at every lane -- a curried door of two arities hands back a closure, writing nothing.
; * arithmetic operators are dyadic: `(+ a b c)` is `((+ a b) c)` -- application, not a 3-way sum,
;   so it church-exponentiates ((+ 192 40 5) = (232 5) = 5^232, a bignum).
; * ⚠ A PREDICATE ANSWERS A BIT -- 0 on false, never the zero point, and both are blue so nothing
;   that TESTS one can tell. `&&` and `||` are VALUE-PRESERVING and so not predicates ((5 && 7) is
;   7, (0 && 1) is ()); glued `?(..)` is the clamp.
; * spec.l sweeps every `?`-named global in (names ()) and NAMES its offenders before failing --
;   the seat that fails is often not the one you are sitting in (`dotl?` lives only in the
;   kernel's book, and test_kernel is what found it).
; * ⚠ LISP TRAP: THERE ARE NO DOTTED PAIRS. `.` is an ordinary punct symbol, so '(a . b) is the
;   THREE-element list (a . b), and in code it opfixes to (. a b), post.l's dot. a pair is BUILT
;   (`><`), never written, so any table row wanting one takes the list spelling.
; * ⚠ HASKELL TRAP: THERE ARE NO RIGHT SECTIONS. the curry law hands you the left one and nothing
;   else, and (1 -) folds to (- 1) -- the SAME function, -4 at 5. so (- 1) is not `subtract 1`;
;   the right section is a lambda, and now a short one: (x \ x - 1).
; * ⚠ C TRAP: COMPARISONS DO CHAIN, python's way -- (1 < 2 < 3) is 1, (0 = 1 = 2) is (), and the
;   shared operand runs ONCE, left to right, with && still short-circuiting. to compare AGAINST a
;   comparison spend the parens: (a < (b < c)) stays nested, and prefix is untouched.
; * the BINDERS read infix and each folds back THROUGH its own lowering, so a spelling is the same
;   FORM and never a lookalike: (a \ b \ c) IS (\ a b c), and (a : 1 b : 2 (a + b)) IS
;   (: a 1 b 2 (a + b)) -- A CHAIN BEING ONE SCOPE. patterns lower in every position either way.
; * a top-level body-less `:` PINS, so `sq x := x * x` is a definition and `:=` is an alias row on
;   `:`. ⚠ `:` is n-ary: (x : f a) binds x to f with body a, not to (f a) -- an applied value
;   spends the parens.
; * the CLAUSE forms `?` and `@` are the two N-ARY operators (band 10): infix they take the
;   scrutinee on the left and keep every arm an operand, so (x @ p b .. else) IS (@ x p b .. else)
;   and (c ? a b) is (? c a b). the leading span is whole, so a compound scrutinee spends no parens.
; * ⚠ an infix ARM still does: a dyadic inside one spans the rest ((c ? a + b 'e) is
;   (? c (+ a (b 'e)))), which is juxtaposition binding tighter, as everywhere.
; * ⚠ `(f)` is not a call -- (f) == f at zero operands, so a nullary helper hands its closure back
;   unrun and nothing errors: `(go)` `(loop)` `(step)` never fire, a silent no-op that keeps biting
;   loops and named-lets. fire every thunk with an operand and let it be the unit: `(go ())`.
; * ⚠ () not 0 there: a 0 in a do-nothing slot is a 0-fossil, sibling to the nil-tail fossil --
;   the ignored slot is the unit, not a number.
; * ⚠ scratch in an lvm_ may not be LIVE AT THE TAIL: the VM is tail-threaded, so anything the
;   frame still owes at the jump turns the tail Continue() into a `ret` and the stack grows every
;   step. the fault is a stack overflow deep in an unrelated test, never a wrong answer.
; * a local read for the last time BEFORE the jump is fine and costs nothing (lvm_udprecv's
;   datagram buffer, lvm_accept's fd); what is barred is an ADDRESS that outlives the body. when
;   the body must survive, use an `ai_noinline static` helper taking `g` (rng_canon, host_cwd).
; * two instruments, not one: `ai_musttail` is owed rather than opportunistic, so a shape that
;   cannot jump REFUSES at compile, and `make vmret` disassembles what was emitted. ⚠ vmret reads
;   the x86-64 host binary only -- a cross lane is on you.
; * ⚠ love has no global state -- a mutable global in C is a bug, never a shortcut, and adding one
;   is FORBIDDEN. state rides `g` or a parameter, a buffer rides `g->hp` or the caller, a table
;   that never changes is `const`; a global sits outside the heap, untraceable and unbakeable.
; * the standing exceptions are the immortals the image codec locates BY ADDRESS (ai_stdin/out/err,
;   the port vtables, the ai_baked_image slot). temporary INSTRUMENTATION is the one licence and it
;   leaves with the hunt -- a probe still in the tree at commit is the bug it was chasing.
; * ⚠ never call malloc/free (calloc/realloc too) directly -- the allocator is one door on `g`:
;   `g->alloc(g, p, n)`, n>0 reserves n bytes, n==0 frees p, answers the block or NULL. it is a
;   HOOK so each seat supplies its own; love.c's ai_libc_alloc is the one site that may name them.
; * ⚠ presence is the wrapper, never the net -- the costliest recurring bug in this tree. every
;   nothing is nil by design ((), 0, "", @()), so `(! x)` fails silently on every legitimate empty;
;   carry presence out of band with the `(1 x)` wrapper tested by `two?`, or a separate predicate.
; * ⚠ and `two?` is false for a string -- it tests cons pairs -- the same trap wearing its other face.
; * quasiquote is gone: the ` list ctor evaluates every element, so quote the literal positions
;   (numbers self-eval, so bare). a lisp-primed hand writes the inverse on reflex -- check every ` twice.
; * a corpus test that twirls a task must (catch p) it: an orphan stalls the kernel runner.
; * python \b-sweeps treat - as a boundary: kebab names with capital segments mangle.
; * the repl reads each line as one expression (1 = 1 answers 1); files read forms. the shell's
;   default help prints `;; a b` and answers the zero point so the session survives every raise;
;   file mode's quits 1 on every scare, a missing name included.
; * so ask presence out of band -- (member? 'x (names ())), never `(lit? x)`, which reads x. a help
;   is INSTALLED, not bound: (hear f) writes the hot_help slot, (hear ()) uninstalls, (heard ()) reads.
;   ⚠ a help takes (a b) and THE ARITY IS THE PROTOCOL -- a stale 3-arg one under-applies to a closure.
; * the crew (crew/, the apps) rides over the core, each owning non-overlapping files so a session
;   can take one in parallel: lux (X11 wm), inle (the kernel, free/), svalbard (the vcs `sb`,
;   doc/sb.md), moon (the C compiler; test_raw, test_drv), rune (symbolic algebra on the q coin).
; * ..lush (the shell 🐚, the distro's console shell and its /bin/sh; test/host/sh.l, doc/lush.md),
;   and the document chain lapiz (the markdown/html/roff lens, writes the man pages) -> papel (the
;   static site, `make site`) -> kiosko (the web server). runnable ones install via `make install`.
; * ⚠ apps add nifs through the host/*.c glob + AI_NIF, no core edit -- love.c, love.h and
;   host/main.c are CORE, and an app session needing a core change stops and asks the core thread,
;   never reaches in.
; * verbs (love/verbs.l): the ONE registry every app dispatch goes through, three doors -- `word` a
;   leading positional (a `/` or a `.l` is ALWAYS the file, so a verb never shadows a script),
;   `seat` argv[0] (a `sed` symlink onto the artifact), and `fire` the seat SCAN.
; * an app pins its own name at its own foot, so the image carrying the app carries the word for it
;   and `love verbs` prints the table rather than a written-down list. `wake` and `bake` are PRIME
;   verbs parsed in C before any love exists, holding rows anyway so a misplaced one is an honest error.
; * ⚠ fire takes the LINE IT SCANS: verbs.l rides the egg, so a bare `cmdline` read there folds to
;   the BAKE's command line and every caller silently scans that instead. gate: test_seat, and a
;   seat that answers () looks exactly like an app with nothing to say.
; * modules: a baked service keeps its names off the global book, and the layers are the RUNTIME's
;   -- no user-facing enter/leave. the chain is the defglob target on top, a use-stack under it,
;   orth last and read-only; run_program pushes the session layer, one load = one layer.
; * `use` is the loader: (use 'x) splices a registered module just below the top (bare names
;   resolve on the walk, never shadow yours), and on a miss loads x from the source library or the
;   filesystem walk -- lib/<x>.l off the cwd, then <seat>/../lib/<x>.l, then its love/ subfolder.
; * PATH picks the love, the love carries its library -- no env vars anywhere. a string is an
;   explicit path; a slashed name ('holo/x64) includes lib/x/y.l into the current head, no layer and
;   no registration. the boot splices keep rng/kanren/bao ambient, so the corpus reads them bare.
; * A MODULE IS ITS TABLET: (from 'holo) hands the real one and (from 'holo 'assemble) reaches a
;   member, `keys` introspects and `pin` writes -- surgery on a live runtime, yours to do. a missing
;   module answers () (the presence guard; `lit?`, never `!`), and (from ()) lists the registry.
; * ⚠ baked consumers fold their bare refs at their own compile, so LOAD ORDER IS THE SCOPE: a use
;   must precede its readers' compiles. ⚠ a body-less-`:` binding whose name collides with a module
;   nom leaks and clobbers it. laws: spec.l's modules section + test/host/loader.l.

;  --- vocabulary & house style --- the words here are a VOCABULARY, never "terminology" or
; "nomenclature": living and chosen, warm not clinical. the style is cozy -- lowercase, plain, a
; touch playful, names that earn their keep -- and we frame in the green, naming what a value keeps.
;
; the celestial numerics: a charm is a fixnum, a sun its full-word overflow, a gem a float, a twin
; gem a complex; a star is any self-netting scalar, a galaxy a tray of them, a constellation any
; numeric at all. the global env is the `book`; a map is a book, a tablet a little book.
;
; comments earn their keep the same way, in C and .l alike: one or two lines, inline where possible,
; paragraph blocks near-forbidden. a comment states what the code cannot -- a ⚠ invariant, a wire
; format, a contract. never what the next line does, or a bug it once fixed (that story is git log).

; --- the shape of it --- one cell is one word: a fixnum is a tagged odd word, anything else a heap
; object whose first word is its hot -- a live external reference, the wire out to the ap that runs
; it. every operation is fully generic, dispatching on a value's KIND.
;
; the kinds form a lattice that is literally the diagonal of the dispatch tables, read four ways:
; dispatch (the enum), order (the true-blue bands), algebra (the strongest +/* theory each rep
; affords) and abstract domain (a pass climbs it by join to a fixpoint -- doc/proto/kinds.l).
;
; the VM is tail-threaded (aps tail-jump, never return -- `make vmret` checks it) over a two-space
; copying heap. the C core is tiny; most of the language is love closures installed reflectively
; from the prel, then laid into a heap image (the *egg*) at compile time.

; --- everything is a function --- (f x y) == ((f x) y) and (f) == f, so application is just
; left-to-right currying. numbers are church numerals, a list of numbers an exponential tower, and
; data self-applies (indexes).
;
; asserts in spec.l read infix -- (3 = 1 + 2) is ((= 3 (+ 1 2))) -- folding by band and, at equal
; band, by the operator's hand: arithmetic left-handed, everything else right, sound by (f) == f.
; the two pillars:
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
((\ [a b] (a + b)) '(3 4))   ; 7     a \ param that is NOT a nom is a PATTERN (love/pat.l's @):
                             ;       opfix lowers it to (\ v (@ v [a b] ..)), define-sugar too
(? 0 'a (1 < 2) 'big 'else)  ; big   ? -- test/result pairs, then a final else
((x \ y \ x + y) 3 4)        ; 7     \ INFIX, the chain flattened: this IS (\ x y (x + y))
(a : 4 b : 5 (a + b))        ; 9     : INFIX and n-ary; a chain is ONE scope, not a nest
((sq x := x * x) 5)          ; 25    : at two is body-less, so a top-level one PINS sq; := aliases :
; `:` doubles as sequencing (bind `_` for effect), `(f x)` on the left is define-sugar, and a
; body-less top-level `:` leaks its bindings to the global scope (how tests share helpers).
;
; a body-having `:` is ONE SCOPE: every name binds over the whole form, so a read before the pin is
; the missing condition carrying the binding site's nom, never an escape to an outer binding of the
; same name. rebinding still reads the previous value (the sequence law); lambda recursion is lazy.
;
; ⚠ an empty form is its head's value -- (:) (?) (\) all read () -- and a nullary helper call is
; the same law and the same trap (the bullet up top).
; demo:
(: (twice f x) (f (f x)))    ; defines twice; (twice (+ 1) 10) ; 12
(: x 1 x (x + 1) x)          ; 2     the sequence law: a rebind reads the previous value
(:)                          ; ()    an empty special form reads its head: the zero point
(: (go x) (+ x 1) (go ()))   ; 1     fire a thunk with the unit; (go) alone is go unrun, never 1

; --- true and false --- THE MEASURE, and the tower it retracts down. every value has a complex
; measure; each rung below is that measure saturated into a smaller set, idempotent and identity
; on its own image -- retractions, not conversions. false is NOTHING: whatever nets <= 0.
;
;   net x        a twin gem      C        the measure itself
;   re (net x)   a gem           R        the unnamed inner rung
;   ceil x       a charm         Z        ..retracted onto the integers
;   $x           a green charm   [0,max]  saturate: ceil with the floor raised to 0
;   ?x           a bit           {0,1}    bit: saturate with the ceiling lowered to 1
;
; each line is a LAW -- read the free x as forall x, answering the charm 1. test/law.l quantifies
; them with lambdas and fuzzes the spread (^n ^^n ^^^n build the nesting), so a law here cannot be
; wrong in silence; proof/rocq/spec.v is where one goes when it earns more than a fuzz.
(ceil x = ceil (re (net x)))         ; 1   ceil retracts onto the integers
($x = (? (0 < ceil x) (ceil x) 0))   ; 1   saturate is ceil with the floor raised to 0
(bit x = (0 < $x))                   ; 1   bit is saturate with the ceiling lowered to 1
(!x = (0 = $x))                      ; 1   nil? is bit's complement -- truth is a SIGN, not a kind
;
; ⚠ A SIGIL IS GLUED, AN OPERATOR IS SPACED, and they share a CHARACTER, not a meaning: `?x` is
; bit where `(? ..)` is the clause form, so the law above spells `bit x` when it must lead. seven
; of the eight sigil NAMES are the operator and not the glued word; `!` is the one fossil bound to
; its own (which is why `(! = nil?)` reads true), and the name `$` is the numeral 1, which is the
; whole of why ($ f x) applies. test/law.l pins all three.

; --- the reference --- test/spec.l carries the whole surface section by section: types, arithmetic
; (an undefined op answers (), the unit riding every lane), order, + and * generic, numeric
; functions, identities, complex, arrays, chains, strings, hashes, casks, macros, control and i/o.
;
; ⚠ `sound` takes text -- a port, a string or a charlist -- and its return IS the read protocol:
; (datum . rest), the rest always a charlist, () at a clean end, the symbol 'torn mid-shape.
; spelled at each site, never a book name.
;
; each law lives in its section comment and the asserts below keep it honest. deep dives:
; doc/measures.md, test/operator.l + test/infixop.l, test/help.l, test/missing.l, test/apcap.l,
; proof/rocq/spec.v.

; --- bootstrapping --- the C core is minimal; the key semantics are love closures installed from
; the prel and shared by both compilers:
;
;   numap -- number application (x**n, or compose ($ n) times).
;   add/mul -- `+` and `*` of functions are church add and compose, so numerals agree.
;   opfix -- the operator factor pass: sigil surface -> core source, run first.
;   boxfix -- the letrec* capture-by-location rewrite: a forward reference indirects through a
;     cell keyed by the binding site's nom; a pre-fill read is the missing condition.
;   feel -- ev.l's source pre-pass, the one entry here NOT from the prel: expand macros, apply
;     boxfix, fold pure globals, mark apply strategy. c0's twin is ana_2's macro lane.
;   maps -- #(..)/map expand to nested pins.
; the *egg* (love/egg.l): the evaluator sits on it twice -- compile the compiler with the C
; bootstrap, recompile the corpus through itself -- and the hatchling installs as `ev` in the image
; at C compile time. `born` records the hatch.
;
; just before birth the egg MOPS UP every runtime-internal nom: the raw cell nifs (peek poke seek;
; spin stays, it is ultimate), the compiler's machinery, every hot lvm_* pointer, the `book`
; itself. compiled references were folded, so only the noms die.
;
; the shell core (love/bao.l) is a registered module: the user verbs re-pin at its foot, the
; plumbing stays sealed. frontends eval ((from 'bao 'bao) 0) or ((from 'bao 'shell) 0), and
; (keys (from 'bao)) is the manifest.
; demo:
(lit? ev)            ; true    ev is installed in the image
born                 ; a fixnum (the hatch time) post-egg; unbound pre-egg
macros               ; ()      mopped up after birth -- off the book, so the nom reads nothing

; --- under the hood --- an op at two is an NxN table indexed by the two kinds, at one its diagonal;
; the three core tables are +, * and apply. a both-fixnum fast path skips the table; otherwise one
; indexed jump picks a lane that widens only as far as the operands need.
;
; the love/ layer (prel ev bao cli egg) drips into every frontend: host, freestanding kernel
; (x86_64/aarch64), wasm. prel is the language and stays minimal -- a library is its own love/*.l,
; lcat'd to out/lib/<name>.h by mk/lib.mk, and every post-egg layer is a module.
;
; ⚠ the boot rebinds a module's one-name surface ALWAYS UNDER THE MODULE'S OWN NAME
; ((: uu (from 'uu))): bound under any other, one word means both a book and whatever else wears it,
; and the lane that skips the rebind hands out the other thing in silence.
;
; the embed sites, seven: host/main.c (twice -- love0's sed-wrapped <name>0.h twins need a gl0_h
; entry), wasm/host.c, free/kmain.c, and port/{playdate,mps2,teensy41}/main.c. each wants a
; header dep in its build file.
;
; the egg's door takes three texts -- `ai_egg_(g, egg, p1, corpus)`, ten call sites -- and stitches
; them so p0 reads egg + p1 and p1 reads the rest. the C reader's sigil half is thus off the boot
; path, and p1.l is the only .l held to the pure lisp subset (egg.l too, via applyq's driver door).
;
; ⚠ prel rides p1: an `ai_evals_` bundling p1's text with a later one puts BOTH on p0, because
; readtext picks its reader once per call.
;
; which modules ride where: coin/rng/q/kanren/uu on host, love0, wasm and the K_TEST kernel; the
; playdate workbench takes q + kanren + rune; a shipped kernel takes uu + bao + holo + peg and the
; whole kore cat. every frontend opens its session with ai_layer_ after boot (bakers never push).
;
; ⚠ a layer leans only on what survives birth -- wrapping a mopped nif means taking it off egg.l's
; mop list. ⚠ a post-egg layer cannot ADD AN OPERATOR: `operators` is mopped and the grammar closes
; at the hatch, `grip` being the one door left onto the table (a refused shape rolls back, scaring).
;
; build codegen lives in love under tools/; the C is freestanding, -Wall -Wextra -Werror.
```
