; lib/splice.l -- the template JIT over the tail-threaded VM. `dis` reads a compiled closure
; back to its op rows, each row's op contributes its OWN machine-form IR -- read out of THIS
; BINARY's .rodata, where the compiler wrote it down -- the dispatch between them is deleted,
; holo assembles the one function that makes, and `nif` installs it. `(use 'splice)`:
;
;   (jit f)        -> a native closure agreeing with f, or () tallying the row it could not say
;   (jit-forms f)  -> the IR it would assemble | ()        -- what the splicer built
;   (jit-ir nom)   -> one handler's own IR | ()            -- what the compiler wrote down
;   (jit-irtab ()) -> (1 table count) | ()                 -- every handler this binary carries
;   (jit-why ())   -> ((reason who ..) . count) rows       -- the coverage census, ranked by use
;   (jit-fired? n) -> did the door native-back a closure since count n
;
; ⚠ AUTO: loading this module lights the LAST natjit lane (love/glaze/hook.l reads
; `(from 'splice)` at fire time), so `(use 'splice)` is the switch -- nothing else turns it on
; and nothing turns it on by itself. ⚠ IN ITS OWN TOP-LEVEL FORM: a `:` builds its lambda
; bindings before its body runs, so a `use` sharing a form with the closures it means to cover
; lights the lane after they are already made -- which reads exactly like a lane that is off. The lane sits UNDER amble on purpose: amble translates
; from the SOURCE and declines a written list of shapes, while this one translates from the
; COMPILED THREAD and says whatever the compiler emitted, so what it adds is exactly amble's
; leavings. The two costs it must stay affordable against are both cached below.
;
; ⚠ NO COMPILER, NO SOURCE TREE, NO OBJECT FILE. `mooncc -fir=lvm_` lays each op handler's IR
; into `ai_lvm_ir` at compile time and nifs.l lays the book-name bridge beside it, so a plain
; love reads what it is made of. That is the whole design: the alternative was scraping op
; bodies out of love.c's TEXT (which a shipped love does not carry) or disassembling our own
; machine code (which would hand back bytes the splicer could not reason about). These forms
; are what the compiler emitted, so holo assembles them as they stand.
; ⚠ IN-PROCESS BY CONSTRUCTION: the one external reference a handler carries becomes a movabs
; of the symbol's LIVE address, read from this process's own symtab and load bias. The bytes
; run in the love that made them and nowhere else -- which is what a JIT is, and why none of
; this can be written out for later.
; ⚠ LOVE_NO_GLAZE=1 while jitting: a glazed closure is a native cell, and `dis` reads its husk.
; The glaze is the specializing tier this one sits UNDER, not its competition.
;
; ⚠ THE BRANCH IS NOT THE OBSTACLE, `Ip` IS. A thread's control flow is entirely internal to it
; -- every target is a same-thread cell pointer -- and `disg` (love/ev.l) hands back the whole
; graph with targets as cell indices. What stops a cond from being spliced is that its handler
; READS Ip, and every one of those reads is a cell disg can name: the operand, `Ip+3` (the
; fallthrough) and `Ip[2]` (the target). Folding Ip is one rung that dissolves the branch family
; and the whole operand family together. doc/moon-regalloc.md, `branches`.
;
; Covered: ANY ARITY (the cur row carries it), the load family (arg/argN/quote-of-charm/quoN/
; quomN) said directly, the push-PAIR family (aa/aq/qa/qq) said as two pushes, the fld family
; (argcap/argcup/argtwo) UNFUSED into a push and an op, and every handler whose IR the compiler
; carried and which passes the splice condition. NOT covered, and it is 90% of the corpus: an
; op that APPLIES (the p variants -- qap/qqp/aap/aqp -- plus ap/apn/argap) or BRANCHES (the cond
; family, whose operand is a thread offset a splice does not have). A row it cannot say declines
; the closure and tallies why -- that report is the coverage census (doc/moon-regalloc.md, the
; auto lane), and it is what ranks the rungs.
; ⚠ AHEAD OF THE FORM, never inside it: `@` is a macro, so it is in the chain only once its
; layer is spliced -- and a `use` inside the form runs when the form is already compiled.
; gen.l carries the same line for the same reason.
(use 'pat)
(:
   jit-memo (tablet 0)                               ; the two tables, read once and kept
   jit-bodies (tablet 0)                             ; a handler nom -> its checked clean body
   jit-blobs (tablet 0)                              ; an op-row signature -> the bytes it made
   ; ⚠ A DECLINE IS DATA, not a message. Under a hook this fires on every closure the runtime
   ; builds, so a stderr line per row would bury the run -- and the ranked tally is the coverage
   ; census the widening rungs are chosen from. `(jit-why ())` answers ((reason who ..) . count)
   ; rows; LOVE_SPLICE_TRACE says them as they happen.
   jit-tally (tablet 0)
   jit-trace (? (getenv "LOVE_SPLICE_TRACE") 1 0)
   (jit-no k m) (: _ (pin jit-tally k (1 + peep jit-tally k 0))
                   _ (? jit-trace (say err ("splice: " + m + "\n")) 0) ())
   (jit-why _) (map (\ k (k >< peep jit-tally k 0)) (keys jit-tally))
   (jit-say m) (jit-no ['said m] m)
   (jit-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) s) ()))

   ; --- WHAT THE COMPILER WROTE DOWN: this binary's own op handlers, as IR ---------------
   ; mooncc -fir=lvm_ lays the machine-form IR of every op handler under the splice cap into
   ; .rodata as `ai_lvm_ir`, one readable datum. So the JIT's source of truth is the BINARY
   ; IT IS RUNNING IN -- not love.c's text, which a shipped love does not carry, and not a
   ; disassembly, which would hand back bytes it could not reason about. holo assembles these
   ; forms as they stand; that is what they are.
   ; ⚠ a love built by another compiler has no `ai_lvm_ir` and this answers () -- which is the
   ; honest report, not a failure: gcc was never asked to write anything down.
   (irsym-off exe nm)                                ; a symbol's FILE offset in our own exe | ()
    (: (le i n) (bind-le exe i n)
       shoff (le 40 8) shentsz (le 58 2) shnum (le 60 2)
       (sh i f w) (le (shoff + ((i * shentsz) + f)) w)
       symi (: (go i) (? (i = shnum) -1 (? ((sh i 4 4) = 2) i (go (i + 1)))) (go 0))
       (? (symi < 0) ()
          (: stroff (sh (sh symi 40 4) 24 8)
             symoff (sh symi 24 8) syment (sh symi 56 8) nsyms (sh symi 32 8 // syment)
             (cstr i) (: (go j) (? (peep exe j 0 = 0) (snip exe i j) (go (j + 1))) (go i))
             (go i) (? (i = nsyms) ()
                       (: o (symoff + (i * syment))
                          (? (= nm (cstr (stroff + (le o 4))))
                             (: va (le (o + 8) 8) sx (le (o + 6) 2)   ; st_value, st_shndx
                                ((sh sx 24 8) + (va - (sh sx 16 8))))  ; sh_offset + (va - sh_addr)
                             (go (i + 1)))))
             (go 0))))
   (jit-irtab _)                                     ; nom -> its forms | () when nothing wrote it down
    (: t (peep jit-memo 'irtab ())
       (? (two? t) t
          (: exe (bind-rd "/proc/self/exe")
             off (irsym-off exe "ai_lvm_ir")
             (? (nil? off) (jit-no ['no-irtab] "this love carries no ai_lvm_ir (built without -fir=lvm_)")
                (: txt (: (go j) (? (= 0 (peep exe j 0)) (snip exe off j) (go (j + 1))) (go off))
                   r (sound txt)
                   rows (? (two? r) <r ())
                   v (: tb (tablet 0)
                        _ (map (\ w (pin tb <w >w)) rows)
                        [1 tb (tally rows)])
                   _ (pin jit-memo 'irtab v) v)))))
   ; one handler's forms | (). ⚠ WHAT THE SPLICER OWES, and the trap is worth the sentence:
   ; a handler's tail is the fixed dispatch triple (ld r0 r5 8)(add r5 r5 8)(jmpr r0) and comes
   ; off every copy but the last, whose bump becomes the whole segment's. Internal labels are
   ; renamed per copy; `la`/`call` targets bind to LIVE addresses through assemble-at's seeded
   ; label table, so a runtime splice needs no relocation at all -- it knows the answers.
   ; ⚠ THE ONE THING THAT IS NOT MECHANICAL: a handler carries its own room guard, and that
   ; guard jumps to lvm_gc, which RESUMES AT Ip -- which in a spliced body is the nif cell, so
   ; a collection halfway through would re-run the ops that already ran. The requirements must
   ; be summed and hoisted to one guard at the top, ahead of any Sp motion, deopting to the
   ; twin exactly as the C route's guard does. A per-op guard left in place is a silent
   ; double-apply, not a crash.
   (jit-ir nm) (: t (jit-irtab ()) (? (two? t) (peep <>t nm ()) ()))

   ; --- the BRIDGE: a book name to the lvm_ that runs it ------------------------------
   ; `dis` can only name the book key (`cup`) -- every lvm_ nom died at the hatch -- and the
   ; IR table is keyed by the C symbol (`lvm_cup`). nifs.l holds both and lays the map beside
   ; the IR, in this binary, as `ai_nif_lvm`. Same door, same reason: derived from the registry,
   ; so a nif cannot enter one table and miss the other.
   (jit-lvmtab _)
    (: t (peep jit-memo 'lvmtab ())
       (? (two? t) t
          (: exe (bind-rd "/proc/self/exe")
             off (irsym-off exe "ai_nif_lvm")
             (? (nil? off) (jit-no ['no-lvmtab] "this love carries no ai_nif_lvm (nifs.h is older than lib/splice.l)")
                (: txt (: (go j) (? (= 0 (peep exe j 0)) (snip exe off j) (go (j + 1))) (go off))
                   r (sound txt)
                   v [1 (? (two? r) <r ())]
                   _ (pin jit-memo 'lvmtab v) v)))))
   (jit-lvmof nm)                                    ; a dis row's nom -> its handler nom | ()
    (? ("lvm_" = snip (string nm) 0 4) nm
       (: t (jit-lvmtab ())
          s (string nm)
          (go l) (@ l ([n h] >< more) (? (s = n) h (go more)) ())
          (? (two? t) (go <>t) ())))

   ; --- THE SPLICER: op rows -> one function's worth of IR ------------------------------
   ; the VM's own convention, and the reason a handler's body needs no rewriting to be
   ; spliced: g=r6, Ip=r5, Hp=r2, Sp=r1, and every op takes its argument from Sp[0] and
   ; leaves its answer there. Two bodies laid end to end already agree about everything.
   jit-tail '((ld r0 r5 8) (add r5 r5 8) (jmpr r0))   ; the dispatch triple every handler ends in
   (dropn l n) (? (n < 1) l (dropn >l (n - 1)))
   (taken l n) (? ((n < 1) || atom? l) () (<l >< taken >l (n - 1)))
   (endswith? l t) (: n (tally l) k (tally t) ((k <= n) && (dropn l (n - k) = t)))
   ; ⚠ THE SPLICE CONDITION, structural and checked rather than assumed. A handler qualifies
   ; only if, tail off, it: touches no frame (the fld family runs `sub sp`), never reads Ip
   ; (an operand-taking op reads its operand FROM the thread, which a splice does not advance),
   ; and leaves itself nowhere -- `jmp lvm_gc` is the one that matters, because gc RESUMES AT
   ; Ip, which in a spliced body is the nif cell, so a collection halfway through would re-run
   ; the ops that already ran. A body failing any of these declines, by name.
   ; ⚠ a predicate answers a BIT: `&&` is value-preserving, so the clamp is not decoration --
   ; spec.l sweeps every `?`-named global in (names ()) and these three leak onto the book.
   (jit-frame? f) ?(member? <f '(sub add lea) && member? 'sp >f)     ; anything that moves the stack
   (jit-ipread? f) ?(member? 'r5 >f)                                ; ..or reads the thread
   (jit-leaves? f) (? (id? <f 'call) 1 (id? <f 'jmpr) 1             ; ..or leaves for good: an
                      (id? <f 'jmp) !(jit-lbl? <>f) 0)              ; internal jmp is the body's own
   ; ⚠ it answers WHICH law broke, not a bit: the tally of those noms is the coverage census,
   ; and each one names a different rung (a frame wants a spill, a gc jump wants the hoisted
   ; guard to own its Have, an Ip read wants the operand said).
   (jit-dirt fs)
    (@ fs (f >< r) (? (jit-frame? f) 'frame  (jit-ipread? f) 'ipread
                      (jit-leaves? f) (? (id? <f 'call) 'call  (id? <f 'jmpr) 'jmpr  'gcjump)
                      (jit-dirt r))
          ())
   ; an internal label. ⚠ `jl`, not gen.l's own `.e5044`: a leading dot does not survive the
   ; reader (it is an ordinary punct symbol, and there are no dotted pairs), so the serializer
   ; respells it -- one convention, stated at both ends.
   (jit-lbl? n) ?(nom? n && ("jl" = snip (string n) 0 2))
   (jit-rn f k)                                      ; this copy's internal labels, made its own
    (? (atom? f) (? (jit-lbl? f) (intern (string f + "_" + show k)) f)
       (jit-rn <f k >< jit-rn >f k))
   ; the push lowerings. an operand-taking op is not spliced, it is SAID -- three forms -- and
   ; the fld family (argcup/argcap/argtwo) is a push FUSED with an op, so the splicer UNFUSES
   ; it: push Sp[i], then splice that op's own handler. The fusion is the VM's, not the meaning's.
   (jit-push-a i) [['ld 'r0 'r1 (8 * i)] '(st r1 -8 r0) '(sub r1 r1 8)]
   (jit-push-q w) [['li 'r0 w] '(st r1 -8 r0) '(sub r1 r1 8)]
   (cwd v) ((2 * v) + 1)                             ; a charm operand as its tagged word
   (jit-fld nm) (? (id? nm 'lvm_argcup) 'cup  (id? nm 'lvm_argcap) 'cap
                   (id? nm 'lvm_argtwo) 'two?  ())
   ; a handler's clean body, tail off -- MEMOIZED, because the checking is per handler and the
   ; splicing is per copy. `[1 body]` or `[0]`; the reason is tallied on the first ask only.
   (jit-body h)
    (: m (peep jit-bodies h ())
       (? (two? m) m
          (: fs (jit-ir h)
             v (? !(two? fs) (: _ (jit-no ['no-ir h] ("no IR for " + string h + " in this binary")) [0])
                  !(endswith? fs jit-tail)
                   (: _ (jit-no ['no-tail h] (string h + " does not end in the dispatch tail")) [0])
                  (: b (taken fs (tally fs - tally jit-tail))
                     d (jit-dirt b)
                     (? (nil? d) [1 b]
                        (: _ (jit-no ['unclean h d] (string h + " is not splice-clean: " + string d)) [0]))))
             _ (pin jit-bodies h v) v)))
   (jit-splice h k)                                  ; ..made this copy's own | ()
    (: v (jit-body h) (? <v (jit-rn <>v k) ()))
   ; one row -> (forms >< words-pushed) | ()
   (jit-row r k)
    (: nm <r os >r
       ; ⚠ presence rides the wrapper, and it has to: operand 0 is the commonest one there is
       ; (arg 0, the first slot), and a bare `(nil? i)` reads that legitimate zero as absence --
       ; the row then declines in silence, which is exactly how it read the first time.
       i9 (? (two? os && charm? <os) [1 <os] ())
       j9 (? (two? os && two? >os && charm? <>os) [1 <>os] ())
       ; ⚠ THE PUSH-PAIR FAMILY, and the operands ride in SOURCE order off a MOVING Sp -- an
       ; index is relative to the Sp of its own load, so two pushes laid in order come out
       ; right with no arithmetic (love.c says this over PushA/PushQ, and jit-push-a inherits
       ; it by construction). ⚠ only a CHARM quote: dis answers 'x for a heap quote or a raw
       ; index, which is the roster refusing to hand a code word out, so those decline.
       ; ⚠ the guard leads each arm rather than riding a helper: an operand slot is read
       ; EAGERLY as an argument, so a shared (pp p q) would index the empty wrapper first.
       two9 ?(two? i9 && two? j9)
       (via h) (: b (jit-splice h k) (? (nil? b) () (b >< 0)))
       (? (id? nm 'lvm_aa) (? two9 ((jit-push-a <>i9 + jit-push-a <>j9) >< 2) ())
          (id? nm 'lvm_aq) (? two9 ((jit-push-a <>i9 + jit-push-q (cwd <>j9)) >< 2) ())
          (id? nm 'lvm_qa) (? two9 ((jit-push-q (cwd <>i9) + jit-push-a <>j9) >< 2) ())
          (id? nm 'lvm_qq) (? two9 ((jit-push-q (cwd <>i9) + jit-push-q (cwd <>j9)) >< 2) ())
          (id? nm 'lvm_arg)  (? (two? i9) (jit-push-a <>i9 >< 1) ())
          (id? nm 'lvm_arg0) (jit-push-a 0 >< 1)   (id? nm 'lvm_arg1) (jit-push-a 1 >< 1)
          (id? nm 'lvm_arg2) (jit-push-a 2 >< 1)   (id? nm 'lvm_arg3) (jit-push-a 3 >< 1)
          (id? nm 'lvm_quote) (? (two? i9) (jit-push-q (cwd <>i9) >< 1) ())
          (id? nm 'lvm_quo0) (jit-push-q 1 >< 1)   (id? nm 'lvm_quo1) (jit-push-q 3 >< 1)
          (id? nm 'lvm_quo2) (jit-push-q 5 >< 1)   (id? nm 'lvm_quo3) (jit-push-q 7 >< 1)
          (id? nm 'lvm_quom1) (jit-push-q -1 >< 1) (id? nm 'lvm_quom2) (jit-push-q -3 >< 1)
          (: fl (jit-fld nm)                        ; a fused push-and-op: unfuse it
             h (jit-lvmof (? (nom? fl) fl nm))
             (? (nil? h) (jit-no ['no-bridge nm] ("no handler nom for " + string nm))
                (nom? fl) (? (two? i9)
                             (: b (jit-splice h k) (? (nil? b) () ((jit-push-a <>i9 + b) >< 1)))
                             ())
                (two? os) (jit-no ['operand nm] ("operand-taking " + string nm + " is not composed yet"))
                (via h)))))
   ; ⚠ THE REFLOW, and it is the whole seam: a handler DELIVERS with (st r1 0 rX) and the next
   ; one OPENS by reading it straight back with (ld rY r1 0). Laid end to end those are adjacent
   ; -- one base, one name, nothing between -- so this is `stldp`'s law with none of its aliasing
   ; question, because both sides are ours. The load goes; a mov stands in when the registers
   ; differ (they never do today, both being r0). This is the op-boundary reload the whole arc
   ; is about, removed at the level where it is a pattern instead of an allocator.
   (jit-seam fs)
    (@ fs (['st 'r1 0 rX] >< ['ld rY 'r1 0] >< rest)
           (['st 'r1 0 rX] >< (? (id? rX rY) () [['mov rY rX]]) + jit-seam rest)
          (f >< rest) (f >< jit-seam rest)
          ())
   ; the whole body: ONE room guard ahead of any Sp motion, deopting to the twin and never to
   ; gc, then the spliced ops, then the dispatch tail advanced past the nif's own two-word cell.
   ; ⚠ the guard is the C route's, read back off its own emission: r7 = Hp + 8*peak + the avail
   ; floor, and an Sp below that takes Ip = cell(Ip[1].x) and continues into the twin -- which
   ; sees an untouched frame, because nothing has moved yet.
   ai-avail-floor 64
   (jit-forms rows)
    (: (go rs k pk net acc)
        (@ rs (r >< more)
              (? (id? <r 'lvm_ret) [acc pk net]
                 (: p (jit-row r k)
                    (? (nil? p) ()
                       (: n2 (net + >p)
                          (go more (k + 1) (? (pk < n2) n2 pk) n2 (acc + <p))))))
              ())
       b (go rows 0 0 0 ())
       (@ b [body0 pk net]
            ('((label jitbody))
             + (? (0 < pk)
                  [['add 'r7 'r2 (8 * pk)] ['add 'r7 'r7 ai-avail-floor] '(cmp r1 r7)
                   '(br ae jitgo) '(ld r5 r5 8) '(ld r0 r5 0) '(jmpr r0) '(label jitgo)]
                  ())
             + jit-seam body0
             ; ⚠ THE COLLAPSE, and without it the answer is a stack deeper than it started:
             ; an arity-1 nif is handed its argument in Sp[0] and owes its answer in that SAME
             ; slot. Every push the body made rides below it, so the result moves back up and Sp
             ; comes home -- Sp[net] = Sp[0]; Sp += net, the C route's own last line.
             + (? (0 < net) [['ld 'r0 'r1 0] ['st 'r1 (8 * net) 'r0] ['add 'r1 'r1 (8 * net)]] ())
             + '((ld r0 r5 16) (add r5 r5 16) (jmpr r0)))
            ()))

   ; --- the live symbol table: this process's own symtab plus its own load bias -----------
   ; we own both sides, so a JIT here resolves a symbol the way a debugger would and never the
   ; way a loader does: /proc/self/exe carries the table holo wrote, and the maps line whose
   ; path is ours and whose file offset is zero carries the bias. ⚠ never the FIRST maps line
   ; -- that is often an unrelated anonymous r-xp region.
   (bind-die m) (: _ (jit-say m) (welp '(bind)))
   (bind-rd p) (: s (jit-slurp p) (? (string? s) s (bind-die ("cannot open " + p))))
   (bind-le s i n) (: (go k a m) (? (k = n) a (go (k + 1) (a + peep s (i + k) 0 * m) (m * 256))) (go 0 0 1))
   (bind-live-tab _) (: exe (bind-rd "/proc/self/exe")
      shoff (bind-le exe 40 8) shentsz (bind-le exe 58 2) shnum (bind-le exe 60 2)
      (sh i f w) (bind-le exe (shoff + i * shentsz + f) w)
      symi (: (go i) (? (i = shnum) -1 (? ((sh i 4 4) = 2) i (go (i + 1)))) (go 0))
      _ (? (0 <= symi) 0 (bind-die "no symtab in /proc/self/exe"))
      stroff (sh (sh symi 40 4) 24 8)
      symoff (sh symi 24 8) syment (sh symi 56 8) nsyms (sh symi 32 8 // syment)
      (cstr i) (: (go j) (? (peep exe j 0 = 0) (snip exe i j) (go (j + 1))) (go i))
      (symva nm) (: (go i) (? (i = nsyms) -1
                     (: o (symoff + i * syment)
                        (? (nm = cstr (stroff + bind-le exe o 4)) (bind-le exe (o + 8) 8) (go (i + 1)))))
                   (go 0))
      symemo (tablet 0)
      exepath (? (member? 'readlink (names ())) (readlink "/proc/self/exe") (bind-die "no readlink nif"))
      base (: mp (bind-rd "/proc/self/maps") n (tally mp)
              (sp? j) (peep mp j 0 = 32)
              (eol? j) ((j = n) || (peep mp j 0 = 10))
              (nlend j) (: (go e) (? (eol? e) e (go (e + 1))) (go j))
              (skipsp j) (: (go e) (? ((e < n) && sp? e) (go (e + 1)) e) (go j))
              (tokend j) (: (go e) (? (eol? e || sp? e) e (go (e + 1))) (go j))
              (fld i f) (: (go j k) (: t (tokend j) (? (k = f) (j >< t) (go (skipsp t) (k + 1)))) (go i 0))
              (hex i e) (: (go j a) (: c (peep mp j 0)
                           (? (e <= j) a
                              (47 < c < 58) (go (j + 1) (a * 16 + c - 48))
                              (96 < c < 103) (go (j + 1) (a * 16 + c - 87))
                              a)) (go i 0))
              (lstart i) (: se (fld i 0) (hex <se (tokend <se)))
              (loff i) (: se (fld i 2) (hex <se >se))
              (lpath i) (: se (fld i 5) (snip mp <se (nlend <se)))
              (go i) (? (n <= i) (bind-die "exe not found in maps")
                        ((loff i = 0) && (exepath = lpath i)) (lstart i)
                        (go (nlend i + 1)))
              (go 0))
      ; ⚠ the symtab walk is linear and string-compares every entry, so a bare lookup is
      ; milliseconds -- per `la`, per copy. Both memos are load-bearing, not tidiness.
      (\ nm (: c (peep symemo nm 0)
               (? (0 < c) c
                  (: v (symva nm)
                     _ (? (v < 0) (bind-die ("no live symbol " + nm)) 0)
                     a (base + v) _ (pin symemo nm a) a)))))
   ; ..and the table itself is built ONCE: it slurps a 6 MB /proc/self/exe to get there.
   (jit-live _) (: t (peep jit-memo 'live ())
                   (? (two? t) <>t
                      (: v (bind-live-tab 0) _ (pin jit-memo 'live [1 v]) v)))

   ; ⚠ NO RELOCATIONS, because a JIT knows the answers: the one external reference a clean
   ; handler carries is (la rX sym), and at runtime that symbol's live address is a NUMBER --
   ; so it becomes (li rX addr), a movabs, and the whole body is position-independent.
   (jit-abs fs lv)
    (@ fs (['la r s] >< rest) (['li r (lv (string s))] >< jit-abs rest lv)
          (f >< rest) (f >< jit-abs rest lv)
          ())
   ; ⚠ THE BLOB CACHE, and the op rows ARE the key -- the same rows denote the same bytes, so a
   ; second closure of a shape already spliced pays a tablet lookup and a `nif`, not an assemble.
   ; This is the glaze's own arrangement (compile per SOURCE SITE, creation reuses the blob) and
   ; it is what makes a creation-time tier affordable at all. `[1 bytes]` or `[0]` -- a decline
   ; is cached too, since re-deciding it is the same work and the tally already counted it once.
   ; ⚠ the bytes carry LIVE addresses, so this cache is as in-process as they are: it is a field
   ; of this love, never written out, and nothing here may ride into a baked image.
   (jit-blob rows)
    (: m (peep jit-blobs rows ())
       (? (two? m) m
          (: fs (jit-forms rows)
             v (? (nil? fs) [0]
                  (: bs (peep ((from 'holo 'assemble) 'x64 (jit-abs fs (jit-live 0))) 'bytes 0)
                     (? (two? bs) [1 bs] (jit-no ['holo] "holo assembled nothing") [0])))
             _ (pin jit-blobs rows v) v)))
   ; ⚠ RE-ENTRANCY, and it is not optional once this rides the creation hook: everything below
   ; is love, so splicing BUILDS closures -- dis's walk, holo's assembler, the tablets -- and
   ; each of those creations re-enters ala, the hook, and this door. The guard makes the splicer
   ; invisible to itself. A scare inside leaves it set, which parks the lane for the rest of the
   ; session: the fail-safe direction, since the alternative is a runtime that cannot build a
   ; closure without recursing.
   jit-busy #0
   jit-fires #0                                      ; how many closures this door native-backed
   (jit-fired? n) (n < peep jit-fires () 0)
   ; THE DOOR: compose from what the compiler wrote down, assemble with holo, install.
   ; Nothing here is a fallback -- a row this cannot say declines by name and the closure
   ; stays exactly what it was.
   ; the manual door. ⚠ `nif` keeps src as value[-1] and the printer reads it as a DATUM, so
   ; handing it the twin CLOSURE prints a mint, not a source: the round trip through the twin's
   ; own printed form is what makes =/show agree here. The hook has the real `s` and uses jit-s.
   (jit f) (: r (sound (show f)) (jit-s f (? (two? r) <r '(\ x x))))
   (jit-s f s) (? (0 < peep jit-busy () 0) ()
                  (: _ (pin jit-busy () 1)
                     r (jit-1 f s)
                     _ (pin jit-busy () 0)
                     _ (? (nil? r) 0 (pin jit-fires () (1 + peep jit-fires () 0)))
                     r))
   ; ⚠ ARITY IS THE CUR ROW, and everything else about a multi-arg splice is already true:
   ; `lvm_arg i` indexes Sp[i] whatever the frame's width, the collapse (Sp[net] = Sp[0];
   ; Sp += net) restores the entry Sp and so is arity-blind, and `nif` lays an arity>=2 cell
   ; that curries to saturation and enters the body at Ip+2 -- with lvm_ret and the interp
   ; twin at THE SAME OFFSETS as the arity-1 cell, which is love.c's own stated law and the
   ; reason the dispatch tail and the guard's deopt need no arity of their own.
   (jit-1 f s)
    (: rows0 (? (member? 'dis (names ())) (dis f) ())
       r0 (? (two? rows0) <rows0 ())
       cur ?(two? r0 && id? <r0 'lvm_cur)
       a9 (? !cur [1 1] (charm? <>r0) [1 <>r0] ())     ; presence rides the wrapper: arity 0 is no arity
       rows (? cur >rows0 rows0)
       (? (atom? rows0) (jit-no ['husk] "dis answers nothing (is this closure already native?)")
          (nil? a9) (jit-no ['no-arity] "a cur row with no readable arity")
          (: b (jit-blob rows)
             nf (from 'glaze 'nif)
             (? !<b ()
                (nil? nf) (jit-no ['no-nif] "no glaze nif on this build")
                (: j (nf (cask <>b) f s <>a9)
                   (? (nil? j) (jit-no ['nif-declined] "nif declined the bytes") j))))))
   )
