; --- doc/proto/spl.l: CALL vs SPLICE, modeled -- an ev.l inliner design spike ---
; ev.l inlines nothing above the nif level. this model is the license an inliner
; would need, said runnable: a stack machine in ev's emitter shape (an emitter is
; (\ k ..) prepending onto the continuation's thread -- love/ev.l "THE EMISSION
; INTERFACE", doc/hom.md the design), and THREE ways to compile a call site,
; which are one body compiler `cb` handed three param READERS:
;
;   ecall   -- the calling convention: args delivered into SEATS, body reads seats
;   ebind   -- the splice, by BINDING: args into locals, body reads locals
;   esubst  -- the splice, by SUBSTITUTION: the arg EMITTER is the reader
;
; the observation is (value . trace) -- an `emit` effect op appends to the trace
; and leaves the stack alone, so a duplicated effect leaves the VALUE untouched
; and only the trace can convict. the laws:
;   * license: ecall == ebind as (value . trace), every sample -- the seat
;     routing is the representative-object bridge, un-forced (hom.md's move)
;   * the flip: a param read TWICE under an effectful arg -- esubst duplicates
;     the effect (trace 2 where bind has 1) while the VALUES STAY EQUAL: the
;     classic by-name breach, invisible at R, and why pureset is a LICENSE
;   * order: arg effects precede body effects, call and bind alike
; NOT modeled: the boxed return (the register machine's s4 laws carry the tail
; story), scare/abort mid-body (a scare here is a trace record, not a control
; exit), recursion. run: cat test/00-init.l doc/proto/spl.l | out/host/love
;
; the uu lift: tools/spl2uu.l emits these threads as test/uusplgen.l;
; test/uuspllaw.l proves the laws forall states, and the cross-machine theorem
; meets the register machine's de5bck there.

; --- the machine: ops are (op x); state [stack locals seats trace]; A the
; --- caller's environment. emitters prepend, compose runs left first.
(: (kop op x) (\ k (link [op x] k))
   (kpushk x) (kop 'pushk x)                        ; push a constant
   (kpusha i) (kop 'pusha i)                        ; push caller arg i
   (kpushl i) (kop 'pushl i)                        ; push local i
   (kpushs i) (kop 'pushs i)                        ; push seat i
   (ksetl i) (kop 'setl i)                          ; pop into local i
   (ksets i) (kop 'sets i)                          ; pop into seat i
   kadd (kop 'add 0)                                ; pop two, push the sum
   kemit (kop 'emit 0)                              ; append TOS to the trace, keep it
   (kseq a b) (\ k (a (b k)))                       ; compose: a's ops first

   ; the interpreter: a thread against env A -> (value . trace)
   (put9 al i v) (link [i v] al)
   (get9 al i) (: e (assoc i al) (? (two? e) (cap (cup e)) (scare 'spl-unbound i)))
   (in9 th A st lo se tr)
    (? !(two? th) (link (cap st) (rev tr))
       (: op (<th 0) x (<th 1)
          (? (id? op 'pushk) (in9 >th A (link x st) lo se tr)
             (id? op 'pusha) (in9 >th A (link (get9 A x) st) lo se tr)
             (id? op 'pushl) (in9 >th A (link (get9 lo x) st) lo se tr)
             (id? op 'pushs) (in9 >th A (link (get9 se x) st) lo se tr)
             (id? op 'setl)  (in9 >th A >st (put9 lo x <st) se tr)
             (id? op 'sets)  (in9 >th A >st lo (put9 se x <st) tr)
             (id? op 'add)   (in9 >th A (link ((cap (cup st)) + <st) (cuup st)) lo se tr)
             (id? op 'emit)  (in9 >th A st lo se (link <st tr))
             (scare 'spl-op op))))
   (run em A) (in9 (em ()) A () () () ())

   ; --- the body compiler: exprs ('p i) ('a i) ('k n) ('add e e) ('emit e),
   ; --- prd the param reader -- the three strategies ARE three prd's
   (cb e prd)
    (? (id? <e 'p) (prd <>e)
       (id? <e 'a) (kpusha <>e)
       (id? <e 'k) (kpushk <>e)
       (id? <e 'add) (kseq (cb <>e prd) (kseq (cb <>>e prd) kadd))
       (id? <e 'emit) (kseq (cb <>e prd) kemit)
       (scare 'spl-expr e))

   ; --- the three call-site compilers, one body against three readers ---
   (args9 as f) (: (go as i) (? !(two? as) (\ k k)
                                (kseq (kseq (cb <as ()) (f i)) (go >as (i + 1))))
                   (go as 0))
   (ecall as body) (kseq (args9 as ksets) (cb body kpushs))
   (ebind as body) (kseq (args9 as ksetl) (cb body kpushl))
   (esubst as body) (cb body (\ i (cb (as i) ())))

   ; --- the samples; a0e is the effectful arg (reads caller arg 0, emits it) ---
   a0e '(emit (a 0))
   e0 ['((a 0)) '(add (p 0) (k 1))]                 ; pure: y + 1
   e1 [[a0e] '(add (p 0) (k 1))]                    ; effect, param read once
   e2 [[a0e] '(add (p 0) (p 0))]                    ; effect, param read TWICE
   e3 [[a0e] '(emit (k 5))]                         ; order: arg effect, then body's
   A9 '((0 7))                                      ; the demo env: arg 0 = 7

   (both f s) (run (f (s 0) (s 1)) A9))

; --- the license: call == bind as (value . trace), every sample ---
(assert
 ((both ecall e0) = (both ebind e0)) ('(8) = (both ebind e0))
 ((both ecall e1) = (both ebind e1)) ('(8 7) = (both ebind e1))
 ((both ecall e2) = (both ebind e2)) ('(14 7) = (both ebind e2))
 ((both ecall e3) = (both ebind e3)) ('(5 7 5) = (both ebind e3)))

; --- the flip: substitution duplicates the effect; the VALUE cannot see it ---
(assert
 ((both esubst e0) = (both ebind e0))              ; pure: substitution is licensed
 ((both esubst e1) = (both ebind e1))              ; read once: still licensed
 (!(= (both esubst e2) (both ebind e2)))          ; read twice: the breach
 ('(14 7 7) = (both esubst e2))                    ; ... value 14 EQUAL, effect DOUBLED
 ((cap (both esubst e2)) = (cap (both ebind e2))))

(puts "spl: ok") (putc 10)
