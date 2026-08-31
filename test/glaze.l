; the glaze's install seam, arch-neutral half: guards only, never an actual run (executing bytes
; is machine-specific) -- plus the decline laws below, which run everywhere and answer the same on
; every target by construction: that is the whole point of a lane that knows when to stand down.
; The executing tests are test/glaze-x86.l; the mechanism, love/glaze/readme.md.
; `nif` -- the codegen-backend install seam (love.c): (nif code interp src ..) wraps
; emitted bytes in a transparent applicable native closure. neutral here (no
; install/run -- the bytes are x86-64; the real round-trip lives in love/glaze/emit.l):
; non-byte / empty code installs nothing.
; by key, never bare: every boot pulls nif/nifx off the book (src/main.c), so raw byte
; installation is reachable only through the glaze, which is the only thing that does it.
; a bare `nif` here would read the zero point and every claim below would answer 1.
; and under the presence guard: love0 and the kernel/wasm frontends carry no glaze, so
; there is no seam there to hold to its word -- (from 'glaze) answers () and this stands down.
(: G (from 'glaze)
   (? G (: N (G 'nif)
           (assert (lit? N)
                   !(N 5 0 0 1) !(N 'x 0 0 1) !(N () 0 0 1) !(N "" 0 0 1)))
      0))

; --- the natjit group lane declines what it cannot emit -------------------------------
; jitgroupir's codegen (emit.l cggir/cggvir) compiles a leaf that is a frame param or a
; whole-number immediate, and nothing else: every other leaf -- a string, a nom, a list, a
; gem -- falls off the end of their `?` into a silent (), emitting no code and leaving
; whatever the accumulator last held. So the lane is only ever as sound as the recognizer
; in front of it, and hook.l's natjit entrance had none: it gated on jgir-ok?, which
; answers "does jitgroupir implement these ops" and says in its own header that it does not
; re-derive the grammar. It was written as a backstop under autogroup, which had already run
; grpfix. hook.l now runs grpfix too (grp-ok?), so both entrances share one recognizer.
;
; each helper below is a `:` whose one binding is a self-recursive helper called from its
; body -- the shape anat-defs reads as a group -- carrying a value the integer group lane
; cannot hold. the wrong answers they used to give are noted; note how plausible they are,
; which is why this is a law and not a footnote: `(acc + " ")` compiled as `acc + k` and
; three spaces came back the triangular number 6.
(: (scar-sp n)   (: (go k acc) (? (k < 1) acc (go (k - 1) (acc + " "))) (go n ""))    ; string built by +
   (scar-dash z) (: (g j) (? (j < 1) "" ("-" + (g (j - 1)))) (g (tally z)))           ; ..the argument-applying twin
   (scar-lit n)  (: (go k acc) (? (k < 1) acc (go (k - 1) "Q")) (go n 0))             ; a bare string constant
   (scar-link n) (: (go k acc) (? (k < 1) acc (go (k - 1) (link acc 1))) (go n 0))    ; a cons
   _ (assert
      ("   " = (scar-sp 3))                                  ; was 6  -- the triangular sum of k
      ("--" = (scar-dash "ab"))                              ; was 1
      ("Q" = (scar-lit 2))                                   ; was 0
      ((link (link (link 0 1) 1) 1) = (scar-link 3)))        ; was 1
   0)

