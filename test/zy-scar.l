; test/zy-scar.l -- THE SCAR: shapes this tree compiles WRONG today. every other
; corpus file states something true; this one states something that OUGHT to be
; true and currently is not, so that a silent wrong answer is a loud red instead.
;
; ⚠ IF THIS FILE IS WHAT FAILED, the language is fine and the COMPILER is not.
; nothing you just edited caused it. read on.
;
; it sorts LAST on purpose (zy- , just ahead of zz-fin): a failed assert stops the
; run, so a scar mid-corpus would mask every file after it. everything else has
; already had its say by the time this speaks.
;
; --- what is wrong -----------------------------------------------------------
; the same source, built two ways from this same tree, disagrees:
;
;   out/host/love   mooncc-built, ai_tco=1   answers the CHARM 6
;   out/host/love0  gcc-built,    ai_tco=0   answers "   "   <- correct
;
; THE TRIGGER: a `:` whose ONLY binding is a self-recursive helper, called from
; its body. a second binding -- before or after, related or not -- makes it right
; again, and so does hoisting the helper to the enclosing scope. that is what the
; controls below pin: three ways to build the same three spaces that DO work, so
; a failure here can only be the shape and never the arithmetic.
;
; ⚠ NECESSARY, NOT SUFFICIENT. not every instance misfires -- crew/vi/core.l's
; vput walks its lines through this exact shape and is correct, as does
; crew/vi/hue.l's hue-carry. so a passing instance got LUCKY; it is not evidence
; that a file is clean, and there is no way from here to tell which of the tree's
; many uses of this idiom are currently wrong. that is the reason this is a scar
; and not a footnote.
;
; --- it is MOONCC'S CODEGEN, not the tail-threaded path ----------------------
; these very asserts settled that on the day they landed, which is the argument
; for having written them down. the corpus runs on four builds and only ONE is
; wrong:
;
;   love0    gcc,   ai_tco=0   3638 -> 3644   PASS
;   kernel   clang, ai_tco=1   3558 -> 3564   PASS   <- tco=1 and CORRECT
;   wasm     emcc               3582 -> 3588   PASS
;   host     mooncc, ai_tco=1                  FAIL
;
; this file adds exactly SIX asserts, so a lane that gained six really did run
; it -- the passes are not a corpus that quietly skipped the file. ai_tco=1
; appears in a build that is right (the kernel, port/inle/kernel.mk's `KCC ?=
; clang` and `-Dai_tco=1`) and in the one that is wrong, so the threading is not
; the discriminator. THE COMPILER IS. three independent C compilers agree with
; each other and mooncc does not.
;
; --- when it is fixed --------------------------------------------------------
; this whole file goes green on its own. DELETE IT THEN -- a fixed bug's tree doc
; is git log, and a scar that has closed is just scar tissue. the two asserts are
; the only thing here worth keeping, and only until they pass.
;
; found 2026-07-28 writing crew/vi/{hue,core}.l -- twice, in two different shapes,
; which is how it earned a file. worked around at tools/hue2vim.l's `pad` and
; crew/vi/core.l's `vsp`, both of which carry a pointer back here.
(: ; the shape, with an accumulator: answers the charm 6 instead of three spaces
   (scar-sp n) (: (go k acc) (? (k < 1) acc (go (k - 1) (acc + " "))) (go n ""))

   ; the shape again, argument applying the enclosing parameter: the helper reads
   ; as an UNFILLED cell, the zero point answers 0, and (0 x) is 1
   (scar-dash z) (: (g j) (? (j < 1) "" ("-" + (g (j - 1)))) (g (tally z)))

   ; --- the controls: the SAME computation, without the trigger --------------
   (ctl-sp n) (: z 0 (go k acc) (? (k < 1) acc (go (k - 1) (acc + " "))) (go n ""))
   (ctl-go k acc) (? (k < 1) acc (ctl-go (k - 1) (acc + " ")))

   _ (assert
      ; these three pass, and pin the arithmetic so the scars below can only be
      ; the shape: by hand, with one extra binding, and hoisted out
      (= "   " ("" + " " + " " + " "))
      (= "   " (ctl-sp 3))
      (= "   " (ctl-go 3 ""))
      (= "--" (: (g j) (? (j < 1) "" ("-" + (g (j - 1)))) z 0 (g 2)))

      ; --- THE SCAR. these two are the bug. ---
      (= "   " (scar-sp 3))
      (= "--" (scar-dash "ab")))
   0)
