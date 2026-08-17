# glaze: the delet soundness law is red

Delete this file when the fix lands.

`make test_glaze` fails at HEAD on the delet-soundness section of test/glaze-x86.l
(the last section): the `_`-bound `(put j ..)` pair is dropped, so

    ;; assert (= (slurp j) (+ (+ "" 1) 2))

`slurp j` should answer both bytes ("12"). This is the exact miscompile the section
was written to pin -- auto.l's value-let inliner (delet, love/glaze/auto.l ~615)
dropping a `_`-bound effect when a pure-integer leaf sibling makes the group
recognizer walk the form.

## what is known

- Both lanes fail identically: the egg (`LOVE_NO_IMAGE=1`) and the woken image.
  Not a boot-lane or bake artifact.
- Reproduce:
  `{ echo "(use 'holo)"; cat crew/holo/x64.l crew/holo/arm64.l test/glaze-x86.l; } | love`
  exit 1, every earlier section ok.
- NOT cec006ab (the multi-line conjunction restyle of auto.l): rebuilding with
  cec006ab^'s auto.l alone (auto.h regenerated, love relinked) fails the same way.
- test_glaze sits in test_extra, so the red predates its discovery; the suspects
  are whatever moved between the last green test_extra and now.
- test_glazefuzz is GREEN (3000 cases, 3340 closures native-backed): the fuzz's
  closure shapes never build a `_`-bound effect under a leaf sibling, so the law
  file is the only net for this class.

## where to look

Candidate commits over the glaze/recognizer surface, newest first:

- df18aeb3 -- bao rewrite "plus the last accessor leftovers elsewhere"
- 8332bfe3 -- glaze: emit, auto, hook and fuzz in the new style (the earlier restyle)
- 57c1a921 -- the jit becomes a module
- 99cd8882 -- welow retired, "auto.l keeps the front-half"

The differential method that cleared cec006ab clears or convicts each in minutes:
`git checkout <commit>^ -- love/glaze/auto.l && make test_glaze` (auto.h and the
binary rebuild from the file). If auto.l swaps never go green, the break is outside
auto.l -- diff the delet section's INPUT instead: dump the form reaching delet at
the failing site and compare against a green build's.

The fix gates on `make test_glaze` and should ride with `make test_glazefuzz`.
