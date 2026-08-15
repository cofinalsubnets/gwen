# the honest face is the missing face of the answer

A reading of the condition system. `help` is the global function that turns a raise into an
**answer** (`(help a b)` → a value, delivered back at the raise site). So when help is missing,
the *answer* is missing — and what shows instead is the **honest face** `;; a b`
(host/main.c's scare exit-face, `ai_scare_face_`).

**Three layers when a condition rises:**
1. **help present** → an **answer** (a value).
2. **welp** (the floor handler — help that has given up, `love/bao.l`) → the **zero point** for a
   bare scare: absence *caught* and handed back as a value (the fromempty, `()`).
3. **nothing at all** → the **honest face** `;; a b`, terminal — not a value, just shown.

So the honest face sits **below even the zero point**. The zero point is the face of a
missing **nom** — absence in the *value* space, which the floor still resolves to a value.
The honest face is the face of a missing **answer** — absence in the *condition* space, with
no floor to convert it, so it never becomes a value: it is just *shown*, and stops.

That is why it is **honest**: an answer would *resolve* the condition; the honest face refuses
to fake one — it shows the raw `a b`, unresolved, and admits there is nothing behind it. The
zero point is absence pretending (gracefully) to be a value; the honest face is absence not
even pretending. **The honest face is the missing answer made visible — the zero point's
reflection across the help boundary, one floor further down.**

The face is only as legible as its payload: a raise carrying two mute numbers gets `;; 0 11`,
which is honest in form and silent in content. A condition worth showing raises a word.

**`trap` is the fourth answer: the condition delivered somewhere else.** The three layers above
all answer *at the raise site* — the help's value is what `(scare a b)` evaluates to. `trap` is a
help that escapes through `call-cc` instead, so the condition arrives at the trapping frame:
`(trap f x n)` answers `(f x)` when it ran clean, and `(n a b)` when a scare landed there. That
is peg's and kanren's `(\ s y n)` shape aimed at conditions, less the success continuation a
caller already has — a clean run just answers, and `n`'s own answer is the form's, so which one
you got is a thing the caller wrote both halves of and no wrapper has to carry. `n` wears the
help's own arity, so `welp` is one. It nests, and the displaced help is back before `n` fires —
a raise inside `n` belongs to the trap *outside*, never to this one.

⚠ **Nothing unwinds.** The escape jumps past every pending restore between the raise and the
`trap`, so a `trap` is only right where the cleanup can live at the *landing site* — moon's entry,
where a compile ends anyway, or a kiosko session, where the socket close already sits after the
call. Where the cleanups are strung along the path (lush's fd swaps, its errexit depth), the
value-threading that pays a test per node is the cheaper trade, and it is not a defect that it is.
lush plans its AST into closures once (`sh-plan`), so what a run pays per node is that one `two?`
on the result and nothing else — the dispatch an escape would have skipped is already gone.

**`mind` is the fifth answer: the cleanup owned by the resource.** `(mind f x c)` answers `(f x)`
and fires `c` on the way out, clean or raised, once either way, with the condition riding on
outward — so the `trap` that was going to catch it still does. It is `trap` with the ownership
turned around: `n` is the *catcher's* cleanup and has to sit in the catcher's frame, `c` is the
*resource's* and travels with it, which is what lets a helper hold its own port however deep under
a `trap` it sits. Built on `trap`, no C: `c` runs inside the `n`, where the displaced help is
already back, and then re-raises. ~145ns over a bare call.

⚠ **The escape is its task's.** A help is *inherited at spawn*, so a child raising under a help it
never installed would invoke a continuation captured in its parent's stack — landing there tears
both, and it took whole programs down. `trap` records the installing task (`myself`) and compares
before it jumps: same task, escape; different, re-seat `prev` and raise *there*, so the child meets
the help it would have met with no `trap` in sight. An outer `trap`'s help meets the same test, so
the chain walks down to an answering help or to none. This is the difference between a runtime a
determined user can do anything with on purpose and one a naive user breaks by accident — `twirl`
inside a `mind`ed body is an ordinary thing to write, and it must not be a trap.

⚠ **Raises only, and that is where this stops.** Only a scare reaches a help, so a bare `(k v)`
into a continuation captured outside jumps clean past every `c` between there and here — pinned as
a passing assert in test/help.l, so the rung that ever closes it fails a law instead of moving the
semantics in silence. Closing it means running love thunks from inside `lvm_kcall`, which in a
tail-threaded VM is a synthesized frame chain, plus a per-task hook slot mirroring `hot_help` at
all four of its sites. Not bought, and on the evidence not worth buying: `call-cc` outside the
tests is `trap` itself and uu's `rejects`, both of which land in the frame that captured — there
is no `k` in this tree that outlives its own escape. The paragraph above is the reason it stays
that way: where cleanups string along the path, threading is the cheaper trade, and `mind` is for
where they do not.

Relates: [[faces]] (the hourglass / one core), the zero point + `welp` in test/spec.l's control
section.
