# autogroup — when the glaze rewrites a file it did not need to

`autogroup` (love/glaze/auto.l) is the entry the ev-rebind hands every top-level `:`.
It lowers the form through nine passes, looks for a glazeable group of first-order
functions, and — if one survives — hands back **the lowered form** in place of the
source. Two facts about that shape, taken together, turned an ordinary library into a
file whose sibling calls read the live book.

```love
(: form (plift (lift (delet (defoliate (loopclose (autospec (debool (dehof (dechurch
                                                     (fold-consts form0))))))))))
   …
   (? (atom? grp) form0                     ; nothing glazes -> the SOURCE, lowering discarded
      (': >< rewrite-bindings >form gnames gdefs)))   ; else -> the LOWERED form, all of it
```

The lowering runs on every candidate file whether or not a group survives. When none
does, its output is thrown away — so a lowering that is wrong is **silent** until some
unrelated change makes `grp` non-empty, and then the whole file compiles wrong at once.

## dehof left names dangling

`dehof` inlines a `:`'s non-recursive higher-order lambda-constant bindings and drops
them. The substitution was one `foldl` of `subst-sym` in binding order:

```love
sub (\ e (dsimp (foldl (\ b pr (subst-sym <pr >pr b)) e lamcs)))
```

A value substituted for a *later* lamc still names an *earlier* one — already spent in
the fold, so never rewritten inside that text, and its binding already dropped. On
lib/source.l six bindings left the form (`src-head src-lay-tgz src-lay-love src-lay
src-ccworks? src-farm`) while `src-lay-love` was still called from `source-main`. The
name then resolved against the book: `;; missing src-lay-love`.

The fix keeps a lamc bound when another lamc's body names it, and takes `keep` from the
final inline set rather than from `lamc?`.

## swgather swallowed by spelling

`swheads` collected call heads with no binder set, and `swgather` pulls in any global
`srcrec` recorded under that name. A name bound as a **local** therefore reached for a
global of the same spelling:

```
ctrl   fns=(tar-get src-die … src-ours_go)       grp=()       -> source.l compiles as written
probe  fns=(want tar-get src-die … src-ours_go)  grp=(want)   -> source.l compiles LOWERED
```

`want` is a local in `seed-main`. The probe's global was a plain `(: (want a b) 0)` in
an earlier layer — body-less, so `srcrec` recorded it — and it glazes trivially, so the
group came back non-empty and the lowered form was committed. That is the cliff: **any
top-level name, anywhere in the session, could switch a file from "compiled as written"
to "compiled from a nine-pass rewrite"**, and which name did it depended on the book.

`swheads` now carries a binder set — `:` binding names, and the fn's own params at the
seed; a `\` subform was already skipped whole. Bigger `bnd` only shrinks the swallow
set, and a callee that misses being swallowed makes `grpfix` reject its caller, so the
group closes smaller or not at all: the direction costs an optimization, never invents
one. `[sw] (want tar-get)` becomes `[sw] (tar-get)` — the stranger leaves, the real
global call head stays.

And because removing one stranger does not remove the shape that made a stranger fatal,
`autogroup` declines when nothing in the surviving group is a binding of this form.
`rewrite-bindings` would have rewritten nothing in that case anyway.

## closed: swheads reads opfixed source

`autogroup` hooks the ev-rebind, so the pipeline walks the form before infix is
factored — and there every `(a op b)` wears its left operand as a head. `(want =
seed-arch ())` is how `want` reached the walk at all; `(x + 1)` and `(n <= i)` arrive
the same way. The cure looked large because the pipeline and `rewrite-bindings` are
written against the raw form — but head collection is a read-only analysis, so only it
had to move: `swheads` now opfixes its input (`opfix` is deep, idempotent, and keeps
binders verbatim) and walks factored source, where an operand can never masquerade as
a head. The binder set stays — a real call head can still be a local fn. The law rides
test/glaze-x86.l.

## the method

The failure looked like a letrec/global-name bug in c0's `ana_v` — a sibling reference
emitted as `lvm_index`. It was not c0 at all: `-l` and `-e` compile through love/ev.l,
and instrumenting every `ana_v` return lane gives byte-identical traces for the failing
and the working session. One flag names the lane in a single run:

```sh
LOVE_NO_GLAZE=1 love -l probe.l -l lib/gz.l -l lib/tar.l -l lib/source.l \
  -e "((peep (from 'verbs 'tab) 'src 0) (list \"/tmp/lay\"))"
```

From there the bisect is per-pass: print each stage's binder-name list beside a
`(occurs? 'the-missing-name form)` bit, and the pass that drops a name it still
references names itself.
