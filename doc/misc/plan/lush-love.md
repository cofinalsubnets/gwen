# plan: love interpolation in lush

A shell-compatible spelling that evaluates a love expression and splices the
answer into a word. The engine half is nearly free — lush *is* love (a cat of
eight `.l` files under a `love` shebang), so `sound`, `opfix` and `ev` are aboard
in-process before lush even loads, and cli.l's `-e` lane already states the law
for string → value (read every datum, fold many to one application, `'torn` means
unfinished). The cost is all in the seams: where the spelling lands in the lexer,
and what law the value obeys in the word machine.

## the terrain

- `lex.l`'s `sh-dol` is the single dispatch on the byte after `$` — one new arm.
  But the dispatch is spelled five times (top-level `lx`, its nested `dq`,
  `sh-dollarize` for heredocs, `sh-wordize` and *its* `dq`), and the seg-kind walk
  is spelled four times in `word.l` (`sh-textof`, `sh-patmask`, `dqat`, `wseg`).
  Miss one and the form works bare but not in `"…"`, or not in a heredoc.
- the body scanner cannot be `sh-subp`: it tracks shell quotes, and a love body
  runs on different lexical law — `'` is a sigil not a quote, `"` strings, `#`
  comments, `#(`, `[ ]`, `\` lambda. The scanner must speak love's quoting while
  sitting inside a shell word. This is the one genuinely new piece.
- the spelling space is tight. `$((…))` is POSIX arithmetic (unimplemented here,
  but taking it forecloses arithmetic and mis-eats imported scripts); `$[…]` is
  ksh arith; `$'…'` is Issue 8; `${!…}` `${…//…}` are bash. The safest ground is
  a spelling that is a hard *error* in lush today — `${` with a non-POSIX op
  reaches `'unterm` — because claiming an error can't silently reinterpret any
  existing script.
- word law: a love value is not a chomped byte stream. What does a list answer —
  N fields like `"$@"`, or one string? Does the result IFS-split, does it glob?
  No precedent (`sh-runsub` only chomps newlines).
- there is no conformance suite; the only collision net is whatever text the
  existing gates happen to contain. The spelling choice carries that risk alone.

## the ladder

- **rung 0 — the eval lane, no syntax.** A `love` builtin: `love EXPR...` evals
  its arguments as one love form and prints the answer; status from the truth
  bit. Lands `sh-loveval` (sound → fold → opfix → ev, wrapped like `sh-imgwrap`'s
  `urun` so a bad form is a status, never a dead shell) and the value→text rule,
  with zero collision surface. Gate: asserts in test/host/sh.l.
- **rung 1 — the love body scanner.** A sibling of `sh-subp` that finds the end
  of a balanced love form under love's own quoting rules, plus a new `'more`
  reason so PS2 gathers an unfinished form (bao's `m`-nom protocol, same shape).
  Provable alone against torn/nested/quoted bodies before any wiring.
- **rung 2 — the spelling.** One arm in `sh-dol`, one seg kind, wired through all
  five `$`-dispatch sites and all four seg-kind sites. Chosen (revisable):
  `${(…)}` — a hard error today, so nothing changes meaning; inside `${}` the
  existing brace scanner already owns the extent. Word law chosen (revisable):
  the answer is one field, never IFS-split, never globbed — quiet like `"${x}"`;
  a list shows in love notation. Splitting can be bought later by unquoting
  conventions if wanted.
- **rung 3 — the round trips.** Heredoc bodies, `${x:-…}` word position, inside
  `"…"`, inside a `$( )` body (which re-lexes at expansion time — the escaping
  must be idempotent across that). doc/lush.md's expansion paragraph, e2e harks.

## choices (revisable)

- rung 0's builtin ships even if the syntax never does — `love` at the prompt is
  a calculator and a probe, and it is the severable half of the arc.
- do not factor the five `$`-dispatch copies as a precondition; add the arm five
  times like the existing forms do, and let a factoring be its own change if the
  duplication ever bites again.
- errors contain, never kill: a bad form is status 1 plus a line on stderr, same
  posture as an unset var under `set -u`.

## difficulty

Medium. No research risk — every mechanism has an in-tree precedent except the
love-quoting body scanner, which is small and provable in isolation. The cost is
carefulness: nine duplicated dispatch sites, the marshalling law, and a spelling
decision whose failure mode (silently re-meaning existing scripts) the gates
cannot catch.
