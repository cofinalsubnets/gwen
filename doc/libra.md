# libra ⚖ -- the .l balance tool

the scales. libra weighs a `.l` file four ways and they are all the same
weighing: `libra` says what is wrong with it, `libra fmt` lays it out,
`libra infix`/`unfix` turn it inside out, and `libra doc` lifts its header out as
a document. one scanner (`lib/lint.l`) under all of them, so the formatter, the
gate and the doc lifter can never disagree about what a paren or a comment is.
the tool is `crew/libra/libra.l`, the gate `make test_hostnif`
(test/host/libra.l), and `make lint` runs it over every tracked `.l`.

an LSP server lived here until 2026-08-16 -- `libra serve`, publishing the same
scan as diagnostics over json-rpc. it never had a consumer, so it went; `lib/json.l`
stays, with no consumer of its own outside its gate.

## the verbs

```
libra FILE ..           weigh them -- balance is the DEFAULT verb, so a bare
                        file list is the whole command
libra check FILE ..     the same thing, spelled out
libra -w FILE ..        ...and strip trailing whitespace while you are there
                        (WITHOUT reindenting)
libra fmt FILE ..       lay it out on stdout; -w rewrites, -n only checks
libra fmt -p FILE ..    ...and MINIFY THE PARENS while you are there
libra infix FILE ..     print it MAX-INFIX and MIN-PAREN, on stdout
libra unfix FILE ..     print it back as PURE PREFIX LISP, on stdout
libra doc FILE ..       its HEADER COMMENT as a document, on stdout
libra doc -ht FILE ..   ...as html; -rf for a man page
libra -h                the usage
```

check is the default because it is the errand that recurs: `make lint` runs it
over every tracked `.l`. every other verb is run by hand and named, and nothing
is gated on layout.

**infix and unfix are the two directions of one pass.** `unfix` is just `opfix`,
the factor pass a compile already runs, printed instead of compiled. `infix` is
its RIGHT INVERSE ([`lib/infix.l`](../lib/infix.l)): for any form it answers a
surface that factors back to exactly that form, choosing the fewest parens it can
prove correct. round-tripping a file through both reproduces it.

⚠ **they print from the DATUM, so comments are not carried** -- `sound` does not
keep them. that is why they are verbs of their own rather than modes of fmt,
which is a reindenter and never touches what is on a line, and why there is no
`-w`: a rewrite that silently deleted every comment in a file is not a thing to
offer. redirect if you mean it.

`infix` always terminates with something correct, because prefix is a fixed point
of opfix -- so the fully-parenthesized spelling is always a valid answer and every
proposal is checked against it before being handed out. on this tree all 1571
top-level forms across 334 files keep the maximal spelling, at 22% fewer parens
than the prefix spelling. the law is gated in
[`test/host/infix.l`](../test/host/infix.l).

**doc is the one verb that KEEPS comments**, and it is the mirror of the two
above it. infix and unfix print from the datum and lose every comment in the
file; `doc` prints the comments and nothing else -- the leading block, the header
that twelve of the nineteen crew tools have instead of a `doc/*.md`.

the lifting is a TEXT walk over the same `lib/lint.l` scanner (`lint-cmts`, which
reports every comment with its text): no reader in the tree keeps comments, so a
datum walk would answer nothing. it lives in libra because reading `.l` is
libra's beat and nothing else in the tree should have to learn what a comment is.
what it hands out is MARKDOWN TEXT, and the showing is
[lapiz](../crew/lapiz/lapiz.l)'s -- which is why one verb offers three surfaces
and libra implements none of them.

```
$ libra doc crew/vi/hue.l | head -3
crew/vi/hue.l -- the .l syntax, written down ONCE, for two readers: the
painter in crew/vi/core.l's vframe, and the vim syntax file, which mk/tools/hue2vim.l
GENERATES from the very table below -- built by make into out/host/syntax.vim and
```

**`make site` is built on it.** the crew tools that have no page here get one
anyway: the build runs `libra doc` over each of them into `out/toolmd/*.md` and
papel makes a site out of markdown exactly as it always has. libra reads `.l`,
papel reads markdown, and neither learns the other's job.

the gate rides `test/host/libra.l` with the rest of the verbs, and its law is
that the document keeps every LETTER of the header -- an extraction that stalls
drops its whole tail in silence, and neither a length nor a block count would
notice. the plan is [`doc/plan/doc-system.md`](plan/doc-system.md); this is its
rung 0.

⚠ **an unknown verb reads as a FILENAME.** `libra serv x.l` says "cannot open
serv" rather than "no such verb". that is the price of the bare file list being
the common case, and it is a real edge.

output is `path:line:col: text`, the shape a compiler prints and an editor's
error list already parses. QUIET when there is nothing to say.

## what it weighs

**balance** -- parens, brackets, braces and unclosed strings. this one is
unconditional and it is the only one that fails the gate: an imbalance means the
file does not read, which is not a matter of opinion. an unclosed opener reports
where it OPENED, not end-of-file, because a dropped paren is the classic `.l`
slip and pointing at the far end of the file is the least useful place to point.

it reads `.l` correctly, which is most of the work: `;` and `#!` start comments,
`"..."` is the only string, and `'` / `` ` `` are READER OPERATORS, never
delimiters -- so it does not trip where a C lexer would.

**singleton** (off by default) -- a form of ONE element is that element, since
`(f)` is `f` at zero operands. so the parens do nothing, and a nullary call
never fires: `(go)` is `go` handed back unrun, silently, with the value you
wanted one curry away. three things are exempt, and each for a reason:

- a CONSTRUCTOR is a datum, not a form -- `'(x)` `` `(x) `` `#(x)` `@(x)`
  `~(x)`. a `'` makes its contents data too, inherited all the way down. (a
  backtick list EVALUATES its elements, so a form inside one is a real form and
  is flagged.)
- a `(` GLUED to an operator run, where the parens may be holding two sigil runs
  apart. merged, two runs lex as ONE — and that is sometimes the same value and
  sometimes a *different operator*: `<(<l)` survives (`<<` is the same caap), but
  `<(= x)` is `(< (= x))` while `<=x` is the single operator `<=`. telling them
  apart needs the operator table, which a balance scan has no business knowing,
  so all of them are exempt.
- an ALL-PUNCTUATION token, the escape idiom: `(+)` is `+` as a value, and
  `(:)` `(?)` `(\)` read their own zero point.

⚠ that last exemption is narrower than "starts with punctuation" on purpose.
`(<>b)` and `<>b` are one value, so those parens really are droppable, and
flagging them is the point.

**deprecated** (empty roster by default) -- names a project has finished with.
comments and strings are not code and are skipped; a quoted `'foo` names `foo`
just as much as bare `foo` does, so it counts.

**shadow** (off by default) -- a binding of one of the dozen words a glued row
(`operators` at arity 0) names: `cap cup net prod abs negate reciprocal fraction bit saturate nil?
dot`. a glued sigil factors to one of these and then resolves like any other
name, so `(: net (a + b) .. )` quietly re-aims every `+x` in its scope. rare,
real, and silent -- which is the whole case for saying it at the binding. it
**reserves nothing**: the rule speaks, the binding stands.

it catches a plain `:` binder name and a `\` param. two things it does not:

- a **bare-name body** over-fires. `(: net 5 net)` warns twice, once for the
  binding and once for the body, because which element is last is not known
  until the closer. a body that is a bare monadic name is rare enough to pay
  one line for.
- the **sugar header** `(: (bit x) ..)` is missed, and cannot be caught here.
  at token time it is element 1 of a form at an even `:` position -- which is
  exactly what the far commoner body call `(: a 1 (bit x))` is. flagging one
  would flag both, and the second is half the tree.

on this tree the rule finds 20 bindings across 332 files, and they are real
ones (`abs` in `lush/glob.l`, `dot` in `cook.l`, `net` in `moon/gen.l`).

## the config

settings live in two files, read in this order, the second overlaying the first:

```
~/.love/etc/libra.l     your taste, wherever the binary happens to live
./.libra.l              this tree's own -- and it travels in git
```

the project file speaks last, which is the direction the module walk already
runs (cwd's `lib/` before the seat's). the search is the CWD only, with no walk
upward. a deprecated-name roster is a fact about a TREE rather than about a
person, which is why the project file exists at all.

```love
; ~/.love/etc/libra.l -- or ./.libra.l
(singleton 1)                                    ; turn the rule on
(shadow 1)                                       ; ...and the sigil-word rule
(deprecated old-thing (worse-thing "use better-thing"))
(strict singleton)                               ; ...and make it fail the gate
(drop-parens 1)                                  ; fmt minifies parens without -p
(drop-singles 0)                                 ; ...but leave (x) alone
(drop-nullary 1)                                 ; ...and take (go) too
```

⚠ `drop-parens` and `drop-singles` are read with `salt-one`, never by PRESENCE:
the tail of `(drop-parens 0)` is `two?` just as much as `(drop-parens 1)`'s is, so
asking whether the key is there would read an explicit OFF as an on.

a setting is one form: the head names it, the tail is its value. an entry in the
roster is a bare name or a `(name "hint")` pair, and the hint is printed after
the name. a repeated key REPLACES rather than appends -- one line, one answer.

**the rules only SPEAK.** singleton, shadow and deprecated print, and the editor
underlines them, but `libra` still exits 0 -- so turning a rule on can never
redden a tree that was green. `(strict <rule>)` promotes one when a project has
actually finished with it. balance is always fatal; a tab never is.

⚠ **a config is DATA, never CODE.** the file is read with `sound`, the datum
reader, and no part of it is ever evaluated. a dotfile cannot run anything, and
nothing in it needs quoting, because nothing in it is evaluated -- write
`(deprecated foo)`, not `(deprecated 'foo)`.

⚠ **a form that is not a setting is simply not a setting.** a bad line is
skipped in silence and the rest of the file still lands. a typo in a dotfile
must not take the tool down with it, and half a config is better than none. a
dropped paren ends the read rather than spinning on it.

both files are optional and absence is the normal case: no config at all is the
same as two empty ones. an EMPTY config file means NO OVERRIDES, which is a
perfectly good thing for a config file to mean -- so the question asked of it is
whether it OPENED, never whether it had bytes.

the machinery is `lib/salt.l` (`(salt 'libra)`), which is not libra's: any crew
app can call `(salt 'its-own-name)` and get the same two-file overlay. see
[salt](#salt-the-shared-door) below.

## salt, the shared door

```love
(use 'salt)
(salt app)          ; -> a tablet of that app's settings
(salt-one c k d)    ; -> the first operand of setting k, or d
(salt-all c k)      ; -> the whole tail of setting k, or ()
(salt-has? c k nm)  ; -> `(1 entry) if nm is listed under k, else ()
```

⚠ `salt-all` cannot tell an ABSENT key from one written with no operand -- both
are `()`. so a switch is `(singleton 1)`, never a bare `(singleton)`, and an
app that wants "the EMPTY roster" spells it with an explicit `()` operand --
`(startup ())` in `crew/lux/config.l`, the other salt consumer.

⚠ a repeated key REPLACES rather than appends, so a roster takes all its entries
in ONE form. lux's `(bind (spec action) (spec action) ..)` is the shape.

⚠ `salt` reads `HOME`. the seat of `/usr/bin/love` is `/usr`, and nobody's
settings live in `/usr/etc`, so config is the one thing a love program finds by
the environment rather than by the seat walk. the module walk still reads none.

`lib/lint.l` takes a plain tablet and reads it with `peep`; it does NOT depend
on salt, because vi cats that file directly and a module it had to carry along
would break the cat. salt fills the tablet, lint only reads it.

## the formatter

`libra fmt` is a REINDENTER and nothing more: it replaces leading whitespace and
leaves every line break, every alignment inside a line, and every comment
exactly where its author put them. it cannot re-flow and will not try. the rule
is one sentence -- a continuation line aligns under its form's FIRST OPERAND, or
one past the open delimiter when the head stands alone on its line -- plus the
pairing rule, since `:` and `?` take their operands two at a time and the house
sets the second of each pair one past the first.

## the paren minifier

`libra fmt -p` also drops the parens that do nothing. it is a format option rather
than a verb of its own, because minifying and reindenting are one errand — make
the file read the way the house writes it — and they compose in a single pass.

the everyday case is the one that abounds: **application binds tighter than every
infix operator**, so a call alone in an operand span never needed its parens.

```love
("cook: command failed (exit " + (show ec) + ")")   ; before
("cook: command failed (exit " + show ec + ")")     ; after
```

⚠ **the decision is opfix's, not libra's.** splice the child's elements into its
parent's operand run, fold both ways, and drop only when the cores come out
IDENTICAL. `op-core` recurses structurally, so a form's fold reads only its own
operand list — the test is local, the file is never read twice, and the precedence
law is *borrowed* rather than copied, so libra cannot drift from the compiler. what
it refuses matters as much as what it takes:

```love
(f (show x))        an application OPERAND -- (f show x) is ((f show) x)
("a" + f (show x))  the group shares its span with f
(x * (a + b))       band
(x + (a + b))       arithmetic is LEFT-handed, so + cannot yield to +
(f + `(a b))        reader sugar owns those parens
(? (! (two? l)) ..) a PUNCT HEAD folds the same and reads far worse
```

⚠ **and the answer is gated LEXICALLY**, because the datum cannot see what a
deletion does to the text. `((show x)+"a")` reads fine and folds equal, but dropping
the bytes leaves `x+` as one token — a different program. so a pair drops only when
it stands FREE: a delimiter, a comment or a quote on both outer sides. that one rule
also disposes of `foo(x)`, `'(x)` and every sigil-glued run, and it costs almost
nothing: right-glued parens are a bare handful of the tree's ~190k code parens.

**singletons ride the same flag** but rest on a different law: `(x)` is `x` by
`(f) == f`, which opfix *declines* to apply — it keeps `((mov r3 r12))` whole,
because a list of one may be data. so that half stays lexical, and it carries the
risk the span half does not: `(f) == f` is false for a MACRO, which reads its
operand's shape rather than its value, and libra cannot know which heads are macros.
`(drop-singles 0)` turns it off. a list of one FORM is left alone either way.

⚠ **the nullary trap stays visible.** `(go)` is `go` handed back unrun, and dropping
those parens is semantics-preserving *and* deletes the only evidence of a bug this
tree keeps hitting. fmt leaves them for `check`'s singleton rule to speak about;
`(drop-nullary 1)` takes them anyway.

**how it is checked.** apply every drop to every tracked `.l` file, re-read, and
compare the compiled cores — they must come out identical — then rebuild the
self-hosting tree from the minified source and run the gate, with the test counts
unmoved.

## the reindenter

**it is BUILT BUT NOT ADOPTED.** nothing is gated on layout and nothing has been
reformatted. tree-wide it would move about a fifth of all lines, because the
tree carries two live conventions (align-to-first-operand, and a hanging indent
that no positional rule reproduces). the open decision is whether to adopt
alignment and reflow the minority, or adopt file by file with a shrinking ignore
list. a width guard as a bridge between the two pays nowhere -- the numbers are
in `lib/lint.l`, do not re-derive them.
