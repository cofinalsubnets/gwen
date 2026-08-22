# libra ⚖ -- the .l balance tool

the scales. libra weighs a `.l` file two ways and they are the same weighing:
`libra` says what is wrong with it, and `libra doc` lifts its header out as a
document. one scanner (`lib/lint.l`) under both, so the gate and the doc lifter
can never disagree about what a paren or a comment is. the tool is
`crew/libra/libra.l`, the gate `make test_hostnif` (test/host/libra.l), and
`make lint` runs it over every tracked `.l`.

an LSP server lived here until 2026-08-16 -- `libra serve`, publishing the same
scan as diagnostics over json-rpc. it never had a consumer, so it went; `lib/json.l`
stays, with no consumer of its own outside its gate.

## the verbs

```
libra FILE ..           weigh them -- balance is the DEFAULT verb, so a bare
                        file list is the whole command
libra check FILE ..     the same thing, spelled out
libra doc FILE ..       its HEADER COMMENT as a document, on stdout
libra doc -ht FILE ..   ...as html; -rf for a man page
libra -h                the usage
```

check is the default because it is the errand that recurs: `make lint` runs it
over every tracked `.l`. `doc` is run by hand and named, and nothing is gated on
layout.

**doc is the verb that KEEPS comments.** the check reads them only to know where
code is not; `doc` prints the comments and nothing else -- the leading block, the
header that twelve of the nineteen crew tools have instead of a `doc/*.md`.

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
painter in crew/vi/core.l's vframe, and the vim syntax file, which tools/hue2vim.l
GENERATES from the very table below -- built by make into out/host/syntax.vim and
```

**the prose's own structure is read, never rewritten.** the corpus was not
composed against a convention, but it is consistent, and three of its habits
carry structure that markdown would otherwise fill flat:

`; --- name ---`
:   a level-2 heading. 41 of them in the tree, 40 padded out with dashes

`; usage:` and the indented lines under it
:   a fence, because the alignment IS the content

any other indented run
:   a fence too -- unless the indent is a list item's WRAP, where lapiz joins it

a leading `⚠`
:   the marker in bold, and a block of its own

and the file's name becomes the level-1 heading. that is the one thing on the
page not taken from the prose, and it is not decoration: papel reads a page's
title, its anchors and its whole contents nav off the headings.

nothing is shielded or escaped any more. a `--- banner ---` used to stop mdread
dead and take the rest of the header with it; lapiz's reader is
[total](../crew/lapiz/lapiz.l) as of 2026-08-16, so the loss was fixed in the
lens rather than papered over here.

**`make site` is built on it.** the crew tools that have no page here get one
anyway: the build runs `libra doc` over each of them into `out/toolmd/*.md` and
papel makes a site out of markdown exactly as it always has. libra reads `.l`,
papel reads markdown, and neither learns the other's job.

the gate rides `test/host/libra.l` with the rest of the verbs, and its law is
that the document keeps every LETTER of the header -- an extraction that stalls
drops its whole tail in silence, and neither a length nor a block count would
notice. the plan is [`doc/misc/plan/doc-system.md`](plan/doc-system.md); this is its
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

**singleton** (ON by default; `(singleton 0)` turns it off; this tree makes it STRICT
in `.libra.l` -- the gate is no gripes, hand-fixed) -- a form of ONE element
is that element, since
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
(singleton 0)                                    ; turn the rule OFF (on by default)
(deprecated old-thing (worse-thing "use better-thing"))
(strict singleton)                               ; ...and make it fail the gate
```

⚠ `singleton` is read with `salt-one`, never by PRESENCE: the tail of
`(singleton 0)` is `two?` just as much as `(singleton 1)`'s is, so asking whether
the key is there would read an explicit OFF as an on.

a setting is one form: the head names it, the tail is its value. an entry in the
roster is a bare name or a `(name "hint")` pair, and the hint is printed after
the name. a repeated key REPLACES rather than appends -- one line, one answer.

**the rules only SPEAK.** singleton and deprecated print, and the editor
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

## the indent plan, and the width guard that was refused

`lint-lay` aligns a continuation line under its form's first operand, or one past
the open delimiter when the head stands alone on its line. `:` and `?` take their
operands two at a time -- name/value, test/result -- and the house sets the second
of each pair one space past the first, so a value is never mistaken for another
name at a glance. Every other form aligns its operands flat.

A **width guard** over that rule was tried and pays nowhere. The idea: cap the
alignment, so a first operand sitting far to the right falls back to one past its
own open paren (black's bargain), on the theory that it would rescue the hand-laid
hanging files. Measured over the tree at thresholds 24/32/40/56:

| | changed lines, before -> after |
|---|---|
| `love/ev.l` (hanging) | 494 -> 506 |
| `test/holo/golden.l` (hanging) | flat |
| `crew/libra/libra.l` (well laid) | 7 -> 22 |
| `lib/lint.l` (well laid) | 1 -> 4 |
| tree-wide churn | 12591 -> 16448 |

The hanging files barely move, because their authors broke to a column *left* of
the enclosing paren, which no cap-and-fallback reproduces; every well-laid file
gets worse. Alignment it is. Do not re-add the guard without new evidence.
