# libra ⚖ -- the .l balance tool

the scales. libra weighs a `.l` file three ways and they are all the same
weighing: `libra` says what is wrong with it, `libra fmt` lays it out, and
`libra serve` says both while you type. one scanner (`lib/lint.l`) under all
three, so the editor, the formatter and the gate can never disagree about what
balanced means. the tool is `crew/libra/libra.l`, the gate `make test_hostnif`
(test/host/libra.l), and `make lint` runs it over every tracked `.l`.

## the verbs

```
libra FILE ..           weigh them -- balance is the DEFAULT verb, so a bare
                        file list is the whole command
libra check FILE ..     the same thing, spelled out
libra -w FILE ..        ...and strip trailing whitespace while you are there
                        (WITHOUT reindenting)
libra fmt FILE ..       lay it out on stdout; -w rewrites, -n only checks
libra serve             speak lsp over stdio
libra -h                the usage
```

check is the default because it is the errand that recurs. fmt is run BY HAND
and nothing is gated on layout; serve waits for an editor that wants it.

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
  apart: `(-(-5))` is `5`, but `--5` raises `missing --5`. some glued runs
  survive being merged (`<(<l)` and `<<l` are both caap) and some do not, and
  telling them apart needs the operator table -- which a balance scan has no
  business knowing. so all of them are exempt.
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
(singleton 1)                                    ; turn the rule on
(deprecated old-thing (worse-thing "use better-thing"))
(strict singleton)                               ; ...and make it fail the gate
```

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
are `()`. so a switch is `(singleton 1)`, never a bare `(singleton)`.

⚠ `salt` reads `HOME`. the seat of `/usr/bin/love` is `/usr`, and nobody's
settings live in `/usr/etc`, so config is the one thing a love program finds by
the environment rather than by the seat walk. the module walk still reads none.

`lib/lint.l` takes a plain tablet and reads it with `peep`; it does NOT depend
on salt, because vi cats that file directly and a module it had to carry along
would break the cat. salt fills the tablet, lint only reads it.

## the lsp server

`libra serve` speaks LSP over stdin/stdout: a header block (`Content-Length: N`,
CRLF) framing a json-rpc body, with `lib/json.l` as the translation desk. it
implements `initialize`, `shutdown`/`exit`, and `textDocument/did{Open,Change,
Close}` with `publishDiagnostics` -- textDocumentSync 1 (Full), because a scanner
that reads the whole document wants the whole document.

⚠ the ports are ARGUMENTS: `(libra-serve conf inp outp)` is the whole server, so
test/host/libra.l drives entire conversations in-process over `(tap ..)` and
`(jug 0)` -- no subprocess, no pipes, no timing.

there is no consumer yet. `textDocument/documentSymbol` is the gating rung if
one ever appears -- Claude Code's own LSP client, for one, exposes only
navigation operations and has no diagnostics operation at all, so a
diagnostics-only server is connectable but useless to it.

## the formatter

`libra fmt` is a REINDENTER and nothing more: it replaces leading whitespace and
leaves every line break, every alignment inside a line, and every comment
exactly where its author put them. it cannot re-flow and will not try. the rule
is one sentence -- a continuation line aligns under its form's FIRST OPERAND, or
one past the open delimiter when the head stands alone on its line -- plus the
pairing rule, since `:` and `?` take their operands two at a time and the house
sets the second of each pair one past the first.

**it is BUILT BUT NOT ADOPTED.** nothing is gated on layout and nothing has been
reformatted. tree-wide it would move about a fifth of all lines, because the
tree carries two live conventions (align-to-first-operand, and a hanging indent
that no positional rule reproduces). the open decision is whether to adopt
alignment and reflow the minority, or adopt file by file with a shrinking ignore
list. a width guard was tried as a bridge between the two and paid nowhere --
the numbers are in `lib/lint.l`, do not re-derive them.
