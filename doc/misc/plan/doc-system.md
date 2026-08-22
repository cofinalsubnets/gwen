# plan: the libra/lapiz documentation system

Docs from the source itself: libra reads them out of comments, lapiz shows them as
markup. The premise needs one correction before anything else — **lapiz already
exists** ([`crew/lapiz/lapiz.l`](../../crew/lapiz/lapiz.l), three surfaces md/ht/rf
over one AST, round-trip laws gated in `test/host/lapiz.l`), and so does the whole
rendering stack above it (papel: titles, anchors, TOC, blurbs, cross-links, index;
`make site`). The writing half of this arc is done. The arc is the *reading* half,
and the reading half has a real hole in the middle of it.

## the hole

No reader in the tree keeps comments. `p0` (C, `core/love.c`) and `sound`
(`love/p1.l`) both drop `;` lines on the floor by design, and libra's own header
says why its print-from-the-datum verbs are stdout-only: printing from the datum
strips every comment. Two lexers *locate* comments without keeping them —
`lib/lint.l` tracks a `'cmt` state (positions, no text), `crew/vi/hue.l` classifies
`'comment` runs (offsets per line). So the extractor cannot be a datum walk; it has
to be a text walk that knows where the comments are.

The second, subtler hole: even with comment spans in hand, nothing associates a
comment with what it documents. The house file shape makes this the hard case —
a tool is one giant `(: name body name body ...)` block, so "the comment above a
definition" means the comment above a *binding pair inside* that block, not above
a top-level form.

## what the corpus actually looks like

The comments are long-form narrative, not docstrings: 30–70 line prose headers,
20–46% comment density, informal structure that is consistent without being
specified — the header block, `; --- section ---` banners, `; usage:` blocks,
`⚠` hazards. 12 of 19 crew tools have no `doc/*.md` at all; their header *is*
their documentation. That asymmetry is the arc's friend: **file-level extraction
(take the header, show it) covers the real gap cheaply; per-symbol extraction
fights the house style.**

## the ladder

- **rung 0 — headers out, pages up.** A `libra doc` verb (or `crew/` member) that
  takes a `.l` file, lifts the leading comment block via lint.l's scanner, maps the
  prose to a lapiz AST, and shows it on any surface. Wire papel to accept `.l`
  sources (its `mdsof` seam is small) so `make site` grows a page per crew tool.
  This alone documents the 12 undocumented tools. Gate: a `test/host/` file proving
  header → md → header-ish round trip on two real crew files.
- **rung 1 — section banners become structure.** `; --- name ---` banners map to
  lapiz heads, `; usage:` to fences, `⚠` lines to a hazard style. Still file-grain.
  The mapping is a tolerant reading of the prose as written, not a new convention
  the corpus must be rewritten into.
- **rung 2 — the association problem.** Comment ↔ binding pairing inside `:`
  blocks, so a page can carry a per-name index. This is the research rung: no
  prior art in the tree, and it forces the scanner question (extend lint.l, lift
  hue-lex, or a trivia-keeping read). Do not start here; rungs 0–1 ship value
  without it, and what they teach about the corpus decides the design.
- **rung 3 — coverage as a gate.** Once extraction is trusted, `make lint` (or a
  doc gate) can say which exported names a page misses. Only worth it if rung 2
  lands.

## choices (revisable)

- extractor rides libra as a verb, not a new crew member — it is a reading of `.l`
  source, which is libra's beat; lapiz becomes a new dependency edge there (mind
  the two-`-l` preload trap papel documents, solved with `want`/`readlink`).
- the scanner is lint.l extended to emit comment spans with text — one more
  consumer of the existing scanner beats a fifth independent encoding of .l
  scanning rules (lint.l, hue.l, p1.l, love.c already each have one).
- comment prose goes through `mdread` rather than a bespoke parser — it is
  markdown-ish already, and lapiz's laws then come for free; where it isn't
  markdown, fix the mapping, not the corpus.
- no new lapiz node types until rung 2 proves one is needed — law 1 makes every
  node a three-surface obligation.

## ✅ rung 0 landed

`libra doc`, and the site pipeline that rides it. the shape is not quite what the
ladder above guessed, and the correction is the useful part of this section.

### the correction: whose job is it

the first cut put the extraction in a `lib/ldoc.l` and taught papel to take `.l`
sources. that was wrong, and the rule that says so is one line: **parsing love
code, including recognizing comments, is libra's job.** papel is lapiz + cook and
should stay that -- it reads markdown and knows nothing else. so:

- the extraction lives IN `crew/libra/libra.l`, beside the formatter and the
  infix pass. nothing else in the tree has to learn what a comment is.
- `make site` runs `libra doc` over each crew tool into `out/toolmd/*.md`, and
  papel builds a site out of markdown exactly as it always has. papel's diff is
  ZERO lines.

### the pieces

- **`lint-cmts`** (`lib/lint.l`) -- every comment in reading order, `(line col
  text kind)`, the text after the introducer and the kind `'semi` / `'bang`.
  lint's fourth walk and its smallest: no stack, three states, so a `;` inside a
  string is not a comment. the two lexers that *located* comments are unchanged;
  this is the one that keeps them.
- **`ldoc-head` / `ldoc-md`** (in libra) -- the header out, as markdown. THE SEAM
  IS MARKDOWN TEXT, not a lapiz AST, which was not the plan's guess and is better
  than it: libra says what the header says, lapiz alone decides what a document
  is, and all three surfaces come free. a bespoke prose parser here would have
  been a second markdown reader in the tree, drifting from the first.
- **THE HEADER** is the run of full-line comments to the first line carrying CODE
  (blanks included, `#!` dropped) -- wider than "to the first blank line", because
  papel's own header ends, blanks, and then goes on for two more paragraphs of
  documentation. what stands to a comment's LEFT decides: blank and it is a
  sentence, code and it is a remark about that code.
- **`libra doc FILE ..`** -- markdown by default, `-ht` html, `-rf` man. lapiz
  loads on that verb alone, so `make lint` over every tracked `.l` pays nothing
  for it.

### what the corpus taught

a `; --- banner ---` line STALLS markdown -- lapiz's `starter` claims it and no
block parser will have it, so mdread stops dead and drops the entire rest of the
header in silence. every crew file opens with one. the fix is one backslash
(lapiz's own escape) and a blank line around it; rung 1 turns them into headings.

the reason to trust that fix is the gate's law: **the document keeps every LETTER
of the header**, alphanumerics compared on both sides, so filling a paragraph,
dropping backticks and eating an escape are not mistaken for loss. it holds
EXACTLY on 368 of the 378 tracked `.l` files (the other ten have no header).
a length or a block count would not have caught the stall.

gate: `test/host/libra.l`, with the rest of libra's verbs.

### also swept

the LSP server went (2026-08-16) -- `libra serve` and its json-rpc lane, which
doc/misc/libra.md itself recorded as having no consumer. `lib/json.l` stays.

## ✅ rung 1 landed

the prose's own structure, read as blocks. all of it is in `libra doc`, and the
mapping is a reading of the corpus as written -- no file was changed to suit it.

`; --- name ---`
:   a level-2 heading. 41 banners, 40 of them padded out with dashes

`; usage:` and the indented lines under it
:   a fence. 4 usage blocks

any other indented run
:   a fence. 532 display lines

an indent under a list item
:   a WRAP, left to lapiz, which now joins it to the item

a leading `⚠`
:   the marker in bold, and a block of its own. 74, of which 22 were jammed against the prose above them

and the file's name becomes the level-1 heading -- the one thing on the page not
taken from the prose, and the one that makes papel work: a page's title, its
anchors and its whole contents nav are read off the headings. before it, every
generated page was titled `lapiz.md` and carried no navigation at all.

**the shield is gone.** rung 0 wrapped a banner in a backslash so that mdread
would not stall on it. that was a defect in the LENS, and it was fixed there
instead: lapiz's markdown reader is now total (every line lands in some block),
and its list items take their lazy continuations, so a wrapped bullet is one
bullet. both are law-gated in `test/host/lapiz.l`.

⚠ THE LETTER LAW STILL HOLDS EXACTLY, all 368 headers -- which is the point of
having stated it that way. the only thing rung 1 drops is a banner's padding
dashes, and a dash is not a letter.

## ✅ the annotated source, and its stylesheet

a THIRD reader of `crew/vi/hue.l`'s class table, after the painter in vi's vframe
and the vim syntax generator: `tools/hue2web.l`. it knows nothing about what a
comment or a sigil is -- it asks hue, the way the other two do, so a class added
to that table arrives in all three without anyone being told.

the theme's own split is what made it cheap. hue.l says "a comment is a
`Comment`"; `crew/vi/config.l` says "in molokayo a `Comment` is `#75715E`". the
vim generator uses the first half and leaves the colour to the reader's
colorscheme; a web page has no colorscheme, so this joins both halves and the
site wears the editor's theme by construction.

- `love tools/hue2web.l css` -- one rule per class, plus the frame around the
  code. LINE NUMBERS come from a css counter, so the markup carries none and
  selecting the code copies the code alone.
- `love tools/hue2web.l src FILE` -- that file as a page, one span per line
  with an `id` so a line is linkable.
- `make site` paints all 17 crew tools into `out/site/<stem>.src.html`, and every
  generated doc page carries a `[the source]` link to its own.

the page CHROME is papel's own `wrap`, reached through the module accessor, with
the stylesheet riding the template's head slot. that is papel LENDING its
template, not papel learning what a `.l` is -- the same line the rest of this arc
holds.

⚠ THE GATE'S LAW: **painting changes no text.** strip every tag from the emitted
`<pre>`, decode the three entities, and what is left is the source file byte for
byte. a highlighter that drops a line, eats a backslash or mis-carries a
multi-line string has rewritten the program it was showing you, and nothing about
the colours would say so. `make -C tools test_hueweb`.

⚠ and `make test_tools` had been unrunnable since the tools/ move (`$(MAKE) -C
tools`, and two `$(R)/tools/` paths inside it). fixed in passing. it is red for
three reasons that are nobody's fault here: `test/host/cook.l` writes a fixture
into `/tmp/cooktest/mk/` without creating the directory, and two of cook's
subst-ref cases fail. all three predate this arc and none is in it.

### what is left

- **publication.** `out/` is gitignored and GitHub Pages serves the repo root, so
  `out/site/` reaches nobody. this is the gap, and it is not a doc-system rung:
  it is a decision about where the built site lives (a tracked `site/`, a first
  CI workflow, or nothing).
- **the crew list on `index.html` is hand-maintained** -- the same `<dl>` papel
  generates for its index. it could stop being written twice.
- **six tools are dropped from the site** (cook, kore, libra, moon, sb, vi), since
  a hand-written `doc/*.md` correctly wins the page name. their headers render
  nowhere.
- rungs 2 and 3 below, unchanged.

## difficulty

Medium. Rungs 0–1 are small and mostly plumbing over machinery that exists and is
law-gated. Rung 2 is genuinely novel (comment-to-form association has no prior art
here) but is severable — the arc pays at rung 0 and regresses nowhere if rung 2
waits.
