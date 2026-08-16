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
doc/libra.md itself recorded as having no consumer. `lib/json.l` stays.

**rung 1 is now the obvious one, and here is the evidence.** an extracted page has
no head-1, so its title is its file name and papel gives it no contents at all --
`hue.html` is a wall of paragraphs. the banners are already isolated as their own
blocks, which is exactly the seam a heading goes in.

## difficulty

Medium. Rungs 0–1 are small and mostly plumbing over machinery that exists and is
law-gated. Rung 2 is genuinely novel (comment-to-form association has no prior art
here) but is severable — the arc pays at rung 0 and regresses nowhere if rung 2
waits.
