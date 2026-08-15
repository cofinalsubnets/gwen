# vi — the editor

crew/vi/ orients here; the laws live in crew/vi/law.l, the gate is `make test_vi`, and every
doubt settles by feeding `vstep` bytes.

## the shape

Five files, over the seeds the repo already had (bao's port-driven editor discipline, kore's
re.l regex engine):

* **crew/vi/core.l** — the PURE engine. A state tablet stepped one byte at a time:
  `(vstep st byte) -> st`, `(vfeed st bytes)`, `(vframe st)` -> one full escape-sequence frame
  as text. No tty, no port, no file io — the ex commands leave a REQUEST on the state (`'dow`
  to write, `'doe` to read, both in uread's `(name)` shape) and flip `'quit`; whoever holds the
  state acts. That purity is the whole test story: the laws drive key sequences and read the
  tablet back, and the frame is lawed to the byte on a tiny screen.
* **crew/vi/vi.l** — the face. Keys off `in` one byte at a time (arrows ESC[A-D decode to kjlh
  with a one-byte pushback so a bare ESC still interleaves), frames onto `out`, the alternate
  screen (?1049) so scrollback survives, `raw` for the tty (cooked restores at exit), winsize
  when there is one (80x24 on a pipe). It performs the engine's write/read requests. Port EOF
  quits — which is what makes `kore vi` fully drivable from a pipe: the smokes script whole
  sessions (`printf 'ihello\033:wq\n' | kore vi f`).
* **crew/vi/hue.l** — the .l syntax written down once, for two readers: the painter in core.l's
  `vframe`, and the vim syntax file, which mk/tools/hue2vim.l generates from the same table, so the
  two readings cannot drift. `make syntax` builds it into `out/host/syntax.vim` and
  `make install` puts it in `~/.vim/syntax/love.vim`; it is never checked in, so there is no
  copy to keep up to date.
* **crew/vi/config.l** — the theme (molokayo) as plain data, keyed by vim highlight group, so
  the generated syntax file can emit `hi def link` lines rather than hardcoded colours.
* **crew/vi/law.l** — the gate.

The pens are the face's to hand over: they want `$COLORTERM` and the user's theme file, and the
engine reads neither.

## the dialect

Motions h j k l 0 ^ $ w b e f F t T ; G gg, all counted; the goal column survives j/k over short
lines. Operators d c y — doubled for lines, with j/k/G/gg linewise, charwise otherwise, and vi's
own cw-is-ce special case. x X D C s r J, i a I A o O, p P (both linewise and charwise registers),
u and ^R (whole-insert granularity), / ? n N (the BRE dialect of re.l, wrapping), ZZ, ^F ^B ^D ^U.

The ex line: `w [NAME]`, `q`, `q!`, `wq`, `x`, a bare line number, `$`, `e`/`o` (`e NAME`, `e!`,
`o NAME` — `:o` reads as `:e` here), `hl` (flip the syntax paint), `lint` (libra's scan over the
buffer), `fmt` (libra's reindenter, in place).

Out of scope, deliberately: visual mode, named registers, `.`, macros, `:s` (sed exists), text
objects; tabs render at the terminal's stops, not ours (the cursor column drifts on tab-heavy
lines); no horizontal scroll (long lines clip at the view's edge); no UTF-8 width awareness
(bytes are columns).

## traps

* LISTS DO NOT INDEX BY APPLICATION — a list of numbers church-towers. Texts index; a line list
  wants an explicit walk (`vnth`) or diff.l's `dindex` trick.
* A find can land on column 0: test `charm?`, never truth (blue zero again).
* Deep `||` chains miscount parens invisibly — `member?` over a byte list reads better and cannot.

## not built

As need arises, in rough order: `.` (the repeat — record the last change's byte string, replay
it), visual mode (a span-selection over the same operators), `:s` ranges over re.l (sed's engine
is right there), named registers, tab-stop-aware rendering + horizontal scroll, and a pty smoke
that drives the face under a real terminal via host/posix.c (as test/host/baoedit.l does for bao's
line editor).
