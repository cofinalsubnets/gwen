# vi -- rung 2 of the distro, the editor

the state of crew/vi/ and the road ahead. this ORIENTS; the laws live in
crew/vi/law.l, the gate is `make test_vi`, and every doubt settles by feeding
vstep bytes. last trued up 2026-07-06.

## the shape

two files over the seeds the repo already had (bao's port-driven editor
discipline, re.l's regex engine):

* **crew/vi/core.l** -- the PURE engine. a state tablet stepped one byte at a
  time: `(vstep st byte) -> st`, `(vfeed st bytes)`, `(vframe st)` -> one full
  escape-sequence frame as text. no tty, no port, no file io -- the ex
  commands leave a write REQUEST on the state (`'dow`, uread's (name) shape)
  and flip `'quit`; whoever holds the state acts. that purity is the whole
  test story: the laws drive key sequences and read the tablet back, and the
  frame is lawed to the byte on a tiny screen.
* **crew/vi/vi.l** -- the face. keys off `in` one byte at a time (arrows
  ESC[A-D decode to kjlh with a one-byte pushback so a bare ESC still
  interleaves), frames onto `out`, the alternate screen (?1049) so scrollback
  survives, `raw` for the tty (cooked restores at exit), winsize when there is
  one (80x24 on a pipe). port EOF quits -- which is what makes `kore vi` fully
  drivable from a pipe: the smokes script whole sessions
  (`printf 'ihello\033:wq\n' | kore vi f`).

## the dialect

motions h j k l 0 ^ $ w b e f F t T ; G gg, all counted; the goal column
survives j/k over short lines. operators d c y -- doubled for lines, with
j/k/G/gg linewise, charwise otherwise, and vi's own cw-is-ce special case.
x X D C s r J, i a I A o O, p P (both linewise and charwise registers),
u and ^R (whole-insert granularity), / ? n N (the BRE dialect of re.l,
wrapping), : with w [NAME] q q! wq x and a line number, ZZ, ^F ^B ^D ^U.

out of scope, documented: visual mode, named registers, `.`, macros, :s (sed
exists), text objects; tabs render at the terminal's stops, not ours (the
cursor column drifts on tab-heavy lines); no horizontal scroll (long lines
clip at the view's edge); no UTF-8 width awareness (bytes are columns).

## the traps this rung paid for

* LISTS DO NOT INDEX BY APPLICATION -- a list of numbers church-towers. texts
  index; a line list wants an explicit walk (vnth) or diff.l's dindex trick.
* a find can land on column 0: test `charm?`, never truth (blue zero again).
* deep `||` chains miscount parens invisibly -- `member?` over a byte list
  reads better and cannot.

## the road ahead

as need arises, in rough order: `.` (the repeat -- record the last change's
byte string, replay it), visual mode (a span-selection over the same
operators), :s ranges over re.l (sed's engine is right there), named
registers, tab-stop-aware rendering + horizontal scroll, and the pty smoke
(drive the face under a real terminal via host/posix.c like boot/baoedit.l).
after vi: rung 3, the chibicc-class C compiler.
