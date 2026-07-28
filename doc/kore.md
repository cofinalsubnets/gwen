# kore -- the busybox rung of the distro

the state of crew/kore/ and the road ahead. this ORIENTS; the laws live in
crew/kore/law.l, the GNU-identical smokes in `make test_kore`, and every doubt
settles by probing the built `kore`. last trued up 2026-07-06.

## the arc

the distro plan (the love-native POSIX environment over the Linux kernel) climbs
three rungs, each independently shippable: **kore** (this one -- the busybox
clone), a **vim clone** over cb + bao's edln/edraw seeds, and a **chibicc-class
C compiler** last, scoped to *compiles love.c*, never the kernel. the pieces that
already existed -- cook (make), sh.l/bao (shell), holo (assembler), the terminal
stack, init.l (pid 1) -- mean kore is the gap between the posix-pid1
container and a livable system. the distro shape: kernel + static `love` + .l
files, busybox's multi-call trick natively.

## kore, the multi-call toolbox

ONE catted script (`out/host/kore`, `bin/kore` installs): the Makefile cats

    text.l core.l fs.l re.l sed.l proc.l diff.l ain.l cook.l asbook.l elf.l kore.l   (korefiles)

behind a `#!/usr/bin/env -S love` shebang. kore.l loads LAST and dispatches off the
program seat of `cmdline` -- `kore TOOL ARGS..`, or symlink a tool's name to kore
and argv[0] picks it (how the distro will shadow at will). the registry is a
tablet, so tool names never collide with the globals they call (the `mkdir`
applet CALLS the `mkdir` nif; different namespaces).

the file discipline, two shapes:

* **a tool with a seat** (tools/ain.l, crew/cook/cook.l): define-only, leaking
  one `<tool>-main`; a body-having tail fires it iff the file's own basename
  sits in the program seat -- so the same file is a standalone tool AND a quiet
  cat member.
* **a toolbox** (core.l, fs.l): many mains, NO seat -- kore is its door.

## the inventory (38 tools, 40 names)

| where | tools |
| --- | --- |
| kore.l (thin mains) | diff (the patience/myers engines), as (elf64 over the holo book) |
| tools/ain.l | nc / ain |
| crew/cook/cook.l | make / cook |
| core.l, the line tools | cat echo head tail wc sort uniq tee |
| core.l, the field tools | cut tr nl rev |
| core.l, the trivia | seq yes true false basename dirname |
| fs.l, the fs tools | ls cp mv rm mkdir rmdir ln touch pwd chmod |
| re.l, the matcher | grep (-n -v -c -l) over the lawed BRE engine |
| sed.l, the editor | sed (-n; s///gp, d, p, q; number/$/regex/range addresses) |
| proc.l, the processes | env sleep kill xargs |

## the discipline (why this stays trustworthy)

* **GNU to the byte.** every tool with printable output is smoked
  byte-identical against the real GNU tool in `make test_kore` (LC_ALL=C for
  sort/ls). the fussy faces are pinned deliberately: wc pads every field to the
  digit width of the byte TOTAL; uniq -c wears width 7; nl is pad-6 + tab and a
  blank line is seven bare spaces; head/tail banner many files with
  `==> name <==`; ls -a is GNU -A. effects (cp/mv/rm/..) are smoked by acting
  and then verifying with the shell.
* **the u-floor.** the shared helpers leak u-prefixed from core.l and are lawed
  pure in law.l: uatoi uread udie upad ujoin uhdr uhead/utail ucount ubase/udir
  usplit ujoinc uspec/upick uset urev ueach, fs.l's uoct/udirp/udest/ucopy, and
  proc.l's udur/uwords.
  `ueach` is the cat walk every whole-input tool rides (files or stdin, `-`
  reads stdin, a miss complains on err and the exit code remembers).
* **the nif lane.** fs effects ride host/posix.c (app-glob AI_NIF,
  no core edit), init.c's posix_ conventions: an effect op answers () ok | a
  POSITIVE errno | EINVAL misuse; a value op answers the value | (). host/posix.c
  holds rename symlink readlink chmod chown utime umask rmdir hardlink
  (`link` the word belongs to the chain ctor). boot/fs.l smokes them in
  test_hostnif.
* **exit codes.** 0 clean, 1 something failed (reported on err, the loop
  continued), 2 usage; diff keeps its classic 0/1/2 triple.

## the traps already paid for

* `show` is the decimal formatter; `string` of a number makes a ONE-CHARM text
  (ain.l's error path still carries that latent bug).
* prel `sort` on strings IS lexicographic (probed via the "b"-vs-"ab"
  discriminator), which is exactly LC_ALL=C -- no comparator needed.
* a symlink TARGET resolves relative to the LINK's directory, not the cwd.
* lines/unlines normalize an unterminated final line (the tool layer's ONE
  normalization); cat copies verbatim, head/tail/sort/uniq normalize like GNU
  sort does (GNU head does not -- known, harmless, unsmoked).
* the nested-loop nom miscompile this rung surfaced was root-caused and FIXED
  in both compilers (ev.l avb's shadow guard + love.c's fars set, lawed in
  test/closure.l); nested loops may share noms freely again.
* a value that can legitimately net 0 -- an end index, a (0 0) span -- reads
  BLUE (falsy): give it uread's (1 ..) success shape. and never name a local
  `err` or `out`; they are the PORTS, and the shadow says into a charm.

## the regex engine (crew/kore/re.l)

landed. a POSIX-BRE dialect -- literals, `.`, `*`, head-`^`/tail-`$`, [..]
classes with ranges/negation (first-] and edge-- literal), \-escapes, \( \)
groups, GNU's \+ \? -- with GNU's leniency (a repeat with no atom is ink) and
GNU's refusals mirrored as parse errors (backrefs, intervals, alternation).
(rebre p) answers (1 nodes ngroups) | (); (rehas nodes s) the boolean; (refind nodes
s i) the leftmost greedy span as (1 start end) -- the (1 ..) shapes because a
match ending at 0 is blue by measure. the matcher is greedy backtracking in
continuation style; the laws (law.l) hold the dialect by hand AND by a seeded
differential fuzz against an independent Brzozowski-derivative oracle. grep
rides it: -n -v -c -l, GNU-byte-identical smokes + the 0/1/2 exit triple
(an unreadable file beats a match). refind carries group SPANS (numbered in
\( order, a repeated group reading as its LAST iteration, GNU's \1) -- sed's
food.

## sed-lite (crew/kore/sed.l)

landed, over re.l. `sed [-n] SCRIPT [FILE..]`: ;/newline-separated commands,
each [ADDR[,ADDR]] VERB; addresses number/$/(BRE)/re/, ranges open-at-first
close-at-later (numeric end at-or-before start = one line, like GNU); verbs
p, d, q (one address), and s/RE/REPL/[g][p] with any delimiter -- & and
\1..\9 in the replacement, the POSIX empty-match rules exact (step after an
empty replacement, DISCARD an empty match where the last one ended:
`s/x*/-/g` on "xbz" is "-b-z-"). input is the concatenated stream ($ = its
last line); unreadable files complain-and-flow, exit 2; a bad script exits 1
(GNU's split). the pure floor (sparse, usub) is lawed; the whole face is
smoked byte-identical vs GNU (a 12-script battery + -n + stdin + the error
faces). out of dialect, documented: GNU's empty-pattern reuse, \n in
replacements, hold space.

## the process tools (crew/kore/proc.l)

landed, no new nifs -- environ/getenv/setenv, spawn (pid | negative errno; a
child that cannot exec _exit(127)s) + wait, still (pty.c's kill), rest (core
sleep, ms). env prints the world or assigns K=V.. and runs the command with
the child's exit; sleep sums decimal durations with s/m/h/d suffixes (udur,
lawed); kill sends -N or -NAME (default TERM) per pid, exit 0/1; xargs
whitespace-splits stdin (quote-blind, documented) onto the command's tail
(echo by default), -n N a batch at a time, exits 0 / 123 (a run failed) /
127 (could not exec), running once even on empty input, all like GNU.

## remaining work, in order

1. **polish, as need arises** -- ls -l (stat already carries size/mtime/mode),
   cp -r, multi-source cp/mv into a directory, sort -n/-k, uniq -d/-u, cut -b,
   tr [:class:] and -ds, echo -e, seq over gems, grep -i/-o/-E, sed -i/y/N.
   none block the distro; add them when a real script wants them.

after kore: the vim clone (rung 2), then the C compiler (rung 3) -- see the
love-distro arc. the multi-call `kore` is already the shape the distro boots with.
