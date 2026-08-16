# kore — the multi-call toolbox

crew/kore/ orients here; the laws live in crew/kore/law.l, the GNU-identical smokes in
`make test_kore`, and every doubt settles by probing the built `kore`.

kore is the distro's coreutils: the love-native POSIX environment over the Linux kernel is
kernel + a static `love` + .l files, and kore is busybox's multi-call trick done natively.

## the shape

ONE cat — the `$(korefiles)` list in crew/build.mk: kore's own toolboxes, lib/lint.l,
crew/vi/, mk/tools/ain.l, the lush files, crew/cook/cook.l and the holo linker files. It is
baked once into `kore.image`, and both `out/host/kore` and the installed `bin/kore` are
three-line `/bin/sh` wrappers that `wake` it — re-evaling the cat per spawn costs ~1.3s, so
only the distro, which has no image to ship, still runs it as a shebang script.
`crew/kore/kore.l` loads LAST and dispatches off the program seat of `cmdline`: `kore TOOL
ARGS..`, or symlink a tool's name to kore and argv[0] picks it (how the distro shadows at
will). The registry is a tablet, so tool names never collide with the globals they call (the
`mkdir` applet CALLS the `mkdir` nif; different namespaces).

The file discipline, two shapes:

* **a tool with a seat** (mk/tools/ain.l, crew/cook/cook.l): define-only, leaking
  one `<tool>-main`; a body-having tail fires it iff the file's own basename
  sits in the program seat — so the same file is a standalone tool AND a quiet
  cat member.
* **a toolbox** (core.l, fs.l): many mains, NO seat — kore is its door.

## the inventory (48 tools, 51 names)

| where | tools |
| --- | --- |
| kore.l (thin mains) | diff (the patience/myers engines), as (elf64 over the holo book), ar (GNU-shape archives + the ranlib index over ld-read, byte-identical smoke), ld (holo's static linker: -pie/-t/-Ttext, byte-identical to mooncc's own link), objcopy (a linked ELF flattened to `-O binary` or `-O ihex`, byte-identical to llvm/gnu objcopy on both) |
| mk/tools/ain.l | nc / ain |
| crew/cook/cook.l | make / cook |
| core.l, the line tools | cat echo head tail wc sort uniq tee |
| core.l, the field tools | cut tr nl rev |
| core.l, the trivia | seq yes true false basename dirname |
| fs.l, the fs tools | ls cp mv rm mkdir rmdir ln touch pwd chmod install readlink cmp |
| re.l, the matcher | grep (-n -v -c -l) over the lawed BRE engine |
| sed.l, the editor | sed (-n; s///gp, d, p, q; number/$/regex/range addresses) |
| awk.l, the language | awk (patterns and actions, BEGIN/END, arrays, user functions) |
| find.l, the walk | find (-name -path -type -print -prune -exec; ( ) ! -a -o; the depths) |
| proc.l, the processes | env sleep kill xargs |
| crew/vi/ | vi |
| crew/lush/ | sh / lush |

## the discipline (why this stays trustworthy)

* **GNU to the byte.** Every tool with printable output is smoked byte-identical against the
  real GNU tool in `make test_kore` (LC_ALL=C for sort/ls). The fussy faces are pinned
  deliberately: wc pads every field to the digit width of the byte TOTAL; uniq -c wears width
  7; nl is pad-6 + tab and a blank line is seven bare spaces; head/tail banner many files with
  `==> name <==`; ls -a is GNU -A. Effects (cp/mv/rm/..) are smoked by acting and then
  verifying with the shell.
* **the u-floor.** The shared helpers leak u-prefixed from core.l and are lawed pure in law.l:
  uatoi uread udie upad ujoin uhdr uhead/utail ucount ubase/udir usplit ujoinc uspec/upick
  uset urev ueach, fs.l's uoct/udirp/udest/ucopy, and proc.l's udur/uwords. `ueach` is the cat
  walk every whole-input tool rides (files or stdin, `-` reads stdin, a miss complains on err
  and the exit code remembers).
* **the exit door.** A main ANSWERS its status as a charm; it does not quit. A leave from deep
  inside a walk rides `udie`, which says its sentence and then `uleave` — a scare carrying the
  status — and `urun` is the driver both faces come back through, flushing the ports and
  answering the charm. So the seat is the one site that quits (`kore-main` answers, and the
  tail of kore.l quits with it), and a caller staying in the image lives through a tool that
  fails: `kore-main` is the in-image door, taking `(link "kore" "ls" "-l")` and answering the
  status. mooncc rides the same floor with two doors of its own — `moon-run` answers, `moon-main`
  quits with what it answers (doc/moon.md). ⚠ nothing unwinds through a scare, so a port a tool
  still holds at the leave is lost, exactly as `quit` lost it. The property is gated in
  test/gate/kore.sh and test/gate/moon.sh; a regression to `quit` passes every other check.
* **the nif lane.** fs effects ride host/posix.c (app-glob AiNif, no core edit) and its
  `posix_` conventions: an effect op answers () ok | a POSITIVE errno | EINVAL misuse; a value
  op answers the value | (). host/posix.c holds rename symlink readlink chmod chown utime
  umask rmdir hardlink (`link` the word belongs to the chain ctor). test/host/fs.l smokes them
  under test_hostnif.
* **exit codes.** 0 clean, 1 something failed (reported on err, the loop continued), 2 usage;
  diff keeps its classic 0/1/2 triple.

## traps

* `show` is the decimal formatter; `string` of a number makes a ONE-CHARM text.
* prel `sort` on strings IS lexicographic (probed via the "b"-vs-"ab" discriminator), which is
  exactly LC_ALL=C — no comparator needed.
* a symlink TARGET resolves relative to the LINK's directory, not the cwd.
* lines/unlines normalize an unterminated final line (the tool layer's ONE normalization); cat
  copies verbatim, head/tail/sort/uniq normalize like GNU sort does (GNU head does not — known,
  harmless, unsmoked).
* a value that can legitimately net 0 — an end index, a (0 0) span — reads BLUE (falsy): give
  it uread's (1 ..) success shape. And never name a local `err` or `out`; they are the PORTS,
  and the shadow says into a charm.

## the regex engine (crew/kore/re.l)

A POSIX-BRE dialect — literals, `.`, `*`, head-`^`/tail-`$`, [..] classes with
ranges/negation (first-] and edge-- literal), \-escapes, \( \) groups, GNU's \+ \? — with
GNU's leniency (a repeat with no atom is ink) and GNU's refusals mirrored as parse errors
(backrefs, intervals, alternation). `(rebre p)` answers `(1 nodes ngroups)` | `()`; `(rehas
nodes s)` the boolean; `(refind nodes s i)` the leftmost greedy span as `(1 start end)` — the
`(1 ..)` shapes because a match ending at 0 is blue by measure. The matcher is greedy
backtracking in continuation style; the laws hold the dialect by hand AND by a seeded
differential fuzz against an independent Brzozowski-derivative oracle. grep rides it: -n -v -c
-l, GNU-byte-identical smokes + the 0/1/2 exit triple (an unreadable file beats a match).
`refind` carries group SPANS (numbered in \( order, a repeated group reading as its LAST
iteration, GNU's \1) — sed's food.

## sed-lite (crew/kore/sed.l)

Over re.l. `sed [-n] SCRIPT [FILE..]`: ;/newline-separated commands, each [ADDR[,ADDR]] VERB;
addresses number/$/(BRE)/re/, ranges open-at-first close-at-later (numeric end at-or-before
start = one line, like GNU); verbs p, d, q (one address), and s/RE/REPL/[g][p] with any
delimiter — & and \1..\9 in the replacement, the POSIX empty-match rules exact (step after an
empty replacement, DISCARD an empty match where the last one ended: `s/x*/-/g` on "xbz" is
"-b-z-"). Input is the concatenated stream ($ = its last line); unreadable files
complain-and-flow, exit 2; a bad script exits 1 (GNU's split). The pure floor (sparse, usub) is
lawed; the whole face is smoked byte-identical vs GNU (a 12-script battery + -n + stdin + the
error faces). Out of dialect, deliberately: GNU's empty-pattern reuse, \n in replacements, hold
space.

## the process tools (crew/kore/proc.l)

No new nifs — environ/getenv/setenv, spawn (pid | negative errno; a child that cannot exec
_exit(127)s) + wait, still (pty.c's kill), rest (core sleep, ms). env prints the world or
assigns K=V.. and runs the command with the child's exit; sleep sums decimal durations with
s/m/h/d suffixes (udur, lawed); kill sends -N or -NAME (default TERM) per pid, exit 0/1; xargs
whitespace-splits stdin (quote-blind, deliberately) onto the command's tail (echo by default),
-n N a batch at a time, exits 0 / 123 (a run failed) / 127 (could not exec), running once even
on empty input, all like GNU.

## awk (crew/kore/awk.l)

A POSIX awk: BEGIN/END, `pattern { action }` items, `expr, expr` ranges, fields with `$0`
rebuilding on either side, the special variables (NR NF FS OFS ORS FILENAME FNR SUBSEP RSTART
RLENGTH CONVFMT OFMT), arrays with `in` and `delete`, user functions whose array parameters
pass **by reference**, and the builtins length substr index split sub gsub match sprintf sin
cos atan2 exp log sqrt int rand srand tolower toupper system close. `-F`, `-v var=value`,
`-f progfile` (repeatable) and command-line `var=value` between file arguments. Regexes are
re.l's ERE dialect; the whole language is smoked byte-identical against gawk in `make
test_kore`, the pure floor is lawed.

Three pieces are worth knowing before reading it:

* **the value is four-faced.** `()` uninitialised, a number, a string, and `('sn n s)` — a
  STRNUM, the thing that came off input looking like a number. It must compare as a number and
  print as the text it arrived in: `"007"` from a field is 7 to `==` and `007` to `print`. Two
  numeric-ish values compare numerically, anything else as text, and that one rule is why the
  field carries both faces rather than one.
* **the numbers are ours.** `show` prints a double round-trip exact; awk owes `%.6g`. So
  aw-fixed/aw-expo/aw-gen do the digits by hand off a normalised mantissa, and aw-sprintf is a
  real printf (flags, width, precision, `d i o u x X c s e E f g G`). They round half away from
  zero where C rounds half to even — a difference an exact decimal tie can reach and a computed
  double essentially never does.
* **`nil?` is a TRUTH test, not a type one.** It answers 1 for `0`, for `-4` and for `""` as
  readily as for `()`. awk leans on the difference every line, so the empty question is asked by
  identity (`aw-nil?` is `(id? v ())`) and never by truth. Getting this wrong prints `0` as `""`.

Out of dialect, deliberately — each a rung, not an oversight: **getline** in every spelling
(it is the one construct that makes the record loop re-entrant, and half a getline is worse
than none); **output pipes** (`print | "cmd"` — plain `>` and `>>` to a file are here);
**RS** other than newline; **ARGV/ARGC and ENVIRON** (the arguments are walked, not published);
printf's `*` width and `#` flag.

## find (crew/kore/find.l)

`find [PATH..] [EXPR]`, PATH defaulting to `.`. Primaries `-name` `-path` (fnmatch, via lush's
`sh-match`) `-type f|d|l` `-print` `-prune` `-exec CMD.. ;` `-true` `-false`, the global
`-maxdepth`/`-mindepth`, and the operators `( )` `!`/`-not` `-a`/`-and` (implicit between two
primaries) `-o`/`-or`, both short-circuiting, `-a` binding tighter. An expression naming no
action gets `-print`, exactly as GNU does.

* **the walk sorts each directory.** GNU hands out readdir order, which is the file system's
  business and repeats for nobody — so `find | sort` on both sides is the only honest way to
  smoke us against it, and that is what the gate does. Sorted is also what a build wants: the
  same tree cuts the same image twice.
* **symlinks are not followed** (GNU's `-P`, the default). The `stat` nif follows, so the type
  read asks `readlink` FIRST — a link answers `l` whatever it points at, and the walk does not
  descend through it. A dangling link is still visited.
* it loads late in the cat because it captures `sh-match` at its define; crew/build.mk says so.

## not built

Polish, as need arises: ls -l (stat already carries size/mtime/mode), cp -r, multi-source cp/mv
into a directory, sort -n/-k, uniq -d/-u, cut -b, tr [:class:] and -ds, echo -e, seq over gems,
grep -i/-o/-E, sed -i/y/N. None block the distro; add them when a real script wants them.
