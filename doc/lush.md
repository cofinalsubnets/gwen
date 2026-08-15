---
title: LUSH
section: 1
source: love @VERSION@
manual: love manual
---

# NAME

lush - the love shell, a POSIX command shell 🐚

# SYNOPSIS

**lush** \[**--login**\] \[**-ceux** \[*command* \[*name* \[*arg*...\]\]\] | *script* \[*arg*...\]\]

**kore** **sh** ... (or an **sh** symlink to kore)

# DESCRIPTION

**lush** is a POSIX-compatible command shell written in **love**(1). It is the project's daily-driver shell, the shell its build and gate scripts run under, and the console shell of the love distro (where `/bin/sh` is a kore symlink that lands here).

Interactively it offers an editable line with Tab completion (commands from PATH and the builtins at command position, pathnames after; `~` folds and unfolds), persistent history (*~/.lush_history*, appended on Enter), a prompt carrying the current directory, the last nonzero exit status, and the git branch when inside a repository, and real job control: `^Z` stops the foreground job, **jobs**/**fg**/**bg** manage it, a trailing `&` backgrounds a pipeline, `$!` names it. An error at the prompt re-prompts; the terminal is never left raw.

The script subset covers pipelines and lists (`| && || ; !`), redirects including io-numbers and `2>&1`, here-documents (`<<`, `<<-`), command substitution (`$( )` and backticks), globbing (`* ? [..]`, quoting-aware), tilde expansion, `if`/`while`/`until`/`for`/`case`, `{ }` groups and `( )` subshells, functions with `return`, shell variables over the environment (`export` promotes), positional parameters with the quoted-`"$@"` law, the `${x:-y}` parameter-expansion family including `${#x}` and `${x#pat}`/`${x%pat}`, and `set -e -u -x`.

Command substitution does not fork. A body that only says its piece -- **pwd**, **:**, **true**, **false** -- is taken by a sink this task wears, with no pipe and no child at all; anything else runs in the shell itself with stdout on a pipe a cooperative task drains, so any size flows. POSIX puts the body in a subshell, and forkless there is nobody to copy the state, so lush puts back by hand what a fork would have taken: the working directory, shell variables, functions, positional parameters, `$0`, the `-e -u -x` flags, and the environment. What it does not put back: a job started inside stays the shell's, an fd opened inside stays open, and **umask** and the signal dispositions stand. `exit` inside a body ends the body with that status, as it would in a subshell.

Some commands do not fork either. When a word names a tool whose main rides this very image and PATH's winner for that word *is* the binary already running, lush calls it here instead of exec'ing it: no fork, no exec, no image wake, and the tool answers its exit status as a value (see **kore**(1) on the status charm). The identity test is the point and it is strict: `stat` through the symlink must match the running binary's own path (`selfpath`), so one artifact on PATH wearing many names takes the lane, while an *installed twin of our own name* -- same tool, different build -- spawns like anything else. A word with a `/` always spawns; a name PATH resolves to somebody else's binary always spawns. The verdict is taken once per word per session, and it is the simple foreground command only -- pipeline stages still spawn, which is what makes them concurrent.

The list of words that may take it is deliberately short: today, **mooncc**(1). A main only qualifies if running it here is the *same thing* as running it there, and three kinds fail that -- a main that quits (it would end its caller), one holding state a single run owns (a recursive `$(MAKE)`), and one that can block (a spawned `cat` or `sleep` is a child `^C` kills and `kill %1` can name; in here neither is true). The compiler is the one that pays, since a build spends its life calling it. Widening this to the bare coreutil names -- what would make the distro's shadow lane free -- waits on the interrupt and stdin story.

For a caller already in the image there is one more door. `sh-oneline` runs a single line the way a subshell would -- output straight onto the caller's fds, and the state a fork would have copied put back by hand, the same roster the command-substitution paragraph above lists. **cook**(1) uses it for recipe lines, which is what lets a whole `CC=mooncc` build pay one image wake instead of one per translation unit. `exit`, `set -e` and `set -u` all end that line rather than the process.

# OPTIONS

**-c** *command* \[*name* \[*arg*...\]\]
:   Run *command* and exit. *name* becomes `$0` (default `lush`), the *arg*s the positional parameters. The command may span lines.
**-e**, **-u**, **-x**
:   The `set` flags, given at invocation. They bundle with each other and with **-c** in one word: `lush -ec 'cmd'` is what a Makefile's `.SHELLFLAGS` writes, and it is how lush can be the `SHELL` of a make.
**--login**
:   A login shell: read */etc/profile*, then *~/.profile*, before anything else. A dash-led `argv[0]` (`-lush`, `-sh`, the mark **login**(1) leaves) is honored too, but the `env -S` shebang usually eats it -- the flag is the reliable door.

With no operands lush is interactive when stdin is a terminal, and reads commands line by line (gathering continuations) otherwise. A *script* operand runs the file, with the remaining operands as positional parameters.

# INVOCATION FILES

A login shell reads */etc/profile* then *~/.profile* first, whatever mode it runs in. An interactive shell then reads `$ENV` when set, else *~/.lushrc*. A broken line in an rc file scares back to the prompt; it cannot take the shell down.

# BUILTINS

The external tools stay external; the builtins are the ones that must run inside the shell's own process: **cd**, **pwd**, **exit**, **export**, **read**, **set**, **shift**, **unset**, **eval**, **.**, **wait**, **local**, **break**, **continue**, **return**, **jobs**, **fg**, **bg**, **:**, **true**, **false**. `return` inside a `.`-sourced file stops that file (the POSIX dot-return), which is how */etc/profile.d* guards bail early.

# MAKING IT YOUR SHELL

Point a terminal emulator at `lush` (installed on PATH by `make install`) -- terminal shells are interactive non-login, so *~/.lushrc* is the file to season. For **chsh**(1), add the absolute path (`~/.local/bin/lush` resolves to the nest) to */etc/shells* and `chsh -s` it; a display manager or **login**(1) then spawns it with a dash `argv[0]` that the shebang drops, so a login-shell entry is best expressed as a two-line wrapper script `exec lush --login "$@"` -- or by sourcing your profile from *~/.lushrc*.

# EXIT STATUS

The status of the last command; `exit N` and a script's final `$?` pass through. `127` when a script operand does not exist, `2` on a syntax error or unexpected end of file.

# SEE ALSO

**love**(1), **kore**(1), **cook**(1), **sh**(1p). The lush sources live in *crew/lush/* of the love tree; *test/host/sh.l* is the executable gate.
