---
title: LUSH
section: 1
source: love @VERSION@
manual: love manual
---

# NAME

lush - the love shell, a POSIX command shell 🐚

# SYNOPSIS

**lush** \[**--login**\] \[**-c** *command* \[*name* \[*arg*...\]\] | *script* \[*arg*...\]\]

**kore** **sh** ... (or an **sh** symlink to kore)

# DESCRIPTION

**lush** is a POSIX-compatible command shell written in **love**(1). It is the project's daily-driver shell, the shell its build and gate scripts run under, and the console shell of the love distro (where `/bin/sh` is a kore symlink that lands here).

Interactively it offers an editable line with Tab completion (commands from PATH and the builtins at command position, pathnames after; `~` folds and unfolds), persistent history (*~/.lush_history*, appended on Enter), a prompt carrying the current directory, the last nonzero exit status, and the git branch when inside a repository, and real job control: `^Z` stops the foreground job, **jobs**/**fg**/**bg** manage it, a trailing `&` backgrounds a pipeline, `$!` names it. An error at the prompt re-prompts; the terminal is never left raw.

The script subset covers pipelines and lists (`| && || ; !`), redirects including io-numbers and `2>&1`, here-documents (`<<`, `<<-`), command substitution (`$( )` and backticks), globbing (`* ? [..]`, quoting-aware), tilde expansion, `if`/`while`/`until`/`for`/`case`, `{ }` groups and `( )` subshells, functions with `return`, shell variables over the environment (`export` promotes), positional parameters with the quoted-`"$@"` law, the `${x:-y}` parameter-expansion family including `${#x}` and `${x#pat}`/`${x%pat}`, and `set -e -u -x`.

# OPTIONS

**-c** *command* \[*name* \[*arg*...\]\]
:   Run *command* and exit. *name* becomes `$0` (default `lush`), the *arg*s the positional parameters. The command may span lines.
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
