---
title: COOK
section: 1
source: love @VERSION@
manual: love manual
---

# NAME

cook - a dependency-driven build tool, make in love

# SYNOPSIS

**cook** \[*options*\] \[*recipe*...\]

**love** **-l** *crew/cook/cook.l* \[*options*\] \[*recipe*...\]

# DESCRIPTION

**cook** brings an *item* up to date when it is missing or older than any of its prerequisites, running the item's recipe over freshened prerequisites first — the whole of make's idea. It is written in **love**(1) and runs on it.

With no **-f**, cook discovers a build file in the current directory, preferring a love-native recipe file over a Makefile: a legacy **Cards.l** first, then a **Cookfile**, then a **Makefile**. Only a file named like a Makefile (**Makefile**, **makefile**, **GNUmakefile**, **\*.mk**) is ever read as make; a **Cards.l** or **Cookfile** is love source. Name one explicitly with **-f** to override. Like **make**, cook takes its options in any order and treats every non-option word as a *recipe* to build; with no recipe it builds the first one declared.

## Makefile

cook reads an ordinary **Makefile** directly — a reasonably GNU-make-compatible import: recursive $(**VAR**) expansion and substitution references, the common functions (**$(**shell**),** $(**wildcard**), $(**dir**), $(**patsubst**), $(**filter**), $(**call**), $(**origin**), ...), **ifeq**/**ifdef** conditionals, **include**, the **= := ?= +=** assignment flavors with the **override** and **export** directives, pattern rules and static patterns, order-only prerequisites, and the **$@** **$<** **$^** **$\*** **$(@D)** **$(@F)** automatic variables. The variable table seeds as make's does: the environment first, then the builtin defaults, then any *NAME*=*VAL* words from the command line — which silence file assignments to the same name. cook also carries a slice of make's **builtin implicit rules** — `%.o: %.c`, `%.o: %.S`, `%.o: %.s` — the database a Makefile never states and Lua 5.4 leans on for 31 of its 34 objects. They are tried only *after* the file's own patterns and only when the stem-source actually exists. Make's **match-anything** builtins (`%: %.c`, `%: %.o`) are deliberately absent: they match every target there is, and GNU only dares them under conditions this resolver does not model — with `love.o` on disk, `%: %.o` will happily link that one object as `love` and leave the whole host floor undefined. Old-style **suffix rules** (`.c.o:` is `%.o: %.c`, `.c:` is `%: %.c`) are read too, keyed on the suffix list so `.PHONY` is never mistaken for one; and `.POSIX:` moves the defaults to `CC=c99`, `CFLAGS=-O1` as make does. Each recipe line is run through **sh**(1), so pipes, globs and redirections work.

When the shell cook would spawn *is* the binary cook is already running — one artifact carrying both, which is what the distro and `love up` install — the line runs in that image instead, through **lush**(1)'s `sh-oneline`. The semantics are a spawned shell's: a `cd` in one line does not reach the next, `.SHELLFLAGS` (`-e -u -x`) are honored, `exit` ends the line, and cook's own status is unchanged. What it buys is that every *other* tool in the image is then in reach too — above all **mooncc**(1), so a `CC=mooncc` build pays one image wake for the whole make rather than one per translation unit. Measured on a 12-file build: 2.03 s spawning, 0.58 s in-image, byte-identical objects. A `SHELL` that is anything else, or a compiler on PATH that is a *different build* of our own name, spawns exactly as before — the identity test is `stat` against `/proc/self/exe`, so a same-name-different-build twin never gets silently substituted.

## Cookfile

A **Cookfile** is ordinary love source that registers recipes and then calls **cook**:

> ```
> (recipe "hello" '("hello.o" "greet.o")
>         '(("cc" "-o" "hello" "hello.o" "greet.o")))
> (recipe 'clean '() '(("rm" "-f" "hello" "hello.o")))
> (cook-all 0)
> ```

A card is (*recipe* *item ingredients steps**):* an *item* is a filename string or a phony symbol, ingredients are the items it needs first, and steps are argv lists (**hark**ed as subprocesses) or thunks. Item ages come from the **stat** nif at nanosecond resolution; a phony symbol owns no file, so it is ageless and always cooks.

A Cookfile drives itself: **(cook-all****0)** builds every *recipe* named on the command line (or the default when none), while **(cook****(ticket****0))** builds just the first. **--emit** generates a Cookfile ending in **(cook-all 0)**.

# OPTIONS

**-f**, **--file** *FILE*
:   The build file (a Makefile or a Cookfile). If omitted, it is discovered in the current directory. For compatibility, when no **-f** is given the first non-option word that names an existing file is taken as the build file and the rest are recipes.
**--emit**
:   Transpile the Makefile to a fully resolved **Cookfile** on standard output — variables, the make functions and pattern rules expanded, each recipe line an (**sh -c** *cmd*) step — then exit without cooking.
**-v**, **--version**
:   Print the version and exit.
**-h**, **--help**
:   Print a usage summary and exit.

(**help** and **version** are also accepted as bare words.)

# OPERANDS

*recipe*
:   An item to build: a filename, or a phony target named in the build file. Any number may be given (they are built in order); the default is the first recipe declared.
*NAME*=*VAL*
:   A command-line variable, as in make: it overrides the environment and silences the build file's own assignments to *NAME* (an **override** directive in the file wins it back).

# EXAMPLES

Build the default target from the build file in the current directory:

> ```
> cook
> ```

Build named targets (in order) from a chosen Makefile:

> ```
> cook -f Makefile clean all
> ```

Transpile a Makefile to a resolved Cookfile:

> ```
> cook --emit -f Makefile > Cookfile
> ```

Without the installed symlink, loading cook by hand:

> ```
> love -l crew/cook/cook.l Makefile host
> ```

# EXIT STATUS

**cook** exits **0** when every cooked item is up to date, and non-zero when a recipe command fails or a needed item has neither a recipe nor a file.

# SEE ALSO

**love**(1), **make**(1), and *crew/cook/example/* for a worked C build.
