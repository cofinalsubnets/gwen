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

**love** **-l** *apps/cook/cook.l* \[*options*\] \[*recipe*...\]

# DESCRIPTION

**cook** brings an *item* up to date when it is missing or older than any of its prerequisites, running the item's recipe over freshened prerequisites first — the whole of make's idea. It is written in **love**(1) and runs on it.

With no **-f**, cook discovers a build file in the current directory, preferring a love-native recipe file over a Makefile: a legacy **Cards.l** first, then a **Cookfile**, then a **Makefile**. Only a file named like a Makefile (**Makefile**, **makefile**, **GNUmakefile**, **\*.mk**) is ever read as make; a **Cards.l** or **Cookfile** is love source. Name one explicitly with **-f** to override. Like **make**, cook takes its options in any order and treats every non-option word as a *recipe* to build; with no recipe it builds the first one declared.

## Makefile

cook reads an ordinary **Makefile** directly — a reasonably GNU-make-compatible import: recursive $(**VAR**) expansion and substitution references, the common functions (**$(**shell**),** $(**wildcard**), $(**dir**), $(**patsubst**), $(**filter**), $(**call**), $(**origin**), ...), **ifeq**/**ifdef** conditionals, **include**, the **= := ?= +=** assignment flavors with the **override** and **export** directives, pattern rules and static patterns, order-only prerequisites, and the **$@** **$<** **$^** **$\*** **$(@D)** **$(@F)** automatic variables. The variable table seeds as make's does: the environment first, then the builtin defaults, then any *NAME*=*VAL* words from the command line — which silence file assignments to the same name. Each recipe line is run through **sh**(1), so pipes, globs and redirections work.

## Cookfile

A **Cookfile** is ordinary love source that registers recipes and then calls **cook**:

> ```
> (recipe "hello" '("hello.o" "greet.o")
>         '(("cc" "-o" "hello" "hello.o" "greet.o")))
> (recipe 'clean '() '(("rm" "-f" "hello" "hello.o")))
> (cook-all 0)
> ```

A card is (*recipe* *item ingredients steps**):* an *item* is a filename string or a phony symbol, ingredients are the items it needs first, and steps are argv lists (run as subprocesses) or thunks. Item ages come from the **stat** nif at nanosecond resolution; a phony symbol owns no file, so it is ageless and always cooks.

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
> love -l apps/cook/cook.l Makefile host
> ```

# EXIT STATUS

**cook** exits **0** when every cooked item is up to date, and non-zero when a recipe command fails or a needed item has neither a recipe nor a file.

# SEE ALSO

**love**(1), **make**(1). The project README, and *tools/cook-example/* for a worked C build.
