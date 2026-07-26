---
title: LOVE
section: 1
source: love @VERSION@
manual: love manual
---

# NAME

love - a curried lisp over a tiny generic core

# SYNOPSIS

**love** \[**-l** *lib*\] \[**-e** *expr*\] \[**-v**\] \[**-h**\] \[**--**\] \[*file* \[*arg* ...\]\]

# DESCRIPTION

**love** is a small lisp in which every value is a total function of one argument and application is left-to-right currying: (*f* *x* *y*) is ((*f* *x*) *y*) and (*f*) is *f*. Numbers are Church numerals, a list of numbers is a tower of exponents, and data self-applies (it indexes). The core is fully generic: every operation dispatches on a value's kind, and the kinds form a lattice ordered by a single total order over all values.

There are three special forms — **:** (letrec\*/sequence), **?** (cond), and **\\** (lambda, or quote with a single operand) — and everything else is a function call. Truth is the net measure: a value is false when it nets to nothing and true when it nets positive.

With a *file* argument (or **-e**), **love** runs it as the program and exits; any further arguments are the program's, in **argv**. With no program it starts an interactive read-eval-print loop when standard input is a terminal, and otherwise reads and evaluates standard input to end of file.

# OPTIONS

**-l** *lib*, **--load** *lib*
:   Preload *lib* (read-eval it to end of file) before the program. May be repeated.
**-e** *expr*, **--eval** *expr*
:   Read-eval the forms in *expr*, as the program.
**-v**, **--version**
:   Print the version and exit.
**-h**, **--help**
:   Print a usage summary and exit.
**--**
:   End option processing: the next argument is the program, the rest are its arguments.
*file*
:   A program to load. The first *file* is the program; further arguments are passed to it in **argv**. A file that cannot be opened is a fatal error.

# EXAMPLES

Evaluate a file:

> ```
> love prog.l
> ```

Load a library, then run a script:

> ```
> love -l lib.l prog.l
> ```

Evaluate an expression:

> ```
> love -e '(. (+ 1 2))'
> ```

Evaluate from standard input:

> ```
> echo '(+ 1 2)' | love
> ```

Start the interactive shell:

> ```
> love
> ```

# EXIT STATUS

**love** exits **0** on success and non-zero on a fatal error, such as a file that cannot be opened or an explicit (*exit* *n*).

# SEE ALSO

The project README and the executable specification in *test/spec.l*, which asserts every claim of the language on every build target.
