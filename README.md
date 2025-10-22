# mitty

mitty is a lisp environment that can be built as a C library, an
executable, or an operating system on various platforms. mitty lisp
is a simple lisp dialect inspired by haskell and scheme.

## mitty vs. scheme

| gwen               |  scheme  |
|--------------------|----------|
| `,`                | `begin`  |
| `?`                | `cond`   |
| `:`                | `letrec` |
| <code>&#96;</code> | `quote`  |
| <code>&#92;</code> | `lambda` |

`define` and `set!` forms are not present. a `:` form at top level with
no body is considered global definition. in all contexts a `:` form
with no body has the value of its last defined variable. `:` supports
recursive function definitions and syntactic sugar for functions similar
to scheme's `define`. bindings other than functions are evaluated
sequentially, and prior bindings within the same form can be referred to
and shadowed in the obvious way.

the syntax of some of the forms is simplified (less parentheses)
gwen compared to scheme.

| gwen                           | scheme                      |
|--------------------------------|-----------------------------|
| `(? a b c d e)`                | `(cond (a b) (c d) (#t e))` |
| `(: a b c d e)`                | `(letrec  ((a b) (c d)) e)` |
| <code>(&#92; a b c d e)</code> | `(lambda (a b c d) e)`      |

in conditionals only `0` is false. this is equivalent to `#f` and `'()`
in scheme. dotted pairs are not read nor displayed (an atom in cdr is
always omitted from display).


argument evaluation order for function expressions can be variable.
if you need a specific order of evaluation, use `,`, `:`, or nested applications.

unlike in other lisp dialects, since functions always act as if applied to one
value at a time, there are no nullary functions, and the value of a singleton list
is the value of the head of the list. nullary functions are simulated by ignoring
the argument to a unary function. variadic functions can be simulated using macros
or implemented with various methods.
