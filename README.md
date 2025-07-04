# gwen lisp

gwen lisp is a simple lisp dialect inspired by haskell and scheme.

## features
- integers, symbols, strings, pairs, hash tables, and functions
- threaded code interpreter with self hosting compiler
- lexical scope, closures, garbage collection, etc
- five special forms plus macros
- every expression has a value (no exceptions)
- every value is a function (data returns itself)
- every function has one argument (automatic currying)

## gwen vs. scheme

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

since all functions are curried, no function is nullary (no arguments)
or variadic (variable number of arguments). variadic functions can be
simulated with macros. if you need a nullary function, write a unary
function and ignore the argument. in the absence of nullary functions,
the value of a singleton list is the value of the car of the list.

evaluation order for function application can be variable. if
you need a specific order use `,` or `:`.
