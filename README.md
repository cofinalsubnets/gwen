# gwen lisp

gwen lisp is a lisp dialect and environment that can be built as a C library,
an executable, or an operating system on various platforms.

## gwen lisp vs. scheme

| gwen               |  scheme  |
|--------------------|----------|
| `,`                | `begin`  |
| `?`                | `cond`   |
| `:`                | `letrec` |
| <code>&#96;</code> | `quote`  |
| <code>&#92;</code> | `lambda` |

the internal syntax of forms is generally simplified compared to scheme by omitting
redundant grouping parentheses.

### definitions

| gwen            | scheme                      |
|-----------------|-----------------------------|
| `(: a b c d e)` | `(letrec  ((a b) (c d)) e)` |

`:` takes any number of name/definition pairs followed by a final expression. if the
final expression is omitted then it becomes the final name. if a name is omitted then
the value of the expression is 0.  a `:` form at top level with no body is a global
definition.  definitions are evaluated in order with repeated names being shadowed in
subsequent values. the value of the `:` expression is the value of the final expression
in the context of all of the definitions. `:` supports similar list based syntactic
sugar for function definitions as `define` in scheme.

### conditionals

| gwen            | scheme                      |
|-----------------|-----------------------------|
| `(? a b c d e)` | `(cond (a b) (c d) (#t e))` |

the only false value in a conditional is 0.

### lambdas

| gwen                           | scheme                 |
|--------------------------------|------------------------|
| <code>(&#92; a b c d e)</code> | `(lambda (a b c d) e)` |

argument evaluation order for function expressions can be variable. for
specific order use `,`, `:`, or nested applications.

### function expressions

| gwen  | scheme |
|-------|--------|
| f     | f      |
| (f)   | f      |
| (f 0) | `(f)`  |

since functions always act as if applied to one value at a time, there are no nullary functions, and the value of a singleton list
is the value of the head of the list. nullary functions are simulated by ignoring
the argument to a unary function. variadic functions can be simulated using macros
or implemented with various methods.
