# gwen lisp

gwen lisp is a lisp dialect and environment that can be built as a library,
an executable, or for bare metal on various platforms. in gwen lisp:

- every expression has a value (there are no runtime errors)
- every value is a function (data are their own constant functions)
- every function is unary (via currying)

there are four special forms:

| gwen               |  scheme equivalent |
|--------------------|----------|
| `?`                | `cond`   |
| `:`                | `let`    |
| <code>&#96;</code> | `quote`  |
| <code>&#92;</code> | `lambda` |

## : (let)

| gwen            | scheme                      |
|-----------------|-----------------------------|
| `(: a b c d e)` | `(let  ((a b) (c d)) e)` |

takes any number of name/definition pairs followed by a final expression. if the
final expression is omitted then the last name given becomes the final expression.
if a final expression is omitted at top level that is considered a global definition.
definitions are evaluated sequentially and recursion is supported. shadowing variables
by redefining them within the same or an enclosed let expression works in the expected
way. scheme-like syntactic sugar for defining lambdas is supported. `:` is also used
for sequencing by assigning to an ignored variable.

## ? (cond)

| gwen            | scheme                      |
|-----------------|-----------------------------|
| `(? a b c d e)` | `(cond (a b) (c d) (#t e))` |

the only false value in a conditional is 0. if a default branch is
omitted then it becomes 0.

## &#92; (lambda)

| gwen                           | scheme                 |
|--------------------------------|------------------------|
| <code>(&#92; a b c d e)</code> | `(lambda (a b c d) e)` |

lambdas are defined over exactly one expression. use `:` if you need sequencing.

## &#96; (quote)

| gwen  | scheme |
|-------|--------|
| <code>(&#96; x)</code> | `(quote x)` |

this one similar to scheme :)

## function expressions

in gwen lisp every function is unary. zero-argument "thunks" must be simulated by passing and ignoring an arbitrary
value (usually `0` or equivalently `()`).

| gwen  | scheme |
|-------|--------|
| f     | f      |
| (f)   | f      |
| (f 0) | `(f)`  |

argument evaluation order is not guaranteed. use `:` for specific ordering.

since functions always act as if applied to one value at a time, there are no nullary
functions, and the value of a singleton list is the value of the head of the list.
nullary functions are simulated by ignoring the argument to a unary function. variadic
functions can be simulated using macros or implemented with sentinels.
