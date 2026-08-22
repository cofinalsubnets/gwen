// naive recursive fibonacci -- function-call and integer-arithmetic stress.
const { bench } = require("../lib/bench");
function fib(n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }
bench("fib", () => fib(30));
