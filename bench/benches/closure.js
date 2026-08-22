const { bench } = require("../lib/bench");
const twice = (f) => (x) => f(f(x));
const adder = (i) => (x) => x + i;
const N = 100000, M = 1000000007;
function work() {
  let acc = 0;
  for (let i = 0; i < N; i++) acc = (acc * 31 + twice(adder(i))(i)) % M;
  return acc;
}
bench("closure", work);
