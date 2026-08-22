// the takeuchi function -- deep non-tail recursion, no allocation.
const { bench } = require("../lib/bench");
function tak(x, y, z) {
  return y < x ? tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y)) : z;
}
bench("tak", () => tak(22, 12, 6));
