// sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
const { bench } = require("../lib/bench");
const N = 5000;
function work() {
  let x = 1;
  const data = [];
  for (let i = 0; i < N; i++) { x = (16807 * x) % 2147483647; data.push(x); }
  data.sort((a, b) => a - b);
  let h = 0;
  for (const v of data) h = (h * 31 + v) % 1000000007;
  return h;
}
bench("sort", work);
