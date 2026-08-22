// mutable hash-table throughput (see bench/benches/hash.l). checksum = N*N.
const { bench } = require("../lib/bench");
const N = 10000;
function work() {
  const h = new Map();
  for (let i = 0; i < N; i++) h.set(97 * i + 1, i);
  let a = 0;
  for (let i = 0; i < N; i++) a += h.get(97 * i + 1);
  for (let i = 0; i < N; i++) { const k = 97 * i + 1; h.set(k, h.get(k) + 1); }
  for (let i = 0; i < N; i++) a += h.get(97 * i + 1);
  return a;
}
bench("hash", work);
