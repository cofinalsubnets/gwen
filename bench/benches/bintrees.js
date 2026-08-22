// bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
// GC-throughput / long-lived-survival workload. build a stretch tree of depth
// max+1, hold a long-lived tree of depth max alive across the run, then for each
// depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their
// node counts. a leaf is null and counts 0.
const { bench } = require("../lib/bench");
const mk = (d) => (d < 1 ? null : [mk(d - 1), mk(d - 1)]);
const ck = (t) => (t === null ? 0 : 1 + ck(t[0]) + ck(t[1]));
function btRun(mn, mx) {
  const stretch = ck(mk(mx + 1));
  const long = mk(mx); // LONG-LIVED -- survives the loop below
  let total = 0;
  for (let d = mn; d <= mx; d += 2) {
    const n = 1 << (mx - d + mn);
    let s = 0;
    for (let i = 0; i < n; i++) s += ck(mk(d));
    total += s;
  }
  return stretch + ck(long) + total;
}
bench("bintrees", () => btRun(4, 14));
