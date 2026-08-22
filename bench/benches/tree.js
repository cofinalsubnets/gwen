// binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^D-1.
const { bench } = require("../lib/bench");
const mk = (d) => (d === 0 ? null : [mk(d - 1), mk(d - 1)]);
const ck = (t) => (t === null ? 0 : 1 + ck(t[0]) + ck(t[1]));
bench("tree", () => ck(mk(16)));
