// Bell numbers in base 36 (see bench/benches/bell.l) -- a bignum (BigInt) stress.
// fresh memo maps per rep; checksum = total characters across all lines.
const { bench } = require("../lib/bench");
const DIGITS = "0123456789abcdefghijklmnopqrstuvwxyz";
const BASE = BigInt(DIGITS.length);
function bellRun(limit) {
  const facts = new Map(), bells = new Map();
  function fact(n) {
    if (facts.has(n)) return facts.get(n);
    let x = 1n;
    for (let m = BigInt(n); m > 1n; m--) x *= m;
    facts.set(n, x);
    return x;
  }
  const choose = (n, k) => fact(n) / (fact(k) * fact(n - k));
  function bell(n) {
    if (bells.has(n)) return bells.get(n);
    let r;
    if (n < 2) r = 1n;
    else { r = 0n; for (let k = 0; k < n; k++) r += choose(n - 1, k) * bell(k); }
    bells.set(n, r);
    return r;
  }
  function show(n) {
    let s = "";
    while (n > 0n) { s = DIGITS[Number(n % BASE)] + s; n /= BASE; }
    return s;
  }
  let total = 0;
  for (let i = 0; ; i++) {
    const b = show(bell(i));
    if (b.length > limit) return total;
    total += b.length;
  }
}
bench("bell", () => bellRun(280));
