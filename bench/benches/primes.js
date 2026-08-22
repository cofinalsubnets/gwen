// count primes below 30000 by trial division; checksum = pi(30000) = 3245.
const { bench } = require("../lib/bench");
function isPrime(n) {
  for (let d = 2; d * d <= n; d++) if (n % d === 0) return false;
  return true;
}
function count(lo, hi) {
  let c = 0;
  for (let n = lo; n < hi; n++) if (isPrime(n)) c++;
  return c;
}
bench("primes", () => count(2, 30000));
