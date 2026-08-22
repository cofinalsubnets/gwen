// build an N-char string by repeated single-char concatenation, then hash it.
const { bench } = require("../lib/bench");
const HMOD = 1000000007;
const N = 4000;
bench("strcat", () => {
  let s = "";
  for (let i = 0; i < N; i++) s += String.fromCharCode(48 + i % 10);
  let h = 0;
  for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % HMOD;
  return h;
});
