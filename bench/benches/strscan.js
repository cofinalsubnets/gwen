// fixed string built once; the timed work is a linear rolling-hash scan.
const { bench } = require("../lib/bench");
const HMOD = 1000000007;
const data = Array.from({ length: 20000 }, (_, i) => String.fromCharCode(32 + (7 * i) % 95)).join("");
bench("strscan", () => {
  let h = 0;
  for (let i = 0; i < data.length; i++) h = (h * 31 + data.charCodeAt(i)) % HMOD;
  return h;
});
