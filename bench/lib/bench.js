// node benchmark harness -- mirrors bench/bench.l.
// bench(name, work) auto-scales the repetition count (doubling until the run
// clears MIN_MS), then prints one line matching the other harnesses:
//     <name> <lang> <reps> <ms> <checksum>
// work is a nullary function returning a deterministic checksum. the same file
// serves node and deno; BENCH_LANG sets the label, default "node".
const MIN_MS = 200.0;
const LANG =
  (typeof process !== "undefined" && process.env && process.env.BENCH_LANG) ||
  (typeof Deno !== "undefined" && Deno.env.get("BENCH_LANG")) ||
  "node";

function bench(name, work) {
  let reps = 1;
  for (;;) {
    const t0 = performance.now();
    let chk;
    for (let i = 0; i < reps; i++) chk = work();
    const ms = performance.now() - t0;
    if (ms >= MIN_MS) {
      console.log(`${name} ${LANG} ${reps} ${ms.toFixed(3)} ${chk}`);
      break;
    }
    reps *= 2;
  }
}

module.exports = { bench };
