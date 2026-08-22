// sum of squares of the odd numbers in [0, N) -- a map/filter/fold list pipeline.
// the idiomatic JS chain .filter().map().reduce() materializes the range + two
// intermediate arrays each call -- the allocation love's deforestation fuses away.
// checksum = 4891344686 (< 2^53, exact as a double).
const { bench } = require("../lib/bench");
bench("deforest", () =>
  Array.from({ length: 20000 }, (_, i) => i)
    .filter(x => x % 2 === 1)
    .map(x => (x * x) % 1000003)
    .reduce((a, b) => a + b, 0));
