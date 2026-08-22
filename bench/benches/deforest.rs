include!("../lib/bench.rs");

use std::hint::black_box;

// sum of (k^2 mod p) of the odd numbers in [0, N) -- a map/filter/fold pipeline.
// checksum = 4891344686 (< 2^53). APPLES-TO-APPLES with love's glaze: only the INPUT n is
// opaque (black_box, no compile-time fold / rep-doubling hang); the `% p` makes the body
// non-polynomial so NEITHER compiler can close it -- both run an honest O(n) FUSED loop
// (love deforests to one native counted loop; rustc -O fuses + vectorizes the chain), which
// is exactly what this bench measures: the abstraction cost of the functional pipeline.
const N: i64 = 20000;

fn main() {
    bench("deforest", || {
        let n = black_box(N);
        (0..n)
            .filter(|x| x % 2 == 1)
            .map(|x| (x * x) % 1000003)
            .fold(0i64, |a, b| a + b)
    });
}
