include!("../lib/bench.rs");

use std::hint::black_box;

// build the list 1..100000 then sum it. checksum = 5000050000.
// the sum of an arithmetic range is closed-formed by LLVM, so each element is
// funnelled through black_box to force the honest O(n) fold over a real Vec.
fn main() {
    bench("sum", || {
        let data: Vec<i64> = (1..=100000).collect();
        data.iter().map(|&x| black_box(x)).fold(0i64, |a, b| a + b)
    });
}
