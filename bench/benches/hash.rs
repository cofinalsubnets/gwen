include!("../lib/bench.rs");

use std::collections::HashMap;

// mutable hash-table throughput (see bench/benches/hash.l). checksum = N*N.
const N: i64 = 10000;

fn main() {
    bench("hash", || {
        let mut h: HashMap<i64, i64> = HashMap::new();
        for i in 0..N {
            h.insert(97 * i + 1, i);
        }
        let mut a: i64 = 0;
        for i in 0..N {
            a += h[&(97 * i + 1)];
        }
        for i in 0..N {
            let k = 97 * i + 1;
            let v = h[&k] + 1;
            h.insert(k, v);
        }
        for i in 0..N {
            a += h[&(97 * i + 1)];
        }
        a
    });
}
