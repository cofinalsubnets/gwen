include!("../lib/bench.rs");

// sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
const N: usize = 5000;

fn main() {
    bench("sort", || {
        let mut x: i64 = 1;
        let mut data: Vec<i64> = Vec::with_capacity(N);
        for _ in 0..N {
            x = (16807 * x) % 2147483647;
            data.push(x);
        }
        data.sort_unstable();
        let mut h: i64 = 0;
        for &v in &data {
            h = (h * 31 + v) % 1000000007;
        }
        h
    });
}
