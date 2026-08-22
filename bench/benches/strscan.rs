include!("../lib/bench.rs");

const HMOD: i64 = 1000000007;

// fixed string built once; the timed work is a linear rolling-hash scan.
// chars are ASCII (32 + (7*i)%95) in 32..126, single-byte.
fn main() {
    let data: Vec<u8> = (0..20000i64).map(|i| (32 + (7 * i) % 95) as u8).collect();
    bench("strscan", || {
        let mut h: i64 = 0;
        for &ch in &data {
            h = (h * 31 + ch as i64) % HMOD;
        }
        h
    });
}
