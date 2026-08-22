include!("../lib/bench.rs");

const HMOD: i64 = 1000000007;
const N: i64 = 4000;

// build an N-char string by repeated single-char concatenation, then hash it.
// the chars are ASCII digits (48 + i%10), so byte and char value coincide.
fn main() {
    bench("strcat", || {
        let mut s = String::new();
        for i in 0..N {
            s.push((48 + i % 10) as u8 as char);
        }
        let mut h: i64 = 0;
        for ch in s.bytes() {
            h = (h * 31 + ch as i64) % HMOD;
        }
        h
    });
}
