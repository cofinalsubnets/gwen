include!("../lib/bench.rs");
fn adder(i: i64) -> impl Fn(i64) -> i64 {
    move |x| x + i
}
fn twice(f: impl Fn(i64) -> i64) -> impl Fn(i64) -> i64 {
    move |x| f(f(x))
}
const N: i64 = 100000;
const M: i64 = 1000000007;
fn main() {
    bench("closure", || {
        let mut acc: i64 = 0;
        for i in 0..N {
            acc = (acc * 31 + twice(adder(i))(i)) % M;
        }
        acc
    });
}
