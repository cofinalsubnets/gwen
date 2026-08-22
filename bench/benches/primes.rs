include!("../lib/bench.rs");

fn is_prime(n: i64) -> bool {
    let mut d = 2;
    while d * d <= n {
        if n % d == 0 {
            return false;
        }
        d += 1;
    }
    true
}

fn count(lo: i64, hi: i64) -> i64 {
    let mut c = 0;
    for n in lo..hi {
        if is_prime(n) {
            c += 1;
        }
    }
    c
}

fn main() {
    bench("primes", || count(2, 30000));
}
