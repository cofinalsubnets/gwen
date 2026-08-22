include!("../lib/bench.rs");

fn fib(n: i64) -> i64 {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    // black_box the argument: with a constant arg LLVM evaluates fib(30) at
    // compile time, so work() returns a literal and the timed loop never clears
    // MIN_MS (reps doubles until it overflows -> hang). Opaque input => real run.
    bench("fib", || fib(std::hint::black_box(30)));
}
