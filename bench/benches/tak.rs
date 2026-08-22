include!("../lib/bench.rs");

fn tak(x: i64, y: i64, z: i64) -> i64 {
    if y < x {
        tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
    } else {
        z
    }
}

fn main() {
    // black_box the args: with constant args LLVM evaluates tak(22,12,6) at
    // compile time, so work() returns a literal and the timed loop never clears
    // MIN_MS (reps doubles until it overflows -> hang). Opaque inputs => real run.
    bench(
        "tak",
        || tak(std::hint::black_box(22), std::hint::black_box(12), std::hint::black_box(6)),
    );
}
