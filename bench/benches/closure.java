import java.util.function.LongUnaryOperator;
class Main {
    static final int N = 100000;
    static final long M = 1000000007L;
    static LongUnaryOperator twice(LongUnaryOperator f) {
        return x -> f.applyAsLong(f.applyAsLong(x));
    }
    static LongUnaryOperator adder(long i) {
        return x -> x + i;
    }
    static long work() {
        long acc = 0;
        for (int i = 0; i < N; i++) acc = (acc * 31 + twice(adder(i)).applyAsLong(i)) % M;
        return acc;
    }
    public static void main(String[] a) {
        Bench.bench("closure", Main::work);
    }
}
