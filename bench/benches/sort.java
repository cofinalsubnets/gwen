import java.util.Arrays;

// sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
class Main {
    static final int N = 5000;

    static long work() {
        long x = 1;
        long[] data = new long[N];
        for (int i = 0; i < N; i++) {
            x = (16807 * x) % 2147483647;
            data[i] = x;
        }
        Arrays.sort(data);
        long h = 0;
        for (long v : data) {
            h = (h * 31 + v) % 1000000007;
        }
        return h;
    }

    public static void main(String[] a) {
        Bench.bench("sort", Main::work);
    }
}
