import java.util.HashMap;

// mutable hash-table throughput. checksum = N*N.
class Main {
    static final int N = 10000;

    static long work() {
        HashMap<Integer, Integer> h = new HashMap<>();
        for (int i = 0; i < N; i++) {
            h.put(97 * i + 1, i);
        }
        long acc = 0;
        for (int i = 0; i < N; i++) {
            acc += h.get(97 * i + 1);
        }
        for (int i = 0; i < N; i++) {
            int k = 97 * i + 1;
            h.put(k, h.get(k) + 1);
        }
        for (int i = 0; i < N; i++) {
            acc += h.get(97 * i + 1);
        }
        return acc;
    }

    public static void main(String[] a) {
        Bench.bench("hash", Main::work);
    }
}
