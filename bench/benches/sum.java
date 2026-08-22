// build the list 1..100000 then sum it. checksum = 5000050000.
class Main {
    static long work() {
        long[] data = new long[100000];
        for (int i = 0; i < 100000; i++) {
            data[i] = i + 1;
        }
        long s = 0;
        for (long v : data) {
            s += v;
        }
        return s;
    }

    public static void main(String[] a) {
        Bench.bench("sum", Main::work);
    }
}
