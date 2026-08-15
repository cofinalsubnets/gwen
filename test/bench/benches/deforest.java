// sum of squares of the odd numbers in [0, N) -- a map/filter/fold pipeline.
// checksum = 4891344686.
class Main {
    static long work() {
        long s = 0;
        for (int x = 0; x < 20000; x++) {
            if (x % 2 == 1) {
                s += ((long) x * x) % 1000003;
            }
        }
        return s;
    }

    public static void main(String[] a) {
        Bench.bench("deforest", Main::work);
    }
}
