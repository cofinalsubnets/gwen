// fixed string built once; the timed work is a linear rolling-hash scan.
class Main {
    static final long HMOD = 1000000007L;
    static final String DATA;
    static {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 20000; i++) {
            sb.append((char) (32 + (7 * i) % 95));
        }
        DATA = sb.toString();
    }

    static long work() {
        long h = 0;
        for (int i = 0; i < DATA.length(); i++) {
            h = (h * 31 + DATA.charAt(i)) % HMOD;
        }
        return h;
    }

    public static void main(String[] a) {
        Bench.bench("strscan", Main::work);
    }
}
