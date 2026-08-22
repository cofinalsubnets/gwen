// build an N-char string by repeated single-char concatenation, then hash it.
class Main {
    static final long HMOD = 1000000007L;
    static final int N = 4000;

    static long work() {
        String s = "";
        for (int i = 0; i < N; i++) {
            s += (char) (48 + i % 10);
        }
        long h = 0;
        for (int i = 0; i < s.length(); i++) {
            h = (h * 31 + s.charAt(i)) % HMOD;
        }
        return h;
    }

    public static void main(String[] a) {
        Bench.bench("strcat", Main::work);
    }
}
