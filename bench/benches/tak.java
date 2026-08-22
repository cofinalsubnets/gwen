class Main {
    static long tak(long x, long y, long z) {
        if (y < x) {
            return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
        }
        return z;
    }

    public static void main(String[] a) {
        Bench.bench("tak", () -> tak(22, 12, 6));
    }
}
