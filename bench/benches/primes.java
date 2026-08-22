// count primes in [2, 30000) by trial division. checksum = 3245.
class Main {
    static boolean isPrime(int n) {
        int d = 2;
        while (d * d <= n) {
            if (n % d == 0) return false;
            d += 1;
        }
        return true;
    }

    static long count(int lo, int hi) {
        long c = 0;
        for (int n = lo; n < hi; n++) {
            if (isPrime(n)) c += 1;
        }
        return c;
    }

    public static void main(String[] a) {
        Bench.bench("primes", () -> count(2, 30000));
    }
}
