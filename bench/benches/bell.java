import java.math.BigInteger;
import java.util.HashMap;

// Bell numbers in base 36 -- a bignum (BigInteger) stress. fresh memo maps per
// rep; checksum = total characters across all rendered lines.
class Main {
    static final String DIGITS = "0123456789abcdefghijklmnopqrstuvwxyz";
    static final BigInteger BASE = BigInteger.valueOf(DIGITS.length());

    static HashMap<Integer, BigInteger> facts;
    static HashMap<Integer, BigInteger> bells;

    static BigInteger fact(int n) {
        BigInteger c = facts.get(n);
        if (c != null) return c;
        BigInteger x = BigInteger.ONE;
        for (int m = n; m > 1; m--) x = x.multiply(BigInteger.valueOf(m));
        facts.put(n, x);
        return x;
    }

    static BigInteger choose(int n, int k) {
        return fact(n).divide(fact(k).multiply(fact(n - k)));
    }

    static BigInteger bell(int n) {
        BigInteger c = bells.get(n);
        if (c != null) return c;
        BigInteger r;
        if (n < 2) r = BigInteger.ONE;
        else {
            r = BigInteger.ZERO;
            for (int k = 0; k < n; k++)
                r = r.add(choose(n - 1, k).multiply(bell(k)));
        }
        bells.put(n, r);
        return r;
    }

    static int show(BigInteger n) {
        int len = 0;
        while (n.signum() > 0) {
            len++;
            n = n.divide(BASE);
        }
        return len;
    }

    static long bellRun(int limit) {
        facts = new HashMap<>();
        bells = new HashMap<>();
        long total = 0;
        for (int i = 0; ; i++) {
            int b = show(bell(i));
            if (b > limit) return total;
            total += b;
        }
    }

    public static void main(String[] a) {
        Bench.bench("bell", () -> bellRun(280));
    }
}
