// bintrees -- the benchmark-game binary-trees: a GC-throughput / long-lived-
// survival workload. build a stretch tree of depth max+1, hold a long-lived tree
// of depth max alive across the run, then for each depth d in min..max step 2
// build 2^(max-d+min) short-lived trees and sum their node counts. a leaf is null
// and counts 0. checksum = stretch + long-count + the depth sums.
class Main {
    static final class Node {
        final Node l, r;
        Node(Node l, Node r) { this.l = l; this.r = r; }
    }

    static Node mk(int d) {
        return d < 1 ? null : new Node(mk(d - 1), mk(d - 1));
    }

    static long ck(Node t) {
        return t == null ? 0 : 1 + ck(t.l) + ck(t.r);
    }

    static long btRun(int mn, int mx) {
        long stretch = ck(mk(mx + 1));
        Node lng = mk(mx); // LONG-LIVED -- survives the loop below
        long total = 0;
        for (int d = mn; d <= mx; d += 2) {
            long n = 1L << (mx - d + mn);
            long s = 0;
            for (long i = 0; i < n; i++) s += ck(mk(d));
            total += s;
        }
        return stretch + ck(lng) + total;
    }

    public static void main(String[] a) {
        Bench.bench("bintrees", () -> btRun(4, 14));
    }
}
