// binary-trees allocation/GC stress. checksum = 2^D - 1.
class Main {
    static final class Node {
        final Node l, r;
        Node(Node l, Node r) { this.l = l; this.r = r; }
    }

    static Node mk(int d) {
        return d == 0 ? null : new Node(mk(d - 1), mk(d - 1));
    }

    static long ck(Node t) {
        return t == null ? 0 : 1 + ck(t.l) + ck(t.r);
    }

    public static void main(String[] a) {
        Bench.bench("tree", () -> ck(mk(16)));
    }
}
