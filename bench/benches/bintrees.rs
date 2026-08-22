include!("../lib/bench.rs");

// bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
// GC-throughput / long-lived-survival workload. distinct heap-allocated nodes (Box):
// a malloc + an individual free PER NODE -- the honest object-churn every GC'd language
// does here, with rust's malloc/free instead of a collector. (we deliberately do NOT
// flatten to an index arena: that measures array indexing, not allocating/reclaiming
// distinct nodes -- the very thing this bench tests.) a leaf is None and counts 0.
enum Tree {
    Leaf,
    Node(Box<Tree>, Box<Tree>),
}

fn mk(d: i64) -> Tree {
    if d < 1 { Tree::Leaf } else { Tree::Node(Box::new(mk(d - 1)), Box::new(mk(d - 1))) }
}

fn ck(t: &Tree) -> i64 {
    match t {
        Tree::Leaf => 0,
        Tree::Node(l, r) => 1 + ck(l) + ck(r),
    }
}

fn bt_run(mn: i64, mx: i64) -> i64 {
    let stretch = ck(&mk(mx + 1));
    let long = mk(mx); // LONG-LIVED -- survives the loop below
    let mut total: i64 = 0;
    let mut d = mn;
    while d <= mx {
        let n: i64 = 1 << (mx - d + mn);
        let mut s: i64 = 0;
        for _ in 0..n {
            s += ck(&mk(d));
        }
        total += s;
        d += 2;
    }
    stretch + ck(&long) + total
}

fn main() {
    bench("bintrees", || bt_run(4, 14));
}
