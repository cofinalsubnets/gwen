include!("../lib/bench.rs");

// binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^16-1.
// distinct heap-allocated nodes (Box): a malloc + an individual free PER NODE -- the
// honest ephemeral-object churn every GC'd language does here, just with rust's
// malloc/free instead of a collector. (we deliberately do NOT flatten to an index
// arena: that measures array indexing, not allocating/reclaiming distinct nodes.)
// a leaf is None and counts 0.
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

fn main() {
    bench("tree", || ck(&mk(16)));
}
