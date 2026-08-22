package main

// bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
// GC-throughput / long-lived-survival workload. build a stretch tree of depth
// max+1, hold a long-lived tree of depth max alive across the run, then for each
// depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their
// node counts. a leaf counts 0. checksum = stretch + long-count + the depth sums.
type tnode struct {
	left, right *tnode
}

func mk(d int) *tnode {
	if d < 1 {
		return nil
	}
	return &tnode{mk(d - 1), mk(d - 1)}
}

func ck(t *tnode) int64 {
	if t == nil {
		return 0
	}
	return 1 + ck(t.left) + ck(t.right)
}

func btRun(mn, mx int) int64 {
	stretch := ck(mk(mx + 1))
	long := mk(mx) // LONG-LIVED -- survives the loop below
	var total int64
	for d := mn; d <= mx; d += 2 {
		n := int64(1) << uint(mx-d+mn)
		var s int64
		for i := int64(0); i < n; i++ {
			s += ck(mk(d))
		}
		total += s
	}
	return stretch + ck(long) + total
}

func main() {
	bench("bintrees", func() int64 { return btRun(4, 14) })
}
