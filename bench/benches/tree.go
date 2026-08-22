package main

// binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^D-1.
type node struct {
	left, right *node
}

func mk(d int) *node {
	if d == 0 {
		return nil
	}
	return &node{mk(d - 1), mk(d - 1)}
}

func ck(t *node) int64 {
	if t == nil {
		return 0
	}
	return 1 + ck(t.left) + ck(t.right)
}

func main() {
	bench("tree", func() int64 { return ck(mk(16)) })
}
