package main

// sum of squares of the odd numbers in [0, N) -- a map/filter/fold pipeline.
// the idiomatic Go chain materializes intermediate slices each call; the
// per-element lambda overhead is the point. checksum = 4891344686.
func main() {
	bench("deforest", func() int64 {
		odd := func(x int64) bool { return x%2 == 1 }
		sq := func(x int64) int64 { return (x * x) % 1000003 }
		var s int64
		filtered := make([]int64, 0, 20000)
		for i := int64(0); i < 20000; i++ {
			if odd(i) {
				filtered = append(filtered, i)
			}
		}
		mapped := make([]int64, len(filtered))
		for i, v := range filtered {
			mapped[i] = sq(v)
		}
		for _, v := range mapped {
			s += v
		}
		return s
	})
}
