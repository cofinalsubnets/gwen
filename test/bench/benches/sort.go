package main

import "sort"

// sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
const sortN = 5000

func main() {
	bench("sort", func() int64 {
		x := int64(1)
		data := make([]int64, 0, sortN)
		for i := 0; i < sortN; i++ {
			x = (16807 * x) % 2147483647
			data = append(data, x)
		}
		sort.Slice(data, func(i, j int) bool { return data[i] < data[j] })
		var h int64
		for _, v := range data {
			h = (h*31 + v) % 1000000007
		}
		return h
	})
}
