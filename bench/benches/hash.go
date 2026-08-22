package main

// mutable hash-table throughput (see bench/benches/hash.l). checksum = N*N.
const hashN = 10000

func main() {
	bench("hash", func() int64 {
		h := make(map[int64]int64, hashN)
		for i := int64(0); i < hashN; i++ {
			h[97*i+1] = i
		}
		var a int64
		for i := int64(0); i < hashN; i++ {
			a += h[97*i+1]
		}
		for i := int64(0); i < hashN; i++ {
			k := 97*i + 1
			h[k] = h[k] + 1
		}
		for i := int64(0); i < hashN; i++ {
			a += h[97*i+1]
		}
		return a
	})
}
