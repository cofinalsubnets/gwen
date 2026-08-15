package main

func isPrime(n int64) bool {
	for d := int64(2); d*d <= n; d++ {
		if n%d == 0 {
			return false
		}
	}
	return true
}

func count(lo, hi int64) int64 {
	var c int64
	for n := lo; n < hi; n++ {
		if isPrime(n) {
			c++
		}
	}
	return c
}

func main() {
	bench("primes", func() int64 { return count(2, 30000) })
}
