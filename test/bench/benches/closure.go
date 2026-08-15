package main

func twice(f func(int64) int64) func(int64) int64 {
	return func(x int64) int64 { return f(f(x)) }
}
func adder(i int64) func(int64) int64 {
	return func(x int64) int64 { return x + i }
}

const closureN = 100000
const closureM = 1000000007

func main() {
	bench("closure", func() int64 {
		var acc int64
		for i := int64(0); i < closureN; i++ {
			acc = (acc*31 + twice(adder(i))(i)) % closureM
		}
		return acc
	})
}
