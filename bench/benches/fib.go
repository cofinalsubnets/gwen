package main

func fib(n int64) int64 {
	if n < 2 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	bench("fib", func() int64 { return fib(30) })
}
