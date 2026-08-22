package main

// build the list 1..100000 then sum it.
func main() {
	bench("sum", func() int64 {
		data := make([]int64, 0, 100000)
		for i := int64(1); i <= 100000; i++ {
			data = append(data, i)
		}
		var s int64
		for _, v := range data {
			s += v
		}
		return s
	})
}
