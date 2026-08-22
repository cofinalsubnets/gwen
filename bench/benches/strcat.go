package main

const hmodStrcat = 1000000007
const strcatN = 4000

// build an N-char string by repeated single-char concatenation, then hash it.
func main() {
	bench("strcat", func() int64 {
		s := ""
		for i := 0; i < strcatN; i++ {
			s += string(rune(48 + i%10))
		}
		var h int64
		for _, ch := range []byte(s) {
			h = (h*31 + int64(ch)) % hmodStrcat
		}
		return h
	})
}
