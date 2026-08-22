package main

const hmodStrscan = 1000000007

// fixed string built once; the timed work is a linear rolling-hash scan.
var strscanData = func() string {
	b := make([]byte, 20000)
	for i := range b {
		b[i] = byte(32 + (7*i)%95)
	}
	return string(b)
}()

func main() {
	bench("strscan", func() int64 {
		var h int64
		for _, ch := range []byte(strscanData) {
			h = (h*31 + int64(ch)) % hmodStrscan
		}
		return h
	})
}
