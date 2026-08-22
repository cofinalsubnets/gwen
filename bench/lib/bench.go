// go benchmark harness -- mirrors bench/bench.l.
// bench(name, work) auto-scales the repetition count (doubling until the run
// clears MIN_MS), then prints one line matching the other harnesses:
//
//	<name> <lang> <reps> <ms> <checksum>
//
// work is a nullary function returning a deterministic checksum. BENCH_LANG
// sets the label, default "go". Compiled together with each benches/<name>.go
// via `go run` -- there is exactly one main (in the bench file).
package main

import (
	"fmt"
	"os"
	"time"
)

const minMs = 200.0

func bench(name string, work func() int64) {
	lang := os.Getenv("BENCH_LANG")
	if lang == "" {
		lang = "go"
	}
	reps := 1
	for {
		t0 := time.Now()
		var chk int64
		for i := 0; i < reps; i++ {
			chk = work()
		}
		ms := float64(time.Since(t0)) / float64(time.Millisecond)
		if ms >= minMs {
			fmt.Printf("%s %s %d %.3f %d\n", name, lang, reps, ms, chk)
			break
		}
		reps *= 2
	}
}
