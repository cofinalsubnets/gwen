package main

import "math/big"

// Bell numbers in base 36 (see bench/benches/bell.l). Fresh memo maps per rep.
// checksum = total characters across all rendered lines.
const digits = "0123456789abcdefghijklmnopqrstuvwxyz"

func bellRun(limit int) int64 {
	base := big.NewInt(int64(len(digits)))
	facts := map[int]*big.Int{}
	bells := map[int]*big.Int{}

	var fact func(n int) *big.Int
	fact = func(n int) *big.Int {
		if v, ok := facts[n]; ok {
			return v
		}
		x := big.NewInt(1)
		for m := n; m > 1; m-- {
			x.Mul(x, big.NewInt(int64(m)))
		}
		facts[n] = x
		return x
	}

	choose := func(n, k int) *big.Int {
		denom := new(big.Int).Mul(fact(k), fact(n-k))
		return new(big.Int).Quo(fact(n), denom)
	}

	var bell func(n int) *big.Int
	bell = func(n int) *big.Int {
		if v, ok := bells[n]; ok {
			return v
		}
		r := big.NewInt(1)
		if n >= 2 {
			r = big.NewInt(0)
			for k := 0; k < n; k++ {
				r.Add(r, new(big.Int).Mul(choose(n-1, k), bell(k)))
			}
		}
		bells[n] = r
		return r
	}

	show := func(n *big.Int) int {
		// number of base-36 digits in n (n > 0 always here for i >= 1; bell(0)=1)
		m := new(big.Int).Set(n)
		mod := new(big.Int)
		s := 0
		for m.Sign() > 0 {
			m.QuoRem(m, base, mod)
			s++
		}
		return s
	}

	var total int64
	for i := 0; ; i++ {
		bl := show(bell(i))
		if bl > limit {
			return total
		}
		total += int64(bl)
	}
}

func main() {
	bench("bell", func() int64 { return bellRun(280) })
}
