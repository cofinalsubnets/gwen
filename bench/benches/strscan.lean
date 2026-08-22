import Bench

-- fold a polynomial rolling hash over a fixed 20000-char string built once,
-- outside the timed loop (see bench/benches/strscan.l) -- the read counterpart to
-- strcat's allocating build. printable bytes 32..126 via (32 + 7*i mod 95).
-- the hash stays < 2^31. checksum = 219660688.
def HMOD : Int := 1000000007

-- built once (a top-level CAF, the "outside the timed loop" part).
def data : String := String.ofList ((List.range 20000).map (fun i => Char.ofNat (32 + (7 * i) % 95)))

-- `z` is the harness's opaque 0, threaded into the hash accumulator (z = 0, so the
-- checksum is unchanged) so the scan is not folded to a constant (lib/Bench.lean).
def main : IO Unit := bench "strscan" (fun z => Id.run do
  let mut h : Int := Int.ofNat z
  for c in data.toList do
    h := (h * 31 + Int.ofNat c.toNat) % HMOD
  return h)
