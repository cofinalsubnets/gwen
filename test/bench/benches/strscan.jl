include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

const STRSCAN_HMOD = 1000000007

# fixed string built once; the timed work is a linear rolling-hash scan.
const STRSCAN_DATA = String(Char[Char(32 + (7 * i) % 95) for i in 0:19999])

function strscan_work()
    h = 0
    for ch in STRSCAN_DATA
        h = (h * 31 + Int(ch)) % STRSCAN_HMOD
    end
    return h
end

bench("strscan", strscan_work)
