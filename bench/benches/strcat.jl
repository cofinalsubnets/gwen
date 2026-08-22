include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

const STRCAT_HMOD = 1000000007
const STRCAT_N = 4000

# build an N-char string by repeated single-char concatenation, then hash it.
function strcat_work()
    s = ""
    for i in 0:STRCAT_N-1
        s = string(s, Char(48 + i % 10))
    end
    h = 0
    for ch in s
        h = (h * 31 + Int(ch)) % STRCAT_HMOD
    end
    return h
end

bench("strcat", strcat_work)
