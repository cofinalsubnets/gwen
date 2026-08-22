include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

# mutable hash-table throughput (see bench/benches/hash.l). checksum = N*N.
const HASH_N = 10000

function hash_work()
    h = Dict{Int,Int}()
    for i in 0:HASH_N-1
        h[97 * i + 1] = i
    end
    a = 0
    for i in 0:HASH_N-1
        a += h[97 * i + 1]
    end
    for i in 0:HASH_N-1
        k = 97 * i + 1
        h[k] = h[k] + 1
    end
    for i in 0:HASH_N-1
        a += h[97 * i + 1]
    end
    return a
end

bench("hash", hash_work)
