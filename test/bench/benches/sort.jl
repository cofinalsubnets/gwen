include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

# sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
const SORT_N = 5000

function sort_work()
    x = 1
    data = Vector{Int}(undef, SORT_N)
    for i in 1:SORT_N
        x = (16807 * x) % 2147483647
        data[i] = x
    end
    sort!(data)
    h = 0
    for v in data
        h = (h * 31 + v) % 1000000007
    end
    return h
end

bench("sort", sort_work)
