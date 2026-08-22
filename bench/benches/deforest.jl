include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

# sum of squares of the odd numbers in [0, N) -- a map/filter/fold pipeline.
# checksum = 4891344686 (< 2^63).
deforest_work() = sum((x * x) % 1000003 for x in 0:19999 if x % 2 == 1)

bench("deforest", deforest_work)
