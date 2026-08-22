include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

# build the array 1..100000 then sum it.
bench("sum", () -> sum(collect(1:100000)))
