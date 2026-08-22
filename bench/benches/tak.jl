include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

tak(x, y, z) = y < x ? tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y)) : z

bench("tak", () -> tak(22, 12, 6))
