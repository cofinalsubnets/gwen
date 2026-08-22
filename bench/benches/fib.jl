include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

fib(n) = n < 2 ? n : fib(n - 1) + fib(n - 2)

bench("fib", () -> fib(30))
