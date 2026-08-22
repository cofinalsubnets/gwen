include(joinpath(@__DIR__, "..", "lib", "bench.jl"))
# twice(adder(i))(i) = 3i; a rolling hash so the inlined loop is real work, not folded to a formula.
twice(f) = x -> f(f(x))
adder(i) = x -> x + i
function closure_work()
    acc = 0
    for i in 0:99999
        acc = (acc * 31 + twice(adder(i))(i)) % 1000000007
    end
    return acc
end
bench("closure", closure_work)
