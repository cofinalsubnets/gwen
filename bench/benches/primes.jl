include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

function isprime(n)
    d = 2
    while d * d <= n
        n % d == 0 && return false
        d += 1
    end
    return true
end

function count_primes(lo, hi)
    c = 0
    for n in lo:hi-1
        isprime(n) && (c += 1)
    end
    return c
end

bench("primes", () -> count_primes(2, 30000))
