include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

# Bell numbers in base 36 (see bench/benches/bell.l). Fresh memo dicts per rep.
# checksum = total characters across all rendered lines. Needs bignums (BigInt).
const BELL_DIGITS = "0123456789abcdefghijklmnopqrstuvwxyz"

function bell_run(limit)
    facts = Dict{Int,BigInt}()
    bells = Dict{Int,BigInt}()

    function fact(n)
        haskey(facts, n) && return facts[n]
        x = BigInt(1)
        m = n
        while m > 1
            x *= m
            m -= 1
        end
        facts[n] = x
        return x
    end

    choose(n, k) = fact(n) ÷ (fact(k) * fact(n - k))

    function bell(n)
        haskey(bells, n) && return bells[n]
        r = n < 2 ? BigInt(1) : sum(choose(n - 1, k) * bell(k) for k in 0:n-1)
        bells[n] = r
        return r
    end

    function show36(n)
        s = ""
        while n > 0
            s = string(BELL_DIGITS[Int(n % 36)+1]) * s
            n = n ÷ 36
        end
        return s
    end

    total = 0
    i = 0
    while true
        b = show36(bell(i))
        if length(b) > limit
            return total
        end
        total += length(b)
        i += 1
    end
end

bench("bell", () -> bell_run(280))
