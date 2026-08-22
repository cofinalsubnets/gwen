include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

# bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
# GC-throughput / long-lived-survival workload. build a stretch tree of depth
# max+1, hold a long-lived tree of depth max alive across the run, then for each
# depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their
# node counts. a leaf is nothing and counts 0.
struct BNode
    l::Union{BNode,Nothing}    # TYPED fields: untyped (::Any) boxes every node and
    r::Union{BNode,Nothing}    # defeats inference -- the classic Julia perf trap.
end

mk(d) = d < 1 ? nothing : BNode(mk(d - 1), mk(d - 1))
ck(t) = t === nothing ? 0 : 1 + ck(t.l) + ck(t.r)

function bt_run(mn, mx)
    stretch = ck(mk(mx + 1))
    long = mk(mx)                       # LONG-LIVED -- survives the loop below
    total = 0
    for d in mn:2:mx
        n = 1 << (mx - d + mn)
        s = 0
        for _ in 1:n
            s += ck(mk(d))
        end
        total += s
    end
    return stretch + ck(long) + total
end

bench("bintrees", () -> bt_run(4, 14))
