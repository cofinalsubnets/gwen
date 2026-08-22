include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

# binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^D-1.
struct Node
    l
    r
end

mk(d) = d == 0 ? nothing : Node(mk(d - 1), mk(d - 1))
ck(t) = t === nothing ? 0 : 1 + ck(t.l) + ck(t.r)

bench("tree", () -> ck(mk(16)))
