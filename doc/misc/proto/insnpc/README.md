# insnpc -- the exact meter (guest instructions per symbol under qemu-user)

An instrument, not a gate. `insnpc.c` is a qemu TCG plugin: one scoreboard slot
per translation block, inline adds, dumped at exit as `pc n_insns execs` lines
plus `# GUEST_INSNS`. `attr.py` maps the PCs through `nm` (PIE bias from the
`# entry` line) and prints a per-symbol diff of two runs, weighted by
n_insns × execs.

    gcc -O2 -shared -fPIC -o insnpc.so insnpc.c $(pkg-config --cflags glib-2.0)
    LOVE_NO_IMAGE=1 qemu-aarch64 -plugin ./insnpc.so,out=pc.A.out love.A corpus.l </dev/null
    python3 attr.py love.A pc.A.out love.B pc.B.out

The count is deterministic to ~5ppm for ONE binary. Across two binaries the
corpus itself walks differently (address-keyed tables, unsorted `keys`), so a
symbol whose code is byte-identical can still move by 10^9 -- read a per-symbol
delta only beside a mnemonic diff of that symbol, and read the whole-corpus
total as the lottery it is. The LAST text symbol swallows the post-text
islands; PCs below the first symbol print as `?-offset`.
