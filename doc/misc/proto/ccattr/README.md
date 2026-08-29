# ccattr -- the corpus gap, per symbol

An instrument, not a gate. The gauge's target row is the corpus under the
mooncc-built love against the gcc/clang-built one, and every codegen lever is
priced by static counts or kernel rows; this reads the target row itself, per
function, so a lever can be chosen by what the workload actually loses on.

    make -C bench ccbench            # leaves out/bench/cc/love-{mooncc,gcc-musl,clang-musl}
    cat test/00-init.l test/spec.l test/uu.l $(ls test/*.l | grep -vE '/(00-init|spec|glaze-x86|uu)\.l$' | LC_ALL=C sort) > corpus.l
    for k in 1 2 3; do for w in mooncc gcc-musl; do
      LOVE_NO_IMAGE=1 perf record -q -F 4000 -e cycles:u -o perf.$w.$k out/bench/cc/love-$w < corpus.l >/dev/null 2>&1
    done; done
    for w in mooncc gcc-musl; do for k in 1 2 3; do
      perf report -i perf.$w.$k --stdio --no-children --sort symbol -F sample,symbol | grep -v '^#' >> sym.$w.txt
    done; done
    python3 ccattr.py sym.mooncc.txt sym.gcc-musl.txt

Interleave the runs (same-run law). gcc inlines statics out of existence, so
check the "only" line: at ~1% the per-symbol ratios are sound. Then
`perf annotate -s <sym>` on both and a side-by-side disassembly name the
shape. Findings in moon-gauge.md ("where the corpus gap sits").
