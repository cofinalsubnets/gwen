#!/bin/sh
# mkhtml.sh <lang-roster> < raw-result-lines > bench.html
#
# Turn the raw "<name> <lang> <reps> <ms> <chk>" lines (the same stream `make
# raw` prints) into a self-contained benchmarks page: one table of per-iteration
# milliseconds (benches down the side, languages across), and a button that
# transposes benches<->languages (handy on a narrow portrait screen).
# <lang-roster> is the column SET (e.g. $(ALL_LANGS)). The page (in JS, at render)
# orders EVERY column by NET ascending -- the GEOMETRIC MEAN of a language's per-bench
# ms/it (magnitude-robust, so each bench counts in proportion and no heavy one dominates;
# bintrees ranks, bell/cdcl/setup don't -- see NET). love takes its HONEST position (still
# tinted gold); a language with no rows sorts last.
# The page embeds its data, so it opens straight off disk -- no server.
roster="$1"

cat <<'HEAD'
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>love benchmarks</title>
<style>
  /* tokyo-night, matching the site (style.css): periwinkle on polar-night blue,
     the self-hosted DOS/V bitmap font, green = a kept (fastest) answer. */
  @font-face {
    font-family: "Web437 DOS/V TWN16";
    src: url("../assets/fonts/Web437_DOS-V_TWN16.woff2") format("woff2"),
         url("../assets/fonts/Web437_DOS-V_TWN16.woff") format("woff");
    font-display: swap;
  }
  :root { color-scheme: dark; }
  * { box-sizing: border-box; }
  body { background: #04060e; color: #a9b1d6;
         font-family: "Web437 DOS/V TWN16", "Px437 IBM VGA8", "DejaVu Sans Mono",
                      ui-monospace, Menlo, Consolas, monospace;
         font-size: 16px; line-height: 1.5; font-variant-ligatures: none;
         -webkit-font-smoothing: none; font-smooth: never;
         margin: 0; padding: 1.6em 1.2em 4em; }
  h1 { font-size: 1.5em; font-weight: normal; letter-spacing: .04em; color: #c0caf5; margin: 0 0 .3em; }
  .note { color: #565f89; font-size: .9em; max-width: 64ch; margin: .3em 0 1.2em; }
  .bar { margin: 0 0 1em; display: flex; gap: .75rem; align-items: baseline; }
  button { font: inherit; padding: .35em .85em; cursor: pointer; color: #a9b1d6;
           border: 1px solid #3b4261; border-radius: 4px; background: #1f2335; }
  button:hover { background: #292e42; }
  #mode { color: #565f89; font-size: .9em; }
  .wrap { overflow: auto; max-height: 86vh; border: 1px solid #292e42; border-radius: 6px; }
  table { border-collapse: collapse; font-variant-numeric: tabular-nums; }
  th, td { padding: .28em .7em; text-align: right; border: 1px solid #292e42; white-space: nowrap; }
  thead th { position: sticky; top: 0; background: #1f2335; color: #737aa2; font-weight: normal;
             box-shadow: inset 0 -1px #3b4261; }
  tbody th { text-align: left; position: sticky; left: 0; background: #1f2335; color: #9aa5ce;
             box-shadow: inset -1px 0 #3b4261; font-weight: normal; }
  td.miss { color: #3b4261; }
  td.fast { color: #9ece6a; font-weight: bold; }              /* a kept answer: the green */
  .love { background: rgba(169,177,214,.09) !important; }        /* the love axis: translucent periwinkle */
  th.love { background: rgba(169,177,214,.20) !important; color: #c0caf5 !important; }
  .netrow th, .netrow td { border-top: 2px solid #545c7e; color: #e0af68; }   /* geomean: geometric-mean ms/it of the fundamentals -- the column-ordering key */
  .extra th, .extra td { color: #737aa2; }                                    /* below the geomean: bell (bignum) + setup (cold start) -- shown, not ranked */
  th.net, td.net { border-left: 2px solid #545c7e; color: #e0af68; }          /* ... as a column in the transposed view */
  td.to { color: #f7768e; }                                                   /* a solver that exceeded the timeout */
  h2 { font-weight: normal; color: #c0caf5; margin: 1.6em 0 .4em; }
  .topnav { font-size: .85em; margin: 0 0 1.2em; }
  .topnav a { color: #7aa2f7; text-decoration: none; }
  .topnav a:hover { text-decoration: underline; }
  .topnav .here { color: #a9b1d6; }
</style>
</head>
<body>
<nav class="topnav"><a href="../index.html">love</a> &nbsp;&middot;&nbsp; <a href="../language.html">language</a> &nbsp;&middot;&nbsp; <a href="../index.html#glossary">glossary</a> &nbsp;&middot;&nbsp; <a href="../crew/index.html">crew</a> &nbsp;&middot;&nbsp; <span class="here">bench</span></nav>
<h1>love benchmarks &mdash; milliseconds per iteration</h1>
<p class="note">Lower is better. Each language self-times its inner loop (reps
auto-scaled past a 200&nbsp;ms floor, so startup is excluded). The fastest cell
per bench is <b style="color:#9ece6a">green</b>; the <span class="love"
style="padding:0 .3em">love</span> axis is tinted; a dot means no implementation
(or an unavailable toolchain). The ranking row orders the columns by the metric you pick above
&mdash; default the <b>geometric mean</b> of each language's per-bench times, so every bench counts in
proportion and no single heavy one dominates (a language is fast at some things, slow at others; try
<b>harmonic</b> or <b>arithmetic</b> and watch the order shift). Below it, shown but
not ranked: <b>bell</b> (bignum &mdash; not every language has it) and <b>setup</b> (cold start:
source &rarr; trivial result, so compiled languages pay their compile).</p>
<div class="bar">
  <button id="btn" type="button">transpose</button>
  <span id="mode"></span>
  <span class="rank">rank by:
    <label><input type="radio" name="metric" value="harmonic">harmonic</label>
    <label><input type="radio" name="metric" value="geometric" checked>geometric</label>
    <label><input type="radio" name="metric" value="arithmetic">arithmetic</label>
  </span>
</div>
<div class="wrap"><div id="t"></div></div>
HEAD

# -- the SECOND table: the SAT-solver shootout (arg 2 = a satrace.sh result file,
# "<instance> <solver> <ms> <verdict>" lines). Rendered STATIC at generation time
# (no transpose needed), solvers ordered by net ms ascending -- love its honest
# position, a timeout ranking it last. Skipped if no file. --
if [ -n "$2" ] && [ -s "$2" ]; then
cat <<'SAT'
<h2>SAT solvers &mdash; milliseconds to solve</h2>
<p class="note">A separate field: love&rsquo;s own CDCL solver (<code>crew/sat/flat.l</code>:
flat cask-resident state driven by four native kernels &mdash; propagation, the whole
conflict handler, the decision, and the <code>fbva</code> grow step &mdash; each assembled
through <code>crew/holo/</code> at solver-build time, specialized to the instance size)
against reference C solvers.
Two row families: PHP(<i>n</i>) &mdash; (<i>n</i>+1) pigeons into <i>n</i> holes, UNSAT
and resolution-hard, where clause learning alone is <b>exponential</b> and love&rsquo;s
<code>fbva</code> factoring pass (extended resolution) earns its keep &mdash; and
rnd<i>n</i>, random 3-SAT at the threshold (<i>m</i> = 4.26<i>n</i>, five fixed-seed
instances summed; the verdict column is the per-instance SAT/UNSAT signature, identical
across every solver): raw search with no factorable structure, the guard against
pigeonhole specialization &mdash; and REAL instances from SATLIB, the classic
competition-era benchmark library (uf/uuf = uniform random at the transition,
satisfiable and proven-UNSATISFIABLE sets; flat = graph 3-coloring), byte-identical
files raced by every solver. love is timed by its own solve clock (interpreter warmup and
the one-time kernel assembly excluded); the C solvers by process wall-clock (their
startup is ~ms). <code>timeout</code> = exceeded the cutoff. The families pull opposite
ways: the big inprocessing solvers (cadical, kissat) are built for structure but their
machinery costs them the small random instances, where the light classics (picosat,
minisat) lead &mdash; love runs ahead of cadical on PHP(5&ndash;7) and mid-field on the
pure random rows; the threshold-SAT rows are a documented per-instance lottery
(uf250 races a 14-instance panel to damp it). The net is the geometric mean over
rows &mdash; the same key the language table uses &mdash; so no single row owns
the column order.</p>
<div class="wrap">
SAT
awk '
  function fmt(x){ return x<1?sprintf("%.4f",x):x<100?sprintf("%.3f",x):sprintf("%.1f",x) }
  { inst=$1; solv=$2; ms=$3
    if(!(inst in si)){iord[++ni]=inst; si[inst]=1}
    if(!(solv in ss)){sord[++ns]=solv; ss[solv]=1}
    val[inst,solv]=ms }
  END{
    for(j=1;j<=ns;j++){s=sord[j]; lg=0; n=0; to=0; for(i=1;i<=ni;i++){v=val[iord[i],s]; if(v=="timeout"){to=1; lg+=log(1e9); n++} else if(v!=""){x=v+0; if(x<0.001)x=0.001; lg+=log(x); n++}} netv[s]=(n?exp(lg/n):1e18); anyto[s]=to; ord[j]=s}
    for(a=1;a<=ns;a++)for(b=a+1;b<=ns;b++)if(netv[ord[b]]<netv[ord[a]]){t=ord[a];ord[a]=ord[b];ord[b]=t}
    for(i=1;i<=ni;i++){m=1e18; for(j=1;j<=ns;j++){v=val[iord[i],ord[j]]; if(v!="timeout"&&v!=""&&v+0<m)m=v+0} minr[iord[i]]=m}
    printf "<table><thead><tr><th>instance</th>"
    for(j=1;j<=ns;j++){s=ord[j]; printf "<th%s>%s</th>", (s=="love"?" class=\047ai\047":""), s}
    print "</tr></thead><tbody>"
    for(i=1;i<=ni;i++){inst=iord[i]; printf "<tr><th>%s</th>", inst
      for(j=1;j<=ns;j++){s=ord[j]; v=val[inst,s]; c=(s=="love"?"love ":"")
        if(v==""){printf "<td class=\047%smiss\047>·</td>",c}
        else if(v=="timeout"){printf "<td class=\047%sto\047>timeout</td>",c}
        else{f=(v+0==minr[inst]?"fast ":""); printf "<td class=\047%s%s\047>%s</td>",c,f,fmt(v+0)}}
      print "</tr>"}
    printf "<tr class=\047netrow\047><th>net</th>"
    for(j=1;j<=ns;j++){s=ord[j]; printf "<td class=\047net%s\047>%s</td>", (s=="love"?" love":""), (anyto[s]?"dnf":fmt(netv[s]))}
    print "</tr></tbody></table>"
  }' "$2"
echo "</div>"
fi

# -- the THIRD extra table: the COMPILER shootout (arg 3 = a ccbench.sh result file,
# "<phase> <compiler> <ms> <note>" lines). Rows build+test, columns the compilers,
# ordered by net = build+test (source to a tested binary), mooncc its honest position.
# Rendered STATIC (like the SAT table); a dnf lane ranks last. Skipped if no file. --
if [ -n "$3" ] && [ -s "$3" ]; then
cat <<'CC'
<h2>compilers &mdash; milliseconds to build love, and to test it</h2>
<p class="note">A third field, love&rsquo;s C toolchain against the incumbents:
<b>mooncc</b> is love&rsquo;s own C compiler (<code>crew/moon/</code>), and it builds
<code>love</code> with <i>no gcc, glibc, or ld</i> &mdash; mooncc lays every object,
<code>mksys</code> emits the syscall leaf, and our own linker (<code>crew/holo/</code>)
binds the executable. The <b>build</b> row is the wall-clock to compile every C
translation unit (<code>core/love.c</code> + <code>host/*.c</code> + the <code>am</code> math
floor) and link a working binary; the <b>test</b> row runs the full corpus (the same
files <code>test_host</code>/<code>test_raw</code> feed) through the binary that build
produced. All three lanes egg-boot (no baked image), so the corpus runs off the freshly
eval&rsquo;d egg either way &mdash; a level field, so the test row also reflects each
binary&rsquo;s own speed (it eval&rsquo;s its compiler out of the egg before the first
test runs). gcc and clang build the identical units at the host&rsquo;s real
<code>-O2</code> flags (minus <code>-Werror</code>, a lint gate, not a speed factor),
against <b>musl</b> and linked <code>-static</code> &mdash; mooncc emits a static
binary carrying its own libc, so the natives are held to the same shape rather than
leaving glibc off the ledger. Which libc does not move the timings (under 0.1%): love
allocates, formats and copies through its own floor, so libc barely runs.
mooncc trades some compile and run throughput for that self-sufficiency; the gap to the
optimizing compilers is modest, and the binary it emits passes the identical corpus.</p>
<p class="note">The last two rows are single C functions rather than the whole corpus
(<code>bench/ccrypto.l</code> drives the <code>chacha20</code> and <code>poly1305</code>
nifs in <code>host/tls.c</code>), and they are here because an average can hide a
lopsided one. <b>chacha</b> indexes a sixteen-word state <i>array</i> in its inner loop;
<b>poly1305</b> keeps its five limbs as scalar <i>locals</i>. mooncc gives a register
home to the second shape and not the first, so the two rows are a gauge: wide chacha
beside narrow poly says the remaining gap is array slots, and the day they close
together is the day that reading was wrong.</p>
<div class="wrap">
CC
awk '
  function fmt(x){ return x<1?sprintf("%.4f",x):x<100?sprintf("%.3f",x):sprintf("%.1f",x) }
  { ph=$1; cc=$2; ms=$3
    if(!(ph in pi)){pord[++np]=ph; pi[ph]=1}
    if(!(cc in ci)){cord[++nc]=cc; ci[cc]=1}
    val[ph,cc]=ms }
  END{
    for(j=1;j<=nc;j++){c=cord[j]; net=0; dn=0; for(i=1;i<=np;i++){v=val[pord[i],c]; if(v=="dnf"||v==""){dn=1; net+=1e9}else net+=v+0} netv[c]=net; anydn[c]=dn; ord[j]=c}
    for(a=1;a<=nc;a++)for(b=a+1;b<=nc;b++)if(netv[ord[b]]<netv[ord[a]]){t=ord[a];ord[a]=ord[b];ord[b]=t}
    for(i=1;i<=np;i++){m=1e18; for(j=1;j<=nc;j++){v=val[pord[i],ord[j]]; if(v!="dnf"&&v!=""&&v+0<m)m=v+0} minr[pord[i]]=m}
    printf "<table><thead><tr><th>phase</th>"
    for(j=1;j<=nc;j++){c=ord[j]; printf "<th%s>%s</th>", (c=="mooncc"?" class=\047ai\047":""), c}
    print "</tr></thead><tbody>"
    for(i=1;i<=np;i++){ph=pord[i]; printf "<tr><th>%s</th>", ph
      for(j=1;j<=nc;j++){c=ord[j]; v=val[ph,c]; cl=(c=="mooncc"?"love ":"")
        if(v==""){printf "<td class=\047%smiss\047>·</td>",cl}
        else if(v=="dnf"){printf "<td class=\047%sto\047>dnf</td>",cl}
        else{f=(v+0==minr[ph]?"fast ":""); printf "<td class=\047%s%s\047>%s</td>",cl,f,fmt(v+0)}}
      print "</tr>"}
    printf "<tr class=\047netrow\047><th>net</th>"
    for(j=1;j<=nc;j++){c=ord[j]; printf "<td class=\047net%s\047>%s</td>", (c=="mooncc"?" love":""), (anydn[c]?"dnf":fmt(netv[c]))}
    print "</tr></tbody></table>"
  }' "$3"
echo "</div>"
fi

echo "<script>"

awk -v langs="$roster" '
  BEGIN { nl = split(langs, L, " ") }
  NF==5 && $3+0>0 {
    if (!($1 in seen)) { order[++n] = $1; seen[$1] = 1 }
    per[$1 SUBSEP $2] = $4/$3
    have[$1 SUBSEP $2] = 1
  }
  END {
    printf "const LANGS=["
    for (j=1;j<=nl;j++) printf "%s\"%s\"", (j>1?",":""), L[j]
    print "];"
    printf "const BENCHES=["
    for (i=1;i<=n;i++) printf "%s\"%s\"", (i>1?",":""), order[i]
    print "];"
    print "const PER={};"
    for (i=1;i<=n;i++) {
      b = order[i]; printf "PER[\"%s\"]={", b; first=1
      for (j=1;j<=nl;j++) {
        lg = L[j]
        if (have[b SUBSEP lg]) {
          printf "%s\"%s\":%.4f", (first?"":","), lg, per[b SUBSEP lg]; first=0
        }
      }
      print "};"
    }
  }
'

cat <<'TAIL'
const fmt = x => x == null ? "·"
  : x < 1 ? x.toFixed(4) : x < 100 ? x.toFixed(3) : x.toFixed(1);

// column order: every language (love included -- its HONEST position) by NET time
// ascending by the net (the GEOMETRIC MEAN of a language's per-bench ms/it; see NET below).
// love is still TINTED (the gold column) wherever it lands. No-data langs sort last.
// NORANK benches are kept OUT of the net, each for a reason geomean can't fix: bell (bignum --
// luajit/rust lack it entirely, so it's not a fair cross-language axis), cdcl (love-only; its perf
// lives in the SAT-solver table), setup (a one-time cold-start cost, not a per-iteration time).
// bell + setup still SHOW as rows below the net; cdcl is dropped from this table. bintrees DOES
// rank now -- the geomean lets the GC/alloc axis count proportionally without a heavy bench
// dominating, and every language allocates distinct nodes (no flatten-to-index optimization), so
// it's apples-to-apples. (mandelbrot is the net's float-grid bench -- it REPLACED the smaller
// plain-f64 `float`, the same escape-grid workload: its idiomatic complex ~(re im) recurrence lowers
// to the real-pair float-grid kernel (twolow) and glazes to native.)
const NORANK = b => b === "bell" || b === "cdcl" || b === "setup";
// RANK(l): the ranking aggregate of a language's per-bench ms/it (lower = faster), chosen by
// the radio. GEOMETRIC is the default + the fair one: magnitude-robust, every bench counts in
// PROPORTION (a 2x on a 0.3 ms fundamental and a 2x on a 30 ms bintrees move it equally), so no
// heavy bench dominates and a closed-form O(1) win helps by its ratio. HARMONIC rewards being
// fast SOMEWHERE (the small values dominate); ARITHMETIC (the mean) lets a heavy bench dominate.
// (A plain SUM is omitted: it is just the mean times the bench count -- the same order.) The
// radio shows how it shifts: love leads geometric/harmonic (the loop-closer), trails arithmetic.
// A bench timed below the clock's resolution records as 0.0000 ms/it (rust's no-op closure);
// 1/0 and log(0) would blow up the harmonic/geometric means, so floor at the display res.
const GEOFLOOR = 1e-4;
let metric = "geometric";
const vals = l => BENCHES.filter(b => !NORANK(b) && PER[b] && PER[b][l] != null).map(b => Math.max(PER[b][l], GEOFLOOR));
const RANK = l => { const xs = vals(l), n = xs.length; if (!n) return Infinity;
  return metric === "harmonic"   ? n / xs.reduce((s, x) => s + 1 / x, 0)
       : metric === "arithmetic" ? xs.reduce((s, x) => s + x, 0) / n
       :                           Math.exp(xs.reduce((s, x) => s + Math.log(x), 0) / n); };
const HAS = l => BENCHES.some(b => !NORANK(b) && PER[b] && PER[b][l] != null);
// the fundamentals (ranked, the main block) and the informational rows shown BELOW the net.
const FUND  = BENCHES.filter(b => !NORANK(b));
const EXTRA = ["bell", "setup"].filter(b => BENCHES.includes(b));   // below the net, not ranked; bintrees now ranks (geomean); cdcl is dropped from the table entirely

// default arrangement: benches down the side, languages across; the button
// flips to languages-down-the-side (handy on a narrow portrait screen).
let transposed = false;

function build() {
  const langs = LANGS.slice().sort((a, b) => (HAS(a) ? RANK(a) : Infinity) - (HAS(b) ? RANK(b) : Infinity));
  const minB = {};
  for (const b of BENCHES) {
    let m = Infinity;
    for (const l of langs) { const v = PER[b] && PER[b][l]; if (v != null && v < m) m = v; }
    minB[b] = m;
  }
  const td = (b, l) => {
    const v = (PER[b] && PER[b][l] != null) ? PER[b][l] : null;
    const c = [];
    if (l === "love") c.push("love");
    if (v == null) c.push("miss"); else if (v === minB[b]) c.push("fast");
    return "<td" + (c.length ? " class='" + c.join(" ") + "'" : "") + ">" + fmt(v) + "</td>";
  };
  const netc = (l) => "<td class='net" + (l === "love" ? " love" : "") + "'>" + (HAS(l) ? fmt(RANK(l)) : "·") + "</td>";
  let h = "<table><thead><tr><th>" + (transposed ? "language" : "bench") + "</th>";
  if (transposed) {                                                          // languages down the rows: FUND benches, then net, then the EXTRA benches, as COLUMNS
    for (const b of FUND) h += "<th>" + b + "</th>";
    h += "<th class='net'>" + metric + "</th>";
    for (const b of EXTRA) h += "<th>" + b + "</th>";
    h += "</tr></thead><tbody>";
    for (const l of langs) {
      h += "<tr><th" + (l === "love" ? " class='love'" : "") + ">" + l + "</th>";
      for (const b of FUND) h += td(b, l);
      h += netc(l);
      for (const b of EXTRA) h += td(b, l);
      h += "</tr>";
    }
  } else {                                                                   // benches down the rows: FUND rows, the net row, then the EXTRA rows (bell, setup) BELOW the net
    for (const l of langs) h += "<th" + (l === "love" ? " class='love'" : "") + ">" + l + "</th>";
    h += "</tr></thead><tbody>";
    for (const b of FUND) { h += "<tr><th>" + b + "</th>"; for (const l of langs) h += td(b, l); h += "</tr>"; }
    h += "<tr class='netrow'><th>" + metric + "</th>";
    for (const l of langs) h += netc(l);
    h += "</tr>";
    for (const b of EXTRA) { h += "<tr class='extra'><th>" + b + "</th>"; for (const l of langs) h += td(b, l); h += "</tr>"; }
  }
  h += "</tbody></table>";
  document.getElementById("t").innerHTML = h;
  document.getElementById("mode").textContent =
    transposed ? "languages × benches" : "benches × languages";
}
document.getElementById("btn").onclick = () => { transposed = !transposed; build(); };
document.querySelectorAll("input[name=metric]").forEach(r =>
  r.onchange = (e) => { metric = e.target.value; build(); });
build();
</script>
</body>
</html>
TAIL
