const gwen_eval = (() => {
  const { isArray } = Array,
    pmap = (f, p) => (s, y, n) => p(s, (s, x) => y(s, f(x)), n),
    drop = a => (s, y, n) => a(s, (s, _) => y(s, []), n),
    re = r => (s, y, n) => (m => m ? y(s.slice(m[0].length), [m[0]]) : n(s))(s.match(r)),
    lit = l => (s, y, n) => s.startsWith(l) ? y(s.slice(l.length), [l]) : n(s),
    alt = (a, b) => (s, y, n) => a(s, y, _ => b(s, y, n)),
    cat = (a, b) => (s, y, n) => a(s, (s, p1) => b(s, (s, p2) => y(s, p1.concat(p2)), n), n),
    opt = (a) => (s, y) => a(s, y, _ => y(s, [])),
    ws = drop(re(/^([ \t\n]*|;[^\r]*)*/)), // whitespace
    exprs = (s, y, n) => cat(opt(ws), cat(expr, opt(exprs)))(s, y, n),
    list = pmap(x => [x], cat(drop(lit("(")), cat(exprs, drop(lit(")"))))),
    atom = pmap(([s])=>[(i=>''+i==s?i:intern(s))(parseFloat((s)))], re(/^[^ \r\n\t()'"]+/)), 
    expr = alt(atom, list), // one expression
    gwen_parse = (s) => expr(s, (_, x) => x[0], e => console.error('parse error', e));

  class Symbol { constructor(nom) { this.nom = nom; } }
  const intern = (sym => s => (y => y ? y : (sym[s] = new Symbol(s)))(sym[s]))({}),
        [Quote, Lambda, Define, Cond, Begin] = ['`', '\\', ':', '?', ','].map(intern);
  const ev = x => l => {
    if (x instanceof Symbol) return l(x);
    if (!isArray(x)) return x;
    if (x.length == 0) return 0;
    const [x0, ...a] = x;
    // examine the first item to see if it's a special form
    if (x0 == Quote) return a[0];
    if (x0 == Begin) return a.reduce((_, x) => ev(x)(l), 0);
    if (x0 == Lambda) return a.slice(0, -1).reduceRight(
      (f, arg) => l => x => f(y => y === arg ? x : l(y)),
      ev(a[a.length-1])
    )(l);

    if (x0 == Cond) for (let i = 0;; i += 2) {
      if (i == a.length) return 0; // no default case => 0
      if (i == a.length - 1) return ev(a[i])(l); // default case
      if (ev(a[i])(l)) return ev(a[i + 1])(l); // conditional branch
    }

    if (x0 == Define) for (let i = 0, bs = () => 0;; i += 2) {
      if (i == a.length) a.push(0); // no inner expression => 0
      // last expression, call bindings and eval
      if (i == a.length - 1) return bs(l), ev(a[i])(l);
      // key, value, closure binding, current store
      let k = a[i], v = a[i+1], cb, l0 = l, b0 = bs;
      // desugar (: (f a b) (g a b)) to (: f (\ a b (g a b)))
      while (isArray(k)) v = [Lambda, ...k.slice(1), v], k = k[0];
      l = x => x === k ? cb : l0(x); // update store
      bs = l => (b0(l), (cb = ev(v)(l)));
    }

    // map eval fold apply
    return x.map(x => ev(x)(l)).reduce(
      (f, x) => typeof(f) === 'function' ? f(x) : f
    );
  };

  const
    G = (() => {
      const
        empty = x => undefined,
        store = (k, v, l) => x => x === k ? v : l(x),
        aa = [
          ["assert", a => { if (a) return a; throw 'assertion failed'; }],
          [".", a => (console.log(a), a)],
          ["+", a => b => a + b],
          ["-", a => b => a - b],
          ["*", a => b => a * b],
          ["/", a => b => a / b],
          ["%", a => b => a % b],
          ["=", a => b => a===b?1:0],
          ["<", a => b => a<b?1:0],
          ["<=", a => b => a<=b?1:0],
          [">=", a=>b=>a>=b?1:0],
          [">", a=>b=>a>b?1:0],
          ["X", a=>b=>[a].concat(Array.isArray(b)?b:[])],
          ["A", a=>Array.isArray(a)?a[0]:a],
          ["B", a=>!Array.isArray(a)||a.length<2?0:a.slice(1)],
        ];
      return aa.reduce((s, [k, v]) => store(intern(k), v, s), empty);
    })(),

    g_sprint = x => {
      if (x instanceof Symbol) return x.nom||'#symbol';
      if (Array.isArray(x)) {
        let len = 0, s = '(';
        for (let i = 0; i < x.length; i++, len++)
          s += (len ? ' ':'') + g_sprint(x[i]);
        return s + ')';
      }
      if (typeof(x)==='function') return '#function';
      return ''+x;
    };


  return s => g_sprint(ev(gwen_parse(s))(G));
})();

if (typeof process != 'undefined') {
  const ev_wrap = s => {
    let ok = false;
    try {
      ok = '0' != gwen_eval(s.toString());
    } catch (e) {
      console.error(e);
    }
    return ok;
  }
  const fs = require('node:fs');
  const files = process.argv.slice(2);
  for (const file of files)
    if (!ev_wrap(fs.readFileSync(file)))
      console.error(`[gwen.js] ${file}: ERROR`);
    else
      console.log(`[gwen.js] ${file}: ok`);
}
