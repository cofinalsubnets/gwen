const gwen = (() => {
  const
    { isArray } = Array,
    [Quote, Lambda, Define, Cond, Begin] = ['`', '\\', ':', '?', ','].map(Symbol.for),
    // global environment
    G = (g => x => g[x])([
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
        ["X", a=>b=>[a].concat(isArray(b)?b:[])],
        ["A", a=>isArray(a)?a[0]:a],
        ["B", a=>!isArray(a)||a.length<2?0:a.slice(1)],
      ].reduce((g, [k, v]) => ((g[Symbol.for(k)] = v), g), {})),

    // read
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
    atom = pmap(([s])=>[(i=>''+i==s?i:Symbol.for(s))(parseFloat((s)))], re(/^[^ \r\n\t()'"]+/)), 
    expr = alt(atom, list),
    gwen_read = (s) => expr(s, (_, x) => x[0], e => console.error('parse error', e)),

    // eval
    ev = x => l => {
      if (typeof(x) === 'symbol') return l(x);
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

      if (x0 == Define) for (let i = 0, b = () => 0, m = l;; i += 2) {
        if (i == a.length) a.push(0); // no inner expression => 0
        // last expression, call bindings and eval
        if (i == a.length - 1) return b(m), ev(a[i])(m);
        // key, value, closure binding, current store
        let k = a[i], v = a[i+1], cb, m0 = m, b0 = b;
        // desugar (: (f a b) (g a b)) to (: f (\ a b (g a b)))
        while (isArray(k)) v = [Lambda, ...k.slice(1), v], k = k[0];
        m = x => x === k ? cb : m0(x); // update store
        b = l => (b0(l), cb = ev(v)(l));
      }

      // map eval fold apply
      return x.map(x => ev(x)(l)).reduce(
        (f, x) => typeof(f) === 'function' ? f(x) : f
      );
    },

    // print
    gwen_show = x => {
      if (isArray(x)) return `(${x.map(gwen_show).join(' ')})`;
      switch (typeof(x)) {
        case 'symbol': return Symbol.keyFor(x) || '#symbol';
        case 'function': return `#\\${x.name}`;
        default: return '' + x;
      }
    };

  return {
    eval: x => ev(x)(G),
    read: gwen_read,
    show: gwen_show,
  }
})();

if (typeof(module) != 'undefined') module.exports = gwen;
