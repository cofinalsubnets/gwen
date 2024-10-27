const gwen = (() => {
  const
    { isArray } = Array, // need this
    // special forms, need these
    [Quote, Lambda, Let, Cond, Begin] = ['`', '\\', ':', '?', ','].map(Symbol.for),

    // analyzing evaluator: expression -> environment -> expression
    ana = x => {
      if (typeof(x) === 'symbol') return l => l(x);
      if (!isArray(x)) return l => x;
      if (!x.length) return l => 0;
      const [x0, ...a] = x;
      if (x0 === Quote) return l => a[0];
      if (x0 === Begin) return x.map(ana).reduce((a, b) => l => (a(l), b(l)), l => 0);
      if (x0 === Lambda) return a.slice(0, -1).reduceRight(
        (f, arg) => l => x => f(y => y === arg ? x : l(y)),
        ana(a[a.length-1])
      );

      if (x0 === Cond) for (let i = 0, f = x => x;; i += 2) {
        if (i === a.length) return f(0); // no default case => 0
        if (i === a.length - 1) return f(ana(a[i])); // default case
        const ant = ana(a[i]), con = ana(a[i+1]), f0 = f;
        f = x => l => ant(l) ? con(l) : f0(x)(l);
      }

      if (x0 === Let) for (let i = 0, b = l => l, m = b;; i += 2) {
        if (i === a.length) a.push(0); // no inner expression => eval 0
        if (i === a.length - 1) return x = ana(a[i]), l => x(b(m(l))); // compose and return
        let k = a[i], v = a[i+1], cb, m0 = m, b0 = b; // key, value, closure binding, current store
        while (isArray(k)) v = [Lambda, ...k.slice(1), v], k = k[0]; // desugar (: (f a b) (g a b)) to (: f (\ a b (g a b)))
        v = ana(v);
        m = l => x => x === k ? cb : m0(l)(x); // new store defaults to old store
        b = l => (cb = v(b0(l)), l); // new binding function calls old binding function
      }

      const ap = (f, x) => typeof(f) === 'function' ? f(x) : f;
      return x.map(ana).reduce((f, x) => l => ap(f(l), x(l)), l => x => x);
    },

    // expression parser
    // parsing functions have type
    //   (string * (string * [any] -> S) * (string -> F) -> S + F
    // where S, F are success and failure callback return types.
    //
    // match a literal string
    lit = l => (s, y, n) => s.startsWith(l) ? y(s.slice(l.length), [l]) : n(s),
    // match a regular expression
    re = r => (s, y, n) => (m => m && !m.index ? y(s.slice(m[0].length), [m[0]]) : n(s))(s.match(r)),
    // sum of parsers
    alt = (a, b) => (s, y, n) => a(s, y, _ => b(s, y, n)),
    // product of parsers
    cat = (a, b) => (s, y, n) => a(s, (s, p1) => b(s, (s, p2) => y(s, p1.concat(p2)), n), n),
    // optional parser
    opt = a => (s, y, _) => a(s, y, _ => y(s, [])),
    // discard current state
    drop = a => (s, y, n) => a(s, (s, _) => y(s, []), n),
    // transform current state
    pmap = (f, p) => (s, y, n) => p(s, (s, x) => y(s, f(x)), n),
    // match whitespace / comments
    ws = drop(re(/([ \t\n]*|;[^\r]*)*/)),
    // match one or more expressions
    exprs = (s, y, n) => cat(opt(ws), cat(expr, opt(exprs)))(s, y, n),
    // match possibly empty list
    list = pmap(x => [x.length ? x : 0], cat(drop(lit("(")), cat(opt(exprs), drop(lit(")"))))),
    // match symbol or number
    atom = pmap(([s])=>[(i=>''+i==s?i:Symbol.for(s))(parseFloat((s)))], re(/[^ \r\n\t()'"]+/)), 
    // match any one expression
    expr = alt(atom, list),

    //
    // parse one expression from a string and return it
    gwen_read = (s) => expr(s, (_, x) => x[0], e => console.error('parse error', e)),

    // top level environment definitions
    global_env = (g => x => g[x])([
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

    // print function
    gwen_show = x => {
      switch (typeof(x)) {
        case 'symbol': return Symbol.keyFor(x) || '#symbol';
        case 'function': return `#\\${x.name}`;
        default: return isArray(x) ? `(${x.map(gwen_show).join(' ')})` : '' + x;
      }
    };

  return {
    eval: x => ana(x)(global_env),
    read: gwen_read,
    show: gwen_show,
  }
})();

if (typeof(module) !== 'undefined') module.exports = gwen;
