const gw = (() => {
  const
    { isArray } = Array, // need this
    // special forms, need these
    [Quote, Lambda, Let, Cond, Begin] = ['`', '\\', ':', '?', ','].map(Symbol.for),

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
    gw_read = (s) => expr(s, (_, x) => x[0], e => console.error('parse error', e)),

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
        ["tget", z=>t=>k=>t instanceof Map && t.has(k) ? t.get(k) : z],
        ["tset", t=>k=>v=>t instanceof Map ? t.set(k, v) : v],
        ["tnew", _=>new Map()],
        ["tlen", t=>t instanceof Map ? t.size : 0],
        ["tdel", z=>t=>k=>t instanceof Map && t.has(k) ? (z = t.get(k), t.delete(k), z) : z],
      ].reduce((g, [k, v]) => ((g[Symbol.for(k)] = v), g), {})),

    // helper functions
    id = x => x, // identity
    K = x => y => x, // make a constant function
    ap = (f, x) => typeof(f) === 'function' ? f(x) : f, // apply

    // evaluator: expression -> environment -> expression
    ev = x => {
      // look up a symbol
      if (typeof(x) === 'symbol') return l => l(x);
      // return self quoting data
      if (!isArray(x)) return K(x);
      // empty list same as zero
      if (x.length === 0) return K(0);
      // examine the first item to see if it's a special form
      const [x0, ...a] = x;
      if (x0 === Quote) return K(a[0]);
      if (x0 === Begin) return a.map(ev).reduce((a, b) => l => (a(l), b(l)), K(0));

      if (x0 === Lambda) return a.slice(0, -1).reduceRight(
        (f, arg) => l => x => f(y => y === arg ? x : l(y)),
        ev(a[a.length-1])
      );

      if (x0 === Cond) for (let i = 0, f = id;; i += 2) {
        if (i === a.length) a.push(0); // no default case => 0
        if (i === a.length - 1) return f(ev(a[i])); // default case
        const ant = ev(a[i]), con = ev(a[i+1]), f0 = f;
        f = alt => f0(l => ant(l) ? con(l) : alt(l));
      }

      if (x0 === Let) for (let i = 0, f = id;; i+= 2) {
        if (i === a.length) a.push(0); // no inner expression => eval 0
        if (i === a.length - 1) return x = ev(a[i]), l => x(f(l));
        let f0 = f, k = a[i], v = a[i+1]; // current binding function, key, value
        while (isArray(k)) v = [Lambda, ...k.slice(1), v], k = k[0]; // desugar (: (f a b) (g a b)) to (: f (\ a b (g a b)))
        v = ev(v), f = l0 => {
          const l1 = f0(x => x === k ? u : l0(x)), u = v(l1);
          return l1;
        };
      }

      return x.map(ev).reduce((f, x) => l => ap(f(l), x(l)), K(id));
    },

    // print function
    gw_show = x => {
      switch (typeof(x)) {
        case 'symbol': return Symbol.keyFor(x) || '#symbol';
        case 'function': return `#\\${x.name}`;
        default: return isArray(x) ? `(${x.map(gw_show).join(' ')})` : '' + x;
      }
    };

  return {
    read: gw_read,
    show: gw_show,
    eval: x => ev(x)(global_env),
  }
})();

if (typeof(module) !== 'undefined') module.exports = gw;
