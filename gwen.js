const gwen = (() => {
  const
    { isArray } = Array,
    [Quote, Lambda, Let, Cond, Begin] = ['`', '\\', ':', '?', ','].map(Symbol.for),
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

    // parsing functions have type
    //   (string * (string * [any] -> S) * (string -> F) -> S + F
    // where S, F are success and failure callback return types.
    //
    // match a literal string
    lit = l => (s, y, n) => s.startsWith(l) ? y(s.slice(l.length), [l]) : n(s),
    // match a regular expression
    re = r => (s, y, n) => (m => m && m.index == 0 ? y(s.slice(m[0].length), [m[0]]) : n(s))(s.match(r)),
    // sum of parsers
    alt = (a, b) => (s, y, n) => a(s, y, _ => b(s, y, n)),
    // product of parsers
    cat = (a, b) => (s, y, n) => a(s, (s, p1) => b(s, (s, p2) => y(s, p1.concat(p2)), n), n),
    // optional parser
    opt = (a) => (s, y, _) => a(s, y, _ => y(s, [])),
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
    // parse one expression from a string and return it
    gwen_read = (s) => expr(s, (_, x) => x[0], e => console.error('parse error', e)),

    // eval
    ev = x => l => {
      // look up a symbol
      if (typeof(x) === 'symbol') return l(x);
      // return self quoting data
      if (!isArray(x)) return x;
      // empty list same as zero
      if (x.length == 0) return 0;
      // examine the first item to see if it's a special form
      const [x0, ...a] = x;
      // first couple are easy...
      //
      // return the thing
      if (x0 === Quote) return a[0];
      // eval in order and return the last one
      if (x0 === Begin) return a.reduce((_, x) => ev(x)(l), 0);

      // lambda expressions build a curried function that
      // binds arguments to symbols in the current environment
      // and then evals the inner expression.
      if (x0 === Lambda) return a.slice(0, -1).reduceRight(
        (f, arg) => l => x => f(y => y === arg ? x : l(y)),
        ev(a[a.length-1])
      )(l);

      // conditionals try to find a branch that passes, not complicated
      if (x0 === Cond) for (let i = 0;; i += 2) {
        if (i == a.length) return 0; // no default case => 0
        if (i == a.length - 1) return ev(a[i])(l); // default case
        if (ev(a[i])(l)) return ev(a[i + 1])(l); // conditional branch
      }

      // this is the hard one ... let expressions ...
      // we need to bind everything first and then evaluate the
      // definitions in the given order, which gives the right behavior
      // for both sequential binding and mutual recursion.
      //
      // there are two accumulator variables in the loop, b and m
      // - b is a function of an environment that evaluates the
      //   definitions and stores the value in a local enclosed
      //   variable (cb), returning the environment (not logically
      //   needed but just lets the return expression be written
      //   nicely :)
      // - m is an updated environment that refers to cb
      //
      // hopefully every cb will have been initialized by the time the
      // variable gets looked up. if not then the value will be undefined,
      // same as in javascript.
      if (x0 === Let) for (let i = 0, b = l => l, m = l;; i += 2) {
        if (i == a.length) a.push(0); // no inner expression => eval 0
        if (i == a.length - 1) return ev(a[i])(b(m)); // call b with m and eval last expression
        let k = a[i], v = a[i+1], cb, m0 = m, b0 = b; // key, value, closure binding, current store
        while (isArray(k)) v = [Lambda, ...k.slice(1), v], k = k[0]; // desugar (: (f a b) (g a b)) to (: f (\ a b (g a b)))
        m = x => x === k ? cb : m0(x); // new store defaults to old store
        b = l => (cb = ev(v)(b0(l)), l); // new binding function calls old binding function
      }

      // map eval fold apply
      return x.map(x => ev(x)(l)).reduce(
        (f, x) => typeof(f) === 'function' ? f(x) : f
      );
    },

    // print
    gwen_show = x => {
      switch (typeof(x)) {
        case 'symbol': return Symbol.keyFor(x) || '#symbol';
        case 'function': return `#\\${x.name}`;
        default: return isArray(x) ? `(${x.map(gwen_show).join(' ')})` : '' + x;
      }
    };

  return {
    eval: x => ev(x)(G),
    read: gwen_read,
    show: gwen_show,
  }
})();

if (typeof(module) != 'undefined') module.exports = gwen;
