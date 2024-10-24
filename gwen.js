const gwen_eval = (() => {
  const
    pmap = (p, f) => (s, y, n) => p(s, (s, x) => y(s, f(x)), n),
    drop = a => (s, y, n) => a(s, (s, _) => y(s, []), n),
    re = r => (s, y, n) => {
      const m = s.match(r);
      return m ? y(s.slice(m[0].length), [m[0]]) : n(s);
    },
    lit = (l) => (s, y, n) => s.startsWith(l) ? y(s.slice(l.length), [l]) : n(s),
    alt = (a, b) => (s, y, n) => a(s, y, _ => b(s, y, n)),
    cat = (a, b) => (s, y, n) => a(s, (s, p1) => b(s, (s, p2) => y(s, p1.concat(p2)), n), n),
    sepBy = (p, l) => (s, y, n) => cat(p, opt(cat(l, sepBy(p, l))))(s, y, n),
    opt = (a) => (s, y) => a(s, y, _ => y(s, [])),
    rep = (a) => (s, y) => opt(cat(a, rep(a)))(s, y),
    atom = pmap(re(/^[a-zA-Z0-9_+~\\:,`*.?<>=/%-]+/), a => a.map(s => (i => ''+(i)==s?i:intern(s))(parseFloat((s))))),
    ws = drop(re(/^([ \t\n]*|;[^\r]*)*/)),
    list = pmap(cat(drop(lit("(")), cat(ws, cat((s,y,n)=>sepBy(alt(atom, list), ws)(s,y,n), cat(ws, drop(lit(")")))))), x => [x]),
    expr = (s, y, n) => alt(atom, list)(s, y, n),
    exprs = cat(ws, cat(sepBy(expr, ws), ws)),
    parse = (p, s) => p(s, (_, x) => x[0], _ => undefined)
      ;

  class Symbol { constructor(nom) { this.nom = nom; } }
  const
    { isArray } = Array,
    symbols = {},
    intern = str => (sym => sym ? sym : (symbols[str] = new Symbol(str)))(symbols[str]),
    [Quote, Lambda, Define, Cond, Begin] = ['`', '\\', ':', '?', ','].map(intern),
    empty = x => undefined,
    store = (k, v, l) => x => x === k ? v : l(x),
    G = (() =>{
      const aa = [
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
    },

    ev = x => l => {
      if (x instanceof Symbol) return l(x);
      if (!Array.isArray(x)) return x;
      if (x.length == 0) return 0;
      const [x0, ...a] = x;
      if (x0 == Quote) return a[0];
      if (x0 == Begin) return (
        a.reduce((_, x) => ev(x)(l), 0));
      if (x0 == Lambda) return (
        a.slice(0, -1).reduceRight(
          (f, arg) => l => x => f(store(arg, x, l)),
          ev(a[a.length-1])
        )(l));
      if (x0 == Cond) return (
        a.length == 0 ? 0 :
        a.length == 1 ? ev(a[0])(l) :
        ev(a[0])(l)   ? ev(a[1])(l) :
                        ev([Cond, ...a.slice(2)])(l));
      if (x0 == Define) {
        if (a.length == 0) return 0;
        if (a.length == 1) return ev(a[0])(l);
        if (a.length % 2 == 0) a.push(a[a.length-2]);
        const bind = (d, qq) => {
          if (d.length < 2) return qq.forEach(x => x(l)), ev(d[0])(l);
          let b, [k, v, ...e] = d, q = l;
          while (isArray(k)) v = [Lambda].concat(k.slice(1)).concat([v]), k = k[0];
          l = x => x === k ? b : q(x);
          qq.push(l => b = ev(v)(l));
          return bind(e, qq);
        }
        return bind(a, []);
      }
      return x.map(x => ev(x)(l)).reduce((f, x) => typeof(f)==='function'?f(x):f);
    };

  return s => g_sprint(ev(parse(expr, s))(G));
})();

if (typeof window != 'undefined') window.addEventListener('load', () => {
  const cmdline = document.getElementById('cmdline');
  cmdline.addEventListener('change', ({target:{value}}) => {
    try {
      alert(gwen_eval(value));
    } catch (e) {
      alert(e);
    }
  });
});
else if (typeof process != 'undefined') {
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
