const
  gw = require('../gw'),
  fs = require('node:fs/promises'),
  test = require('node:test'),
  assert = require('node:assert/strict'),
  suff = 'gw',
  testDir = '../test';

test('expressions', t => {
  const
    s = '(1 2 (3 (4) 5) 6 (7 eight) 9)',
    l = [1, 2, [3, [4], 5], 6, [7, Symbol.for('eight')], 9];
  return Promise.all([
    t.test('read', t => assert.deepEqual(l, gw.read(s))),
    t.test('show', t => assert.equal(s, gw.show(l))),
  ]);
});

test('common tests', t => {
  const [Let, Assert] = [':', 'assert'].map(Symbol.for);
  return Promise.all([ 'church', 'closure', 'heron', 'lambda', 'tak' ].map(n =>
    fs.readFile(`${testDir}/${n}.${suff}`).then(r =>
      t.test(n, x => gw.eval([Let, Assert, assert, gw.read(n.toString())])))));
});

test('fib', t => {
  const
    n = 20,
    fib = n => n < 3 ? 1 : fib(n-1)+fib(n-2),
    prog = `(: (fib n) (? (< n 3) 1 (: f_1 (fib (- n 1)) f_2 (fib (- n 2)) (+ f_1 f_2))) (fib ${n}))`;
  assert(gw.eval(gw.read(prog)) === fib(n));
});

test('sequential', t => {
  const
    a = 2,
    b = a + 3,
    c = b * 9,
    prog = '(: a 2 b (+ a 3) c (* b 9) (+ a c))';
  assert(gw.eval(gw.read(prog)) === a + c);
});

test('even/odd', t => {
  const
    oddp = x => !evenp(x),
    evenp = x =>
      gw.eval(gw.read(
        `(: (even n) (? (> n 0) (odd (- n 1)) 1)
            (odd n) (? (> n 0) (even (- n 1)) 0)
          (even ${x}))`));
  assert(evenp(22));
  assert(oddp(23));
});
