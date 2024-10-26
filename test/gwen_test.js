const
  gwen = require('../gwen'),
  fs = require('node:fs'),
  test = require('node:test'),
  assert = require('node:assert/strict');

const
  s = '(1 2 (3 (4) 5) 6 (7 eight) 9)',
  l = [1, 2, [3, [4], 5], 6, [7, Symbol.for('eight')], 9];
test('gwen.read', t => assert.deepEqual(l, gwen.read(s)));
test('gwen.show', t => assert.equal(s, gwen.show(l)));

const
  [Let, Assert] = [':', 'assert'].map(Symbol.for),
  sharedTests = [ 'church', 'closure', 'heron' ];
for (const t of sharedTests) {
  const expr = gwen.read(fs.readFileSync(`test/${t}.gw`).toString());
  test(t, x => gwen.eval([Let, Assert, assert, expr]));
}

test('fib', t => {
  const
    n = 20,
    fib = n => n < 3 ? 1 : fib(n-1)+fib(n-2),
    p = `(: (fib n) (? (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))) (fib ${n}))`;
  assert(gwen.eval(gwen.read(p)) === fib(n));
});
