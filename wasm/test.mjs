// wasm test harness: run the love test corpus through the emscripten build
// and gate on the zz-fin summary, the same contract as the native test_host
// (`cat $t | love` then grep "tests pass"). A third runtime after the host
// binary and the love0 bootstrap -- this one exercises wasm's <data.h>
// override (sentinel-ap data kinds, no flat code-address space).
//
// Usage: node wasm/test.mjs [--love <love.js>] <corpus.l...>
//   (the Makefile passes out/wasm/love.js and $t, in order)
//
// The module path is a PARAMETER because the gate must not build over the
// committed wasm/love.js -- see ../Makefile. Default is the gate's own build;
// point --love at wasm/love.js to exercise the artifact that actually ships.
import { readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';

const argv = process.argv.slice(2);
let mod = new URL('../out/wasm/love.js', import.meta.url).href;
if (argv[0] === '--love') {
  if (argv.length < 2) { console.error('--love wants a path'); process.exit(2); }
  mod = pathToFileURL(argv[1]).href;
  argv.splice(0, 2);
}
const files = argv;
if (!files.length) { console.error('usage: test.mjs [--love <love.js>] <corpus.l...>'); process.exit(2); }
const { default: Love } = await import(mod);
// The shim bakes only prel+ev (the page feeds the REPL through ai_eval),
// but the native runner has the shell core baked too -- and the corpus tests its
// surface (zev/charms). bao.l is that core (repl.l was consolidated into it), so
// load it first so the wasm test sees the same full stack.
const src = [readFileSync('love/bao.l', 'utf8'),
             ...files.map(f => readFileSync(f, 'utf8'))].join('\n');

const m = await Love();
const init = m.ccall('ai_init', 'number', [], []);
if (init !== 0) { console.error(`ai_init failed (code ${init})`); process.exit(1); }

m.ccall('ai_out_reset', 'null', [], []);
// Marshal the corpus through the HEAP, not ccall('string') -- that copies onto
// the wasm stack, and the ~350K corpus overflows it ("memory access out of
// bounds"). ai_eval takes a const char*, so pass a malloc'd pointer.
const nbytes = m.lengthBytesUTF8(src) + 1;
const ptr = m._malloc(nbytes);
m.stringToUTF8(src, ptr, nbytes);
let code = 0;
try {
  code = m.ccall('ai_eval', 'number', ['number'], [ptr]);
} catch (e) {
  // a failing assert calls (exit 1); under emscripten that throws to unwind.
  // Fall through to the output check, which won't find the pass summary.
  code = (e && typeof e.status === 'number') ? e.status : 1;
}
m._free(ptr);

const out = m.UTF8ToString(m.ccall('ai_out_ptr', 'number', [], []),
                           m.ccall('ai_out_len', 'number', [], []));
process.stdout.write(out.endsWith('\n') ? out : out + '\n');

// "tests pass" appears once, in the final zz-fin summary -- so a truncated or
// aborted run fails here loudly rather than passing by accident.
const clean = out.replace(/\x1b\[[0-9;]*m/g, '');
if (code === 0 && /tests pass/.test(clean) && !/assert failed/.test(clean)) process.exit(0);
console.error(`WASM TEST FAILED (eval code ${code})`);
process.exit(1);
