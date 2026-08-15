#!/usr/bin/env python3
"""uu_parity.py -- audit test/uu.l against the published UniMath sources.

Every name defined in test/uu.l (defn/defq/axm) must be one of:
  (a) a name published in the installed UniMath package
      (/usr/lib/ocaml/coq/user-contrib/UniMath, e.g. arch coq-unimath), or
  (b) prefixed uu- : local glue that claims no UniMath ancestry.
Anything else is an impostor and fails the audit. Also reports coverage
per published file -- the road to parity.

Usage: python3 mk/tools/py/uu_parity.py [path/to/uu.l]
Exit 1 on impostors, 0 otherwise (skips with a note if UniMath absent).
"""
import re, glob, sys, os

UNIMATH = '/usr/lib/ocaml/coq/user-contrib/UniMath'
UU_L = sys.argv[1] if len(sys.argv) > 1 else 'test/uu.l'

if not os.path.isdir(UNIMATH):
    print('uu_parity: UniMath not installed, audit skipped')
    sys.exit(0)

pub = {}
total = {}
for path in sorted(glob.glob(f'{UNIMATH}/**/*.v', recursive=True)):
    short = path.split('UniMath/')[1]
    names = re.findall(
        r'^\s*(?:Definition|Theorem|Lemma|Corollary|Proposition|Axiom|Fixpoint|Record|Inductive)\s+([A-Za-z0-9_\']+)',
        open(path).read(), re.M)
    total[short] = len(set(names))
    for n in names:
        pub.setdefault(n, short)
# Preamble constructors and notations that are names in uu.l's object language
for extra in ['tpair', 'pr1', 'pr2', 'idpath', 'ii1', 'ii2', 'tt', 'succ',
              'total2', 'paths', 'coprod', 'nat', 'bool', 'unit', 'empty', 'UU']:
    pub.setdefault(extra, 'Foundations/Preamble.v')

names = re.findall(r"\((?:defn|defq|axm) '([A-Za-z0-9_-]+)", open(UU_L).read())
local = [n for n in names if n.startswith('uu-')]
matched = [n for n in names if not n.startswith('uu-') and n in pub]
impostors = [n for n in names if not n.startswith('uu-') and n not in pub]

print(f'{UU_L}: {len(names)} names = {len(matched)} published'
      f' + {len(local)} uu-local + {len(impostors)} impostors')
from collections import Counter
cov = Counter(pub[n] for n in matched)
for f, c in cov.most_common():
    print(f'  {c:3}/{total.get(f, "?"):>3} {f}')
if impostors:
    print('IMPOSTORS (unpublished names without uu- prefix):')
    for n in impostors:
        print(f'  {n}')
    sys.exit(1)
print('parity audit clean: every unprefixed name is published UniMath')
