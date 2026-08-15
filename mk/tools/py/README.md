# mk/tools/py — the retired Python build tools

These three scripts used to live in `mk/tools/` and were the build's real
codegen/lint helpers. They have since been rewritten in ll lisp (the `.g`
files one directory up), which is the whole point of the project: the toolchain
that builds ll should itself be written in ll, so the only hard build-time
dependency is a C compiler (plus a disassembler for the TCO check).

| Python (here)      | ll replacement (`mk/tools/`) | what it does |
|--------------------|-----------------------------|--------------|
| `gen_data.py`   | `gen_data.l`             | reflects the data-sentinel stride out of a compiled `data.o` into a generated `data.h` so `g_typ()` is one shift |
| `vmret.py`         | `vmret.l`                   | disassembles an ELF and flags `lvm_*` handlers that emit a `ret` instead of tail-jumping |

## Why keep them?

They are the **golden reference** for the rewrites. Each ll tool is required
to produce byte-for-byte identical output to its Python ancestor, and the
`mk/tools/Makefile` gates (`test_gen_data`, `test_vmret`, all run by
`make test_tools` from the root) enforce exactly that by diffing the two over
every build artifact present. Keeping the Python around means a regression in a
`.g` tool — or a behavioural drift introduced by a future change to the ll
compiler/runtime — is caught immediately against an independent implementation.

They are no longer invoked by any build: `gen_data.l` generates every
frontend's `data.h` (host, kernel), and `make vmret` runs
`vmret.l`. The scripts here are frozen
— edit the `.g` tools, not these — and exist solely so the gates have something
trustworthy to check against. If a ll tool ever needs to intentionally change
its output, update the corresponding Python reference in the same commit so the
gate stays meaningful.
