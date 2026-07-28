#!/usr/bin/env python3
# emit compile_commands.json -- the compilation database clangd wants.
#
# WHY THIS EXISTS: clangd runs its fallback command from THE FILE'S OWN DIRECTORY, so a
# relative -I in .clangd resolves against host/ or port/inle/ and `love.h` is not found.
# that miss is FATAL, the parse stops, and every name after it reports undeclared -- a
# flood of diagnostics that says nothing about the code. a database carries absolute
# directories per entry, so the paths land where they mean.
#
# the flag groups mirror the real builds: host/build.mk (hcc) and port/inle/kernel.mk
# (kcflags + kcppflags). they are few and they are stable; when one moves, move it here.
#
# the generated file is machine-specific (absolute paths) and gitignored. regenerate with
# `make ccdb`. the generated headers under out/ must exist first, so run a build before it.

import json
import os
import subprocess

R = subprocess.run(
    ["git", "rev-parse", "--show-toplevel"],
    capture_output=True, text=True, check=True,
).stdout.strip()

# the host lane: love.c, host/, libc/, and the crew's C.
HOST = [
    "-std=gnu23", "-Dai_tco=1",
    f"-I{R}", f"-I{R}/out/lib", f"-I{R}/out/host", f"-I{R}/crew/quay", f"-I{R}/libc",
]

# the freestanding kernel: its own libc, its own headers, nothing hosted.
INLE = [
    "-std=gnu23", "-Dai_tco=1",
    "-ffreestanding", "-nostdinc",
    "-DLIMINE_API_REVISION=3", "-DK_TEST",
    f"-I{R}", f"-I{R}/out/lib", f"-I{R}/out/host", f"-I{R}/crew/quay",
    f"-I{R}/port/inle", f"-I{R}/crew/moon/include", f"-I{R}/libc",
]

# moon's replacement libc -- compiled against its OWN headers, so glibc's declarations
# would "conflict" with the real ones.
MOONLIBC = [
    "-std=gnu23", "-ffreestanding", "-nostdinc", f"-I{R}/crew/moon/include", f"-I{R}",
]

# the playdate workbench takes its lcat'd cas.h out of out/playdate.
PLAYDATE = HOST + [
    "-DTARGET_PLAYDATE=1", "-DTARGET_EXTENSION=1",
    f"-I{R}/port/playdate", f"-I{R}/out/playdate",
]

# two files reach for a VENDOR SDK that lives outside the tree. point the database at it
# when the environment names one; otherwise the file stays out of the database entirely
# and .clangd quiets it, since no flag set here could resolve the include.
PD_SDK = os.environ.get("PLAYDATE_SDK_PATH")
EMSCRIPTEN = os.environ.get("EMSDK")


def flags_for(path):
    if path.startswith("crew/moon/lib/"):
        return MOONLIBC
    if path.startswith("port/inle/"):
        f = list(INLE)
        if path.startswith("port/inle/aarch64/"):
            f.append("--target=aarch64-unknown-none-elf")
        return f
    if path.startswith("port/playdate/"):
        f = list(PLAYDATE)
        if PD_SDK:
            f.append(f"-I{PD_SDK}/C_API")
        return f
    if path.startswith("wasm/") and EMSCRIPTEN:
        return HOST + [f"-I{EMSCRIPTEN}/upstream/emscripten/system/include"]
    return HOST


def main():
    tracked = subprocess.run(
        ["git", "-C", R, "ls-files", "*.c"],
        capture_output=True, text=True, check=True,
    ).stdout.split()

    db = []
    for rel in sorted(tracked):
        # rp2040 lives in the separate l-ports repo and still includes ../../gwen.h, a
        # name retired in the rename to love.h. nothing here can resolve it.
        if rel.startswith("port/rp2040/"):
            continue
        # the vendor-SDK files, absent their SDK. .clangd quiets these paths to match.
        if rel == "port/playdate/pdglue.c" and not PD_SDK:
            continue
        if rel.startswith("wasm/") and not EMSCRIPTEN:
            continue
        db.append({
            "directory": R,
            "file": os.path.join(R, rel),
            "arguments": ["/usr/bin/clang", "-fsyntax-only", *flags_for(rel), rel],
        })

    out = os.path.join(R, "compile_commands.json")
    with open(out, "w") as fh:
        json.dump(db, fh, indent=2)
        fh.write("\n")
    print(f"{out}: {len(db)} entries")


if __name__ == "__main__":
    main()
