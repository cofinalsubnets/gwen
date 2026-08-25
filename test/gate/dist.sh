#!/bin/sh
# test/gate/dist.sh -- the artifact answers: verb dispatch (positional + argv[0]),
# the nested kore dispatch, the bare cc door, the image chain, the in-image lane,
# -e still evals and `--` still forces the file. seconds; rides test_slow.
#
# usage: dist.sh MODE DIST
set -u

mode=$1
dist=$2
name=test_dist

fail() { echo "FAIL $name: $*" >&2; exit 1; }

# the artifact NEEDS its baked image (the verbs live there), so every artifact run
# clears LOVE_NO_IMAGE -- the guard against a caller's exported egg.
run() { env -u LOVE_NO_IMAGE "$@"; }

dabs=$(CDPATH= cd -- "$(dirname -- "$dist")" && pwd)/$(basename -- "$dist")

case $mode in
smoke)
  s=out/dist/.smoke
  rm -rf "$s"; mkdir -p "$s"

  run "$dist" kore true                            || fail "kore true (the nested dispatch)"
  # the source door, cheaply: the seed lays its tree and the very archive it carried,
  # and lays NOTHING ELSE -- no binary beside the source. the fixpoint stays
  # test_distboot's; THIS is the leg that keeps the verb from going dark between releases.
  ( cd "$s" && run "$dabs" source ) > "$s/src.log" 2>&1 \
                                                   || { tail -3 "$s/src.log"; fail "love source did not lay"; }
  srcd=$(echo "$s"/love-*/)
  [ ! -e "$srcd/bin" ]                             || fail "love source laid a bin/ -- the tree is source, nothing else"
  ls "$srcd"/out/dist/love-*.tar.gz >/dev/null 2>&1 || fail "love source laid no archive"
  run "$dist" sb 2>&1 | grep -q "patch-set vcs"  || fail "sb usage"
  run "$dist" mooncc 2>&1 | grep -q "usage: mooncc" || fail "mooncc verb usage"
  # the CC-under-make lane: the build recipes hand this command its image back
  # with `LOVE_NO_IMAGE=` (empty = unset, main.c) -- pin that an empty value
  # does not egg-boot the artifact (which would read "mooncc" as a filename).
  LOVE_NO_IMAGE= "$dist" mooncc 2>&1 | grep -q "usage: mooncc" \
                                                   || fail "empty LOVE_NO_IMAGE suppressed the image"
  # the bare cc door: from an empty cwd with an empty HOME, the artifact
  # compiles hello world from its CARRIED source, entirely in memory -- `love
  # cc` anywhere, no tree, no install, and NOTHING written outside the cwd.
  mkdir -p "$s/bare/home"
  printf '#include <stdio.h>\nint main(void) { printf("bare door\\n"); return 0; }\n' > "$s/bare/hi.c"
  # timeout 15: the carried runtime makes this ~0.2 s; a fall back to
  # compiling 197 members (~28 s) is a regression this leg must SEE
  ( cd "$s/bare" && HOME=$PWD/home timeout 15 env -u LOVE_NO_IMAGE "$dabs" cc hi.c && ./a.out ) > "$s/bare.log" 2>&1 \
                                                   || { tail -3 "$s/bare.log"; fail "the bare cc door (or it took the 28 s compile lane)"; }
  grep -q "bare door" "$s/bare.log"                || fail "bare cc: the exe did not answer"
  [ -z "$(ls -A "$s/bare/home")" ]                 || fail "bare cc: wrote into HOME ($(ls -A "$s/bare/home"))"
  run "$dist" -e '(? (2 = (1 + 1)) (quit 0) (quit 1))' || fail "-e still evals"

  # --- the docs verb rides the one image -----------------------------------------
  run "$dist" libra doc lib/serve.l > "$s/libra.md" 2>&1 \
    || fail "libra did not wake"
  grep -q "love serve" "$s/libra.md" \
    || fail "libra woke but answered nothing: $(head -3 "$s/libra.md")"

  ln -sf "$dabs" "$s/sb"
  run "$s/sb" 2>&1 | grep -q "usage: sb"       || fail "the argv[0] door"
  # ⚠ NOT a name already symlinked above -- `>` through a symlink writes the artifact
  echo '(quit 7)' > "$s/kore"
  ( cd "$s" && run "$dabs" -- kore ); [ $? -eq 7 ] || fail "-- should force the file lane"

  # --- the in-image lane -------------------------------------------------------
  # one artifact on PATH under many names is the shape the whole thing turns on: lush
  # runs a tool whose main rides THIS image instead of exec'ing it, and cook runs its
  # recipe lines through lush instead of spawning a shell -- so a CC=mooncc make pays
  # ONE image wake, not one per TU. the gate is not the speed, it is that the two
  # lanes AGREE: the same objects, and the same make semantics a spawned /bin/sh gives.
  sabs=$(cd "$s" && pwd)
  mkdir -p "$sabs/bin" "$sabs/cbin" "$sabs/w/sub" "$sabs/refo"
  for n in sh mooncc cook; do ln -sf "$dabs" "$sabs/bin/$n"; done
  # the reference lane's compiler: a COPY. same bytes, different file -- so the skew
  # guard refuses the shortcut and every TU is exec'd, which is the old behaviour.
  cp "$dabs" "$sabs/love-copy" && ln -sf "$sabs/love-copy" "$sabs/cbin/mooncc"
  i=1; while [ $i -le 4 ]; do printf 'int f%d(int x){return x+%d;}\n' $i $i > "$sabs/w/s$i.c"; i=$((i+1)); done
  cat > "$sabs/w/Makefile" <<'MK'
.SHELLFLAGS := -ec
CC = mooncc
all: $(patsubst %.c,%.o,$(wildcard s*.c)) scope
%.o: %.c
	$(CC) -c $< -o $@
scope:
	cd sub && pwd
	pwd
	V=set-in-line; echo "V=$$V"
	echo "next=[$$V]"
	-exit 7
	echo past-ignored
MK
  ( cd "$sabs/w" && SHELL=/bin/sh PATH=$sabs/cbin:/usr/bin:/bin run "$dabs" cook ) > "$sabs/ref.out" 2>&1
  echo "exit=$?" >> "$sabs/ref.out"
  ls "$sabs/w"/*.o >/dev/null 2>&1 || fail "the reference lane laid no objects"
  cp "$sabs/w"/*.o "$sabs/refo/" && rm -f "$sabs/w"/*.o
  ( cd "$sabs/w" && PATH=$sabs/bin:/usr/bin:/bin run "$dabs" cook ) > "$sabs/img.out" 2>&1
  echo "exit=$?" >> "$sabs/img.out"
  cmp -s "$sabs/ref.out" "$sabs/img.out" \
    || fail "in-image cook diverged from a spawned /bin/sh: $(diff "$sabs/ref.out" "$sabs/img.out" | head -6)"
  for f in "$sabs/refo"/*.o; do
    cmp -s "$f" "$sabs/w/$(basename "$f")" || fail "in-image mooncc laid a different $(basename "$f")"
  done
  # ..and PROVE the lane engaged, not merely that it could have. agreement alone
  # would still hold if the call site quietly stopped consulting the decision --
  # both sides would just be the spawn. so run a real line through lush IN this
  # process and ask whether the decision was taken and kept: an unconsulted lane
  # leaves the cache untouched, whatever the predicate on its own would answer.
  ( PATH=$sabs/bin:/usr/bin:/bin && export PATH \
    && run "$dabs" -e '(: _ (use (name "lush")) _ (sh-oneline (list "-c") "mooncc -zzz") (quit (? (two? (peep sh-imgc "mooncc" 0)) 0 1)))' ) \
     >/dev/null 2>&1 \
    || fail "lush ran a command without taking its own in-image decision"
  # the checksums ride the same lane, and the same two things are asked of them: that the
  # decision was taken and kept, and that the answer it produced -- through a redirect,
  # which a tool running in here takes the way a builtin does -- is the one GNU gives.
  ln -sf "$dabs" "$sabs/bin/sha256sum"
  printf 'love\n' > "$sabs/w/sum.in"
  ( cd "$sabs/w" && PATH=$sabs/bin:/usr/bin:/bin && export PATH \
    && run "$dabs" -e '(: _ (use (name "lush")) _ (sh-oneline (list "-c") "sha256sum sum.in > sum.out") (quit (? (two? (peep sh-imgc "sha256sum" 0)) 0 1)))' ) \
     >/dev/null 2>&1 \
    || fail "lush spawned sha256sum where its own main rides this image"
  if command -v sha256sum > /dev/null 2>&1; then
    ( cd "$sabs/w" && sha256sum sum.in ) > "$sabs/w/sum.ref"
    cmp -s "$sabs/w/sum.out" "$sabs/w/sum.ref" || fail "in-image sha256sum disagreed with GNU"
  fi
  # ..and cook's own: two recipe lines are ONE process in-image, one process EACH spawned
  mkdir -p "$sabs/pw"
  printf 'all:\n\t@echo $$$$\n\t@echo $$$$\n' > "$sabs/pw/Makefile"
  ( cd "$sabs/pw" && PATH=$sabs/bin:/usr/bin:/bin && export PATH && run "$dabs" cook ) > "$sabs/cp.out" 2>&1
  [ "$(sed -n 1p "$sabs/cp.out")" = "$(sed -n 2p "$sabs/cp.out")" ] \
    || fail "cook spawned a shell per recipe line where it could have run them here"
  ( cd "$sabs/pw" && SHELL=/bin/sh PATH=/usr/bin:/bin && export PATH SHELL && run "$dabs" cook ) > "$sabs/cp2.out" 2>&1
  [ "$(sed -n 1p "$sabs/cp2.out")" = "$(sed -n 2p "$sabs/cp2.out")" ] \
    && fail "cook ran its lines in-image while SHELL was a foreign /bin/sh"
  # the skew guard, stated directly: same bytes, different file -> the shortcut is refused
  ( PATH=$sabs/cbin:/usr/bin:/bin && export PATH \
    && run "$dabs" -e '(: _ (use (name "lush")) (quit (? (two? (sh-imgfn "mooncc")) 1 0)))' ) \
    || fail "the in-image lane engaged for a mooncc that is a DIFFERENT file"

  # a bare name we do NOT own must never go in-image, whatever rides this image
  ( cd "$sabs/w" && PATH=/usr/bin:/bin run "$dabs" sh -c 'ls Makefile' ) 2>&1 | grep -q Makefile \
    || fail "a foreign ls must still spawn"

  echo "test_dist: the artifact is multi-call -- sb/cook/kore/kiosko/mooncc dispatch, files and -e untouched"
  echo "test_dist: the in-image lane -- cook's lines and mooncc run in THIS image, byte-for-byte and semantics-for-semantics what a spawned sh gives"
  ;;

*) echo "dist.sh: unknown mode $mode" >&2; exit 1 ;;
esac
