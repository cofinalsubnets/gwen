#!/bin/sh
# test/gate/dist.sh -- the download door (self-host rung 3), two depths:
#
#   smoke  the artifact answers: verb dispatch (positional + argv[0]), the nested
#          kore dispatch, up's no-origin refusal, down's no-nest answer, -e still
#          evals, `--` still forces the file. seconds; rides test_slow (test_dist).
#
#   up     the whole door in anger, test/host/seedhttp.l's shape writ large: the
#          repo tree seed-recorded into a scratch origin, kiosko (the artifact's
#          own verb) serving that .seed/ on loopback, then `love up URL` into a
#          scratch HOME -- sync materializes ~/.love/src, cook install builds and
#          lands the nest -- and a second up proves the no-op. minutes (a full
#          from-source build); opt-in (make test_up).
#
# usage: dist.sh MODE DIST
set -u

mode=$1
dist=$2
name=test_dist
[ "$mode" = up ] && name=test_up

fail() { echo "FAIL $name: $*" >&2; exit 1; }

# the artifact NEEDS its baked image (the verbs live there); the make environment
# exports LOVE_NO_IMAGE=1 for the corpus, so every artifact run here unsets it.
run() { env -u LOVE_NO_IMAGE "$@"; }

dabs=$(CDPATH= cd -- "$(dirname -- "$dist")" && pwd)/$(basename -- "$dist")

case $mode in
smoke)
  s=out/dist/.smoke
  rm -rf "$s"; mkdir -p "$s"

  run "$dist" kore true                            || fail "kore true (the nested dispatch)"
  run "$dist" seed 2>&1 | grep -q "patch-set vcs"  || fail "seed usage"
  run "$dist" mooncc 2>&1 | grep -q "usage: mooncc" || fail "mooncc verb usage"
  # the CC-under-make lane: the Makefile blanket-exports LOVE_NO_IMAGE=1, and
  # the love0 recipes hand THIS command its image back with `LOVE_NO_IMAGE=`
  # (empty = unset, main.c) -- pin that an empty value does not egg-boot the
  # artifact (which would read "mooncc" as a filename).
  LOVE_NO_IMAGE= "$dist" mooncc 2>&1 | grep -q "usage: mooncc" \
                                                   || fail "empty LOVE_NO_IMAGE suppressed the image"
  run "$dist" up >/dev/null 2>&1
  [ $? -eq 2 ]                                     || fail "up without an origin should refuse (exit 2)"
  HOME=$dabs.nowhere run "$dist" down 2>&1 | grep -q "no nest" || fail "down without a nest"
  run "$dist" -e '(? (2 = (1 + 1)) (quit 0) (quit 1))' || fail "-e still evals"

  ln -sf "$dabs" "$s/seed"
  run "$s/seed" 2>&1 | grep -q "usage: seed"       || fail "the argv[0] door"
  echo '(quit 7)' > "$s/up"
  ( cd "$s" && run "$dabs" -- up ); [ $? -eq 7 ]   || fail "-- should force the file lane"

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
    && run "$dabs" -e '(: _ (sh-oneline (list "-c") "mooncc -zzz") (quit (? (two? (peep sh-imgc "mooncc" 0)) 0 1)))' ) \
     >/dev/null 2>&1 \
    || fail "lush ran a command without taking its own in-image decision"
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
    && run "$dabs" -e '(quit (? (two? (sh-imgfn "mooncc")) 1 0))' ) \
    || fail "the in-image lane engaged for a mooncc that is a DIFFERENT file"

  # a bare name we do NOT own must never go in-image, whatever rides this image
  ( cd "$sabs/w" && PATH=/usr/bin:/bin run "$dabs" sh -c 'ls Makefile' ) 2>&1 | grep -q Makefile \
    || fail "a foreign ls must still spawn"

  echo "test_dist: the artifact is multi-call -- up/down/seed/cook/kore/kiosko/mooncc dispatch, files and -e untouched"
  echo "test_dist: the in-image lane -- cook's lines and mooncc run in THIS image, byte-for-byte and semantics-for-semantics what a spawned sh gives"
  ;;

up)
  s=out/dist/.up
  rm -rf "$s"; mkdir -p "$s/home" "$s/origin"
  port=7434

  # the origin: this repo's TRACKED tree, seed-recorded fresh. git names the
  # files (the same rim use as `make lint`); the working copies ride, so an
  # uncommitted fix is in the release the gate tests. NOT the whole directory:
  # untracked local state (a .claude/ of session logs weighs ~700M here) is
  # nobody's release.
  echo "DIST origin: recording the tree"
  git ls-files -z | tar --null -T - -cf - | tar -C "$s/origin" -xf - || fail "tree copy"
  ( cd "$s/origin" && run "$dabs" seed record "dist gate origin" ) || fail "seed record"

  oroot=$(CDPATH= cd -- "$s/origin" && pwd)/.seed
  # the explicit `exec` is load-bearing: backgrounding the `run` FUNCTION left
  # $! naming the subshell, not the server -- the kill hit the wrapper and the
  # kiosko lived on reparented to init, holding the gate's stdout pipe open
  # (make looked hung long after it passed) and the artifact ETXTBSY against
  # the next rebake. -9 because the artifact rides a plain TERM out.
  ( exec env -u LOVE_NO_IMAGE "$dist" kiosko -p $port -q "$oroot" ) &
  kpid=$!
  trap 'kill -9 $kpid 2>/dev/null' EXIT INT TERM
  sleep 1

  home=$(CDPATH= cd -- "$s/home" && pwd)
  echo "DIST up: sync + cook install into $home/.love (a full from-source build -- minutes)"
  HOME=$home run "$dist" up "http://127.0.0.1:$port/" || fail "love up"

  nest=$home/.love
  [ -x "$nest/bin/love" ] || fail "no $nest/bin/love"
  HOME=$home run "$nest/bin/love" -e '(? (2 = (1 + 1)) (quit 0) (quit 1))' || fail "the installed love does not answer"
  [ -x "$nest/bin/seed" ] || fail "no $nest/bin/seed"
  HOME=$home run "$nest/bin/seed" >/dev/null 2>&1
  [ -e "$home/.local/bin/love" ] || fail "no ~/.local/bin/love compat link"

  m1=$(stat -c %Y "$nest/bin/love")
  echo "DIST up again: the no-op"
  HOME=$home run "$dist" up "http://127.0.0.1:$port/" || fail "second up"
  m2=$(stat -c %Y "$nest/bin/love")
  [ "$m1" = "$m2" ] || fail "second up rebuilt the binary (mtime moved)"

  echo "$name: the download door whole -- one artifact pulled the tree over http, cooked the nest, and the second up changed nothing"
  ;;

*) echo "dist.sh: unknown mode $mode" >&2; exit 1 ;;
esac
