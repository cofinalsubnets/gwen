#!/bin/sh
# test/gate/dist.sh -- the download door (self-host rung 3), two depths:
#
#   smoke  the artifact answers: verb dispatch (positional + argv[0]), the nested
#          kore dispatch, up's no-origin refusal, down's no-nest answer, -e still
#          evals, `--` still forces the file. seconds; rides test_all (test_dist).
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

  echo "test_dist: the artifact is multi-call -- up/down/seed/cook/kore/kiosko/mooncc dispatch, files and -e untouched"
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
