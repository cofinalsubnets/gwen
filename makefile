m=makefile
n=lips
b=$n.bin
t=$(wildcard test/*)
h=$(wildcard *.h)
s=$(wildcard *.c)
o=$(s:.c=.o)
v=`git rev-parse HEAD`

# why these compiler flags :
# - fixnums need sign-extended bit shifting.
# - inlining bloats code and GCC even does it for tail calls,
#   which is silly. turn it off by default.
# - stack smash protection also hurts tco.
c=cc -g -O2 -flto -DNOM=\"$n\" -DVN=\"$v\"\
	-Wall -Wno-shift-negative-value\
	-fno-inline -fno-stack-protector

test: $n
	@/usr/bin/env TIMEFORMAT="in %Rs" bash -c "time ./$b $t"

# build
$n: $b
	@strip -o $@ $b
	@stat -c "$@ %sB" $@
$b: $m $h $o
	@$c -o $@ $o
	@stat -c "$@ %sB" $@
.c.o:
	@$c -c $<
	@stat -c "$@ %sB" $@

# install
dest=~/.local/bin
$(dest)/$n: $n
	@cp $n $(dest)
	@echo $@
install: $(dest)/$n

# vim stuff
install-vim:
	@make -sC vim

# tasks
clean:
	@rm -rf `git check-ignore *`
perf: perf.data
	@perf report
perf.data: $b $t
	@perf record ./$^
valg: $b $t
	@valgrind ./$^
sloc:
	@which cloc && cloc --force-lang=Lisp,l --force-lang=Lisp,$n . || cat $s $h $m | grep -v ' *//.*' | grep -v '^$$' | wc -l
bins: $o $n $b
	@stat -c "%n %sB" $^
bench: $b
	@make -sC bench

.PHONY: test clean perf valg sloc bins install vim bench
