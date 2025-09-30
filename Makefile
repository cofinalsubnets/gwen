default: host_all test
host/%:
	make -C host $(notdir $@)

pd/%:
	make -C pd $(notdir $@)
os/%:
	make -C pd $(notdir $@)

# name of this file
# name and file extension
n=gw
x=gw
#build
bin=host/$n
target ?= host

all: host_all pd_all os_all

host_all: host/$n host/$n.1

pd_all: pd/$n.pdx

os_all:
	make -C os

test: test_c
test_sequence=$(sort $(wildcard test/*.$x))
test_all: test_c test_js
test_c: $(bin)
	$(bin) $(test_sequence)
test_js:
	cd js && npm test

clean:
	rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: $b
	valgrind --error-exitcode=1 ./$^ $(test_sequence)
# count lines of code
cloc:
	cloc --by-file-by-lang --force-lang=Lisp,$x $c $(main_c) $h *.$x
# size of binaries
bits: host/$n
	readelf -S host/$n | grep -A 1 '\(text\|data\)'
disasm: host/$n
	rizin -A $<
# profiling on linux
perf.data: host/$n
	perf record host/$n $(test_sequence)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
repl: $b
	rlwrap ./$<
serve:
	darkhttpd .
include install.mk
