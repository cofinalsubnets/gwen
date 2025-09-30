default: all test
host/%:
	@make -C host $(notdir $@)

pd/%:
	make -C pd $(notdir $@)

# name of this file
# name and file extension
n=gw
x=gw
#build
bin=host/$n
target ?= host

all: all_host all_pd

all_host: host/$n host/$n.1

all_pd: pd/$n.pdx


test: test_c
test_sequence=$(sort $(wildcard test/*.$x))
test_c: $(bin)
	@$(bin) $(test_sequence)
test_js:
	@cd js && npm test
test_all: test_c test_js

clean:
	rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: $b
	valgrind --error-exitcode=1 ./$^ $(test_sequence)
# count lines of code
cloc:
	cloc --by-file-by-lang --force-lang=Lisp,$x $c $(main_c) $h *.$x
# size of binaries
bits: $b
	du -h $^
disasm: $b
	rizin -A $<
# profiling on linux
perf.data: $b
	@echo $@
	@perf record ./$^ $(test_sequence)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
repl: $b
	rlwrap ./$<
serve:
	darkhttpd .
include install.mk
