clean:
	rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: bin/host/$n
	valgrind --error-exitcode=1 bin/host/$n test/*.$x
# profiling on linux
perf.data: bin/host/$n
	perf record bin/host/$n test/*.$x
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $^
repl: bin/host/$n
	rlwrap bin/host/$n
serve:
	darkhttpd .

pd/%:
	make -C pd $(notdir $@)
os/%:
	make -C pd $(notdir $@)
