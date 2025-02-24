# makefile for p
# name and file extension
n=p
x=p

default: test

# c headers and source files
h=$(wildcard *.h)
c=$(filter-out main.c, $(wildcard *.c))

#build
CFLAGS ?= -std=c99 -g -O2 -Wall -fpic\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-asynchronous-unwind-tables -fno-stack-protector
cc=$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)

%.tc.o: %.c
	$(cc) -c $< -o $@

%.tr.o: %.c
	$(cc) -c $< -o $@ -DTCO=0

lib$n.%.a: $(c:.c=.%.o)
	ar rcs $@ $^
#	strip --strip-unneeded $@
	ranlib $@

lib$n.%.so: $(c:.c=.%.o)
	$(cc) -shared -o $@ $^

$n.%.bin: main.c lib$n.%.a
	$(cc) $^ -o $@

# tco / trampoline binaries
tc_bin=$n.tc.bin
tr_bin=$n.tr.bin


# installlation
# default install to home directory under ~/.local/
DESTDIR ?= $(HOME)/
PREFIX ?= .local/
VIMPREFIX ?= .vim/

dest=$(DESTDIR)$(PREFIX)
vimdir=$(DESTDIR)$(VIMPREFIX)

prelude=prelude.$x
source_binary=$(tc_bin)
source_static_library=lib$n.tc.a
source_prelude=$(prelude)
source_c_header=$n.h
source_manpage=$n.1
source_vim_ftdetect=$n.ftdetect.vim
source_vim_syntax=$n.syntax.vim
source_shared_library=lib$n.tc.so

all: $(source_binary) $(source_static_library) $(source_c_header)\
	$(source_prelude) $(source_manpage) $(source_vim_syntax) $(source_shared_library)

$(source_manpage): $n.1.md
	pandoc -s -t man -o $@ $<

# all installed file paths
target_binary=$(dest)/bin/$n
target_static_library=$(dest)/lib/lib$n.a
target_prelude=$(dest)/lib/$n/prelude.$x
target_c_header=$(dest)/include/$n.h
target_manpage=$(dest)/share/man/man1/$n.1
target_vim_ftdetect=$(vimdir)/ftdetect/$n.vim
target_vim_syntax=$(vimdir)/syntax/$n.vim
target_shared_library=$(dest)/lib/lib$n.so
target_files=$(target_binary) $(target_static_library)\
						 $(target_prelude) $(target_c_header)\
						 $(target_vim_ftdetect) $(target_vim_syntax)\
						 $(target_manpage) $(target_shared_library)

install: $(target_files)
uninstall:
	rm -f $(target_files)

.PHONY:	all install uninstall

$(target_binary): $(source_binary)
	install -D -m 755 -s $< $@
$(target_vim_ftdetect): $(source_vim_ftdetect)
	install -D -m 644 $< $@
$(target_vim_syntax): $(source_vim_syntax)
	install -D -m 644 $< $@
$(target_static_library): $(source_static_library)
	install -D -m 644 $< $@
$(target_manpage): $(source_manpage)
	install -D -m 644 $< $@
$(target_prelude): $(source_prelude)
	install -D -m 644 $< $@
$(target_c_header): $(source_c_header)
	install -D -m 644 $< $@
$(target_shared_library): $(source_shared_library)
	install -D -m 644 $< $@

tests=$(sort $(wildcard test/*.$x))
test: test_c
test_c: test_tc test_tr
test_all: test_c test_js
test_js:
	npm test
test_tc: $(tc_bin)
	@echo '[tco]'
	@/usr/bin/env TIMEFORMAT="in %Rs" sh -c "time ./$(tc_bin) $(prelude) $(tests)"
test_tr: $(tr_bin)
	@echo '[trampoline]'
	@/usr/bin/env TIMEFORMAT="in %Rs" sh -c "time ./$(tr_bin) $(prelude) $(tests)"
.PHONY: test test_all test_c test_js test_tr test_tc

clean:
	rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: $(tc_bin)
	valgrind --error-exitcode=1 ./$(tc_bin) $(prelude) $(tests)
# count lines of code
sloc:
	cloc --force-lang=Lisp,$x * test/* lib/*
# size of binaries
bits: $(tc_bin) $(tr_bin)
	du -h $^
disasm: $(tc_bin)
	rizin -A $<
# profiling on linux
perf.data: $(tc_bin)
	perf record ./$(tc_bin) $(prelude) $(tests)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<
repl: $(tc_bin) $(prelude)
	rlwrap ./$(tc_bin) -i $(prelude)
serve:
	darkhttpd .

.PHONY: clean valg sloc bits disasm perf flame repl serve
.NOTINTERMEDIATE: $(c:.c=.tc.o) $(c:.c=.tr.o)
