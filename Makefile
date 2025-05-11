# name of this file
m=Makefile

# name and file extension
n=p
x=p

# default version
t=tc # tail called
#t=tr # trampolined

default: test

# c headers and source files
h=$(wildcard *.h)
c=$(filter-out main.c, $(wildcard *.c))

#build
CFLAGS ?= \
  -std=c99 -g -O2 -Wall -fpic \
 	-Wstrict-prototypes -Wno-shift-negative-value \
  -fno-asynchronous-unwind-tables -fno-stack-protector \
  -falign-functions
cc=$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)

# tail called objects
%.tc.o: %.c $h $m
	$(cc) -c $< -o $@ -DTCO=1
# trampolined objects
%.tr.o: %.c $h $m
	$(cc) -c $< -o $@ -DTCO=0
# static library
lib$n.%.a: $(c:.c=.%.o)
	ar rcs $@ $^
# shared library
lib$n.%.so: $(c:.c=.%.o)
	$(cc) -shared -o $@ $^
# executable
$n.%.bin: main.c lib$n.%.a
	$(cc) $^ -o $@

# installlation
# default install to home directory under ~/.local/
DESTDIR ?= $(HOME)/
PREFIX ?= .local/

dest=$(DESTDIR)$(PREFIX)

prelude=prelude.$x
built_binary=$n.$t.bin
built_static_library=lib$n.$t.a
built_prelude=$(prelude)
built_c_header=$n.h
built_manpage=$n.1
built_vim_ftdetect=$n.ftdetect.vim
built_vim_syntax=$n.syntax.vim
built_shared_library=lib$n.$t.so

all: $(built_binary) $(built_static_library) $(built_c_header)\
	$(built_prelude) $(built_manpage) $(built_vim_syntax) $(built_shared_library)

$(built_manpage): $n.1.md
	pandoc -s -t man -o $@ $<

# all installed file paths
VIMPREFIX ?= .vim/
vimdir=$(DESTDIR)$(VIMPREFIX)
installed_binary=$(dest)/bin/$n
installed_static_library=$(dest)/lib/lib$n.a
installed_prelude=$(dest)/lib/$n/prelude.$x
installed_c_header=$(dest)/include/$n.h
installed_manpage=$(dest)/share/man/man1/$n.1
installed_vim_ftdetect=$(vimdir)/ftdetect/$n.vim
installed_vim_syntax=$(vimdir)/syntax/$n.vim
installed_shared_library=$(dest)/lib/lib$n.so
installed_files=$(installed_binary) $(installed_static_library)\
						 $(installed_prelude) $(installed_c_header)\
						 $(installed_vim_ftdetect) $(installed_vim_syntax)\
						 $(installed_manpage) $(installed_shared_library)

install: $(installed_files)
uninstall:
	rm -f $(installed_files)

$(installed_binary): $(built_binary)
	install -D -m 755 -s $< $@
$(installed_vim_ftdetect): $(built_vim_ftdetect)
	install -D -m 644 $< $@
$(installed_vim_syntax): $(built_vim_syntax)
	install -D -m 644 $< $@
$(installed_static_library): $(built_static_library)
	install -D -m 644 $< $@
$(installed_manpage): $(built_manpage)
	install -D -m 644 $< $@
$(installed_prelude): $(built_prelude)
	install -D -m 644 $< $@
$(installed_c_header): $(built_c_header)
	install -D -m 644 $< $@
$(installed_shared_library): $(built_shared_library)
	install -D -m 644 $< $@

tests=$(sort $(wildcard test/*.$x))
test: test_c
test_c: test_tc test_tr
test_all: test_c test_js
test_js:
	npm test
test_tc: $n.tc.bin
	@echo '[tail called]'
	@/usr/bin/env TIMEFORMAT="in %Rs" sh -c "time ./$n.tc.bin $(prelude) $(tests)"
test_tr: $n.tr.bin
	@echo '[trampolined]'
	@/usr/bin/env TIMEFORMAT="in %Rs" sh -c "time ./$n.tr.bin $(prelude) $(tests)"

clean:
	rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: valg-tc
valg-%: $n.%.bin
	valgrind --error-exitcode=1 ./$^ $(prelude) $(tests)
# count lines of code
sloc:
	cloc --force-lang=Lisp,$x * test/* lib/*
# size of binaries
bits: $n.tc.bin $n.tr.bin
	du -h $^
disasm: $n.$n.bin
	rizin -A $<
# profiling on linux
perf.data: $n.$t.bin
	perf record ./$^ $(prelude) $(tests)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<
repl: $n.$t.bin
	rlwrap ./$< $(prelude) -i
serve:
	darkhttpd .

.NOTINTERMEDIATE:
.PHONY: \
  all install uninstall \
  test test_all test_c test_js test_tr test_tc \
	clean valg sloc bits disasm perf flame repl serve
