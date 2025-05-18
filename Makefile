# name of this file
m=Makefile

# name and file extension
n=p
x=p

# default version
t=tc
#t=tr

default: test

# c headers and source files
h=$n.h
c=$n.c

#build
CFLAGS ?= \
  -std=gnu17 -g -O2 -Wall -fpic \
 	-Wstrict-prototypes -Wno-shift-negative-value \
  -fno-asynchronous-unwind-tables -fno-stack-protector \
  -falign-functions
ver=$(shell git rev-parse HEAD)
cc=$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -D VERSION='"$(ver)"'

# tail called objects
$n.tc.o: $n.c $n.h boot.h $m
	@echo $@
	$(cc) -c $< -o $@ -DTCO
# trampolined objects
$n.tr.o: $n.c $n.h boot.h $m
	@echo $@
	@$(cc) -c $< -o $@
# static library
lib$n.%.a: $n.%.o
	@echo $@
	@ar rcs $@ $^
# shared library
lib$n.%.so: $n.%.o
	@echo $@
	@$(cc) -shared -o $@ $^

# executable
$n.%.bin: main.c boot.h lib$n.%.a
	@echo $@
	@$(cc) $^ -o $@

boot.h: boot.p $m
	@echo $@
	@echo "static const char boot_prog[] =" > $@
	@cat $< | ([ -e ./$n.$t.bin ] && ./$n.$t.bin cat.$x || cat) | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/.*/"&\\n"/' >> $@
	@echo ";" >> $@

# installlation
# default install to home directory under ~/.local/
DESTDIR ?= $(HOME)/
PREFIX ?= .local/

dest=$(DESTDIR)$(PREFIX)

built_binary=$n.$t.bin
built_static_library=lib$n.$t.a
built_c_header=$n.h
built_manpage=$n.1
built_vim_ftdetect=$n.ftdetect.vim
built_vim_syntax=$n.syntax.vim
built_shared_library=lib$n.$t.so

all: $(built_binary) $(built_static_library) $(built_c_header)\
	$(built_manpage) $(built_vim_syntax) $(built_shared_library)

$(built_manpage): $n.1.md
	@echo $@
	@pandoc -s -t man -o $@ $<


# all installed file paths
VIMPREFIX ?= .vim/
vimdir=$(DESTDIR)$(VIMPREFIX)
installed_binary=$(dest)/bin/$n
installed_static_library=$(dest)/lib/lib$n.a
installed_c_header=$(dest)/include/$n.h
installed_manpage=$(dest)/share/man/man1/$n.1
installed_vim_ftdetect=$(vimdir)/ftdetect/$n.vim
installed_vim_syntax=$(vimdir)/syntax/$n.vim
installed_shared_library=$(dest)/lib/lib$n.so
installed_files=$(installed_binary) $(installed_static_library)\
						 $(installed_c_header)\
						 $(installed_vim_ftdetect) $(installed_vim_syntax)\
						 $(installed_manpage) $(installed_shared_library)

install: $(installed_files)
uninstall:
	rm -f $(installed_files)

$(installed_binary): $(built_binary)
	@echo $@
	@install -D -m 755 -s $< $@
$(installed_vim_ftdetect): $(built_vim_ftdetect)
	@echo $@
	@install -D -m 644 $< $@
$(installed_vim_syntax): $(built_vim_syntax)
	@echo $@
	@install -D -m 644 $< $@
$(installed_static_library): $(built_static_library)
	@echo $@
	@install -D -m 644 $< $@
$(installed_manpage): $(built_manpage)
	@echo $@
	@install -D -m 644 $< $@
$(installed_c_header): $(built_c_header)
	@echo $@
	@install -D -m 644 $< $@
$(installed_shared_library): $(built_shared_library)
	@echo $@
	@install -D -m 644 $< $@

tests=$(sort $(wildcard test/*.$x))
test: test_c
test_c: test_tc test_tr
test_all: test_c test_js
test_js:
	npm test
test_tc: $n.tc.bin
	@echo '[tail called]'
	@./$n.tc.bin $(tests)
test_tr: $n.tr.bin
	@echo '[trampolined]'
	@./$n.tr.bin $(tests)

clean:
	rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: valg-tc
valg-%: $n.%.bin
	valgrind --error-exitcode=1 ./$^ $(tests)
# count lines of code
sloc:
	cloc --force-lang=Lisp,$x * test/* lib/*
# size of binaries
bits: $n.tc.bin $n.tr.bin
	du -h $^
disasm: $n.$t.bin
	rizin -A $<
# profiling on linux
perf.data: $n.$t.bin
	perf record ./$^ $(tests)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<
repl: $n.$t.bin
	rlwrap ./$<
serve:
	darkhttpd .

.NOTINTERMEDIATE:
.PHONY: \
  all install uninstall \
  test test_all test_c test_js test_tr test_tc \
	clean valg sloc bits disasm perf flame repl serve
