default: test

# name of this file
m=Makefile
# name and file extension
n=gw
x=gw

#build
# c files and headers
main_c=main.c
c=$(filter-out $(main_c), $(wildcard *.c))
boot_h=boot.h boot.0.h
h=$(filter-out $(boot_h), $(wildcard *.h))

b=$n.bin
0=$n.0.bin
o=$(c:.c=.o)

CFLAGS=\
  -std=gnu17 -g -O2 -Wall -fpic\
	-Wno-unused-function\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-plt -fno-exceptions -fomit-frame-pointer\
  -fno-asynchronous-unwind-tables -fno-stack-protector\
	-fno-stack-clash-protection -fcf-protection=none\
  -falign-functions

cc=$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)\
	 -D g_version='"$(shell git rev-parse HEAD)"'

built_binary=$b
$(built_binary): $o main.c boot.h $m
	@echo $@
	@$(cc) $o main.c -o $@

.c.o:
	@echo $@
	@$(cc) -c $<

$0: $o main.c boot.0.h $m
	@echo $@
	@$(cc) $o main.c -o $@ -Dg_main_h='"boot.0.h"'

# sed command to escape lisp text into C string format
sed=sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/.*/"&\\n"/'
boot.0.h: main.$x
	@echo $@
	@$(sed) <$< >$@
boot.h: main.$x cat.$x $0
	@echo $@
	@./$0 cat.$x <$< | $(sed) >$@

built_manpage=$n.1
$(built_manpage): $0 manpage.$x
	@echo $@
	@./$^ > $@

all: $(built_binary) $(built_manpage)

# installlation
# default install to home directory under ~/.local/

DESTDIR ?= $(HOME)/
PREFIX ?= .local/
VIMPREFIX ?= .vim/
vimdir=$(DESTDIR)$(VIMPREFIX)
dest=$(DESTDIR)$(PREFIX)
# all installed file paths
installed_binary=$(DESTDIR)/$(PREFIX)/bin/$n
installed_manpage=$(DESTDIR)/$(PREFIX)/share/man/man1/$n.1
installed_vim_ftdetect=$(DESTDIR)/$(VIMPREFIX)/ftdetect/$n.vim
installed_vim_syntax=$(DESTDIR)/$(VIMPREFIX)/syntax/$n.vim
installed_vim_files=$(installed_vim_syntax) $(installed_vim_ftdetect)
installed_files=$(installed_binary) $(installed_vim_files) $(installed_manpage)
install: $(installed_files)
uninstall:
	rm -f $(installed_files)

$(installed_binary): $(built_binary)
	@echo $@
	@install -D -m 755 -s $< $@
$(installed_vim_ftdetect): vim/ftdetect/$n.vim
	@echo $@
	@install -D -m 644 $< $@
$(installed_vim_syntax): vim/syntax/$n.vim
	@echo $@
	@install -D -m 644 $< $@
$(installed_manpage): $(built_manpage)
	@echo $@
	@install -D -m 644 $< $@

tests=$(sort $(wildcard test/*.$x))
test: test_c
test_all: test_c test_js
test_js:
	cd js && npm test
test_c: $b
	@./$b $(tests)

clean:
	rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: $b
	valgrind --error-exitcode=1 ./$^ $(tests)
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
	@perf record ./$^ $(tests)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
repl: $b
	rlwrap ./$<
serve:
	darkhttpd .

.PHONY: \
  all install uninstall \
  test test_all test_c test_js test_tr test_tc \
	clean valg sloc bits disasm perf repl serve \
	default_target
