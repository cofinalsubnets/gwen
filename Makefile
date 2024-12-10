test: test_tc test_tr

n=gwen
x=gw
c=c
b=_b
l=l
d=doc

prelude=$l/prelude.$x
tco_binary=$b/$n.tc.bin
trampoline_binary=$b/$n.tr.bin

tests=$(sort $(wildcard test/*.$x))
#build
CFLAGS ?= -std=c99 -g -O2 -Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-asynchronous-unwind-tables -fno-stack-protector
cc=$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)

$(tco_binary): $c/main.c $b/$n.tc.o
	$(cc) $^ -o $@
$(trampoline_binary): $c/main.c $b/$n.tr.o
	$(cc) -DTCO=0 $^ -o $@

$b/$n.tc.o: $c/$n.c $c/$n.h
	$(cc) -c $< -o $@
$b/$n.tr.o: $c/$n.c $c/$n.h
	$(cc) -c -DTCO=0 $< -o $@


# all the local files to install
made_binary=$(tco_binary)
made_static_library=$b/lib$n.a
made_manpage=$b/$n.1
made_vim_ftdetect=vim/$n.ftdetect.vim
made_vim_syntax=vim/$n.syntax.vim
made_files=$(made_binary) $(made_static_library)\
						 $(made_vim_ftdetect) $(made_vim_syntax)\
						 $(made_manpage)

$(made_static_library): $b/$n.o
	ar rcs $@ $b/$n.o
	strip --strip-unneeded $@
	ranlib $@
$(made_manpage): $d/$n.1.md $b
	pandoc -s -t man -o $@ $<

# installlation
# default install to home directory under ~/.local/
DESTDIR ?= $(HOME)/
PREFIX ?= .local/
VIMPREFIX ?= .vim/
dest=$(DESTDIR)$(PREFIX)
vimdir=$(DESTDIR)$(VIMPREFIX)

# all the target files
installed_binary=$(dest)/bin/$n
installed_static_library=$(dest)/lib/lib$n.a
installed_prelude=$(dest)/lib/$n/prelude.$x
installed_c_header=$(dest)/include/$n.h
installed_manpage=$(dest)/share/man/man1/$n.1
installed_vim_ftdetect=$(vimdir)/ftdetect/$n.vim
installed_vim_syntax=$(vimdir)/syntax/$n.vim
installed_files=$(installed_binary) $(installed_static_library)\
						 $(installed_prelude) $(installed_c_header)\
						 $(installed_vim_ftdetect) $(installed_vim_syntax)\
						 $(installed_manpage)

install: all $(installed_files)
uninstall:
	rm -f $(installed_files)

$(installed_binary): $(made_binary)
	install -D -m 755 -s $< $@
$(installed_vim_ftdetect): $(made_vim_ftdetect)
	install -D -m 644 $< $@
$(installed_vim_syntax): $(made_vim_syntax)
	install -D -m 644 $< $@
$(installed_static_library): $(made_static_library)
	install -D -m 644 $< $@
$(installed_manpage): $(made_manpage)
	install -D -m 644 $< $@
$(installed_prelude): $(prelude)
	install -D -m 644 $< $@
$(installed_c_header): $c/$n.h
	install -D -m 644 $< $@

clean:
	rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: $(tco_binary)
	valgrind --error-exitcode=1 ./$(tco_binary) $(prelude) $(tests)
# count lines of code
sloc:
	cloc --force-lang=Lisp,$x * test/* lib/*
# size of binaries
bits: $(tco_binary) $(trampoline_binary)
	du -h $^
disasm: $(tco_binary)
	rizin -A $<
# profiling on linux
perf.data: $(tco_binary)
	perf record ./$(tco_binary) $(prelude) $(tests)
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $<
flame: flamegraph.svg
	xdg-open $<
repl: $(tco_binary) $(prelude)
	rlwrap ./$(tco_binary) -i $(prelude)
serve:
	darkhttpd .
test_js:
	cd js && npm test
test_tc: $(tco_binary)
	@echo '[tco]'
	@/usr/bin/env TIMEFORMAT="in %Rs" sh -c "time ./$(tco_binary) $(prelude) $(tests)"
test_tr: $(trampoline_binary)
	@echo '[trampoline]'
	@/usr/bin/env TIMEFORMAT="in %Rs" sh -c "time ./$(trampoline_binary) $(prelude) $(tests)"
.PHONY: test_all all test_c test_js install uninstall clean valg sloc bits disasm perf flame repl serve
