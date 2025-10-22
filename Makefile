n=mitty
x=mi
m=bin/host/$n
l=bin/host/lcat
.PHONY: test
test: bin/host/$n
	@echo MI test
	@$m test/*.$x

#build
target ?= host
#build
# c files and headers
h=$(wildcard src/*.h)
c=$(wildcard src/*.c)
host_o=$(addprefix bin/host/, $(c:.c=.o) sys.o)
host_h=bin/host/main.h host/sys.h bin/boot.h
a=bin/host/$n.a
so=bin/host/lib$n.so
flags:= -std=gnu17 -g -O2 -pipe\
 	-Wall -Wextra -Wstrict-prototypes -Wno-unused-parameter -Wno-shift-negative-value\
	-falign-functions -fomit-frame-pointer -fno-stack-check -fno-stack-protector\
 	-fno-exceptions -fno-asynchronous-unwind-tables -fno-stack-clash-protection\
 	-fcf-protection=none\
	-flto -fpic
cc=$(CC) -Isrc -Ibin $(flags)\
	 -D g_version='"$(shell git rev-parse HEAD)"'\
	 -D g_target=g_target_$(target)

bin/host/$n: bin/host/main.o $a
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ $^
bin/host/lcat: bin/host/lcat.o $a
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ $^
bin/host/$n.a: $(host_o)
	@echo AR $@
	@mkdir -p $(dir $@)
	@ar rcs $@ $^
bin/host/lib$n.so: $(host_o)
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -shared -o $@ $^
bin/host/%.o: %.c $h Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c $< -o $@
bin/host/%.o: host/%.c $h Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c -Ibin/host $< -o $@
bin/host/main.o: host/main.c bin/host/main.h bin/boot.h $h Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c -Ibin/host $< -o $@

#.c.o:
#	@echo $@
#	@$(cc) -c $< -o $@

# sed command to escape lisp text into C string format
bin/boot.h: bin/host/lcat host/lcat.sed src/boot.$x
	@echo LC $@
	@$l < src/boot.$x | sed -f host/lcat.sed >$@

bin/host/main.h: bin/host/lcat host/lcat.sed host/main.$x
	@echo LC $@
	@$l < host/main.$x | sed -f host/lcat.sed > $@

bin/host/$n.1: bin/host/$n host/manpage.$x
	@echo MI $@
	@$m host/manpage.$x > $@


# installlation
# default install to home directory under ~/.local/
PREFIX ?= .local/
VIMPREFIX ?= .vim/
DESTDIR ?= $(HOME)/
dest=$(DESTDIR)/$(PREFIX)/
vimdest=$(DESTDIR)/$(VIMPREFIX)/
installs=\
 	$(dest)/bin/$n\
  $(dest)/share/man/man1/$n.1\
  $(dest)/lib/$n.a\
  $(dest)/lib/lib$n.so\
  $(dest)/include/$n.h\
  $(vimdest)/ftdetect/$n.vim\
  $(vimdest)/syntax/$n.vim
install: $(installs)
uninstall:
	@echo RM $(abspath $(installs))
	@rm -f $(installs)

$(dest)/include/$n.h: src/$n.h
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@
$(dest)/lib/$n.a: bin/host/$n.a
	@echo IN $(abspath $@)
	@install -D -m 755 $< $@
$(dest)/lib/lib$n.so: bin/host/lib$n.so
	@echo IN $(abspath $@)
	@install -D -m 755 $< $@
$(dest)/bin/$n: bin/host/$n
	@echo IN $(abspath $@)
	@install -D -m 755 -s $< $@
$(dest)/share/man/man1/$n.1: bin/host/$n.1
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@
$(vimdest)/ftdetect/$n.vim: vim/ftdetect/$n.vim
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@
$(vimdest)/syntax/$n.vim: vim/syntax/$n.vim
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@

clean:
	@echo CLEAN
	@rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: bin/host/$n
	valgrind --error-exitcode=1 $m test/*.$x
# profiling on linux
perf.data: bin/host/$n
	perf record $m test/*.$x
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $^
repl: bin/host/$n
	rlwrap $m
serve:
	darkhttpd .

pd/%:
	make -C pd $(notdir $@)
os/%:
	make -C pd $(notdir $@)
