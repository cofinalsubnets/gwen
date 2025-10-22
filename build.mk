#build
target ?= host
#build
# c files and headers
h=$(wildcard src/*.h)
c=$(wildcard src/*.c)
host_o=$(addprefix bin/host/, $(c:.c=.o) sys.o)
host_h=bin/host/main.h host/sys.h bin/boot.h
a=bin/host/lib$n.a
so=bin/host/lib$n.so
CFLAGS ?=\
  -std=gnu17 -g -O2 -Wall -fpic\
	-Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-plt -fno-exceptions -fomit-frame-pointer\
  -fno-asynchronous-unwind-tables -fno-stack-protector\
	-fno-stack-clash-protection -fcf-protection=none\
  -falign-functions
cc=$(CC) -Isrc $(CFLAGS)\
	 -D g_version='"$(shell git rev-parse HEAD)"'\
	 -D g_target=g_target_$(target)

bin/host/$n: bin/host/main.h bin/boot.h host/main.c $a
	@echo CC $@
	@$(cc) -o $@ -Ibin -Ibin/host host/main.c $a
bin/host/lcat: host/lcat.c $a
	@echo CC $@
	$(cc) -o $@ -Ibin -Ibin/host $^
bin/host/lib$n.a: $(host_o)
	@echo AR $@
	@ar rcs $@ $(host_o)
bin/host/lib$n.so: $o
	gcc -shared -o $@ $^
bin/host/%.o: %.c $h Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c $< -o $@
bin/host/sys.o: host/sys.c $h Makefile
	@echo CC $@
	@$(cc) -c $< -o $@
bin/host/lcat.o: host/lcat.c $h Makefile
	@echo CC $@
	@$(cc) -c $< -o $@

#.c.o:
#	@echo $@
#	@$(cc) -c $< -o $@

# sed command to escape lisp text into C string format
bin/boot.h: bin/host/lcat host/lcat.sed src/boot.$x
	@echo CAT $@
	@bin/host/lcat < src/boot.$x | sed -f host/lcat.sed >$@

bin/host/main.h: bin/host/lcat host/lcat.sed host/main.$x
	@echo CAT $@
	@bin/host/lcat < host/main.$x | sed -f host/lcat.sed > $@

bin/$n.1: $n manpage.$x
	bin/host/$n manpage.$x > $@
