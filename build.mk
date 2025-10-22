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
CFLAGS ?=\
  -std=gnu17 -g -O2 -Wall -fpic\
	-Wall\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-plt -fno-exceptions -fomit-frame-pointer\
  -fno-asynchronous-unwind-tables -fno-stack-protector\
	-fno-stack-clash-protection -fcf-protection=none\
  -falign-functions
cc=$(CC) -Isrc -Ibin $(CFLAGS)\
	 -D g_version='"$(shell git rev-parse HEAD)"'\
	 -D g_target=g_target_$(target)
ld=ld

bin/host/$n: bin/host/main.o $a
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(CC) -o $@ $^
bin/host/lcat: bin/host/lcat.o $a
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ $^
bin/host/$n.a: $(host_o)
	@echo AR $@
	@mkdir -p $(dir $@)
	@ar rcs $@ $^
bin/host/lib$n.so: $a
	@echo CC	$@
	@mkdir -p $(dir $@)
	@gcc -shared -o $@ $^
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
	@echo MI $@
	@bin/host/lcat < src/boot.$x | sed -f host/lcat.sed >$@

bin/host/main.h: bin/host/lcat host/lcat.sed host/main.$x
	@echo MI $@
	@bin/host/lcat < host/main.$x | sed -f host/lcat.sed > $@

bin/$n.1: $n manpage.$x
	@echo MI $@
	@bin/host/$n manpage.$x > $@
