# name of this file
m=Makefile
# name and file extension
n=gw
x=gw

target ?= host

#build
# c files and headers
main_h=main.h ../src/boot.h
main_c=main.c lcat.c
h=$(filter-out $(main_h), $(wildcard *.h) $(wildcard ../src/*.h))
c=$(wildcard ../src/*.c)

b=$(target)/$n
built_binary=$b
o=sys.o $(notdir $(c:.c=.o))
a=$n.a
so=lib$n.so

CFLAGS=\
	-I../src\
  -std=gnu17 -g -O2 -Wall -fpic\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-plt -fno-exceptions -fomit-frame-pointer\
  -fno-asynchronous-unwind-tables -fno-stack-protector\
	-fno-stack-clash-protection -fcf-protection=none\
  -falign-functions

cc=$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)\
	 -D g_version='"$(shell git rev-parse HEAD)"'\
	 -D g_target=g_target_$(target)

$n: $m $(main_h) $h $o main.c
	$(cc) $o main.c -o $@

$n.a: $m $h $o
	ar rcs $@ $^

lib$n.so: $o
	gcc -shared -o $@ $^

sys.o: sys.c
	$(cc) -c $< -o $@

%.o: ../src/%.c $h
	$(cc) -c $< -o $@

lcat: $o $(wildcard *.c)
	$(cc) $o lcat.c -o $@

# sed command to escape lisp text into C string format
../src/boot.h: lcat lcat.sed ../src/boot.$x
	./lcat <../src/boot.$x | sed -f lcat.sed >$@

main.h: lcat lcat.sed main.$x
	./lcat <main.$x | sed -f lcat.sed >$@

$n.1: $n manpage.$x
	./$^ > $@

clean:
	make -C .. clean
