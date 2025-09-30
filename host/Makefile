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

CFLAGS=\
	-I../src\
  -std=gnu17 -g -Os -Wall -fpic\
	-Wno-unused-function\
 	-Wstrict-prototypes -Wno-shift-negative-value\
	-fno-plt -fno-exceptions -fomit-frame-pointer\
  -fno-asynchronous-unwind-tables -fno-stack-protector\
	-fno-stack-clash-protection -fcf-protection=none\
  -falign-functions

cc=$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)\
	 -D g_version='"$(shell git rev-parse HEAD)"'\
	 -D g_target=g_target_$(target)

$n: $m $(main_h) $h $o $(wildcard *.c)
	@echo $@
	@$(cc) $o main.c -o $@

$a: $m $h $o
	@echo $@
	@ar rcs $@ $^

sys.o: sys.c
	@echo $@
	@$(cc) -c $< -o $@

%.o: ../src/%.c
	@echo $@
	@$(cc) -c $< -o $@

lcat: $o $(wildcard *.c)
	@echo $@
	@$(cc) $o lcat.c -o $@

# sed command to escape lisp text into C string format
sed=sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/.*/"&\\n"/'
../src/boot.h: lcat ../src/boot.$x
	@echo $@
	@./lcat <../src/boot.$x | $(sed) >$@

main.h: lcat main.$x
	@echo $@
	@./lcat <main.$x | $(sed) >$@
