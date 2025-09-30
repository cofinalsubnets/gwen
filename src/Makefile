# name of this file
m=Makefile
# name and file extension
n=gw
x=gw

target ?= host

#build
# c files and headers
h=$(filter-out boot.h, $(wildcard *.h))
c=$(wildcard *.c)
o=$(c:.c=.o)

b=$(target)/$n
built_binary=$b

CFLAGS=\
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

$n.a: $o
	@echo $@
	@ar rcs $@ $^ 

.c.o:
	@echo $@
	@$(cc) -c $< -o $@
