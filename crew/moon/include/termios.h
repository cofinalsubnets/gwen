#ifndef _AI_TERMIOS_H
#define _AI_TERMIOS_H
#include <sys/types.h>   /* pid_t, for the tc[gs]etpgrp pair */
typedef unsigned int  tcflag_t;
typedef unsigned char cc_t;
typedef unsigned int  speed_t;
/* glibc x86-64 layout: 4 flag words, a line byte, 32 control chars, 2 speeds = 60 bytes */
struct termios {
  tcflag_t c_iflag, c_oflag, c_cflag, c_lflag;
  cc_t c_line;
  cc_t c_cc[32];
  speed_t c_ispeed, c_ospeed;
};
/* the flags the kernels agree on */
#define BRKINT  2
#define ISTRIP 32
#define INPCK  16
#define ICRNL 256
#define OPOST   1
#define ECHO    8
#define TCSANOW   0
#define TCSADRAIN 1
#define TCSAFLUSH 2
#define IXON 1024
#define ISIG    1
#define ICANON  2
#define IEXTEN 32768
#define VTIME   5
#define VMIN    6
#define TCIFLUSH  0
#define TCOFLUSH  1
#define TCIOFLUSH 2
#define TCOOFF 0
#define TCOON  1
#define TCIOFF 2
#define TCION  3
int tcgetattr(int, struct termios*);
int tcsetattr(int, int, struct termios const*);
int tcsendbreak(int, int);
int tcdrain(int);
int tcflush(int, int);
int tcflow(int, int);
pid_t tcgetpgrp(int);
int tcsetpgrp(int, pid_t);
#endif
