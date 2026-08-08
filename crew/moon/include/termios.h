#ifndef _AI_TERMIOS_H
#define _AI_TERMIOS_H
#include <sys/types.h>   /* pid_t, for the tc[gs]etpgrp pair */
/* glibc x86-64 layout: 4 flag words, a line byte, 32 control chars, 2 speeds = 60 bytes */
typedef unsigned int  tcflag_t;
typedef unsigned char cc_t;
typedef unsigned int  speed_t;
struct termios {
  tcflag_t c_iflag, c_oflag, c_cflag, c_lflag;
  cc_t c_line;
  cc_t c_cc[32];
  speed_t c_ispeed, c_ospeed;
};
/* c_iflag */
#define BRKINT  2
#define ISTRIP 32
#define INPCK  16
#define ICRNL 256
#define IXON 1024
/* c_lflag */
#define ISIG    1
#define ICANON  2
#define ECHO    8
#define IEXTEN 32768
/* c_oflag */
#define OPOST   1
/* c_cc index */
#define VTIME   5
#define VMIN    6
#define TCSANOW   0
#define TCSADRAIN 1
#define TCSAFLUSH 2
/* tcflush's queue selector */
#define TCIFLUSH  0
#define TCOFLUSH  1
#define TCIOFLUSH 2
/* tcflow's action */
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
