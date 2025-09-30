#ifndef _g_host_sys_h
#define _g_host_sys_h
#include "i.h"
#include <stdio.h>
typedef FILE *g_file;
#define g_stdout stdout
#define g_stdin stdin
#define g_stderr stderr
#define g_fprintf fprintf
#define g_fputc putc

g_vm p_isatty, readf, read0;
#endif
