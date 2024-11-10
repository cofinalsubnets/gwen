#ifndef _gwen_h
#define _gwen_h
// freestanding headers
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __STDC_HOSTED__
#include <stdio.h> // not a freestanding header
typedef FILE *gwen_file;
#else
typedef struct gwen_file *gwen_file;
#endif

// thanks !!
typedef struct gwen_core *gwen_core;
typedef enum {
  GwenStatusEof = -1,
  GwenStatusOk = 0,
  GwenStatusOom = 1,
} gwen_status;

gwen_core gwen_open(void);
void gwen_close(gwen_core),
  gwen_write1f(gwen_core, gwen_file);
gwen_status
  gwen_read1f(gwen_core, gwen_file),
  gwen_eval(gwen_core);
size_t gwen_drop(gwen_core, size_t);

#endif
