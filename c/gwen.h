#ifndef _gwen_h
#define _gwen_h
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
typedef struct GwenCore GwenCore, *gwen_core;
typedef enum GwenStatus {
  GwenStatusOk = 0,
  GwenStatusOom = 1,
  GwenStatusEof = 7,
} GwenStatus, gwen_status;

GwenCore *gwen_open(void);
void gwen_close(GwenCore*),
  gwen_write1f(GwenCore*, gwen_file);
GwenStatus
  gwen_read1f(GwenCore*, gwen_file),
  gwen_eval(GwenCore*);
size_t gwen_drop(GwenCore*, size_t);

#endif
