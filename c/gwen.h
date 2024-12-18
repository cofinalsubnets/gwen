#ifndef _gwen_h
#define _gwen_h
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __STDC_HOSTED__
#include <stdio.h> // not a freestanding header
typedef FILE GwenFile, *gwen_file;
#else
typedef struct GwenFile GwenFile, *gwen_file;
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
  gwen_write1f(GwenCore*, GwenFile*);
GwenStatus
  gwen_read1f(GwenCore*, GwenFile*),
  gwen_eval(GwenCore*);
size_t gwen_drop(GwenCore*, size_t);
#endif
