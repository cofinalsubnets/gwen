#ifndef _p_h
#define _p_h
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __STDC_HOSTED__
#include <stdio.h> // not a freestanding header
typedef FILE PFile, *p_file;
#else
typedef struct PFile PFile, *p_file;
#endif

// thanks !!
typedef struct PCore PCore;
typedef enum PStatus {
  PStatusEof = -1,
  PStatusOk = 0,
  PStatusOom = 1,
} PStatus;

PCore
  *p_open(void);

void
  p_close(PCore*),
  p_write1f(PCore*, PFile*);

PStatus
  p_read1f(PCore*, PFile*),
  p_eval(PCore*);

size_t
  p_height(PCore*),
  p_drop(PCore*, size_t);
#endif
