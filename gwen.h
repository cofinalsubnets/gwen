#ifndef _gwen_h
#define _gwen_h
#include <stddef.h>
#include <stdint.h>
#ifdef __STDC_HOSTED__
#include <stdio.h>
typedef FILE *gwen_file;
#else
typedef struct gwen_file *gwen_file;
#endif

// thanks !!
typedef struct gwen_core *gwen_core;
typedef enum gwen_status {
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

typedef struct gwen_input {
  int (*getc)(gwen_core, struct gwen_input*),
      (*ungetc)(gwen_core, int, struct gwen_input*),
      (*eof)(gwen_core, struct gwen_input*);
  intptr_t data[]; } *gwen_input;
typedef struct gwen_output {
  int (*putc)(gwen_core, int, struct gwen_output*);
  intptr_t data[]; } *gwen_output;
#endif
