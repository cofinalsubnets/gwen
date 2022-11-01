// liblisa definitions
#ifndef _lisa_h
#define _lisa_h
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct la *la;
typedef intptr_t la_ob;

la la_ini(void);
bool la_open(la);
void
  la_fin(la),
  la_close(la);

la_ob
  la_ev(la, la_ob),
  la_rx(la, FILE*);

int la_tx(la, FILE*, la_ob);

#endif
