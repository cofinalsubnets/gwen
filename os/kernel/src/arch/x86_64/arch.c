#include "k.h"
#include "log.h"
void k_stop(void) { asm ("hlt"); }

void resume(g_task *t) {
}
