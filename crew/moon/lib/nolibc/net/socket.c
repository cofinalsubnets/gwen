#include "../impl.h"

int socket(int d, int t, int p) {
  if (__ai_osv >= 2) d = (int) __ai_affb(d), t = (int) __ai_sotype(t);
  return (int) er(sc3(NR_socket, d, t, p)); }
