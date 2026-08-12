#include "../impl.h"

int socket(int d, int t, int p) { return (int) er(sc3(NR_socket, d, t, p)); }
