#include "../impl.h"

int fileno(FILE *f) { return f->fd; }
