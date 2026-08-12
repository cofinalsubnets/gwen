#include "../impl.h"

DIR *opendir(char const *p) {
  int fd = open(p, O_RDONLY | O_DIRECTORY | O_CLOEXEC, 0);
  if (fd < 0) return 0;
  DIR *d = malloc(sizeof(DIR));
  if (!d) { close(fd); return 0; }
  d->fd = fd; d->pos = 0; d->len = 0;
  return d; }
