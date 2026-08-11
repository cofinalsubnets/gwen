#include "../impl.h"

int execvp(char const *f, char *const *av) {
  char const *s = f;
  while (*s) { if (*s == '/') return execv(f, av); s++; }
  char const *path = getenv("PATH");
  if (!path) path = "/usr/local/bin:/usr/bin:/bin";
  char buf[4096];
  size_t fn = strlen(f);
  while (*path) {
    size_t i = 0;
    while (path[i] && path[i] != ':') i++;
    if (i + 1 + fn + 1 < sizeof buf) {
      memcpy(buf, path, i);
      buf[i] = '/';
      memcpy(buf + i + 1, f, fn + 1);
      execv(buf, av); }                     /* returns only on failure; keep walking */
    path += i;
    if (*path == ':') path++; }
  __errno_v = ENOENT;
  return -1; }
