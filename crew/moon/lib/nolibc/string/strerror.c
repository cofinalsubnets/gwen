#include "../impl.h"

static char const *const __errs[] = { 0,
  "Operation not permitted", "No such file or directory", "No such process",
  "Interrupted system call", "Input/output error", "No such device or address",
  "Argument list too long", "Exec format error", "Bad file descriptor",
  "No child processes", "Resource temporarily unavailable", "Cannot allocate memory",
  "Permission denied", "Bad address", "Block device required",
  "Device or resource busy", "File exists", "Invalid cross-device link",
  "No such device", "Not a directory", "Is a directory",
  "Invalid argument", "Too many open files in system", "Too many open files",
  "Inappropriate ioctl for device", "Text file busy", "File too large",
  "No space left on device", "Illegal seek", "Read-only file system",
  "Too many links", "Broken pipe", "Numerical argument out of domain",
  "Numerical result out of range" };
char *strerror(int e) {
  if (e > 0 && e <= 34) return (char *) __errs[e];
  static char b[24]; snprintf(b, sizeof b, "error %d", e); return b; }
