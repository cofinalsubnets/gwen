#include "p.h"
#include "boot.h"
int main(int ac, const char **av) {
  return p_main(boot_prog, av); }
