int main(void) { int s = 0;
  if (1) s += 20; else e0: s += 100;   /* the then-arm must not fall into the labeled else */
  if (0) x0: s += 100;                 /* a labeled then-arm stays under its test */
  do d0: s += 22; while (0);
  return s; }
