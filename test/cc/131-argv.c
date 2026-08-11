/* a LIBC-FREE main(argc, argv): the weak crt0 tail must unpack the OS stack
 * (argc at [sp], argv right after) -- no nolibc pulled, so the weak __ai_start
 * is exactly what runs. the battery invokes with no arguments. */
int main(int argc, char **argv) {
  if (argc != 1) return 1;
  if (!argv) return 2;
  if (!argv[0]) return 3;
  if (argv[0][0] == 0) return 4;
  if (argv[1] != 0) return 5;
  return 42;
}
