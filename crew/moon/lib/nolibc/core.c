/* crew/moon/lib/nolibc/core.c -- the floor every link takes: the syscall table
 * and its wrappers, errno, mem/string, malloc, env, stdio, exit and the entry.
 * The optional areas are members beside this one; see impl.h. */
#include "impl.h"

char **environ;
static long *__auxv;

/* ---- errno: one int (love is single-threaded), kernel -errno unwrapped ---- */
int __errno_v;
int *__errno_location(void) { return &__errno_v; }

/* the classic table (errno 1..34), the range real packages print; past it the
 * number speaks for itself. the texts are the canonical POSIX ones -- m4's
 * check suite string-compares "No such file or directory". */
/* rand/random: one 64-bit LCG (Knuth's MMIX multiplier), read off the high bits
 * where an LCG's period per bit is longest. rand takes 15, random 31. */



/* ---- exit: the atexit chain, the stdio flush, then exit_group ---- */


/* ---- malloc: K&R's first-fit free list over 1MB mmap arenas. 16-byte units,
 * 16-byte alignment, coalescing free. love mallocs pools (big, rare) and codec /
 * line buffers (small, freed) -- this shape covers both without ceremony. ---- */
typedef struct __mhdr { struct __mhdr *next; size_t size; } __mhdr;   /* size in units */

/* alloca: no native / __builtin form, so malloc backs it and stack depth
 * reclaims it. both arches grow DOWN, so a frame that has returned sits at a
 * HIGHER address than the current probe; on each call we free every block
 * whose mark sits BELOW `here` (its frame unwound past). blocks from the same
 * or an ancestor frame (mark >= here) stay. leak-free without a
 * stack-direction probe. */
/* argv[0], stashed by __ai_start below -- gnulib's progname module reaches for
 * this and would otherwise die at the link */
char const *__ai_progname = "";


/* ---- stdio: FILE is a fd plus (for write streams) a flush buffer. stdout is
 * the one hot stream -- love's fd_putc sends EVERY output byte through fputc, so
 * it buffers 8KB (line-flushed on a tty, glibc's shape); stderr never buffers;
 * fopen'd streams buffer 4KB. reads are unbuffered (image.c freads whole
 * files), which keeps fseek/ftell honest as plain lseek. ---- */
FILE *stdin = &__stdf[0], *stdout = &__stdf[1], *stderr = &__stdf[2];


/* the conversion flags, gathered off the % once so neither field function
 * takes a parameter per flag. */
/* ---- the exact decimal of a double --------------------------------------
 *
 * a double is m * 2^e with m a 53-bit INTEGER, so its decimal form is finite:
 * at most 309 digits before the point and 1074 after. bd[] holds every one of
 * them, nine to a limb in base 1e9, reached by doubling or halving the
 * mantissa -- so rounding and emission read EXACT digits and the answer is
 * byte-equal to glibc, which is what test_libc compares.
 *
 * ⚠ the lane this replaced turned digits out of the double itself, normalising
 * by repeated `/= 10`. that spends a rounding per decade, and the error lands
 * exactly where a long precision asks to read: %.17g of 1e300 came back wrong
 * from its 16th digit, %.20f of 0.1 answered twenty zeros where the value
 * carries ...00555, and a TIE could not be broken at all -- the residue that
 * decides it had already been rounded away, so %.0f of 2.5 said 3 where every
 * conforming printf says 2. an approximate converter cannot be gated against
 * an exact one; that is the whole reason this is a bignum and not a patch.
 */




/* ---- signals: glibc's 152-byte sigaction folded onto the kernel's 32-byte
 * one. BOTH arches carry the restorer slot (aarch64 is the odd asm-generic
 * arch that kept SA_RESTORER in its uapi) -- but only x86-64 needs it filled
 * (sys.o's __ai_sigret); aarch64 leaves flag+slot zero and the kernel lays
 * its vdso return trampoline. ---- */



/* ---- dirent over getdents64: the kernel record IS our struct dirent ---- */


/* its sibling, added for connect's handshake park: SO_ERROR after POLLOUT is the
   one way to tell a completed connect from a refused one, and love has no other
   door to it. sys/socket.h has always declared it. */

/* ---- the load bias: 0 for a fixed-base ET_EXEC, the ASLR slide for a -pie
 * ET_DYN. AT_PHDR is the runtime address of the program headers, which sit at
 * file offset 64 inside the p_offset==0 PT_LOAD, so bias = AT_PHDR - 64 - that
 * segment's link-time p_vaddr (0x400000 for EXEC -> 0; 0 for PIE -> the slide). ---- */
static unsigned long __ai_bias(void) {
  unsigned long phdr = 0; Elf64_Half phnum = 0;
  for (long *a = __auxv; a && a[0]; a += 2) {
    if (a[0] == 3) phdr = (unsigned long) a[1];        /* AT_PHDR */
    if (a[0] == 5) phnum = (Elf64_Half) a[1]; }        /* AT_PHNUM */
  if (!phdr) return 0;
  Elf64_Phdr const *ph = (Elf64_Phdr const *) phdr;
  for (Elf64_Half i = 0; i < phnum; i++)
    if (ph[i].p_type == PT_LOAD && ph[i].p_offset == 0)
      return phdr - 64 - (unsigned long) ph[i].p_vaddr;
  return 0; }

/* ---- -pie self-relocation. The linker (crew/holo/link.l) laid the exe at base 0
 * and left every abs64 data pointer holding its base-0 offset, plus a table of
 * those sites bracketed by __start_/__stop_ai_rela. Add the real load base to each
 * -- the whole of static-PIE relocation, no dynamic loader. Must run before any
 * such pointer is dereferenced (top of __ai_start). An ET_EXEC binary links an
 * EMPTY table (start == stop), so this is a no-op there. ---- */
extern long __start_ai_rela[], __stop_ai_rela[];
static void __ai_reloc(void) {
  unsigned long bias = __ai_bias();
  for (long *p = __start_ai_rela; p < __stop_ai_rela; p++)
    *(unsigned long *) (bias + (unsigned long) *p) += bias; }

/* ---- dl_iterate_phdr off the auxv (AT_PHDR/AT_PHNUM): one callback covers "the
 * main program", carrying the real load bias so image.c's bake walk bounds the
 * in-binary pointers correctly under -pie (0 for a fixed-base ET_EXEC). ---- */
int dl_iterate_phdr(int (*cb)(struct dl_phdr_info *, unsigned long, void *), void *data) {
  unsigned long phdr = 0, phnum = 0;
  for (long *a = __auxv; a && a[0]; a += 2) {
    if (a[0] == 3) phdr = (unsigned long) a[1];        /* AT_PHDR */
    if (a[0] == 5) phnum = (unsigned long) a[1]; }     /* AT_PHNUM */
  if (!phdr) return 0;
  struct dl_phdr_info in;
  memset(&in, 0, sizeof in);
  in.dlpi_addr = __ai_bias();
  in.dlpi_name = "";
  in.dlpi_phdr = (Elf64_Phdr const *) phdr;
  in.dlpi_phnum = (Elf64_Half) phnum;
  return cb(&in, sizeof in, data); }

/* ---- the entry: crt0 hands us the arg vector base (argc at [sp]); unpack
 * argv/envp/auxv, arm stdio, run main, exit with its answer. this STRONG
 * definition overrides crt0's weak call-main tail (the linker's weak machinery
 * is the whole switch -- no flags anywhere). ---- */
void __ai_start(long *sp) {
  __ai_osv = __ai_osdetect();   /* which kernel: settled before any other syscall */
  long argc = sp[0];
  char **argv = (char **) (sp + 1);
  char **e = argv + argc + 1;
  if (argc > 0 && argv[0]) __ai_progname = argv[0];   /* getprogname's answer */
  environ = e;
  while (*e) e++;
  __auxv = (long *) (e + 1);
  __ai_reloc();                 /* -pie: slide abs64 data pointers before any is used (no-op for ET_EXEC) */
  stdout->fd = 1;
  stdout->wr = 1;
  stdout->buf = __obuf;
  stdout->cap = sizeof __obuf;
  stdout->line = isatty(1);
  stderr->fd = 2;
  stderr->wr = 1;
  exit(main((int) argc, argv)); }

