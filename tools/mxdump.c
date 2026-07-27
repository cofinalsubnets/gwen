// mxdump.c -- dump love.c's +/* dispatch matrices as DATA: one love form per
// table, kind names and lane names in enum order. tools/mx2coq.l reads the
// forms and generates proof/rocq/mx.v, where the tables' SHAPE becomes
// machine-checked (band factorization, dispatch commutativity -- test_mx).
//
// The matrices and their lane aps are static in love.c by design, so this TU
// includes the core WHOLE -- the tables are read out of the same compilation
// the narrative describes, and nothing in the core changes for the dump.
// Compiled and run at gate time only; an unrecognized lane pointer prints
// UNKNOWN, which the generator refuses loudly.
#include "../love.c"

int printf(char const *restrict, ...);

// the five host-seam symbols love.c imports (host/*.c provides them in a real
// frontend); main below only READS the matrices, so inert stubs satisfy the link
struct ai_port_vt const ai_fd_port_vt;
struct ai_io ai_stdin, ai_stdout, ai_stderr;
uintptr_t ai_clock(void) { return 0; }

static char const *const kname[KN] = {
 "KMint", "KNom", "KCharm", "KWide", "KFlo", "KCplx", "KBig", "KVec",
 "KArrZ", "KArrR", "KArrC", "KArrO", "KString", "KChain", "KMap", "KHot" };

static char const *lname(lvm_t *f) {
 return f == lvm_addn ? "addn" : f == lvm_add_string ? "add_string"
      : f == lvm_add_seq ? "add_seq" : f == lvm_addh ? "addh"
      : f == lvm_0 ? "zero" : f == lvm_bin_unit ? "bin_unit"
      : f == lvm_muln ? "muln"
      : f == lvm_mul_rep ? "mul_rep" : f == lvm_mul_cart ? "mul_cart"
      : f == lvm_mulh ? "mulh" : "UNKNOWN"; }

static void grid(char const *nm, lvm_t *const mx[KN][KN]) {
 printf("(%s", nm);
 for (int i = 0; i < KN; i++) {
  printf("\n (");
  for (int j = 0; j < KN; j++) printf(j ? " %s" : "%s", lname(mx[i][j]));
  printf(")"); }
 printf(")\n"); }

int main(void) {
 printf("(kinds");
 for (int i = 0; i < KN; i++) printf(" %s", kname[i]);
 printf(")\n");
 grid("add", ai_add_mx);
 grid("mul", ai_mul_mx);
 return 0; }
