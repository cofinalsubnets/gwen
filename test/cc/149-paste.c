/* C11 6.10.3.3 -- ## with an EMPTY operand. An argument with no preprocessing tokens
 * becomes a placemarker: it pastes to whatever it meets, two of them paste to another,
 * and every one left over is deleted before the result is rescanned.
 *
 * Ours had no placemarker, so an empty operand left the ## unfolded. `A ## B` with B
 * empty emitted a literal ## into the C stream, and `A ## B ; bob` pasted A with the
 * `;` -- a join that cannot relex, which kept A and ATE the semicolon. The macros with
 * tokens on either side of the paste are the ones that hold that second half.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern int printf(const char *, ...);

#define CAT(A, B) A##B
#define THREE(A, B, C) A##B##C
#define VCAT(A, ...) A##__VA_ARGS__
#define TAIL(A, B) sink = A##B ; tail
#define LEAD(A, B) lead = 1 ; A##B

int main(void) {
  int ok = 0;
  int jim = 5, jimbo = 7;

  /* an empty RIGHT operand is the left token alone, an empty LEFT the right token */
  if (CAT(jim, ) == 5) ok++;
  if (CAT(, jim) == 5) ok++;
  /* both empty: the paste is nothing at all, so this needs an expression around it */
  { int n = 1 CAT(, ) + 1; if (n == 2) ok++; }
  /* ..and a paste with both operands present still joins */
  if (CAT(jim, bo) == 7) ok++;
  /* a three-way paste with the middle empty: two placemarkers fold away in turn */
  if (THREE(jim, , bo) == 7) ok++;
  /* an empty variadic tail is a placemarker like any other argument */
  if (VCAT(jim) == 5) ok++;
  if (VCAT(jim, bo) == 7) ok++;

  /* the tokens AFTER the paste survive it -- the semicolon is the whole point */
  { int sink = 0, tail = 0; TAIL(jim, ) = 4;   if (sink == 5 && tail == 4) ok++; }
  { int sink = 0, tail = 0; TAIL(jim, bo) = 9; if (sink == 7 && tail == 9) ok++; }
  /* ..and the tokens BEFORE it do too */
  { int lead = 0, zed = 0; LEAD(zed, ) = 6;    if (lead == 1 && zed == 6) ok++; }

  printf("jim=%d jimbo=%d ok=%d\n", jim, jimbo, ok);
  return ok;
}
