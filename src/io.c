#include "i.h"

void transmit(state v, FILE* o, word x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (datp((verb) x)) gettyp(x)->emit(v, o, x);
  else fprintf(o, "#%lx", x); }

// parser

// internal parser functions
static str
  rx_atom_chars(state, FILE*),
  rx_str(state, FILE*);
static word
  rx_ret(state, FILE*, word),
  rxr(state, FILE*),
  rx_two(state, FILE*),
  rx_atom(state, str),
  rx_q(state, FILE*);

static Inline word pull(state v, FILE *i, word x) { return
  ((word (*)(state, FILE*, word)) pop1(v))(v, i, x); }

// FIXME should distinguish between OOM and parse error
status receive(state v, FILE *i) {
  word x; return
    !push1(v, (word) rx_ret) ? OomError :
    !(x = rxr(v, i)) ? feof(i) ? Eof : DomainError :
    push1(v, x) ? Ok : OomError; }

status NoInline receive2(state f, const char *_i) {
  size_t len = strlen(_i);
  char *i = malloc(len + 1);
  if (!i) return OomError;
  memcpy(i, _i, len);
  i[len] = 0;
  FILE *in = fmemopen(i, len, "r");
  if (!in) return free(i), OomError;
  status s = receive(f, in);
  return fclose(in), free(i), s; }

////
/// " the parser "
//
// simple except it uses the managed stack for recursion.

// get the next token character from the stream
static NoInline int rx_char(FILE *i) {
  for (int c;;) switch (c = getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (getc(i)) {
      case '\n': case EOF: return rx_char(i); } } }

static word rx_ret(state v, FILE* i, word x) { return x; }

static word rx_two_cons(state v, FILE* i, word x) {
  word y = pop1(v);
  return pull(v, i, x ? (ob) pair(v, y, x) : x); }

static word rx_two_cont(state v, FILE* i, word x) { return
  !x || !push2(v, (word) rx_two_cons, x) ? pull(v, i, 0) : rx_two(v, i); }

static ob rx_q_cont(state f, FILE *i, word x) {
  if (x && (x = (word) pair(f, x, nil)) && (x = (word) pair(f, nil, x)) && push1(f, x)) {
    str s = strof(f, Quote);
    x = pop1(f);
    if (!s) x = 0;
    else A(x) = (ob) s; }
  return pull(f, i, x); }

static NoInline word rxr(state v, FILE* i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(v, i, 0);
    case '(': return rx_two(v, i);
    case '"': return pull(v, i, (word) rx_str(v, i));
    case '\'': return rx_q(v, i);
    default: ungetc(c, i);
             str a = rx_atom_chars(v, i);
             word x = a ? rx_atom(v, a) : 0;
             return pull(v, i, x); } }

static ob rx_q(state f, FILE *i) {
  return push1(f, (word) rx_q_cont) ? rxr(f, i) : pull(f, i, 0); }

static ob rx_two(state l, FILE* i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(l, i, nil);
    default: return ungetc(c, i),
      push1(l, (word) rx_two_cont) ? rxr(l, i) : pull(l, i, 0); } }

static str buf_new(state f) {
  str s = cells(f, Width(struct str) + 1);
  return s ? str_ini(s, sizeof(ob)) : s; }

static NoInline str buf_grow(state f, str s) {
  str t; size_t len = s->len; return
    avec(f, s, t = cells(f, Width(struct str) + 2 * b2w(len))),
    !t ? t : (memcpy(t->text, s->text, len),
              str_ini(t, 2 * len)); }
  
// read the contents of a string literal into a string
static NoInline str rx_str(li v, FILE* p) {
  str o = buf_new(v);
  for (size_t n = 0, lim = sizeof(ob); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = getc(p)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, o; }
  return 0; }
// read the characters of an atom (number or symbol)
// into a string
static NoInline str rx_atom_chars(state v, FILE* p) {
  str o = buf_new(v);
  for (size_t n = 0, lim = sizeof(word); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      default: o->text[n++] = x; continue;
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': ungetc(x, p);
      case EOF: return o->len = n, o; }
  return 0; }

#include <ctype.h>
static NoInline word rx_atom_n(state v, str b, size_t inset, int sign, int rad) {
  static const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
  size_t len = b->len;
  if (inset >= len) fail: return (ob) b;
  intptr_t out = 0;
  do {
    int dig = 0, c = tolower(b->text[inset++]);
    while (digits[dig] && digits[dig] != c) dig++;
    if (dig >= rad) goto fail;
    out = out * rad + dig;
  } while (inset < len);
  return putnum(sign * out); }

static NoInline word rx_atom(state v, str b) {
  intptr_t i = 0, len = b->len, sign = 1;
  while (i < len) switch (b->text[i]) {
    case '+': i += 1; continue;
    case '-': i += 1, sign *= -1; continue;
    case '0': if (i+1 < len) {
      const char *r = "b\2s\6o\10d\12z\14x\20n\44";
      for (char c = tolower(b->text[i+1]); *r; r += 2)
        if (*r == c) return rx_atom_n(v, b, i+2, sign, r[1]); }
    default: goto out; } out:
  return rx_atom_n(v, b, i, sign, 10); }
