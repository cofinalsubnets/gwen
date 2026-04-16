#include "i.h"
struct g_in g_stdin = { .getc = (void*) ggetc, .ungetc = (void*) gungetc, .eof = (void*) geof };
struct g_out g_stdout = { .putc = (void*) gputc, .flush = (void*) gflush };
static struct g *grbufn(struct g *f) {
 if (g_ok(f = g_have(f, str_type_width + 2))) {
  union u *k = bump(f, str_type_width + 1);
  *--f->sp = word(k);
  struct g_vec *o = (struct g_vec*) k;
  ini_str(o, sizeof(intptr_t)); }
 return f; }

static struct g *grbufg(struct g *f) {
 size_t len = len(f->sp[0]),
        req = str_type_width + 2 * b2w(len);
 if (g_ok(f = g_have(f, req))) {
  struct g_vec *o = bump(f, req);
  ini_str(o, 2 * len);
  memcpy(txt(o), txt(f->sp[0]), len);
  f->sp[0] = (word) o; }
 return f; }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int g_r_getc(struct g*f, struct g_in *i) {
 for (int c;;) switch (c = i->getc(f, i)) {
  default: return c;
  case '#': case ';':
   while (!i->eof(f, i) && (c = i->getc(f, i)) != '\n' && c != '\r');
  case 0: case ' ': case '\t': case '\n': case '\r': case '\f':
   continue; } }

long strtol(char const*restrict, char**restrict, int);

struct g *g_read1(struct g*f, struct g_in *i) {
 if (!g_ok(f)) return f;
 int c = g_r_getc(f, i);
 switch (c) {
  case '(':  return g_reads(f, i);
  case ')': case EOF:  return encode(f, g_status_eof);
  case '\'': return
   f = gxr(g_push(g_read1(f, i), 1, g_nil)),
   g_ok(f) ? gxl(g_push(f, 1, f->quote)) : f;
  case '"': {
   size_t n = 0;
   f = grbufn(f);
   for (size_t lim = sizeof(word); g_ok(f); f = grbufg(f), lim *= 2)
    for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
     if ((c = i->getc(f, i)) == EOF || c == '"' ||
         (c == '\\' && (c =i->getc(f, i)) == EOF))
      return len(b) = n, f;
   return f; } }

 uintptr_t n = 1, lim = sizeof(intptr_t);
 if (g_ok(f = grbufn(f)))
  for (txt(f->sp[0])[0] = c; g_ok(f); f = grbufg(f), lim *= 2)
   for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
    switch (c = i->getc(f, i)) {
     default: continue;
     case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
     case '(': case ')': case '"': case '\'': case 0 : case EOF:
      i->ungetc(f, c, i);
      len(b) = n;
      txt(b)[n] = 0; // zero terminate for strtol ; n < lim so this is safe
      char *e;
      long j = strtol(txt(b), &e, 0);
      if (*e == 0) f->sp[0] = gputnum(j);
      else f = g_intern(f);
      return f; }
 return f; }

struct g *g_reads(struct g *f, struct g_in* i) {
 intptr_t n = 0;
 for (int c; g_ok(f); n++) {
  c = g_r_getc(f, i);
  if (c == EOF || c == ')') break;
  i->ungetc(f, c, i);
  f = g_read1(f, i); }
 for (f = g_push(f, 1, g_nil); n--; f = gxr(f));
 return f; }

g_vm(g_vm_read) {
 switch (Pack(f), g_code_of(f = g_read1(f, &g_stdin))) {
  default: return f;
  case g_status_eof:
   f = g_core_of(f); // nothing to read
   Unpack(f);
   Ip += 1;
   return Continue();
  case g_status_ok:
   Unpack(f);
   Sp[1] = Sp[0];
   Sp += 1;
   Ip += 1;
   return Continue(); } }

g_vm(g_vm_getc) {
 Pack(f);
 int i = ggetc(f);
 Unpack(f);
 Sp[0] = gputnum(i);
 Ip += 1;
 return Continue(); }
