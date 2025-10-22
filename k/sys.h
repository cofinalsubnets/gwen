typedef int g_file;
#define g_stdin 0
#define g_stdout 1
#define g_stderr 2
void k_logf(const char*, ...), k_log_char(char);
#define g_fprintf(_, ...) k_logf(__VA_ARGS__)
#define g_fputc(_, ...) k_log_char(__VA_ARGS__)
#define EOF (-1)
static Inline g_core *g_run(g_core *f) {
  return !g_ok(f) ? f : f->ip->ap(f, f->ip, f->hp, f->sp); }
