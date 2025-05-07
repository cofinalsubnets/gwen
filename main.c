#include "p.h"
#include <getopt.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#define Ok PStatusOk
#define Eof PStatusEof
#define Oom PStatusOom

static PStatus catf(PCore*f, p_file in, p_file out, bool *y) {
  for (;;) {
    PStatus s = p_read1f(f, in);
    if (s == Eof) return Ok;
    if (s != Ok) return s;
    if (*y) putchar(' ');
    else *y = true;
    p_write1f(f, out); } }

static PStatus expcat(PCore*f, char **av, bool usestdin) {
  PStatus s = Ok;
  bool y = false;
  while (s == Ok && *av) {
    p_file in = fopen(*av++, "r");
    if (!in) s = Oom;
    else s = catf(f, in, stdout, &y), fclose(in); }
  if (s == Ok && usestdin)
    s = catf(f, stdin, stdout, &y);
  return s; }

static const char *help = // help message
  "usage: %s [options] [files]\n"
  "options:\n"
  "  -h show this message and exit\n"
  "  -i read from stdin (default if no files given)\n"
  "  -c cat s-expressions from files and/or stdin\n"
  ;

static FILE *try_open(char *nom) {
  FILE *file = fopen(nom, "r");
  if (!file) fprintf(stderr, "# error opening %s: %s\n", nom, strerror(errno));
  return file; }

static PStatus run_file(PCore*f, p_file in) {
  PStatus s;
  // evaluate expressions for side effects
  while ((s = p_read1f(f, in)) == Ok && (s = p_eval(f)) == Ok)
    p_drop(f, 1);
  return s == Eof ? Ok : s; }

static PStatus p_repl(PCore*f, p_file in, p_file out) {
  for (PStatus s;;) {
    fprintf(out, ">>> ");
    if ((s = p_read1f(f, in)) == Eof) return Ok;
    if (s == Ok && (s = p_eval(f)) == Ok)
      p_write1f(f, out),
      fprintf(out, "\n"),
      p_drop(f, 1); } }

static PStatus run(PCore*f, char **av, bool usestdin) {
  PStatus s = Ok;
  for (; s == Ok && *av; av++) {
    FILE *file = try_open(*av);
    if (!file) return Eof;
    s = run_file(f, file);
    fclose(file); }
  if (s != Ok || !usestdin) return s;
  if (isatty(STDIN_FILENO)) return p_repl(f, stdin, stdout);
  return run_file(f, stdin); }

int main(int ac, char **av) {
  bool usestdin = false, cat = false;
  // read command line flags
  for (;;) switch (getopt(ac, av, "chi")) {
    default: return EXIT_FAILURE;
    case 'c': cat = true; continue;
    case 'h': fprintf(stdout, help, *av); return EXIT_SUCCESS;
    case 'i': usestdin = true; continue;
    case -1: goto out; } out:
  av += optind;
  usestdin = usestdin || ac == optind;
  PCore *f = p_open();
  PStatus s = f ? (cat ? expcat : run)(f, av, usestdin) : Oom;
  p_close(f);
  return s; }
