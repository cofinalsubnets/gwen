/* tools/lit.c -- one text file to a C string literal, ONE SOURCE LINE PER C LINE.
   the bootstrap's escaper. love0 is assembled FROM the love sources, so it cannot
   lcat the very files it is made of, and this needs no interpreter -- only the CC that
   builds love0 anyway. mk/lib.mk's sed_h ran four substitutions for this; the C is
   the same four and drops sed from the build.
   ⚠ line-preserving on purpose: out/lib/<name>0.h diffs then read like their source,
   and a one-line edit stays a one-line diff. the lcat headers are the other shape --
   whole files minified onto ONE line -- which is why they are not made here.
   a file argument or stdin (tests0.h cats the corpus through a pipe). */
#include <stdio.h>

int main(int argc, char **argv) {
    FILE *f = argc > 1 ? fopen(argv[1], "rb") : stdin;
    if (!f) { fprintf(stderr, "lit: cannot read %s\n", argv[1]); return 1; }
    int c, open = 0;
    while ((c = fgetc(f)) != EOF) {
        if (!open) { fputc('"', stdout); open = 1; }
        /* the escapes, then the line's own close: sed's -e chain in order */
        if (c == '\n')      { fputs("\\n\"\n", stdout); open = 0; }
        else if (c == '\\') fputs("\\\\", stdout);
        else if (c == '"')  fputs("\\\"", stdout);
        else                fputc(c, stdout);
    }
    /* an unterminated last line still closes, and stays unterminated: sed prints its
       pattern space but adds no newline the input did not have */
    if (open) fputs("\\n\"", stdout);
    if (argc > 1) fclose(f);
    return fflush(stdout) || ferror(stdout);
}
