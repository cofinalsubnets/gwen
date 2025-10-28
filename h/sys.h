#include <stdio.h>
struct fi { struct g_in in; FILE *file; };
struct g *g_readfi(struct g*, struct fi*);
int p_file_getc(struct g_in *i);
int p_file_ungetc(struct g_in *i, int c);
int p_file_eof(struct g_in *i);
