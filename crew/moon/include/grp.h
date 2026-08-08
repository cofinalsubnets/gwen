#ifndef _AI_GRP_H
#define _AI_GRP_H
/* freestanding grp.h for cc: the group-database entry + its lookups. */
#include <sys/types.h>

struct group {
	char  *gr_name;   /* group name */
	char  *gr_passwd; /* password */
	gid_t  gr_gid;    /* group id */
	char **gr_mem;    /* null-terminated member list */
};

struct group *getgrnam(char const*);
struct group *getgrgid(gid_t);
struct group *getgrent(void);
void          setgrent(void);
void          endgrent(void);
int setgroups(size_t, gid_t const*);   /* the supplementary list, dropped before a setuid */
#endif
