#ifndef _AI_DIRENT_H
#define _AI_DIRENT_H
typedef struct __dirstream DIR;   /* opaque; glibc owns it */
struct dirent {
  unsigned long  d_ino;
  long           d_off;
  unsigned short d_reclen;
  unsigned char  d_type;
  char           d_name[256];
};
#define DT_UNKNOWN 0
#define DT_FIFO    1
#define DT_CHR     2
#define DT_DIR     4
#define DT_BLK     6
#define DT_REG     8
#define DT_LNK    10
#define DT_SOCK   12
DIR *opendir(char const*);
DIR *fdopendir(int);
struct dirent *readdir(DIR*);
int closedir(DIR*);
int dirfd(DIR*);
#endif
