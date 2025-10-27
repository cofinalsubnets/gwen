#include <stdint.h>
#include <stdarg.h>
struct cb {
  uint32_t rows, cols, row, col, flag;
  uint8_t cb[]; };

void
cb_log(struct cb*, const char*),
cb_put_char(struct cb*, char),
  cb_vlogf(struct cb*, const char*, va_list),
  cb_log_n(struct cb*, uintptr_t n, uintptr_t base),
  cb_fill(struct cb*, uint8_t),
     cb_cur(struct cb*, uint32_t row, uint32_t col),
     cb_mv_cur(struct cb*, uint32_t, uint32_t);

