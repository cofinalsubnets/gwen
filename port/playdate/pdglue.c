// the SDK half: the ONLY file that includes pd_api.h, gcc-compiled on device
// (mooncc parses no SDK header and, more to the point, the pd->* function
// pointers speak the hard-float SP ABI -- getCrankAngle returns a float in
// s0 -- while mooncc's floats ride widened-as-double d-regs. every crossing
// flattens to ints and pointers here, so the seam stays word-only and the
// moon side never sees a float or a variadic.)
#include "pd_api.h"
#include "pdglue.h"

_Static_assert(PDG_ROWSIZE == LCD_ROWSIZE, "LCD rowsize drifted");
_Static_assert(PDG_ROWS == LCD_ROWS, "LCD rows drifted");

static PlaydateAPI *PD;
static PDButtons b_current, b_pushed, b_released;

int pdg_crank_docked(void) { return PD->system->isCrankDocked(); }
int pdg_crank_deg(void) { return (int) PD->system->getCrankAngle() % 360; }
void pdg_poll_buttons(void) { PD->system->getButtonState(&b_current, &b_pushed, &b_released); }
unsigned pdg_pushed(void) { return (unsigned) b_pushed; }
void *pdg_realloc(void *p, size_t n) { return PD->system->realloc(p, n); }
unsigned pdg_ms(void) { return PD->system->getCurrentTimeMilliseconds(); }
void pdg_log(const char *s) { PD->system->logToConsole("%s", s); }
unsigned char *pdg_frame(void) { return PD->graphics->getFrame(); }
void pdg_mark_updated(void) { PD->graphics->markUpdatedRows(0, LCD_ROWS); }
void pdg_set_update(int (*cb)(void *)) { PD->system->setUpdateCallback(cb, NULL); }
// read a whole bundled data file into buf (<= cap bytes); -1 = no file.
// the wake image rides the pdx this way (kFileRead reads the bundle).
int pdg_file_read(const char *path, void *buf, unsigned cap) {
  SDFile *f = PD->file->open(path, kFileRead);
  if (!f) return -1;
  int n = PD->file->read(f, buf, cap);
  PD->file->close(f);
  return n; }

int eventHandler(PlaydateAPI *pd, PDSystemEvent event, uint32_t arg) {
  if (event != kEventInit) return 0;
  PD = pd;
  love_init();
  return 0; }
