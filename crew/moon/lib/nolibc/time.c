/* crew/moon/lib/nolibc/time.c -- the calendar: no timezone database, so
 * localtime IS gmtime (UTC). */
#include "impl.h"

/* ---- the calendar: no timezone database, so localtime IS gmtime (UTC). the
 * civil-from-days is Hinnant's exact integer algorithm (1970-01-01 = Thursday,
 * wday 4). asctime lays glibc's fixed 26-byte "Www Mmm dd hh:mm:ss yyyy\n". ---- */
static char __tzutc[4] = "UTC";
char *tzname[2] = { __tzutc, __tzutc };
long timezone = 0;
int daylight = 0;
void tzset(void) { }                               /* the zone is UTC and always was */
struct tm *gmtime(time_t const *tp) {
  static struct tm tm;
  long t = *tp;
  long days = t / 86400, secs = t % 86400;
  if (secs < 0) { secs += 86400; days -= 1; }
  tm.tm_hour = (int) (secs / 3600);
  tm.tm_min = (int) (secs % 3600 / 60);
  tm.tm_sec = (int) (secs % 60);
  tm.tm_wday = (int) (((days % 7) + 4 + 7) % 7);
  long z = days + 719468;
  long era = (z >= 0 ? z : z - 146096) / 146097;
  long doe = z - era * 146097;
  long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
  long y = yoe + era * 400;
  long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
  long mp = (5 * doy + 2) / 153;
  long d = doy - (153 * mp + 2) / 5 + 1;
  long m = mp < 10 ? mp + 3 : mp - 9;
  y += (m <= 2);
  tm.tm_year = (int) (y - 1900);
  tm.tm_mon = (int) (m - 1);
  tm.tm_mday = (int) d;
  tm.tm_yday = 0;
  tm.tm_isdst = 0;
  tm.tm_gmtoff = 0;
  tm.tm_zone = "UTC";
  return &tm; }
struct tm *localtime(time_t const *tp) { return gmtime(tp); }
static void __d2(char *p, int v) { p[0] = (char) (48 + v / 10 % 10); p[1] = (char) (48 + v % 10); }
double difftime(time_t a, time_t b) { return (double) (a - b); }
time_t mktime(struct tm *tm) {                     /* the exact inverse of gmtime (UTC -- no tz, like localtime) */
  long y = tm->tm_year + 1900, m = tm->tm_mon + 1, d = tm->tm_mday;
  y -= m <= 2;
  long era = (y >= 0 ? y : y - 399) / 400;
  long yoe = y - era * 400;
  long doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;   /* Hinnant days-from-civil */
  long doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
  long days = era * 146097 + doe - 719468;
  return days * 86400 + tm->tm_hour * 3600L + tm->tm_min * 60L + tm->tm_sec; }
char *asctime(struct tm const *tm) {
  static char b[26];
  static char const *wd = "SunMonTueWedThuFriSat";
  static char const *mo = "JanFebMarAprMayJunJulAugSepOctNovDec";
  int i;
  int w = tm->tm_wday, mn = tm->tm_mon, y = tm->tm_year + 1900;
  if (w < 0 || w > 6) w = 0;
  if (mn < 0 || mn > 11) mn = 0;
  for (i = 0; i < 3; i++) b[i] = wd[w * 3 + i];
  b[3] = ' ';
  for (i = 0; i < 3; i++) b[4 + i] = mo[mn * 3 + i];
  b[7] = ' ';
  __d2(b + 8, tm->tm_mday); if (b[8] == '0') b[8] = ' ';
  b[10] = ' ';
  __d2(b + 11, tm->tm_hour); b[13] = ':';
  __d2(b + 14, tm->tm_min);  b[16] = ':';
  __d2(b + 17, tm->tm_sec);  b[19] = ' ';
  __d2(b + 20, y / 100); __d2(b + 22, y % 100);
  b[24] = 10; b[25] = 0;
  return b; }
char *ctime(time_t const *tp) { return asctime(gmtime(tp)); }
/* strftime: the everyday conversions (lua's os.date; %c is the asctime lay).
 * unknown specifiers echo literally; answers 0 when the buffer runs out. */
size_t strftime(char *s, size_t max, char const *fmt, struct tm const *tm) {
  static char const *wdl = "Sunday\0   Monday\0   Tuesday\0  Wednesday\0Thursday\0 Friday\0   Saturday";
  static char const *mol = "January\0  February\0 March\0    April\0    May\0      June\0     "
                           "July\0     August\0   September\0October\0  November\0 December";
  size_t n = 0;
  char b[26];
  for (; *fmt; fmt++) {
    char const *p = 0;
    int v = -1, w = 2;
    if (*fmt != '%') { if (n + 1 >= max) return 0; s[n++] = *fmt; continue; }
    fmt++;
    switch (*fmt) {
      case 'Y': v = tm->tm_year + 1900; w = 4; break;
      case 'y': v = (tm->tm_year + 1900) % 100; break;
      case 'm': v = tm->tm_mon + 1; break;
      case 'd': v = tm->tm_mday; break;
      case 'H': v = tm->tm_hour; break;
      case 'M': v = tm->tm_min; break;
      case 'S': v = tm->tm_sec; break;
      case 'j': v = tm->tm_yday + 1; w = 3; break;
      case 'p': p = tm->tm_hour < 12 ? "AM" : "PM"; break;
      case 'a': memcpy(b, wdl + tm->tm_wday * 10, 3); b[3] = 0; p = b; break;
      case 'A': p = wdl + tm->tm_wday * 10; break;
      case 'b': memcpy(b, mol + tm->tm_mon * 10, 3); b[3] = 0; p = b; break;
      case 'B': p = mol + tm->tm_mon * 10; break;
      case 'c': { char *a = asctime(tm); memcpy(b, a, 24); b[24] = 0; p = b; break; }
      case 'x': { snprintf(b, sizeof b, "%02d/%02d/%02d", tm->tm_mon + 1, tm->tm_mday, (tm->tm_year + 1900) % 100); p = b; break; }
      case 'X': { snprintf(b, sizeof b, "%02d:%02d:%02d", tm->tm_hour, tm->tm_min, tm->tm_sec); p = b; break; }
      case '%': p = "%"; break;
      default:  b[0] = '%'; b[1] = *fmt; b[2] = 0; p = b; break; }
    if (v >= 0) { snprintf(b, sizeof b, w == 4 ? "%04d" : w == 3 ? "%03d" : "%02d", v); p = b; }
    if (p) { size_t l = strlen(p); if (n + l >= max) return 0; memcpy(s + n, p, l); n += l; } }
  if (n >= max) return 0;
  s[n] = 0;
  return n; }
