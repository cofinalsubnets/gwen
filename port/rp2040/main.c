// Raspberry Pi Pico (RP2040) firmware -- every C file compiled by mooncc -t
// thumb1 (Cortex-M0+, ARMv6-M). No Pico SDK, no CMake, no vendor headers.
//
// NOT love, for the same reason the nucleo446 is not: the 264 KB SRAM is under
// the arena even a prel-only bake wants, so this is the TOOLCHAIN on silicon.
// v6-M is the LEANEST target this compiler has -- no FPU at all (every float
// and double is a libcall), no hardware divide (so is every / and %), and only
// the low registers for most instructions. test_mps2_t1 runs that ISA under
// qemu's M7; this port is the same lane on the chip it was written for.
//
// Boot, first-light the LED, banner the clock over UART0, then run the battery
// and let the LED say how it went -- slow blink all green, fast blink a miss.
// The console is UART0 on GPIO0(TX)/GPIO1(RX) at 115200 8N1, reachable over any
// USB-serial adapter.
//
// ⚠ what this port does NOT yet have is a LINK: the boot2 stage wants its CRC
// stamped and the .uf2 packed (tools/, still in the pre-rename .g dialect), and
// thumb relocations are not in holo's linker. `make test_embed` compiles it;
// nothing links it. See port/rp2040/Makefile.
#include <stdint.h>
#include "rp2040.h"

static void puts_(const char *s) {
  for (; *s; s++) { if (*s == '\n') serial_putc('\r'); serial_putc(*s); } }

static void putu(uint32_t v) {
  char b[10]; int n = 0;
  do { b[n++] = '0' + (char)(v % 10u); v /= 10u; } while (v);
  while (n) serial_putc(b[--n]); }

// --- the battery ----------------------------------------------------------
// volatile inputs keep every operand a runtime value, so nothing here folds at
// compile time into the answer it is supposed to be computing.
static volatile double da = 2.5, db = -1.25, one = 1.0, two = 2.0, big = 1e9;
static volatile float ff = 1.5f;
static volatile long long p64 = 0x123456789ABLL;
static volatile unsigned long long u64 = 0xFEDCBA9876543210ULL;
static volatile int i7 = 7, i2 = 2, i1000 = 1000, im7 = -7;
static volatile unsigned u9 = 900000007u;

double am_sin(double), am_cos(double), am_sqrt(double), am_exp(double),
       am_log(double), am_atan2(double, double);

struct zn { double re, im; };
static struct zn zmake(double re, double im) {
  struct zn z; z.re = re; z.im = im; return z; }         // memory-returned
static double znorm(struct zn z) { return z.re*z.re + z.im*z.im; }

static int run(void) {
  int ok = 0;
#define CK(x) do { ok++; \
    puts_("; check "); putu((uint32_t)ok); \
    if (x) puts_(" ok\n"); else { puts_(" FAIL\n"); return 100 + ok; } } while (0)
  // v6-M has NO divide instruction: every one of these is an __aeabi_ call
  CK(i1000 / i7 == 142);
  CK(i1000 % i7 == 6);
  CK(im7 / i2 == -3);                  // C truncates toward zero, not down
  CK(im7 % i2 == -1);
  CK(u9 / 7u == 128571429u);
  CK(u9 % 7u == 4u);
  // and no FPU: doubles are the full soft-float set
  CK(da + db == 1.25);
  CK(da - db == 3.75);
  CK(da * db == -3.125);
  CK(da / db == -2.0);
  CK(da > db);
  CK(db < 0.0);
  CK((long long)(da * 4.0) == 10);
  CK((double)i7 / (double)i2 == 3.5);
  // bare floats soften too (no FPv4 to fall back on, unlike the nucleo)
  CK(ff * ff == 2.25f);
  CK((double)ff == 1.5);
  CK(ff + ff == 3.0f);
  // 64-bit pairs on an 8-register machine
  CK(u64 % 1000ULL == 720ULL);
  CK(-p64 == -1250999896491LL);
  CK(p64 * 3 == 3752999689473LL);
  CK(u64 >> 16 == 0xFEDCBA987654ULL);
  CK((long long)u64 == -81985529216486896LL);
  CK(u64 / 1000ULL == 18364758544493064ULL);
  // the am math floor, bit-exact (the values harnessam.c pins against gcc)
  CK(am_sin(one) == 0.8414709848078965);
  CK(am_cos(two) == -0.41614683654714241);
  CK(am_sqrt(two) == 1.4142135623730951);
  CK(am_exp(one) == 2.7182818284590455);
  CK(am_log(two) == 0.69314718055994529);
  CK(am_atan2(one, two) == 0.46364760900080609);
  CK(am_sin(big) == 0.54584344944869956);      // the big-argument reduction
  // a composite through memory, the shape AAPCS returns via the sret pointer
  { struct zn z = zmake(da, db);
    CK(z.re == da && z.im == db);
    CK(znorm(z) == da*da + db*db); }
  return ok; }   // 32

int main(void) {
  // first light before anything else: a board that dies later still shows the
  // vector table + crt0 + clock bring-up worked.
  gpio_init(LED_PIN); gpio_set_dir(LED_PIN, 1); gpio_put(LED_PIN, 1);
  serial_init();
  puts_("\n; mooncc/rp2040 (cortex-m0+, thumb1)\n");
  int r = run();
  puts_(r <= 100 ? "; all " : "; FAILED at ");
  putu((uint32_t)(r <= 100 ? r : r - 100));
  puts_(r <= 100 ? "/32 green\n" : "\n");
  // the LED is the status channel when no terminal is watching.
  uint32_t half = r <= 100 ? 500u : 120u;
  for (;;) {
    gpio_put(LED_PIN, 1); { uint32_t t = clock_ms(); while (clock_ms() - t < half) {} }
    gpio_put(LED_PIN, 0); { uint32_t t = clock_ms(); while (clock_ms() - t < half) {} } } }
