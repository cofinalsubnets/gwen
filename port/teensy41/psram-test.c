// PSRAM smoke test -- a bake-free standalone main (the bringup ledger's
// fast-iteration pattern): probe the FlexSPI2 PSRAM, print the chip ids and
// size, then pattern-test the whole mapped span. Serial is the report
// channel (LPUART6, 115200); the LED seals the verdict: slow blink = PASS,
// solid = FAIL, fast blink = no PSRAM found. Built by `make -C port/teensy41
// psramtest` (mooncc, same objects as the love build minus main.o).
#include <stdint.h>
#include "teensy41.h"
#include "psram.h"

#define PSRAM ((volatile uint32_t *) 0x70000000u)

static void puts_(const char *s) { while (*s) serial_putc(*s++); }
static void puthex(uint32_t v) {
  for (int i = 28; i >= 0; i -= 4) serial_putc("0123456789abcdef"[(v >> i) & 15]); }
static void putdec(uint32_t v) {
  char b[12]; int n = 0;
  do { b[n++] = '0' + v % 10; v /= 10; } while (v);
  while (n) serial_putc(b[--n]); }

// main.c is absent from this build; ai_clock lives in teensy41.c
uintptr_t ai_clock(void);
static void sleep_ms(uintptr_t ms) {
  uintptr_t start = ai_clock();
  while (ai_clock() - start < ms) ; }

static void blink_forever(uint32_t ms) {
  for (;;) {
    gpio_put(LED_BIT, 1); sleep_ms(ms);
    gpio_put(LED_BIT, 0); sleep_ms(ms); } }

int main(void) {
  gpio_init(LED_BIT); gpio_set_dir(LED_BIT, 1); gpio_put(LED_BIT, 1);
  puts_("\r\n; psram probe\r\n");
  uint32_t mb = psram_init();
  puts_("; chip0 id "); puthex(psram_chip_id(0));
  puts_("  chip1 id "); puthex(psram_chip_id(1));
  puts_("\r\n; size "); putdec(mb); puts_(" MB\r\n");
  if (mb == 0) { puts_("; NO PSRAM -- check solder\r\n"); blink_forever(120); }

  uint32_t words = (mb << 20) / 4;
  uint32_t bad = 0, first_bad = 0, first_want = 0, first_got = 0;
  // pass 1: address-tagged word every 1 KB across the whole span (fast, hits
  // both chips and the chip seam), then read back
  for (uint32_t i = 0; i < words; i += 256) PSRAM[i] = 0xA5000000u + i;
  for (uint32_t i = 0; i < words; i += 256) {
    uint32_t got = PSRAM[i], want = 0xA5000000u + i;
    if (got != want && !bad++) { first_bad = i; first_want = want; first_got = got; } }
  // pass 2: dense walk over the first and last 64 KB (bus integrity + the
  // top of the second chip), inverted tag so pass 1 residue can't fake it
  for (uint32_t i = 0; i < 16384; i++) {
    PSRAM[i] = ~(0x5A000000u + i);
    PSRAM[words - 1 - i] = ~(0xC3000000u + i); }
  for (uint32_t i = 0; i < 16384; i++) {
    uint32_t got = PSRAM[i], want = ~(0x5A000000u + i);
    if (got != want && !bad++) { first_bad = i; first_want = want; first_got = got; }
    got = PSRAM[words - 1 - i]; want = ~(0xC3000000u + i);
    if (got != want && !bad++) { first_bad = words - 1 - i; first_want = want; first_got = got; } }

  if (bad) {
    puts_("; FAIL "); putdec(bad); puts_(" bad words, first @ word ");
    puthex(first_bad); puts_(" want "); puthex(first_want);
    puts_(" got "); puthex(first_got); puts_("\r\n");
    gpio_put(LED_BIT, 1);
    for (;;) ; }
  puts_("; PASS "); putdec(mb); puts_(" MB clean\r\n");
  // heartbeat + echo: prove BOTH wire directions continuously
  { uint32_t t = 0; uintptr_t last = ai_clock();
    puts_("; echo ready\r\n");
    for (;;) {
      if (serial_rx_ready()) {
        int c = serial_getc();
        puts_("; heard "); puthex((uint32_t) c); puts_("\r\n"); }
      if (ai_clock() - last >= 2000) {
        last = ai_clock();
        puts_("; tick "); putdec(t++); puts_("\r\n");
        gpio_put(LED_BIT, t & 1); } } } }
