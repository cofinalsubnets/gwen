#include <stdio.h>
#include "esp_timer.h"
#include "driver/uart_vfs.h"
#include "driver/uart.h"
#include "g.h"

g_noinline uintptr_t g_clock(void) {
  return esp_timer_get_time() / 1000; }
int gputc(struct g*, int c) { return putc(c, stdout); }
int ggetc(struct g*) { return getc(stdin); }
int gungetc(struct g*, int c) { return ungetc(c, stdin); }
int geof(struct g*) { return feof(stdin); }
int gflush(struct g*f) { return fflush(stdout); }

void app_main(void) {
  // console init
  setvbuf(stdin, NULL, _IONBF, 0);
  uart_driver_install(CONFIG_ESP_CONSOLE_UART_NUM, 256, 0, 0, NULL, 0);
  uart_vfs_dev_use_driver(CONFIG_ESP_CONSOLE_UART_NUM);
  uart_vfs_dev_port_set_rx_line_endings(CONFIG_ESP_CONSOLE_UART_NUM, ESP_LINE_ENDINGS_CR);
  uart_vfs_dev_port_set_tx_line_endings(CONFIG_ESP_CONSOLE_UART_NUM, ESP_LINE_ENDINGS_CRLF);

  struct g *f = g_ini();
  putc(g_code_of(f), stdout); }
