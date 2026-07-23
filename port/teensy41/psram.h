// FlexSPI2 external PSRAM (the Teensy 4.1 bottom pads, mapped 0x70000000).
// psram_init() brings the controller + up to two QPI chips up and answers
// total MB (0 / 8 / 16); psram_chip_id(0/1) answers the raw probe ids
// (0x..5D0D / 0x..5D9D = real chips; 0 or 0xFFFF.. = absent or bad solder).
#pragma once
#include <stdint.h>
uint32_t psram_init(void);
uint32_t psram_chip_id(int slot);
