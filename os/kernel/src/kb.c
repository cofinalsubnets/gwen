#include "k.h"
#include "log.h"

#define kb_code_lshift 0x2a
#define kb_code_rshift 0x36
#define kb_code_extend 0xe0
#define kb_code_delete 0x53
#define kb_code_ctl 0x1d
#define kb_code_alt 0x38
#define kb_flag_rshift 1
#define kb_flag_lshift 2
#define kb_flag_rctl   4
#define kb_flag_lctl   8
#define kb_flag_ralt   16
#define kb_flag_lalt   32
#define kb_flag_extend 128
#define kb_flag_alt (kb_flag_lalt|kb_flag_ralt)
#define kb_flag_ctl (kb_flag_lctl|kb_flag_rctl)
#define kb_flag_shift (kb_flag_lshift|kb_flag_rshift)

static const uint8_t
  kb2ascii[128] = {
     0,  27, '1',  '2', '3', '4', '5', '6',
   '7', '8', '9',  '0', '-', '=',   8,   9,
   'q', 'w', 'e',  'r', 't', 'y', 'u', 'i',
   'o', 'p', '[',  ']',  10,   0, 'a', 's',
   'd', 'f', 'g',  'h', 'j', 'k', 'l', ';',
  '\'', '`',   0, '\\', 'z', 'x', 'c', 'v',
   'b', 'n', 'm',  ',', '.', '/',   0, '*',
     0, ' ' },
  shift_kb2ascii[128] = {
     0,  27, '!',  '@', '#', '$', '%', '^',
   '&', '*', '(',  ')', '_', '+',   8,   9,
   'Q', 'W', 'E',  'R', 'T', 'Y', 'U', 'I',
   'O', 'P', '{',  '}',  10,   0, 'A', 'S',
   'D', 'F', 'G',  'H', 'J', 'K', 'L', ':',
   '"', '~',   0,  '|', 'Z', 'X', 'C', 'V',
   'B', 'N', 'M',  '<', '>', '?',   0, '*',
     0, ' ' };

void kb_int(uint8_t code) {
  static uint8_t flags;
  if (code == kb_code_extend) {
    flags |= kb_flag_extend;
    return; }
  if (flags & kb_flag_extend) {
    flags &= ~kb_flag_extend;
    if (code < 128) switch (code) {
      case kb_code_alt: flags |= kb_flag_ralt; break;
      case kb_code_ctl: flags |= kb_flag_rctl; break;
      case kb_code_delete:
        if (flags & kb_flag_ctl & kb_flag_alt) k_reset();
        break;
      default: }
    else switch (code - 128) {
      case kb_code_alt: flags &= ~kb_flag_ralt; break;
      case kb_code_ctl: flags &= ~kb_flag_rctl; break;
      default: } }
  else {
    if (code < 128) switch (code) {
      case kb_code_lshift: flags |= kb_flag_lshift; break;
      case kb_code_rshift: flags |= kb_flag_rshift; break;
      case kb_code_alt:    flags |= kb_flag_lalt;   break;
      case kb_code_ctl:    flags |= kb_flag_lctl;   break;
      default: k_log_char((flags & (kb_flag_lshift | kb_flag_rshift) ? shift_kb2ascii : kb2ascii)[code]); }
    else switch (code - 128) {
      case kb_code_lshift: flags &= ~kb_flag_lshift; break;
      case kb_code_rshift: flags &= ~kb_flag_rshift; break;
      case kb_code_alt:    flags &= ~kb_flag_lalt;   break;
      case kb_code_ctl:    flags &= ~kb_flag_lctl;   break;
      default: } } }

