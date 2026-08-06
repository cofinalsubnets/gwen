/* love_data.ld -- pin the data sentinels' layout (love.c's DSENT, love.h's ai_typ).

   Each sentinel lays in love_data.N, N its enum d value. The `. = __start + N*16`
   after each KEEP pins the stride at love.h's ai_data_stride whatever the bodies
   measure, so love.h reads the kind as a shift and never has to reflect the real
   one. A body that outgrew the stride would push the location counter backwards --
   ld refuses that out loud, which is the check. ⚠ the base takes the STRIDE's
   own alignment, not a lesser one: the sentinels carry aligned(ai_data_stride), so
   a base under it lets ld bump the counter past the slot it was placing.

   The output name leads with a dot, so it is no C identifier and ld defines no
   bounds for it; the two here are the ASSERT's, not C's. love.h takes lvm_sym --
   slot 0 -- as its base, so no link owes a synthesized bracket anywhere.

   The lone INSERT augments the default script instead of replacing it, so this
   drops onto a host link as a plain -T fragment. A seat with its own full script
   (port/inle/*.lds, the board .lds) carries an equivalent block; holo needs none --
   it lays each section on the grain the object declares, which is the same tiling. */
SECTIONS {
  .love_data : ALIGN(16) {
    __start_love_data = .;
    KEEP(*(love_data.0))  . = __start_love_data + 16;
    KEEP(*(love_data.1))  . = __start_love_data + 32;
    KEEP(*(love_data.2))  . = __start_love_data + 48;
    KEEP(*(love_data.3))  . = __start_love_data + 64;
    KEEP(*(love_data.4))  . = __start_love_data + 80;
    KEEP(*(love_data.5))  . = __start_love_data + 96;
    KEEP(*(love_data.6))  . = __start_love_data + 112;
    KEEP(*(love_data.7))  . = __start_love_data + 128;
    KEEP(*(love_data.8))  . = __start_love_data + 144;
    __stop_love_data = .;
  }
} INSERT AFTER .text;
ASSERT(__stop_love_data - __start_love_data == 9 * 16,
       "love_data: slot count moved -- sync love.h's ai_data_n and this script")
