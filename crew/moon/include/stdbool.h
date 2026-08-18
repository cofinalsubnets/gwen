#ifndef _AI_STDBOOL_H
#define _AI_STDBOOL_H
/* mooncc predefines bool/true/false (cpp.l, C23-style), so this is a no-op there.
   ANOTHER compiler reading these headers under -nostdinc has no other stdbool --
   the KCC=clang kernel lane -- and C11 spells them here. bool is _Bool: one byte. */
#if !defined(__mooncc__) && (!defined(__STDC_VERSION__) || __STDC_VERSION__ < 202311L)
#define bool _Bool
#define true 1
#define false 0
#endif
#define __bool_true_false_are_defined 1
#endif
