/* stdarg.h -- crew/moon's own, for `mooncc`. cc implements the three __builtin_va_*
 * forms directly, over the target's real va_list -- the System V AMD64 24-byte
 * struct or the AAPCS64 32-byte one -- fed by cc's variadic prologue from a
 * register save area. both are an array of one so a va_list DECAYS to a pointer
 * when passed to another function (the C `va_list` habit -- love.c's gvzprintf/
 * ai_pushr take one), and va_arg mutates the shared state. the field LAYOUT
 * matches gcc's on each target, so a cc-compiled variadic function (ai_push) is
 * callable from gcc-built objects and vice versa. (on aarch64 gcc's va_list is
 * a bare struct passed by reference -- a composite > 16 bytes -- which is the
 * same wire as the array's decay: one pointer at the 32-byte struct.) the
 * offset fields are `int`: SysV's run 0..176, AAPCS64's are NEGATIVE -- counted
 * back from the top of each save area, rising toward 0. */
#ifndef _CC_STDARG_H
#define _CC_STDARG_H

/* under a real gcc/clang (the freestanding kernel lane rides these headers too)
 * the va_list type is the COMPILER'S: its __builtin_va_* insist on their own
 * __va_list_tag, so the hand layouts below would be rejected, not just wrong.
 * mooncc predefines neither __GNUC__ nor __clang__, so this forks clean. */
#if defined(__GNUC__) || defined(__clang__)
typedef __builtin_va_list va_list;
#else
#ifdef __aarch64__
typedef struct {
  void *__stack;                 /* the next anonymous arg on the caller stack */
  void *__gr_top;                /* one past the x0..x7 save area */
  void *__vr_top;                /* one past the q0..q7 save area */
  int __gr_offs;                 /* -(unnamed gp slots)*8, rising by 8 to 0 */
  int __vr_offs;                 /* -(unnamed vr slots)*16, rising by 16 to 0 */
} __va_list_tag;
#elif defined(__arm__)
typedef struct {                 /* AAPCS32: one running pointer -- the prologue
                                  * pushed r0-r3 contiguous with the caller's
                                  * stack args, so anonymous WORDS just walk */
  void *__ap;
} __va_list_tag;
#elif defined(__riscv)
typedef struct {                 /* LP64: one running pointer, 8-byte slots --
                                  * anonymous args ride the GP registers only,
                                  * laid contiguous with the caller stack args */
  void *__ap;
} __va_list_tag;
#else
typedef struct {
  int gp_offset;
  int fp_offset;
  void *overflow_arg_area;
  void *reg_save_area;
} __va_list_tag;
#endif

typedef __va_list_tag va_list[1];
#endif

#define va_start(ap, last) __builtin_va_start(ap, last)
#define va_arg(ap, type)   __builtin_va_arg(ap, type)
#define va_end(ap)         __builtin_va_end(ap)
#if defined(__GNUC__) || defined(__clang__)
#define va_copy(dst, src)  __builtin_va_copy(dst, src)
#else
#define va_copy(dst, src)  ((dst)[0] = (src)[0])
#endif

#endif
