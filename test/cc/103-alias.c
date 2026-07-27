/* __attribute__((alias("target"))) -- musl's weak_alias, its single most-used
 * idiom (189 files): `extern __typeof(old) new __attribute__((__weak__,
 * __alias__("old")))` gives `new` as another NAME for `old`'s address, weakly
 * bound so a strong definition elsewhere wins the link.
 *
 * covered: an alias onto a defined function, onto a STATIC function (musl's
 * exit.c aliases a local dummy so a real implementation can override it), and
 * onto a data object (its __stdin_used trio). the binding itself -- weak vs
 * strong -- is a link-time property the batteries cannot see; the moon-musl
 * rung is what proves it, by linking musl's two competing __stdout_used. */

#define weak_alias(old, new) \
	extern __typeof(old) new __attribute__((__weak__, __alias__(#old)))

static int impl(int x) { return x * 3; }
int __base(int x) { return impl(x) + 1; }
weak_alias(__base, base_alias);
int base_alias(int);

static void dummy(void) { }
weak_alias(dummy, hook_alias);
void hook_alias(void);

static long counter = 40;
weak_alias(counter, counter_alias);
extern long counter_alias;

int main(void)
{
	hook_alias();                                  /* the static-target alias runs */
	return __base(5) + base_alias(2)               /* 16 + 7 */
	     + (int)counter_alias                      /* 40, the same object */
	     + (&counter_alias == &counter ? 3 : 0);   /* ..at the same address */
}
