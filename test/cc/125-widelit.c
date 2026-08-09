/* wide literals carry their element type (cts 00220): the lexer keeps the prefix
 * as the token kind with a canonical-UTF-8 value, parse desugars to a BOUNDED
 * compound literal of the element type (L -> wchar, u -> char16 + surrogate
 * pairs, U -> char32, u8 stays bytes), and the ordinary init machinery lays
 * elements -- so globals, locals, braces, elision and sizeof all agree with gcc.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern int printf(const char *, ...);
typedef __WCHAR_TYPE__ wchar_t;
typedef unsigned short char16_t;
typedef unsigned int char32_t;

wchar_t g[] = L"héllo";                 /* global: the static image */
wchar_t gb[] = { L"ab" };               /* braced (C11 6.7.9p14) */
char16_t u16[] = u"a€𝄞";                /* U+1D11E rides a surrogate pair */
char32_t u32[] = U"a€𝄞";
char u8s[] = u8"é";                     /* u8 stays bytes */
struct S { wchar_t w[4]; int x; } sm = { L"hi", 9 };   /* elision: the literal takes the member */

int main(void)
{
	wchar_t l[] = L"€x";            /* local fill */
	wchar_t e[] = L"\x4f60\n";      /* a high escape + a control escape */
	const wchar_t *p = L"ab" L"cd"; /* wide+wide concatenation */
	const wchar_t *q = "AB" L"CD";  /* narrow+wide: the wide side wins */

	int r = 0;
	r += sizeof(L"ab") == 12;
	r += sizeof(u"ab") == 6;
	r += sizeof(U"ab") == 12;
	r += sizeof(u8"é") == 3;
	r += sizeof(g) == 24;
	r += g[1] == 0xE9 && g[5] == 0;
	r += gb[0] == 'a' && gb[2] == 0;
	r += u16[1] == 0x20AC && u16[2] == 0xD834 && u16[3] == 0xDD1E && u16[4] == 0;
	r += u32[1] == 0x20AC && u32[2] == 0x1D11E && u32[3] == 0;
	r += (unsigned char)u8s[0] == 0xC3 && (unsigned char)u8s[1] == 0xA9;
	r += sm.w[0] == 'h' && sm.w[1] == 'i' && sm.x == 9;
	r += l[0] == 0x20AC && l[1] == 'x' && l[2] == 0;
	r += e[0] == 0x4F60 && e[1] == 10;
	r += p[2] == 'c' && p[4] == 0;
	r += q[1] == 'B' && q[2] == 'C' && q[4] == 0;
	r += L'a' == 97;
	r += L'\x2603' == 0x2603;       /* multi-cp L'ab' is a clex law: clang refuses the shape */
	return r;
}
