/* universal character names (C99 6.4.3), mandatory since C99 and absent until
 * 2026-08-14. A UCN names a code POINT, not a byte -- which is the whole trap:
 * "\u00E4" in a NARROW string is the two utf-8 bytes C3 A4, where "\xE4" is the
 * one byte E4. The lexer already re-encoded escapes for the wide faces; the narrow
 * one had to learn it, so escseq reports whether the escape was a UCN.
 *
 * ⚠ a UCN in an IDENTIFIER still refuses -- loudly, and it is the remaining half.
 */
char narrow[] = "\u00E4";
char emoji[]  = "\U0001F600";

int main(void)
{
    /* the narrow string is utf-8, so one code point becomes two bytes */
    if (sizeof narrow != 3) return 1;
    if ((unsigned char)narrow[0] != 0xC3) return 2;
    if ((unsigned char)narrow[1] != 0xA4) return 3;

    /* ..and a supplementary code point becomes four */
    if (sizeof emoji != 5) return 4;
    if ((unsigned char)emoji[0] != 0xF0) return 5;
    if ((unsigned char)emoji[3] != 0x80) return 6;

    /* the wide faces take the code point itself; u16 splits the surrogate pair */
    if (sizeof(L"\u00E4") != 2 * sizeof(int)) return 7;
    if (L"\u00E4"[0] != 0xE4) return 8;
    if (sizeof(U"\U0001F600") != 2 * sizeof(unsigned)) return 9;
    if (U"\U0001F600"[0] != 0x1F600) return 10;
    if (u"\U0001F600"[0] != 0xD83D) return 11;
    if (u"\U0001F600"[1] != 0xDE00) return 12;

    /* a UCN concatenates like any other escape. ⚠ NOT "\u0041" for 'A': C11 6.4.3p2
       forbids a UCN naming a basic-set character, and gcc 13 refuses it outright. */
    if (sizeof("\u00E4" "BC") != 5) return 13;
    if ((unsigned char)("\u00E4" "BC")[2] != 'B') return 14;

    return 0;
}
