#include <unistd.h>
#include <stdint.h>

#define CAT(x, y, z)  CAT_(x, y, z)
#define CAT_(x, y, z) x ## y ## z

#define trim(c) do (c) = READ_CHAR(); while (c == ' ' || c == '\t' || c == '\r' || c == '\n')
#define DIGITS(b) (      \
	(b) == 8  ? 3  : \
	(b) == 16 ? 6  : \
	(b) == 32 ? 10 : \
	(b) == 64 ? 18 : \
-1)

/* -- data types -- */
#include "lang.h"

typedef uint8_t       byte;
#define langint       CAT(int, BITS, _t)
typedef double        real;
typedef uint8_t       boolean;

/* -- I/O primitives -- */
#if defined(READ_CHAR)
char READ_CHAR (void)
{
    char c;
    read(0, &c, 1);
    return c;
}
#endif

#if defined(READ_STRING)
void READ_STRING (langint maxLen, char buf[static maxLen])
{
    long i = 0;
    while (i < maxLen - 1)
    {
        char c = READ_CHAR();
        buf[i] = c;
        if (buf[i] == '\n')
            break;
        i++;
    }
    buf[i] = '\0';
}
#endif

#if defined(READ_BYTE)
byte READ_BYTE (void)
{
    byte result = 0;
    char c;

    trim(c);
    for (; c > '/' && c <= '9'; c = READ_CHAR())
        result = result * 10 + (c - '0');
    return result;
}
#endif

#if defined(READ_INTEGER)
langint READ_INTEGER (void)
{
    long value = 0;
    int8_t sign = 1;
    char c;

    trim(c);
    if (c == '-')
        sign = -1, c = READ_CHAR();
    for (; c >= '0' && c <= '9'; c = READ_CHAR())
        value = value * 10 + (c - '0');

    return sign * value;
}
#endif

#if defined(READ_BOOLEAN)
boolean READ_BOOLEAN (void)
{
    char buf[6];
    buf[0] = READ_CHAR();
    READ_STRING(buf[0] == 't' ? 4 : 5, buf);
    return buf[0] == 't';
}
#endif

#if defined(WRITE_CHAR)
void WRITE_CHAR (char c)
{
    write(1, &c, 1);
}
#endif

#if defined(WRITE_STRING)
void WRITE_STRING (char *s)
{
    while (*s)
        WRITE_CHAR(*s++);
}
#endif

#if defined(WRITE_BYTE)
void WRITE_BYTE (byte n)
{
    char digits[DIGITS(8)];
    char count = 0;

    if (n == 0)
        return (void) WRITE_CHAR('0');

    for (; n != 0; n /= 10)
        digits[count++] = n % 10;
    while (count--)
        WRITE_CHAR(digits[count] + '0');
}
#endif

#if defined(WRITE_INTEGER)
void WRITE_INTEGER (langint n)
{
    char digits[DIGITS(BITS)];
    char count = 0;

    if (n < 0)
        WRITE_CHAR('-'), n = -n;
    else if (n == 0)
        return (void) WRITE_CHAR('0');

    for (; n != 0; n /= 10)
        digits[count++] = n % 10;
    while (count--)
        WRITE_CHAR(digits[count] + '0');
}
#endif

#if defined(WRITE_BOOLEAN)
void WRITE_BOOLEAN (boolean b)
{
    if (b) WRITE_STRING("true");
    else   WRITE_STRING("false");
}
#endif

/* -- math functions -- */

#if defined(abs)
langint ABS (langint n)
{
    return __builtin_labs(n);
}
#endif

#if defined(fabs)
real FABS (real r)
{
    return __builtin_fabs(r);
}
#endif

#if defined(sqrt)
real SQRT (real r)
{
    return __builtin_sqrt(r);
}
#endif

#if defined(sin)
real SIN (real r)
{
    return __builtin_sinl(r);
}
#endif

#if defined(cos)
real COS (real r)
{
    return __builtin_cosl(r);
}
#endif

#if defined(tan)
real TAN (real r)
{
    return __builtin_tanl(r);
}
#endif

#if defined(arctan)
real ARCTAN (real r)
{
    return __builtin_atanl(r);
}
#endif

#if defined(exp)
real EXP (real r)
{
    return __builtin_exp(r);
}
#endif

#if defined(ln)
real LN (real r)
{
    return __builtin_log(r);
}
#endif

#if defined(pi)
real PI (void)
{
    return __builtin_acos(-1.0);
}
#endif

/* -- Type conversion helpers -- */

#if defined(trunc)
langint TRUNC (real r)
{
    return __builtin_trunc(r);
}
#endif

#if defined(round)
langint ROUND (real r)
{
    return __builtin_llround(r);
}
#endif

#if defined(ord)
langint ORD (char c)
{
    return (langint)(c);
}
#endif

#if defined(CHR)
char CHR (langint n)
{
    return (char)(n);
}
#endif

#if defined(EXTEND)
langint EXTEND (byte b)
{
    return (langint)b;
}
#endif

#if defined(SHRINK)
byte SHRINK (langint n)
{
    return (byte)n;
}
#endif

/* -- String manipulation -- */

#if defined(STRLEN)
long STRLEN (char *s)
{
    long len = 0;
    while (*s++)
        len++;
    return len;
}
#endif

#if defined(STRCMP)
langint STRCMP (char *s1, char *s2)
{
    while (*s1 && *s2) {
        if (*s1 != *s2)
            return (byte)*s1 - (byte)*s2;
        s1++;
        s2++;
    }
    return (byte)*s1 - (byte)*s2;
}
#endif

#if defined(STRCPY)
void STRCPY (char *dst, char *src)
{
    while (*src)
        *dst++ = *src++;
}
#endif

#if defined(STRCAT)
void STRCAT (char *dst, char *src)
{
    /* advance dst to its null terminator */
    while (*dst)
        dst++;
    /* append src */
    while (*src)
        *dst++ = *src++;
}
#endif

