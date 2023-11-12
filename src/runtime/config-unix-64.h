/*
 * Various platform specific configuration.
 */

/*
 * Include stdio functions.
 * Without this none of the file I/O in System.IO is available.
 */
#define WANT_STDIO 1

/*
 * Include ops for floating point arithmetic.
 * Without this +,-,* etc will not be available for the Double type.
 */
#define WANT_FLOAT 1

/*
 * Include <math.h>
 * Without this, exp,sin, etc are not available.
 */
#define WANT_MATH 1

/*
 * Number of bits in a word.  Only 32 and 64 are supported.
 */
#define WORD_SIZE 64

/*
 * Find First Set
 * This macro must be defined.
 * It return the number of the least significant bit that is set.
 * Numberings starts from 1.  If no bit is set, it should return 0.
 */
#define FFS ffsl

/*
 * This is the character used for comma-separation in printf.
 * Defaults to "'".
 */
/* #define PCOMMA "'" */


#include <termios.h>
#include <unistd.h>

/*
 * Set the terminal in raw mode and read a single character.
 * Return this character, or -1 on any kind of failure.
 */
static int
getraw(void)
{
  struct termios old, new;
  char c;
  int r;
  
  if (tcgetattr(0, &old))
    return -1;
  cfmakeraw(&new);
  if (tcsetattr(0, TCSANOW, &new))
    return -1;
  r = read(0, &c, 1);
  (void)tcsetattr(0, TCSANOW, &old);
  if (r == 1)
    return c;
  else
    return -1;
}
/*
 * Get a raw input character.
 * If undefined, the default always returns -1
 */
#define GETRAW getraw


/*
 * Get time since some epoch in milliseconds.
 */
#include <sys/time.h>
uint64_t
gettimemilli(void)
{
  struct timeval tv;
  (void)gettimeofday(&tv, NULL);
  return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}
#define GETTIMEMILLI gettimemilli

/*
 * The ERR macro should report an error and exit.
 * If not defined, a generic one will be used.
 */
/* #define ERR(s) */
/* #define ERR1(s,a) */

#define GCRED    1              /* do some reductions during GC */
#define FASTTAGS 1              /* compute tag by pointer subtraction */
#define INTTABLE 1              /* use fixed table of small INT nodes */
#define SANITY   1              /* do some sanity checks */
#define STACKOVL 1              /* check for stack overflow */

