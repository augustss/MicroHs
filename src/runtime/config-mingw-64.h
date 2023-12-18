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
#define FFS __builtin_ffsll

/*
 * This is the character used for comma-separation in printf.
 * Defaults to "'".
 */
/* #define PCOMMA "'" */

/*
 * The ERR macro should report an error and exit.
 * If not defined, a generic one will be used.
 */
/* #define ERR(s,a) */
/* #define ERR1(s,a) */

/*
 * Allocate memory on heap, void *MALLOC(size_t size)
 */
#define MALLOC malloc

/*
 * Free heap memory, void free(void *p)
 */
#define FREE free

#define GCRED    1              /* do some reductions during GC */
#define FASTTAGS 1              /* compute tag by pointer subtraction */
#define INTTABLE 1              /* use fixed table of small INT nodes */
#define SANITY   1              /* do some sanity checks */
#define STACKOVL 1              /* check for stack overflow */
