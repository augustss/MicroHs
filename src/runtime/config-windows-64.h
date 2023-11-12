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
#pragma warning(disable : 4996)
#pragma intrinsic(_BitScanForward)
static inline int
FFS(int64_t arg)
{
  unsigned long r;
  if (_BitScanForward64(&r, arg))
    return (int)(r+1);
  else
    return 0;
}

/*
 * This is the character used for comma-separation in printf.
 * Defaults to "'".
 * Windows does not support this.
 */
#define PCOMMA ""

/*
 * Get a raw input character.
 * If undefined, the default always returns -1
 */
/* #define GETRAW getraw */

/*
 * Get time since some epoch in milliseconds.
 * If undefined, return 0.
 */
#define GETTIMEMILLI gettimemilli
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

uint64_t
gettimemilli(void)
{
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    time /= 10000L;
    time += system_time.wMilliseconds * 1000;
    return time;
}


/*
 * The ERR macro should report an error and exit.
 * If not defined, a generic one will be used.
/* #define ERR(s,a) */
