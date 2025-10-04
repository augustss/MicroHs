/* Copyright 2025 Lennart Augustsson
 * See LICENSE file for full license.
 */

/*
 * Various platform specific configuration.
 */

/*
 * Include stdio functions.
 * Without this none of the file I/O in System.IO is available.
 */
#define WANT_STDIO 1

/*
 * Include file descriptor functions.
 * Without this none of the file I/O in System.IO is available.
 */
#define WANT_FD 1

/*
 * Include ops for floating point arithmetic.
 * Without this +,-,* etc will not be available for the Float type.
 */
#define WANT_FLOAT32 1

/*
 * Include ops for floating point arithmetic.
 * Without this +,-,* etc will not be available for the Double type.
 * Using this on a 32 bit platform will make cells be 12 bytes instead of 8,
 */
#define WANT_FLOAT64 1

/*
 * Include <math.h>
 * Without this exp,sin,sqrt etc will not be available.
 */
#define WANT_MATH 1

/*
 * Include ops for 64 bit integer arithmetic.
 * Using this on a 32 bit platform will make cells be 12 bytes instead of 8.
 * On 64 bit platforms there is always 64 bit arithmetic.
 */
#define WANT_INT64 1

/*
 * Include MD5 checksumming code
 */
#define WANT_MD5 1

/*
 * Include profiling code
 */
#define WANT_TICK 1

/*
 * Include directory manipulation
 */
#define WANT_DIR 1

/*
 * Include time_t type
 */
#define WANT_TIME 1

/*
 * We want a signal handler for SIGINT.
 */
#define WANT_SIGINT 1

/*
 * Symbolic names for tag when things go wrong.
 */
#define WANT_TAGNAMES 1

/*
 * errno related stuff
 */
#define WANT_ERRNO 1

/*
 * Use CPU counters.
 * Only available on:
 *  - MacOS with M4 CPU
 *  - Linux with supported CPUs (e.g., x86_64)
 */
#define WANT_KPERF 0

/*
 * This is the character used for comma-separation in printf.
 * Defaults to "'".
 */
/* #define PCOMMA "'" */


/*
 * The ERR macro should report an error and exit.
 * If not defined, a generic one will be used.
 */
/* #define ERR(s) */
/* #define ERR1(s,a) */

#define GCRED    1              /* do some reductions during GC */
#define INTTABLE 1              /* use fixed table of small INT nodes */
#define SANITY   1              /* do some sanity checks */
#define STACKOVL 1              /* check for stack overflow */

/******** Utility functions ********/

#include <inttypes.h>
#include <termios.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <stdio.h>
#include <locale.h>
#include <limits.h>

#if defined(__APPLE__) && defined(__MACH__)   // any Darwin (macOS, iOS, tvOS, watchOS)
  #include <TargetConditionals.h>
  #if TARGET_OS_OSX
    #define ISMACOS 1
  #endif
#endif

#if defined(__linux__)
  #define ISLINUX 1
#endif
