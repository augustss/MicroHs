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
 * Without this, exp,sin, etc are not available.
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
 * This is Windows
 */
#define ISWINDOWS 1

#include <inttypes.h>
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <intrin.h>
#include <stdlib.h>
#include <stdio.h>
#include <locale.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>

/* Make these empty */
#define NORETURN
#define PACKED

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
