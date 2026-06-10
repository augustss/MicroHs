/*
  Name:     macros.c
  Purpose:  Support macros for temporary variable handling.
  Author:   M. J. Fromberger

  Copyright (C) 2002 Michael J. Fromberger, All Rights Reserved.

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 */

// Discourage direct inclusion from the .h files.
#if !defined(USE_IMATH_INTERNAL_TEMP_MACROS)
#error This file should not be included from library headers.
#endif

/* Declare a block of N temporary mpz_t values.
   These values are initialized to zero.
   You must add CLEANUP_TEMP() at the end of the function.
   Use TEMP(i) to access a pointer to the ith value.
 */
#define DECLARE_TEMP(N)                   \
  struct {                                \
    mpz_t value[(N)];                     \
    int len;                              \
    mp_result err;                        \
  } temp_ = {                             \
      .len = (N),                         \
      .err = MP_OK,                       \
  };                                      \
  do {                                    \
    for (int i = 0; i < temp_.len; i++) { \
      mp_int_init(TEMP(i));               \
    }                                     \
  } while (0)

/* Clear all allocated temp values. */
#define CLEANUP_TEMP()                    \
  CLEANUP:                                \
  do {                                    \
    for (int i = 0; i < temp_.len; i++) { \
      mp_int_clear(TEMP(i));              \
    }                                     \
    if (temp_.err != MP_OK) {             \
      return temp_.err;                   \
    }                                     \
  } while (0)

/* A pointer to the kth temp value. */
#define TEMP(K) (temp_.value + (K))

/* Evaluate E, an expression of type mp_result expected to return MP_OK.  If
   the value is not MP_OK, the error is cached and control resumes at the
   cleanup handler, which returns it.
*/
#define REQUIRE(E)                        \
  do {                                    \
    temp_.err = (E);                      \
    if (temp_.err != MP_OK) goto CLEANUP; \
  } while (0)
