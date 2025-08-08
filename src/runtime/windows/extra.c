/* Copyright 2025 Lennart Augustsson
 * See LICENSE file for full license.
 */
#include <conio.h>

/*
 * Find First Set
 * This macro must be defined.
 * It returns the number of the least significant bit that is set.
 * Numberings starts from 1.  If no bit is set, it should return 0.
 */
//#pragma warning(disable : 4996)
#pragma intrinsic(_BitScanForward64)
static inline int
ffs(int64_t arg)
{
  unsigned long r;
  if (_BitScanForward64(&r, arg))
    return (int)(r+1);
  else
    return 0;
}
#define FFS ffs

#if defined(_M_X64)
#define POPCOUNT __popcnt64
#elif defined(_M_IX86)
#define POPCOUNT __popcnt
#endif

static inline uint64_t clz(uint64_t x) {
  unsigned long count;
  if (_BitScanReverse64(&count, x)) {
    return 63 - (uint64_t)count;
  } else {
    return 64;
  }
}
#define CLZ clz

static inline uint64_t ctz(uint64_t x) {
  unsigned long count;
  if (_BitScanForward64(&count, x)) {
    return (uint64_t)count;
  } else {
    return 64;
  }
}
#define CTZ ctz

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
#define GETRAW getraw
int
getraw(void)
{
  static char buf[2];
  int c;
  if (buf[0]) {
    c = buf[0];
    buf[0] = buf[1];
    buf[1] = 0;
  } else {
  tryagain:
    c = _getch();
    if (c == 0xe0 || c == 0x00) {
      switch(_getch()) {
      case 0x48: buf[1] = 'A'; break; /* Up arrow */
      case 0x50: buf[1] = 'B'; break; /* Down arrow */
      case 0x4d: buf[1] = 'C'; break; /* Right arrow */
      case 0x4b: buf[1] = 'D'; break; /* Left arrow */
      default: goto tryagain;
      }
      buf[0] = '[';
      c = 0x1b;                 /* ESC */
    }
  }
  return c;
}

/*
 * Get time since some epoch in milliseconds.
 * If undefined, return 0.
 */
#define GETTIMEMILLI gettimemilli

uint64_t
gettimemilli(void)
{
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time, msec;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    msec = (time - EPOCH) / 10000L;
    //msec = time + system_time.wMilliseconds;
    return msec;
}
