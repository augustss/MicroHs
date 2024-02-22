#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>

#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define MIN(x,y) ((x) < (y) ? (x) : (y))

#define MAXWIN 8192
#define MAXLEN (9 + 255)
#define MINMATCH 3
#define MINOFFS 1

/*
 * Encoding inspired by FastLZ
 *  000nnnnn                      - literal run follows, with n+1 bytes
 *  111ooooo pppppppp nnnnnnnn    - at offset o*256+p, copy n+9 bytes (maximum offset is 8191)
 *  nnnooooo pppppppp             - at offset o*256+p, copy n+2 bytes (n != 0, n != 7)
 * Match length is >= 3.
 */

/* dst must be big enough to hold the decompressed result */
size_t
lz77d(uint8_t *src, size_t srclen, uint8_t *adst)
{
  uint8_t *end = src + srclen;
  uint8_t *dst = adst;

  while (src < end) {
    int op = *src++;
    int opx = op & 0x1f;        /* part of offset, or literal length */
    op >>= 5;
    if (op == 0) {
      /* 000nnnnn */
      do {
        *dst++ = *src++;
      } while (--opx >= 0);
    } else {
      size_t len;
      size_t offs = opx * 256 + *src++ + MINOFFS;
      if (op == 7) {
        /* 111ooooo pppppppp nnnnnnnn */
        len = 9 + *src++;
      } else {
        /* nnnooooo pppppppp */
        len = 2 + op;
      }
      //printf("match cur=%d offs=%d len=%d\n", (int)(dst - adst), (int)offs, (int)len);
      uint8_t *p = dst - offs;
      for (size_t i = 0; i < len; i++) {
        *dst++ = *p++;
      }
    }
  }
  return dst - adst;
}

static size_t
match(uint8_t *src, uint8_t *win, uint8_t *end)
{
  size_t n;

  for(n = 0; *src == *win && src < end && n < MAXLEN; src++, win++, n++)
    ;
  return n;
}

size_t
lz77c(uint8_t *src, size_t srclen, uint8_t *adst)
{
  uint8_t *cur;
  uint8_t *end = src + srclen;
  uint8_t *dst = adst;

  for (cur = src; cur < end; ) {
    int win_end = cur - src;
    int win_len = MIN(win_end, MAXWIN);
    //size_t max_len = MIN(MAXLEN, end - cur);
    /* Inefficient match loop */
    size_t match_len = MINMATCH-1;
    size_t match_offs = 0;
    /* Compression is slow, since we use brute force to find a match. */
    for (size_t offs = MINOFFS; offs < win_len; offs++) {
      size_t n = match(cur, cur - offs, end);
      if (n > match_len) {
        match_len = n;
        match_offs = offs;
      }
    }
    if (match_len >= MINMATCH) {
      //printf("match cur=%d offs=%d len=%d str=%.*s\n", (int)(cur-src), (int)match_offs, (int)match_len, (int)match_len, cur);
      //printf("match cur=%d offs=%d len=%d\n", (int)(cur-src), (int)match_offs, (int)match_len);
      /* found a match */
      cur += match_len;
      match_offs -= MINOFFS;
      match_len -= 2;
      int hi = match_offs >> 8;
      int lo = match_offs & 0xff;
      if (match_len < 7) {
        *dst++ = (match_len << 5) + hi;
        *dst++ = lo;
      } else {
        *dst++ = (7 << 5) + hi;
        *dst++ = lo;
        *dst++ = match_len - 7;
      }
    } else {
      /* generate a literal */
      /* how long should it be?  3 seems to be a sweet spot */
      size_t len = MIN(3, end - cur);
      *dst++ = len - 1;
      for (size_t i = 0; i < len; i++) {
        *dst++ = *cur++;
      }
    }
  }
  return dst - adst;
}

int
main(int argc, char **argv)
{
  FILE *fi = fopen(argv[1], "r");
  FILE *fo = fopen(argv[2], "w");
  int dec = argc > 3;
  fseek(fi, 0, SEEK_END);
  size_t ilen = ftell(fi);
  size_t olen;
  fseek(fi, 0, SEEK_SET);
  uint8_t *ibuf = malloc(ilen);
  uint8_t *obuf = malloc((dec?10:2)*ilen);
  fread(ibuf, ilen, 1, fi);
  if (dec)
    olen = lz77d(ibuf, ilen, obuf);
  else
    olen = lz77c(ibuf, ilen, obuf);
  fwrite(obuf, olen, 1, fo);
  exit(0);
}
