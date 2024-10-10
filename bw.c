#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef uint32_t index_t;

/*
 * |.......................................|
 *         ^           ^
 *         a           b
 *                     <-      n         ->
 *         <-  m     ->
 * <-  o ->
 */
static uint8_t *compar_arg;
static size_t compar_len;
int compar(const void *pa, const void *pb)
{
  index_t a = *(index_t*)pa;
  index_t b = *(index_t*)pb;
  int r;
  if (a == b)
    return 0;
  if (a < b) {
    size_t n = compar_len - b; /* bytes until end of buffer */
    r = memcmp(compar_arg + a, compar_arg + b, n);
    if (r)
      return r;
    size_t m = b - a;
    r = memcmp(compar_arg + a + n, compar_arg, m);
    if (r)
      return r;
    size_t o = a;
    return memcmp(compar_arg, compar_arg + m, o);
  } else {
    size_t n = compar_len - a; /* bytes until end of buffer */
    r = memcmp(compar_arg + a, compar_arg + b, n);
    if (r)
      return r;
    size_t m = a - b;
    r = memcmp(compar_arg, compar_arg + b + n, m);
    if (r)
      return r;
    size_t o = a;
    return memcmp(compar_arg + m, compar_arg, o);
    
  }
  return 0;
}

/* Sort all rotations of buf, and the indices of the sorted strings in res. */
void
sort_buffer(uint8_t *buf, size_t buflen, index_t *res)
{
  for(size_t i = 0; i < buflen; i++)
    res[i] = i;
  compar_arg = buf;
  compar_len = buflen;
  qsort(res, buflen, sizeof(index_t), compar);
}

void
put_rep(FILE *out, size_t n)
{
  if (n > 127)
    put_rep(out, n / 128);
  fputc(n % 128 + 128, out);
}

#define MINRLE 3

void
rle(FILE *out, uint8_t *data, size_t len)
{
  for(size_t i = 0; i < len; ) {
    uint8_t c = data[i++];
    size_t n;
    for(n = 1; i < len && data[i] == c; i++, n++)
      ;
    if (n >= MINRLE) {
      put_rep(out, n - 1);
      fputc(c, out);
    } else {
      while(n-- > 0)
        fputc(c, out);
    }
  }
}

size_t
get_rep(FILE *in)
{
  size_t n = 0;
  for(;;) {
    int c = fgetc(in);
    //fprintf(stderr,"get_rep %02x\n", c);
    if (c < 0) abort();
    if (c < 128) {
      ungetc(c, in);
      return n;
    }
    n = n * 128 + c - 128;
  }
}

void
unrle(FILE *in, uint8_t *data, size_t len)
{
  for(size_t i = 0; i < len; ) {
    size_t n = get_rep(in);
    int c = fgetc(stdin);
    //fprintf(stderr,"unrle %02x\n", c);
    if (c < 0) abort();
    //fprintf(stderr, "n=%d c='%c'\n", (int)n, c);
    n += 1;
    for(size_t j = 0; j < n; j++) {
      data[i++] = c;
    }
    //fprintf(stderr, "i=%d\n", (int)i);
  }
}

#define MAXSIZE 10000000

int
main(int argc, char **argv)
{
  int encode = argc == 1;

  if (encode) {
    fprintf(stderr, "encode\n");
    uint8_t *data = malloc(MAXSIZE);
    size_t len = fread(data, 1, MAXSIZE, stdin);
    index_t *res = malloc(len * sizeof(index_t));

    sort_buffer((uint8_t*)data, len, res);
#if 0
    for(size_t i = 0; i < len; i++) {
      index_t offs = res[i];
      //fprintf(stderr, "offs=%d\n", (int)offs);
      fwrite(data + offs, 1, len - offs, stdout);
      fwrite(data,1, offs, stdout);
      fputs("\n", stdout);
    }
#endif
    uint8_t *last = malloc(len + 1);
    index_t zero;
    for(size_t i = 0; i < len; i++) {
      index_t offs = res[i];
      last[i] = data[(offs + len - 1) % len];
      if (offs == 0)
        zero = i;
    }
    last[len] = 0;
    fprintf(stderr, "len=%d, zero=%d\n", (int)len, (int)zero);
    // fprintf(stderr, "%s\n", last);
    put_rep(stdout, len); fputc(0, stdout);
    put_rep(stdout, zero); fputc(0, stdout);
    rle(stdout, last, len);
    // fwrite(last, 1, len, stdout);
  } else {
    fprintf(stderr, "decode\n");
    size_t len = get_rep(stdin); if (fgetc(stdin) != 0) abort();
    size_t zero = get_rep(stdin); if (fgetc(stdin) != 0) abort();
    fprintf(stderr, "len=%d, zero=%d\n", (int)len, (int)zero);
    uint8_t *data = malloc(len);
    unrle(stdin, data, len);
    // fwrite(data, 1, len, stdout);
#define MAXCHAR 128
    size_t count[MAXCHAR];
    index_t *pred = malloc(len * sizeof(index_t));
    uint8_t *odata = malloc(len);
    for(size_t i = 0; i < MAXCHAR; i++) {
      count[i] = 0;
    }
    for(size_t i = 0; i < len; i++) {
      pred[i] = count[data[i]]++;
    }
    size_t sum = 0;
    for(size_t i = 0; i < MAXCHAR; i++) {
      size_t s = count[i];
      count[i] = sum;
      sum += s;
    }
    size_t i = zero;
    for(size_t j = len; j > 0; j--) {
      odata[j - 1] = data[i];
      i = pred[i] + count[data[i]];
    }
    fwrite(odata, 1, len, stdout);
 }
  exit(0);
}
