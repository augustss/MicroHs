
/***************** BFILE *******************/
/*
 * BFILE is a family of stream objects.
 * There are two kinds: endpoints (source/sink) and transducers.
 * The streams are typically bytes.
 *
 *  FILE   source/sink for stdio FILE handles, handles
 *  buf    source/sink where the read/write uses a memory buffer.
 *
 *  lz77   transducer for LZ77 compression
 *         put - compresses a byte
 *         get - returns an decompressed byte
 *  rle    transducer for Run Length Encoding
 *         put - compresses an ASCII character
 *         get - returns a decompressed ASCII
 *  bwt    transducer for Burrows-Wheeler Transform
 *         put - transforms a byte
 *         get - untransforms a byte
 *  utf8   transducer for UTF8 encoding
 *         put - encodes a Unicode code point (int) to bytes
 *         get - decodes a Unicode code point
 */

/* Sanity checking */
void foo(void)
{
  printf("foo\n");
}
#define CHECKBFILE(p, f) do { if (p->getb != f) { foo(); ERR("CHECKBFILE"); } } while(0)

/* BFILE will have different implementations, they all have these methods */
typedef struct BFILE {
  int (*getb)(struct BFILE*);
  void (*ungetb)(int, struct BFILE*);
  void (*putb)(int, struct BFILE*);
  void (*flushb)(struct BFILE*);
  void (*closeb)(struct BFILE*);
} BFILE;

static INLINE int
getb(BFILE *p)
{
  return p->getb(p);
}

static INLINE void
ungetb(int c, BFILE *p)
{
  p->ungetb(c, p);
}

static INLINE void
putb(int c, BFILE *p)
{
  p->putb(c, p);
}

static INLINE void
closeb(BFILE *p)
{
  p->closeb(p);
}

static INLINE void
flushb(BFILE *p)
{
  p->flushb(p);
}

void
putsb(const char *str, struct BFILE *p)
{
  char c;
  while ((c = *str++))
    putb(c, p);
}

size_t
readb(void *abuf, size_t size, BFILE *p)
{
  uint8_t *buf = abuf;
  size_t s;
  for (s = 0; s < size; s++) {
    int c = getb(p);
    if (c < 0)
      break;
    buf[s] = c;
  }
  return s;
}

/* convert -n to a string, handles MINBOUND correctly */
void
putnegb(value_t n, BFILE *p)
{
  int c = '0' - n % 10;
  if (n <= -10) {
    putnegb(n / 10, p);
  }
  putb(c, p);
}

void
putdecb(value_t n, BFILE *p)
{
  if (n < 0) {
    putb('-', p);
    putnegb(n, p);
  } else {
    putnegb(-n, p);
  }
}

void
putint32(value_t n, BFILE *p)
{
  putb(n % 256, p); n /= 256;
  putb(n % 256, p); n /= 256;
  putb(n % 256, p); n /= 256;
  putb(n % 256, p);
}

value_t
getint32(BFILE *p)
{
  value_t b0 = getb(p);
  value_t b1 = getb(p);
  value_t b2 = getb(p);
  value_t b3 = getb(p);
  return ((((b3 * 256) + b2) * 256) + b1) * 256 + b0;
}

/***************** BFILE from/to memory buffer *******************/
struct BFILE_buffer {
  BFILE    mets;
  size_t   b_size;
  size_t   b_pos;
  uint8_t  *b_buffer;
};

int
getb_buf(BFILE *bp)
{
  struct BFILE_buffer *p = (struct BFILE_buffer *)bp;
  CHECKBFILE(bp, getb_buf);
  if (p->b_pos >= p->b_size)
    return -1;
  return p->b_buffer[p->b_pos++];
}

void
putb_buf(int c, BFILE *bp)
{
  struct BFILE_buffer *p = (struct BFILE_buffer *)bp;
  CHECKBFILE(bp, getb_buf);
  if (p->b_pos >= p->b_size) {
    p->b_size *= 2;
    p->b_buffer = realloc(p->b_buffer, p->b_size);
    if (!p->b_buffer)
      ERR("putb_buf");
  }
  p->b_buffer[p->b_pos++] = c;
}

void
ungetb_buf(int c, BFILE *bp)
{
  struct BFILE_buffer *p = (struct BFILE_buffer *)bp;
  CHECKBFILE(bp, getb_buf);
  if (p->b_pos == 0)
    ERR("ungetb");
  p->b_buffer[--p->b_pos] = (uint8_t)c;
}

void
closeb_rd_buf(BFILE *bp)
{
  CHECKBFILE(bp, getb_buf);
  FREE(bp);
}

void
closeb_wr_buf(BFILE *bp)
{
  CHECKBFILE(bp, getb_buf);
  FREE(bp);
}

void
flushb_buf(BFILE *bp)
{
  CHECKBFILE(bp, getb_buf);
}

struct BFILE*
openb_rd_buf(uint8_t *buf, size_t len)
{
  struct BFILE_buffer *p = MALLOC(sizeof(struct BFILE_buffer));
  if (!p)
    memerr();
  p->mets.getb = getb_buf;
  p->mets.ungetb = ungetb_buf;
  p->mets.putb = 0;
  p->mets.flushb = 0;
  p->mets.closeb = closeb_rd_buf;
  p->b_size = len;
  p->b_pos = 0;
  p->b_buffer = buf;
  return (struct BFILE*)p;
}

struct BFILE*
openb_wr_buf(void)
{
  struct BFILE_buffer *p = MALLOC(sizeof(struct BFILE_buffer));
  if (!p)
    memerr();
  p->mets.getb = getb_buf;      /* Just to make CHECKFILE happy */
  p->mets.ungetb = 0;
  p->mets.putb = putb_buf;
  p->mets.flushb = flushb_buf;
  p->mets.closeb = closeb_wr_buf;
  p->b_size = 1000;
  p->b_pos = 0;
  p->b_buffer = malloc(p->b_size);
  if (!p->b_buffer)
    ERR("openb_wr_buf");
  return (struct BFILE*)p;
}

/* 
 * Get the buffer used by writing.
 * This should be the last operation before closing,
 * since the buffer can move when writing.
 * The caller of openb_wr_buf() and get_buf() owns
 * the memory and must free it.
 */
void
get_buf(struct BFILE *bp, uint8_t **bufp, size_t *lenp)
{
  struct BFILE_buffer *p = (struct BFILE_buffer *)bp;
  CHECKBFILE(bp, getb_buf);
  *bufp = p->b_buffer;
  *lenp = p->b_pos;
}

#if WANT_STDIO
/***************** BFILE via FILE *******************/
struct BFILE_file {
  BFILE    mets;
  FILE    *file;
};

int
getb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  return fgetc(p->file);
}

void
ungetb_file(int c, BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  ungetc(c, p->file);
}

void
putb_file(int c, BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  (void)fputc(c, p->file);
}

void
flushb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  fflush(p->file);
}

void
closeb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  fclose(p->file);
  FREE(p);
}

void
freeb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  FREE(p);
}

BFILE *
add_FILE(FILE *f)
{
  struct BFILE_file *p = MALLOC(sizeof (struct BFILE_file));
  if (!p)
    memerr();
  p->mets.getb   = getb_file;
  p->mets.ungetb = ungetb_file;
  p->mets.putb   = putb_file;
  p->mets.flushb = flushb_file;
  p->mets.closeb = closeb_file;
  p->file = f;
  return (BFILE*)p;
}
#endif


#if WANT_LZ77
/***************** BFILE via simple LZ77 decompression *******************/
struct BFILE_lz77 {
  BFILE    mets;
  BFILE    *bfile;              /* underlying BFILE */
  uint8_t  *buf;
  size_t   len;
  size_t   pos;
  int      read;
  int      numflush;
};

int
getb_lz77(BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);
  if (p->pos >= p->len)
    return -1;
  return p->buf[p->pos++];
}

void
ungetb_lz77(int c, BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);
  p->pos--;
}

void
putb_lz77(int b, BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);

  if (p->pos >= p->len) {
    p->len *= 2;
    p->buf = realloc(p->buf, p->len);
    if (!p->buf)
      memerr();
  }
  p->buf[p->pos++] = b;
}

/* Compress and write to output BFILE */
void
flushb_lz77(BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);

  /* If we have had a flush, and there is no new data then do nothing */
  if (p->numflush++ && !p->pos)
    return;
  uint8_t *obuf;
  size_t olen = lz77c(p->buf, p->pos, &obuf);
  putsb("LZ1", p->bfile);              /* Version no */
  putint32(olen, p->bfile);            /* 32 bit length */
  for (size_t i = 0; i < olen; i++) {
    putb(obuf[i], p->bfile);           /* and the data */
  }
  free(obuf);
  p->pos = 0;
}

void
closeb_lz77(BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);

  if (!p->read) {
    /* We are in write mode, so compress and push it down */
    flushb_lz77(bp);
    FREE(p->buf);
  }

  closeb(p->bfile);
  FREE(p);
}

BFILE *
add_lz77_decompressor(BFILE *file)
{
  struct BFILE_lz77 *p = MALLOC(sizeof(struct BFILE_lz77));

  if (!p)
    memerr();
  memset(p, 0, sizeof(struct BFILE_lz77));
  p->mets.getb = getb_lz77;
  p->mets.ungetb = ungetb_lz77;
  p->mets.putb = 0;
  p->mets.flushb = 0;
  p->mets.closeb = closeb_lz77;
  p->read = 1;
  p->bfile = file;
  p->numflush = 0;

  /* First check version */
  if (getb(file) != 'L' || getb(file) != 'Z' || getb(file) != '1')
    ERR("Bad LZ77 signature");

  size_t size = getint32(file); /* then read size */
  uint8_t *buf = MALLOC(size);  /* temporary buffer for input */
  if (!buf)
    memerr();
  for(size_t i = 0; i < size; i++) {
    buf[i] = getb(file);        /* and read data */
  }
  p->len = lz77d(buf, size, &p->buf); /* decompress */
  FREE(buf);
  p->pos = 0;
  return (BFILE*)p;
}

BFILE *
add_lz77_compressor(BFILE *file)
{
  struct BFILE_lz77 *p = MALLOC(sizeof(struct BFILE_lz77));

  if (!p)
    memerr();
  memset(p, 0, sizeof(struct BFILE_lz77));
  p->mets.getb = getb_lz77;
  p->mets.ungetb = 0;
  p->mets.putb = putb_lz77;
  p->mets.flushb = flushb_lz77;
  p->mets.closeb = closeb_lz77;
  p->read = 0;
  p->bfile = file;
  p->numflush = 0;

  p->len = 25000;
  p->buf = MALLOC(p->len);
  if (!p->buf)
    memerr();
  p->pos = 0;
  return (BFILE*)p;
}

#endif  /* WANT_LZ77 */

#if WANT_RLE
/***************** BFILE via RLE decompression *******************/
/*
 * Run Length Encoding for ASCII
 * Format
 * c                -  c       one ASCII character
 * 0x80+n c         -  n       repetitions of ASCII character c
 * 0x80+n 0x80+m c  -  n*128+m repetitions of ASCII character c
 * ... for longer run lengths
 */

struct BFILE_rle {
  BFILE    mets;
  BFILE    *bfile;              /* underlying BFILE */
  size_t   count;
  int      byte;
  int      unget;
};

int
get_rep(BFILE *in)
{
  size_t n = 0;
  for(;;) {
    int c = getb(in);
    //fprintf(stderr,"get_rep %02x\n", c);
    if (c < 0)
      return -1;
    if (c < 128) {
      ungetb(c, in);
      return n;
    }
    n = n * 128 + c - 128;
  }
}

int
getb_rle(BFILE *bp)
{
  struct BFILE_rle *p = (struct BFILE_rle*)bp;
  CHECKBFILE(bp, getb_rle);
  if (p->unget >= 0) {
    int c = p->unget;
    p->unget = -1;
    return c;
  }
  if (p->count) {
    p->count--;
    return p->byte;
  } else {
    int n = get_rep(p->bfile);
    if (n < 0)
      return -1;
    p->count = n;
    p->byte = getb(p->bfile);
    return p->byte;
  }
}

void
ungetb_rle(int c, BFILE *bp)
{
  struct BFILE_rle *p = (struct BFILE_rle*)bp;
  CHECKBFILE(bp, getb_rle);
  p->unget = c;
}

void
put_rep(BFILE *out, size_t n)
{
  if (n > 127)
    put_rep(out, n / 128);
  putb(n % 128 + 128, out);
}

void
putb_rle(int b, BFILE *bp)
{
  struct BFILE_rle *p = (struct BFILE_rle*)bp;
  CHECKBFILE(bp, getb_rle);

  if (b == p->byte) {
    p->count++;
  } else {
    if (p->count > 2) {
      /* More than 2 repeating chars, it's worth compressing */
      put_rep(p->bfile, p->count - 1);
      putb(p->byte, p->bfile);
    } else {
      while(p->count-- > 0)
        putb(p->byte, p->bfile);
    }
    p->count = 1;
    p->byte = b;
  }
}

void
closeb_rle(BFILE *bp)
{
  struct BFILE_rle *p = (struct BFILE_rle*)bp;
  CHECKBFILE(bp, getb_rle);

  closeb(p->bfile);
}

void
flushb_rle(BFILE *bp)
{
  /* There is nothing we can do */
}

BFILE *
add_rle_decompressor(BFILE *file)
{
  struct BFILE_rle *p = MALLOC(sizeof(struct BFILE_rle));

  if (!p)
    memerr();
  p->mets.getb = getb_rle;
  p->mets.ungetb = ungetb_rle;
  p->mets.putb = 0;
  p->mets.flushb = 0;
  p->mets.closeb = closeb_rle;
  p->count = 0;
  p->unget = -1;
  p->bfile = file;

  return (BFILE*)p;
}

BFILE *
add_rle_compressor(BFILE *file)
{
  struct BFILE_rle *p = MALLOC(sizeof(struct BFILE_rle));

  if (!p)
    memerr();
  p->mets.getb = getb_rle;
  p->mets.ungetb = 0;
  p->mets.putb = putb_rle;
  p->mets.flushb = flushb_rle;
  p->mets.closeb = closeb_rle;
  p->count = 0;
  p->byte = -1;
  p->bfile = file;

  return (BFILE*)p;
}

#endif  /* WANT_RLE */

/***************** BFILE with UTF8 encode/decode *******************/

struct BFILE_utf8 {
  BFILE    mets;
  BFILE    *bfile;
  int      unget;
};

/* This is not right with WORD_SIZE==16 */
int
getb_utf8(BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;
  CHECKBFILE(bp, getb_utf8);
  int c1, c2, c3, c4;

  /* Do we have an ungetb character? */
  if (p->unget >= 0) {
    c1 = p->unget;
    p->unget = -1;
    return c1;
  }
  c1 = getb(p->bfile);
  if (c1 < 0)
    return -1;
  if ((c1 & 0x80) == 0)
    return c1;
  c2 = getb(p->bfile);
  if (c2 < 0)
    return -1;
  if ((c1 & 0xe0) == 0xc0)
    return ((c1 & 0x1f) << 6) | (c2 & 0x3f);
  c3 = getb(p->bfile);
  if (c3 < 0)
    return -1;
  if ((c1 & 0xf0) == 0xe0)
    return ((c1 & 0x0f) << 12) | ((c2 & 0x3f) << 6) | (c3 & 0x3f);
  c4 = getb(p->bfile);
  if (c4 < 0)
    return -1;
  if ((c1 & 0xf8) == 0xf0)
    return ((c1 & 0x07) << 18) | ((c2 & 0x3f) << 12) | ((c3 & 0x3f) << 6) | (c4 & 0x3f);
  ERR("getb_utf8");
}

void
ungetb_utf8(int c, BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;
  CHECKBFILE(bp, getb_utf8);
  if (p->unget >= 0)
    ERR("ungetb_utf8");
  p->unget = c;
}

void
putb_utf8(int c, BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8 *)bp;
  CHECKBFILE(bp, getb_utf8);
  if (c < 0)
    ERR("putb_utf8: < 0");
  if (c < 0x80) {
    putb(c, p->bfile);
    return;
  }
  if (c < 0x800) {
    putb(((c >> 6 )       ) | 0xc0, p->bfile);
    putb(((c      ) & 0x3f) | 0x80, p->bfile);
    return;
  }
  if (c < 0x10000) {
    putb(((c >> 12)       ) | 0xe0, p->bfile);
    putb(((c >> 6 ) & 0x3f) | 0x80, p->bfile);
    putb(((c      ) & 0x3f) | 0x80, p->bfile);
    return;
  }
  if (c < 0x110000) {
    putb(((c >> 18)       ) | 0xf0, p->bfile);
    putb(((c >> 12) & 0x3f) | 0x80, p->bfile);
    putb(((c >> 6 ) & 0x3f) | 0x80, p->bfile);
    putb(((c      ) & 0x3f) | 0x80, p->bfile);
    return;
  }
  ERR("putb_utf8");
}

void
flushb_utf8(BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;
  CHECKBFILE(bp, getb_utf8);

  flushb(p->bfile);
}

void
closeb_utf8(BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;
  CHECKBFILE(bp, getb_utf8);

  closeb(p->bfile);
  FREE(p);
}

BFILE *
add_utf8(BFILE *file)
{
  struct BFILE_utf8 *p = MALLOC(sizeof(struct BFILE_utf8));
  
  if (!p)
    memerr();
  p->mets.getb = getb_utf8;
  p->mets.ungetb = ungetb_utf8;
  p->mets.putb = putb_utf8;
  p->mets.flushb = flushb_utf8;
  p->mets.closeb = closeb_utf8;
  p->bfile = file;
  p->unget = -1;

  return (BFILE*)p;
}
