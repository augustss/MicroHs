
/***************** BFILE *******************/
/*
 * BFILE is a family of stream objects.
 * There are two kinds: endpoints (source/sink) and transducers.
 * The streams are typically bytes.
 *
 *  FILE   source/sink for stdio FILE handles
 *  FD     source/sink for file descriptors
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
 *  buf    transducer that just buffers
 *         It will convert multiple putb() into writeb(),
 *         and use a single readb() instead of multiple getb().
 *         The buffer size is set on creation.
 *  crlf   transducer that turns LF into CRLF
 *         put - turn LF into CR,LF
 *         get - turn CR,LF into LF
 */

/**** Buffers for collecting data. */

struct bfbuffer {
  size_t   size;                /* total size of buffer */
  size_t   pos;                 /* current index into buffer */
  uint8_t *buf;                 /* actual data */
};

void
bfbuffer_init(struct bfbuffer *bf, size_t size)
{
  bf->size = size;
  bf->pos = 0;
  bf->buf = MALLOC(size);
  if (!bf->buf)
    ERR("bfbuffer_init");
}

int
bfbuffer_get(struct bfbuffer *bf)
{
  if (bf->pos >= bf->size)
    return -1;
  return bf->buf[bf->pos++];
}

void
bfbuffer_snoc(struct bfbuffer *bf, int byte)
{
  if (bf->pos >= bf->size) {
    bf->size *= 2;
    bf->buf = REALLOC(bf->buf, bf->size);
    if (!bf->buf)
      ERR("bfbuffer_snoc");
  }
  bf->buf[bf->pos++] = byte;
}

size_t
bfbuffer_read(struct bfbuffer *bf, uint8_t *buf, size_t size)
{
  if (bf->pos + size > bf->size) {
    size = bf->size - bf->pos;
  }
  memcpy(buf, bf->buf + bf->pos, size);
  bf->pos += size;
  return size;
}

size_t
bfbuffer_write(struct bfbuffer *bf, const uint8_t *str, size_t size)
{
  if (bf->pos + size > bf->size) {
    do {
      bf->size *= 2;
    } while (bf->pos + size > bf->size);
    bf->buf = REALLOC(bf->buf, bf->size);
    if (!bf->buf)
      ERR("bfbuffer_write");
  }
  memcpy(bf->buf + bf->pos, str, size);
  bf->pos += size;
  return size;
}

void
bfbuffer_free(struct bfbuffer *bf)
{
  FREE(bf->buf);
}

/*********************************/

/* Sanity checking */
#define CHECKBFILE(p, f) do { if (p->getb != f) { ERR("CHECKBFILE"); } } while(0)

/* BFILE will have different implementations, they all have these methods */
typedef struct BFILE {
  int (*getb)(struct BFILE*);
  void (*ungetb)(int, struct BFILE*);
  void (*putb)(int, struct BFILE*);
  void (*flushb)(struct BFILE*);
  void (*closeb)(struct BFILE*);
  size_t (*readb)(uint8_t *, size_t, struct BFILE*); /* optional block read */
  size_t (*writeb)(const uint8_t *, size_t, struct BFILE*); /* optional block write */
  int (*isattyb)(struct BFILE*);                            /* optional tty test */
  int64_t (*filesizeb)(struct BFILE*);                      /* optional file size */
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

/* Is the BFILE connected to a terminal? */
static INLINE int
isattyb(BFILE *p)
{
  if (p->isattyb)
    return p->isattyb(p);
  else
    return 0;                   /* no method, so not a terminal */
}

static INLINE int64_t
filesizeb(BFILE *p)
{
  if (p->filesizeb)
    return p->filesizeb(p);
  else
    return -1;                  /* no method, so size unknown */
}

void
putsb(const char *str, struct BFILE *p)
{
  char c;
  while ((c = *str++))
    putb(c, p);
}

size_t
readb(uint8_t *buf, size_t size, BFILE *p)
{
  if (p->readb) {
    /* If there is a read method, use it. */
    return p->readb(buf, size, p);
  } else {
    /* Otherwise, do it byte by byte. */
    size_t s;
    for (s = 0; s < size; s++) {
      int c = getb(p);
      if (c < 0)
        break;
      buf[s] = c;
    }
    return s;
  }
}

size_t
writeb(const uint8_t *str, size_t size, BFILE *p)
{
  if (p->writeb) {
    /* If there is a write method, use it. */
    return p->writeb(str, size, p);
  } else {
    /* Otherwise, do it byte by byte. */
    size_t s;
    for(s = 0; s < size; s++) {
      putb(str[s], p);
    }
    return s;
  }
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

#if WANT_INT64
void
putnegb64(int64_t n, BFILE *p)
{
  int c = '0' - n % 10;
  if (n <= -10) {
    putnegb64(n / 10, p);
  }
  putb(c, p);
}

void
putdecb64(int64_t n, BFILE *p)
{
  if (n < 0) {
    putb('-', p);
    putnegb64(n, p);
  } else {
    putnegb64(-n, p);
  }
}
#endif  /* WANT_INT64 */

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
struct BFILE_mem {
  BFILE    mets;
  struct bfbuffer bf;
};

int
getb_mem(BFILE *bp)
{
  struct BFILE_mem *p = (struct BFILE_mem *)bp;
  CHECKBFILE(bp, getb_mem);

  return bfbuffer_get(&p->bf);
}

void
putb_mem(int c, BFILE *bp)
{
  struct BFILE_mem *p = (struct BFILE_mem *)bp;
  CHECKBFILE(bp, getb_mem);

  bfbuffer_snoc(&p->bf, c);
}

void
ungetb_mem(int c, BFILE *bp)
{
  struct BFILE_mem *p = (struct BFILE_mem *)bp;
  CHECKBFILE(bp, getb_mem);
  if (p->bf.pos == 0)
    ERR("ungetb");
  p->bf.pos--;
  if (p->bf.buf[p->bf.pos] != (uint8_t)c)
    ERR("ungetb");
}

void
closeb_rd_mem(BFILE *bp)
{
  CHECKBFILE(bp, getb_mem);
  FREE(bp);
}

void
closeb_wr_mem(BFILE *bp)
{
  CHECKBFILE(bp, getb_mem);
  FREE(bp);
}

void
flushb_mem(BFILE *bp)
{
  CHECKBFILE(bp, getb_mem);
}

size_t
readb_mem(uint8_t *buf, size_t size, BFILE *bp)
{
  struct BFILE_mem *p = (struct BFILE_mem *)bp;
  CHECKBFILE(bp, getb_mem);

  return bfbuffer_read(&p->bf, buf, size);
}

size_t
writeb_mem(const uint8_t *str, size_t size, BFILE *bp)
{
  struct BFILE_mem *p = (struct BFILE_mem *)bp;
  CHECKBFILE(bp, getb_mem);

  return bfbuffer_write(&p->bf, str, size);
}

struct BFILE*
openb_rd_mem(const uint8_t *buf, size_t len)
{
  struct BFILE_mem *p = MALLOC(sizeof(struct BFILE_mem));
  if (!p)
    memerr();
  p->mets.getb = getb_mem;
  p->mets.ungetb = ungetb_mem;
  p->mets.putb = 0;
  p->mets.flushb = 0;
  p->mets.closeb = closeb_rd_mem;
  p->mets.readb = readb_mem;
  p->mets.writeb = 0;
  p->mets.isattyb = 0;
  p->bf.size = len;
  p->bf.pos = 0;
  p->bf.buf = (uint8_t *)buf;
  return (struct BFILE*)p;
}

struct BFILE*
openb_wr_mem(void)
{
  struct BFILE_mem *p = MALLOC(sizeof(struct BFILE_mem));
  if (!p)
    memerr();
  p->mets.getb = getb_mem;      /* Just to make CHECKFILE happy */
  p->mets.ungetb = 0;
  p->mets.putb = putb_mem;
  p->mets.flushb = flushb_mem;
  p->mets.closeb = closeb_wr_mem;
  p->mets.readb = 0;
  p->mets.writeb = writeb_mem;
  p->mets.isattyb = 0;
  bfbuffer_init(&p->bf, 1000);
  return (struct BFILE*)p;
}

/*
 * Get the buffer used by writing.
 * This should be the last operation before closing,
 * since the buffer can move when writing.
 * The caller of openb_wr_mem() and get_mem() owns
 * the memory and must free it.
 */
void
get_mem(struct BFILE *bp, uint8_t **bufp, size_t *lenp)
{
  struct BFILE_mem *p = (struct BFILE_mem *)bp;
  CHECKBFILE(bp, getb_mem);
  *bufp = p->bf.buf;
  *lenp = p->bf.pos;
}

#if WANT_STDIO
/***************** BFILE via FILE *******************/
#if defined(_WIN32)
#include <io.h>                 /* for _isatty() */
#endif

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

int
isattyb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
#if defined(_WIN32)
  return _isatty(_fileno(p->file));
#else
  return isatty(fileno(p->file));
#endif
}

int64_t
filesizeb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
#if WANT_DIR
  return get_FILE_size(p->file);
#else
  return -1;
#endif
}

void
freeb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  FREE(p);
}

size_t
readb_file(uint8_t *buf, size_t size, BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  return fread(buf, 1, size, p->file);
}

size_t
writeb_file(const uint8_t *str, size_t size, BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  CHECKBFILE(bp, getb_file);
  return fwrite(str, 1, size, p->file);
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
  p->mets.readb  = readb_file;
  p->mets.writeb = writeb_file;
  p->mets.isattyb = isattyb_file;
  p->mets.filesizeb = filesizeb_file;
  p->file = f;
  return (BFILE*)p;
}
#endif


#if WANT_FD
/***************** BFILE via file descriptor *******************/
struct BFILE_fd {
  BFILE    mets;
  int      unget;
  int      fd;
};

int
getb_fd(BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
  uint8_t c;
  if (p->unget >= 0) {
    c = p->unget;
    p->unget = -1;
  } else {
    if (read(p->fd, &c, 1) != 1)
      return -1;
  }
  return c;
}

void
ungetb_fd(int c, BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
  p->unget = c;
}

void
putb_fd(int c, BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
  uint8_t u = c;
  if (write(p->fd, &u, 1) <= 0)
    ERR("putb_fd");
}

void
flushb_fd(BFILE *bp)
{
  CHECKBFILE(bp, getb_fd);
}

void
closeb_fd(BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
  close(p->fd);
  FREE(p);
}

int
isattyb_fd(BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
  return isatty(p->fd);
}

int64_t
filesizeb_fd(BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
#if WANT_DIR
  return get_fd_size(p->fd);
#else
  return -1;
#endif
}

void
freeb_fd(BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
  FREE(p);
}

size_t
readb_fd(uint8_t *buf, size_t size, BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
  return read(p->fd, buf, size);
}

size_t
writeb_fd(const uint8_t *str, size_t size, BFILE *bp)
{
  struct BFILE_fd *p = (struct BFILE_fd *)bp;
  CHECKBFILE(bp, getb_fd);
  return write(p->fd, str, size);
}

BFILE *
add_fd(int fd)
{
  struct BFILE_fd *p = MALLOC(sizeof (struct BFILE_fd));
  if (!p)
    memerr();
  p->mets.getb   = getb_fd;
  p->mets.ungetb = ungetb_fd;
  p->mets.putb   = putb_fd;
  p->mets.flushb = flushb_fd;
  p->mets.closeb = closeb_fd;
  p->mets.readb  = readb_fd;
  p->mets.writeb = writeb_fd;
  p->mets.isattyb = isattyb_fd;
  p->mets.filesizeb = filesizeb_fd;
  p->fd = fd;
  p->unget = -1;
  return (BFILE*)p;
}
#endif


#if WANT_LZ77
/***************** BFILE via simple LZ77 decompression *******************/
struct BFILE_lz77 {
  BFILE    mets;
  BFILE    *bfile;              /* underlying BFILE */
  struct bfbuffer bf;
  int      read;
  int      numflush;
};

int
getb_lz77(BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);

  return bfbuffer_get(&p->bf);
}

void
ungetb_lz77(int c, BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);
  p->bf.pos--;
}

void
putb_lz77(int b, BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);

  bfbuffer_snoc(&p->bf, b);
}

/* Compress and write to output BFILE */
void
flushb_lz77(BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);

  /* If we have had a flush, and there is no new data then do nothing */
  if (p->numflush++ && !p->bf.pos)
    return;
  uint8_t *obuf;
  size_t olen = lz77c(p->bf.buf, p->bf.pos, &obuf);
  putsb("LZ1", p->bfile);              /* Version no */
  putint32(olen, p->bfile);            /* 32 bit length */
  for (size_t i = 0; i < olen; i++) {
    putb(obuf[i], p->bfile);           /* and the data */
  }
  FREE(obuf);
  p->bf.pos = 0;
  flushb(p->bfile);
}

void
closeb_lz77(BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);

  if (!p->read) {
    /* We are in write mode, so compress and push it down */
    flushb_lz77(bp);
    bfbuffer_free(&p->bf);
  }

  closeb(p->bfile);
  FREE(p);
}

int
isattyb_lz77(BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77*)bp;
  CHECKBFILE(bp, getb_lz77);

  return isattyb(p->bfile);
}

size_t
readb_lz77(uint8_t *buf, size_t size, BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77 *)bp;
  CHECKBFILE(bp, getb_lz77);

  return bfbuffer_read(&p->bf, buf, size);
}

size_t
writeb_lz77(const uint8_t *str, size_t size, BFILE *bp)
{
  struct BFILE_lz77 *p = (struct BFILE_lz77 *)bp;
  CHECKBFILE(bp, getb_lz77);

  return bfbuffer_write(&p->bf, str, size);
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
  p->mets.readb = readb_lz77;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_lz77;
  p->mets.filesizeb = 0;
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
  p->bf.size = lz77d(buf, size, &p->bf.buf); /* decompress */
  FREE(buf);
  p->bf.pos = 0;
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
  p->mets.readb = 0;
  p->mets.writeb = writeb_lz77;
  p->mets.isattyb = isattyb_lz77;
  p->mets.filesizeb = 0;
  p->read = 0;
  p->bfile = file;
  p->numflush = 0;

  bfbuffer_init(&p->bf, 25000);
  return (BFILE*)p;
}

#endif  /* WANT_LZ77 */

#if WANT_RLE
/***************** BFILE via RLE decompression *******************/
/*
 * Run Length Encoding for ASCII
 * Format
 * c                -  c         one ASCII character
 * 0x80+n c         -  n+1       repetitions of ASCII character c, n>1
 * 0x80+n 0x80+m c  -  n*128+m+1 repetitions of ASCII character c
 * ... for longer run lengths
 * Non-ASCII (i.e., >= 128) have very poor encoding:
 * 0x81 c-0x80
 */

struct BFILE_rle {
  BFILE    mets;
  BFILE    *bfile;              /* underlying BFILE */
  size_t   count;
  int      byte;
  int      unget;
  int      read;
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
    if (n < 0) {
      return -1;
    } else if (n == 1) {
      /* repetition count == 1 means a non-ASCII byte */
      return getb(p->bfile) | 0x80;
    } else {
      p->count = n;
      p->byte = getb(p->bfile);
      return p->byte;
    }
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
out_rle(struct BFILE_rle *p)
{
  if (p->count > 2) {
    /* More than 2 repeating chars, it's worth compressing */
    put_rep(p->bfile, p->count - 1);
    putb(p->byte, p->bfile);
  } else {
    while(p->count-- > 0)
      putb(p->byte, p->bfile);
  }
}

void
putb_rle(int b, BFILE *bp)
{
  struct BFILE_rle *p = (struct BFILE_rle*)bp;
  CHECKBFILE(bp, getb_rle);

  if (b & 0x80) {
    /* Encode non-ASCII as repetition count 1, followed by the byte with MSB removed */
    out_rle(p);
    put_rep(p->bfile, 1);
    putb(b & 0x7f, p->bfile);
    p->count = 0;
    p->byte = -1;
  } else if (b == p->byte) {
    p->count++;
  } else {
    out_rle(p);
    p->count = 1;
    p->byte = b;
  }
}

void
closeb_rle(BFILE *bp)
{
  struct BFILE_rle *p = (struct BFILE_rle*)bp;
  CHECKBFILE(bp, getb_rle);

  if (!p->read)
    flushb(bp);

  closeb(p->bfile);
}

int
isattyb_rle(BFILE *bp)
{
  struct BFILE_rle *p = (struct BFILE_rle*)bp;
  CHECKBFILE(bp, getb_rle);

  return isattyb(p->bfile);
}

void
flushb_rle(BFILE *bp)
{
  struct BFILE_rle *p = (struct BFILE_rle*)bp;
  CHECKBFILE(bp, getb_rle);

  /* output last byte(s) */
  out_rle(p);
  p->count = 0;
  flushb(p->bfile);
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
  p->mets.readb = 0;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_rle;
  p->count = 0;
  p->unget = -1;
  p->bfile = file;
  p->read = 1;

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
  p->mets.readb = 0;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_rle;
  p->count = 0;
  p->byte = -1;
  p->bfile = file;
  p->read = 0;

  return (BFILE*)p;
}

#endif  /* WANT_RLE */

#if WANT_BWT
/***************** BFILE via Burrows-Wheeler Transform *******************/
/*
 */

struct BFILE_bwt {
  BFILE    mets;
  BFILE    *bfile;              /* underlying BFILE */
  size_t   count;
  struct bfbuffer bf;
  int      read;
  int      numflush;
};

int
getb_bwt(BFILE *bp)
{
  struct BFILE_bwt *p = (struct BFILE_bwt*)bp;
  CHECKBFILE(bp, getb_bwt);

  return bfbuffer_get(&p->bf);
}

void
ungetb_bwt(int c, BFILE *bp)
{
  struct BFILE_bwt *p = (struct BFILE_bwt*)bp;
  CHECKBFILE(bp, getb_bwt);
  p->bf.pos--;
}

void
putb_bwt(int b, BFILE *bp)
{
  struct BFILE_bwt *p = (struct BFILE_bwt*)bp;
  CHECKBFILE(bp, getb_bwt);

  bfbuffer_snoc(&p->bf, b);
}

void
closeb_bwt(BFILE *bp)
{
  struct BFILE_bwt *p = (struct BFILE_bwt*)bp;
  CHECKBFILE(bp, getb_bwt);

  if (!p->read)
    flushb(bp);

  closeb(p->bfile);
}

int
isattyb_bwt(BFILE *bp)
{
  struct BFILE_bwt *p = (struct BFILE_bwt*)bp;
  CHECKBFILE(bp, getb_bwt);

  return isattyb(p->bfile);
}

size_t
readb_bwt(uint8_t *buf, size_t size, BFILE *bp)
{
  struct BFILE_bwt *p = (struct BFILE_bwt *)bp;
  CHECKBFILE(bp, getb_bwt);

  return bfbuffer_read(&p->bf, buf, size);
}

size_t
writeb_bwt(const uint8_t *str, size_t size, BFILE *bp)
{
  struct BFILE_bwt *p = (struct BFILE_bwt *)bp;
  CHECKBFILE(bp, getb_bwt);

  return bfbuffer_write(&p->bf, str, size);
}

/* Sort all rotations of buf, and the indices of the sorted strings in res. */
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
  uint32_t a = *(uint32_t*)pa;
  uint32_t b = *(uint32_t*)pb;
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

void
sort_buffer(uint8_t *buf, size_t buflen, uint32_t *res)
{
  for(size_t i = 0; i < buflen; i++)
    res[i] = i;
  compar_arg = buf;
  compar_len = buflen;
  qsort(res, buflen, sizeof(uint32_t), compar);
}

uint32_t
encode_bwt(uint8_t *data, size_t len, uint8_t *last)
{
  uint32_t *res = MALLOC(len * sizeof(uint32_t));
  if (!res)
    ERR("encode_bwt");
  sort_buffer(data, len, res);
  uint32_t zero = 0;
  for(size_t i = 0; i < len; i++) {
    uint32_t offs = res[i];
    last[i] = data[(offs + len - 1) % len];
    if (offs == 0)
      zero = i;
  }
  FREE(res);
  return zero;
}

void
flushb_bwt(BFILE *bp)
{
  struct BFILE_bwt *p = (struct BFILE_bwt*)bp;
  CHECKBFILE(bp, getb_bwt);

  /* If we have had a flush, and there is no new data then do nothing */
  if (p->numflush++ && !p->bf.pos)
    return;
  putsb("BW1", p->bfile);             /* version no */
  putint32(p->bf.pos, p->bfile);      /* 32 bit length */
  uint8_t *last = MALLOC(p->bf.pos);
  if (!last)
    ERR("flushb_bwt");
  size_t zero = encode_bwt(p->bf.buf, p->bf.pos, last);
  putint32(zero, p->bfile);
  for(size_t i = 0; i < p->bf.pos; i++)
    putb(last[i], p->bfile);
  FREE(last);
  p->bf.pos = 0;
  flushb(p->bfile);
}

#define MAX_BYTE 256

void
decode_bwt(uint8_t *data, size_t len, uint8_t *odata, size_t zero)
{
  size_t count[MAX_BYTE];
  uint32_t *pred = MALLOC(len * sizeof(uint32_t));
  for(size_t i = 0; i < MAX_BYTE; i++) {
    count[i] = 0;
  }
  for(size_t i = 0; i < len; i++) {
    pred[i] = count[data[i]]++;
  }
  size_t sum = 0;
  for(size_t i = 0; i < MAX_BYTE; i++) {
    size_t s = count[i];
    count[i] = sum;
    sum += s;
  }
  size_t i = zero;
  for(size_t j = len; j > 0; j--) {
    odata[j - 1] = data[i];
    i = pred[i] + count[data[i]];
  }
  FREE(pred);
}

BFILE *
add_bwt_decompressor(BFILE *file)
{
  struct BFILE_bwt *p = MALLOC(sizeof(struct BFILE_bwt));

  if (!p)
    memerr();
  memset(p, 0, sizeof(struct BFILE_bwt));
  p->mets.getb = getb_bwt;
  p->mets.ungetb = ungetb_bwt;
  p->mets.putb = 0;
  p->mets.flushb = 0;
  p->mets.closeb = closeb_bwt;
  p->mets.readb = readb_bwt;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_bwt;
  p->mets.filesizeb = 0;
  p->read = 1;
  p->bfile = file;
  p->numflush = 0;

  /* First check version */
  if (getb(file) != 'B' || getb(file) != 'W' || getb(file) != '1')
    ERR("Bad BWT signature");

  size_t size = getint32(file); /* then read size */
  uint32_t zero = getint32(file);

  uint8_t *buf = MALLOC(size);  /* temporary buffer for input */
  if (!buf)
    memerr();
  for(size_t i = 0; i < size; i++) {
    buf[i] = getb(file);        /* and read data */
  }
  bfbuffer_init(&p->bf, size);
  decode_bwt(buf, size, p->bf.buf, zero); /* decode */
  FREE(buf);

  return (BFILE*)p;
}

BFILE *
add_bwt_compressor(BFILE *file)
{
  struct BFILE_bwt *p = MALLOC(sizeof(struct BFILE_bwt));

  if (!p)
    memerr();
  p->mets.getb = getb_bwt;
  p->mets.ungetb = 0;
  p->mets.putb = putb_bwt;
  p->mets.flushb = flushb_bwt;
  p->mets.closeb = closeb_bwt;
  p->mets.readb = 0;
  p->mets.writeb = writeb_bwt;
  p->mets.isattyb = isattyb_bwt;
  p->mets.filesizeb = 0;
  p->read = 0;
  p->bfile = file;
  p->numflush = 0;

  bfbuffer_init(&p->bf, 25000);
  return (BFILE*)p;
}

#endif  /* WANT_BWT */

/***************** BFILE with UTF8 encode/decode *******************/

/*
 * This transducer is always compiled in: mk_std() and mkStringU() in eval.c
 * use it unconditionally for stdio and ByteString-to-String decoding, so it
 * must exist regardless of WANT_UTF8. The flag instead selects the decode
 * policy below: real UTF-8 when set, one Char per raw byte when not, so
 * every byte<->Char boundary in the runtime agrees on what "no UTF-8" means.
 */
struct BFILE_utf8 {
  BFILE    mets;
  BFILE    *bfile;
  int      unget;
  uint8_t  pend[3];
  int      npend;
};

static int
next_raw_utf8(struct BFILE_utf8 *p)
{
  if (p->npend > 0)
    return p->pend[--p->npend];
  return getb(p->bfile);
}

static void
push_raw_utf8(struct BFILE_utf8 *p, int b)
{
  if (b >= 0)
    p->pend[p->npend++] = (uint8_t)b;
}

/* XXX: This is not right with WORD_SIZE==16 */
int
getb_utf8(BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;
  CHECKBFILE(bp, getb_utf8);
  int c1;

  /* Do we have an ungetb character? */
  if (p->unget >= 0) {
    c1 = p->unget;
    p->unget = -1;
    return c1;
  }
  c1 = next_raw_utf8(p);
#if !WANT_UTF8
  return c1;                   /* one Char per raw byte, no decoding */
#else
  int c2, c3, c4;
  if (c1 < 0)
    return -1;
  if ((c1 & 0x80) == 0)
    return c1;
  if ((c1 & 0xe0) == 0xc0) {
    c2 = next_raw_utf8(p);
    if ((c2 & 0xc0) == 0x80) {
      int c = ((c1 & 0x1f) << 6) | (c2 & 0x3f);
      /* accept overlong encoding of 0, modified UTF-8 uses that */
      if (c == 0 || c >= 0x80)
        return c;
    }
    push_raw_utf8(p, c2);
    return 0xdc00 | c1;
  }
  if ((c1 & 0xf0) == 0xe0) {
    c2 = next_raw_utf8(p);
    if ((c2 & 0xc0) == 0x80) {
      c3 = next_raw_utf8(p);
      if ((c3 & 0xc0) == 0x80) {
        int c = ((c1 & 0x0f) << 12) | ((c2 & 0x3f) << 6) | (c3 & 0x3f);
        if (c >= 0x800 && (c < 0xd800 || c > 0xdfff))
          return c;
      }
      push_raw_utf8(p, c3);
    }
    push_raw_utf8(p, c2);
    return 0xdc00 | c1;
  }
  if ((c1 & 0xf8) == 0xf0) {
    c2 = next_raw_utf8(p);
    if ((c2 & 0xc0) == 0x80) {
      c3 = next_raw_utf8(p);
      if ((c3 & 0xc0) == 0x80) {
        c4 = next_raw_utf8(p);
        if ((c4 & 0xc0) == 0x80) {
          int c = ((c1 & 0x07) << 18) | ((c2 & 0x3f) << 12) | ((c3 & 0x3f) << 6) | (c4 & 0x3f);
          if (c >= 0x10000 && c < 0x110000)
            return c;
        }
        push_raw_utf8(p, c4);
      }
      push_raw_utf8(p, c3);
    }
    push_raw_utf8(p, c2);
    return 0xdc00 | c1;
  }
  return 0xdc00 | c1;
#endif  /* WANT_UTF8 */
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
#if !WANT_UTF8
  putb(c, p->bfile);            /* one raw byte per Char, no encoding */
  return;
#else
  if (0xdc80 <= c && c <= 0xdcff) {
    putb(c & 0xff, p->bfile);
    return;
  }
  if ((c & 0x1ff800) == 0xd800)
    c = 0xfffd;
  if (0 < c && c < 0x80) {      /* modified UTF-8 avoids 0 */
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
#endif  /* WANT_UTF8 */
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

int
isattyb_utf8(BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;
  CHECKBFILE(bp, getb_utf8);

  return isattyb(p->bfile);
}

int64_t
filesizeb_utf8(BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;
  CHECKBFILE(bp, getb_utf8);

  return filesizeb(p->bfile);
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
  p->mets.readb = 0;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_utf8;
  p->mets.filesizeb = filesizeb_utf8;
  p->bfile = file;
  p->unget = -1;
  p->npend = 0;

  return (BFILE*)p;
}

/***************** BFILE that just buffers *******************/

#if WANT_BUF
struct BFILE_buf {
  BFILE    mets;
  BFILE    *bfile;
  int      unget;
  size_t   size;
  size_t   cur;
  size_t   pos;
  int      linebuf;
  int      read;
  uint8_t *buf;
};

int
getb_buf(BFILE *bp)
{
  struct BFILE_buf *p = (struct BFILE_buf*)bp;
  CHECKBFILE(bp, getb_buf);
  p->read = 1;
  if (p->unget >= 0) {
    int c = p->unget;
    p->unget = -1;
    return c;
  }
  if (p->size == 0) {
    return getb(p->bfile);
  }
  if (p->pos >= p->cur) {
    int r = readb(p->buf, p->size, p->bfile);
    if (r <= 0)
      return -1;
    p->cur = r;
    p->pos = 0;
  }
  return p->buf[p->pos++];  
}

void
ungetb_buf(int c, BFILE *bp)
{
  struct BFILE_buf *p = (struct BFILE_buf*)bp;
  CHECKBFILE(bp, getb_buf);
  if (p->unget >= 0)
    ERR("ungetb_buf");
  p->unget = c;
}

void
putb_buf(int c, BFILE *bp)
{
  struct BFILE_buf *p = (struct BFILE_buf *)bp;
  CHECKBFILE(bp, getb_buf);
  if (p->size == 0) {
    putb(c, p->bfile);
  }
  if (p->pos >= p->size) {
    int r = writeb(p->buf, p->size, p->bfile);
    if (r != p->size)
      ERR("putb_buf");
    p->pos = 0;
  }
  p->buf[p->pos++] = c;
  if (p->linebuf && c == '\n') {
    writeb(p->buf, p->pos, p->bfile);
    p->pos = 0;
  }
}

void
flushb_buf(BFILE *bp)
{
  struct BFILE_buf *p = (struct BFILE_buf*)bp;
  CHECKBFILE(bp, getb_buf);

  if (!p->read) {
    (void)writeb(p->buf, p->pos, p->bfile);
    p->pos = 0;
  }

  flushb(p->bfile);
}

void
closeb_buf(BFILE *bp)
{
  struct BFILE_buf *p = (struct BFILE_buf*)bp;
  CHECKBFILE(bp, getb_buf);

  if (!p->read)
    flushb(bp);

  closeb(p->bfile);
  FREE(p);
}

int
isattyb_buf(BFILE *bp)
{
  struct BFILE_buf *p = (struct BFILE_buf*)bp;
  CHECKBFILE(bp, getb_buf);

  return isattyb(p->bfile);
}

int64_t
filesizeb_buf(BFILE *bp)
{
  struct BFILE_buf *p = (struct BFILE_buf*)bp;
  CHECKBFILE(bp, getb_buf);

  return filesizeb(p->bfile);
}

/* bufsize==0 no buffering, bufsize<0 linebuffered, bufsize>0 regular */
BFILE *
add_buf(BFILE *file, int bufsize)
{
  struct BFILE_buf *p = MALLOC(sizeof(struct BFILE_buf));

  if (!p)
    memerr();
  p->mets.getb = getb_buf;
  p->mets.ungetb = ungetb_buf;
  p->mets.putb = putb_buf;
  p->mets.flushb = flushb_buf;
  p->mets.closeb = closeb_buf;
  p->mets.readb = 0;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_buf;
  p->mets.filesizeb = filesizeb_buf;
  p->bfile = file;
  p->unget = -1;
  p->pos = 0;
  p->cur = 0;
  p->read = 0;
  if (bufsize < 0) {
    p->size = -bufsize;
    p->linebuf = 1;
  } else {
    p->size = bufsize;
    p->linebuf = 0;
  }
  if (p->size) {
    p->buf = MALLOC(p->size);
    if (!p->buf)
      memerr();
  } else {
    p->buf = 0;
  }
  return (BFILE*)p;
}
#endif  /* WANT_BUF */

/***************** BFILE that adds CR *******************/

#if WANT_CRLF
struct BFILE_crlf {
  BFILE    mets;
  BFILE    *bfile;
};

int
getb_crlf(BFILE *bp)
{
  struct BFILE_crlf *p = (struct BFILE_crlf*)bp;
  CHECKBFILE(bp, getb_crlf);

  int c = getb(p->bfile);
  if (c != '\r')
    return c;
  int d = getb(p->bfile);
  if (d == '\n')
    return d;
  if (d >= 0)
    ungetb(d, p->bfile);
  return c;
}

void
ungetb_crlf(int c, BFILE *bp)
{
  struct BFILE_crlf *p = (struct BFILE_crlf*)bp;
  CHECKBFILE(bp, getb_crlf);
  ungetb(c, p->bfile);
}

void
putb_crlf(int c, BFILE *bp)
{
  struct BFILE_crlf *p = (struct BFILE_crlf *)bp;
  CHECKBFILE(bp, getb_crlf);
  if (c == '\n')
    putb('\r', p->bfile);
  putb(c, p->bfile);
}

void
flushb_crlf(BFILE *bp)
{
  struct BFILE_crlf *p = (struct BFILE_crlf*)bp;
  CHECKBFILE(bp, getb_crlf);

  flushb(p->bfile);
}

void
closeb_crlf(BFILE *bp)
{
  struct BFILE_crlf *p = (struct BFILE_crlf*)bp;
  CHECKBFILE(bp, getb_crlf);

  closeb(p->bfile);
  FREE(p);
}

int
isattyb_crlf(BFILE *bp)
{
  struct BFILE_crlf *p = (struct BFILE_crlf*)bp;
  CHECKBFILE(bp, getb_crlf);

  return isattyb(p->bfile);
}

int64_t
filesizeb_crlf(BFILE *bp)
{
  struct BFILE_crlf *p = (struct BFILE_crlf*)bp;
  CHECKBFILE(bp, getb_crlf);

  return filesizeb(p->bfile);
}

BFILE *
add_crlf(BFILE *file)
{
  struct BFILE_crlf *p = MALLOC(sizeof(struct BFILE_crlf));

  if (!p)
    memerr();
  p->mets.getb = getb_crlf;
  p->mets.ungetb = ungetb_crlf;
  p->mets.putb = putb_crlf;
  p->mets.flushb = flushb_crlf;
  p->mets.closeb = closeb_crlf;
  p->mets.readb = 0;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_crlf;
  p->mets.filesizeb = filesizeb_crlf;
  p->bfile = file;
  return (BFILE*)p;
}
#endif  /* WANT_CRLF */

#if WANT_BASE64
/***************** Base64 encode/decode *******************/

struct BFILE_b64 {
  BFILE mets;
  BFILE *bfile;          /* underlying BFILE */
  int read;              /* 1 = decoder, 0 = encoder */

  /* encoder state */
  uint8_t encbuf[3];     /* collect up to 3 bytes */
  int encpos;            /* how many bytes in encbuf (0..3) */
  size_t linelen;        /* 0 = no wrap; otherwise wrap column (e.g., 76) */
  size_t outcol;         /* current column count for wrapping */

  /* decoder state */
  int unget;
  uint8_t outbuf[3];
  int outpos, outlen;
};

static int
getb_b64(BFILE *bp) {
  struct BFILE_b64 *p = (struct BFILE_b64*)bp;
  CHECKBFILE(bp, getb_b64);

  if (!p->read)
    ERR("getb_b64: not in read mode");

  if (p->unget >= 0) {
    int c = p->unget;
    p->unget = -1;
    return c;
  }
  if (p->outpos < p->outlen)
    return p->outbuf[p->outpos++];

  /* need to decode the next quartet */
  int v[4], got = 0;
  for (;;) {
    int c = getb(p->bfile);
    if (c < 0) {
      if (got == 0)
        return -1; /* EOF cleanly */
      ERR("base64: truncated input");
    }

    int k;
    if ((unsigned)(c - 'A') <= 25) k = c - 'A';
    else if ((unsigned)(c - 'a') <= 25) k = c - 'a' + 26;
    else if ((unsigned)(c - '0') <= 9) k = c - '0' + 52;
    else if (c == '+') k = 62;
    else if (c == '/') k = 63;
    else if (c == '=') k = -3;
    else if (c == ' ' || c == '\t' || c == '\n' || c == '\r') k = -2;
    else k = -1;

    if (k == -2)
      continue;        /* skip whitespace */
    if (k >= 0 || k == -3) {
      v[got++] = k;
      if (got == 4)
        break;
    } else {
      ERR("base64: invalid character");
    }
  }

  /* convert quartet -> bytes */
  if (v[0] < 0 || v[1] < 0)
    ERR("base64: bad padding");
  uint32_t triple = ((uint32_t)v[0] << 18) | ((uint32_t)v[1] << 12);
  if (v[2] == -3) {
    p->outbuf[0] = (triple >> 16) & 0xff;
    p->outlen = 1;
  } else {
    triple |= ((uint32_t)v[2] << 6);
    if (v[3] == -3) {
      p->outbuf[0] = (triple >> 16) & 0xff;
      p->outbuf[1] = (triple >> 8) & 0xff;
      p->outlen = 2;
    } else {
      triple |= (uint32_t)v[3];
      p->outbuf[0] = (triple >> 16) & 0xff;
      p->outbuf[1] = (triple >> 8) & 0xff;
      p->outbuf[2] = triple & 0xff;
      p->outlen = 3;
    }
  }
  p->outpos = 0;
  return p->outbuf[p->outpos++];
}

static void
ungetb_b64(int c, BFILE *bp) {
  struct BFILE_b64 *p = (struct BFILE_b64*)bp;
  CHECKBFILE(bp, getb_b64);
  if (!p->read)
    ERR("ungetb_b64: not in read mode");
  if (p->unget >= 0)
    ERR("ungetb_b64: double unget");
  p->unget = c;
}

static const char b64_alphabet[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
#define B64_PAD 64               /* index into b64_alphabet */

/* emit one 4-char Base64 quad to the underlying file, with optional wrapping */
static INLINE void
b64_put_quad(int a, int b, int c, int d, struct BFILE_b64 *p) {
  if (p->linelen && p->outcol + 4 > p->linelen) {
    putb('\n', p->bfile);
    p->outcol = 0;
  }
  putb(b64_alphabet[a], p->bfile);
  putb(b64_alphabet[b], p->bfile);
  putb(b64_alphabet[c], p->bfile);
  putb(b64_alphabet[d], p->bfile);
  p->outcol += 4;
}

static void
putb_b64(int byte, BFILE *bp) {
  struct BFILE_b64 *p = (struct BFILE_b64*)bp;
  CHECKBFILE(bp, getb_b64);
  if (p->read)
    ERR("putb_b64: not in write mode");

  p->encbuf[p->encpos++] = byte;

  if (p->encpos == 3) {
    uint32_t x = ((uint32_t)p->encbuf[0] << 16) |
                 ((uint32_t)p->encbuf[1] << 8)  |
                 ((uint32_t)p->encbuf[2]);
    b64_put_quad((x >> 18) & 0x3f,
                 (x >> 12) & 0x3f,
                 (x >> 6)  & 0x3f,
                 x         & 0x3f,
                 p);
    p->encpos = 0;
  }
}

static void
flushb_b64(BFILE *bp) {
  struct BFILE_b64 *p = (struct BFILE_b64*)bp;
  CHECKBFILE(bp, getb_b64);

  if (p->read)
    return;

  /* finalize any pending tail (1 or 2 bytes) with padding */
  if (p->encpos == 1) {
    uint32_t x = ((uint32_t)p->encbuf[0]) << 16;
    b64_put_quad((x >> 18) & 0x3f,
                 (x >> 12) & 0x3f,
                 B64_PAD,
                 B64_PAD,
                 p);
    p->encpos = 0;
  } else if (p->encpos == 2) {
    uint32_t x = ((uint32_t)p->encbuf[0] << 16) |
                 ((uint32_t)p->encbuf[1] << 8);
    b64_put_quad((x >> 18) & 0x3f,
                 (x >> 12) & 0x3f,
                 (x >> 6)  & 0x3f,
                 B64_PAD,
                 p);
    p->encpos = 0;
  }

  flushb(p->bfile);
}

static void
closeb_b64(BFILE *bp) {
  struct BFILE_b64 *p = (struct BFILE_b64*)bp;
  CHECKBFILE(bp, getb_b64);

  if (!p->read) {
    flushb_b64(bp);       /* emits tail + flushes */
    if (p->linelen && p->outcol) {
      /* newline-terminate the final line if wrapping was enabled */
      putb('\n', p->bfile);
    }
  }

  closeb(p->bfile);
  FREE(p);
}

static int
isattyb_b64(BFILE *bp) {
  struct BFILE_b64 *p = (struct BFILE_b64*)bp;
  CHECKBFILE(bp, getb_b64);

  return isattyb(p->bfile);
}

static int64_t
filesizeb_b64(BFILE *bp) {
  struct BFILE_b64 *p = (struct BFILE_b64*)bp;
  CHECKBFILE(bp, getb_b64);

  return filesizeb(p->bfile);
}

BFILE *
add_base64_encoder(BFILE *file) {
  struct BFILE_b64 *p = MALLOC(sizeof(struct BFILE_b64));
  if (!p)
    memerr();
  memset(p, 0, sizeof *p);
  p->mets.getb   = getb_b64;
  p->mets.ungetb = 0;
  p->mets.putb   = putb_b64;
  p->mets.flushb = flushb_b64;
  p->mets.closeb = closeb_b64;
  p->mets.readb  = 0;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_b64;
  p->mets.filesizeb = filesizeb_b64;

  p->bfile  = file;
  p->read   = 0;
  p->encpos = 0;
  p->linelen = 76;
  p->outcol  = 0;

  p->unget = -1;
  p->outpos = 0;
  p->outlen = 0;

  return (BFILE*)p;
}

BFILE *
add_base64_decoder(BFILE *file) {
  struct BFILE_b64 *p = MALLOC(sizeof(struct BFILE_b64));
  if (!p)
    memerr();
  memset(p, 0, sizeof *p);
  p->mets.getb   = getb_b64;
  p->mets.ungetb = ungetb_b64;
  p->mets.putb   = 0;
  p->mets.flushb = flushb_b64;
  p->mets.closeb = closeb_b64;
  p->mets.readb  = 0;
  p->mets.writeb = 0;
  p->mets.isattyb = isattyb_b64;
  p->mets.filesizeb = filesizeb_b64;
  p->bfile = file;
  p->read = 1;
  p->unget = -1;
  p->outpos = 0;
  p->outlen = 0;
  return (BFILE*)p;
}

#endif  /* WANT_BASE64 */


#if WANT_LZMA

#define LZMA_IMPLEMENTATION
#define _7ZIP_ST
#include "lzma.h"
#undef MIN
#undef MAX

/* This transducer was written by Clause Code and debugged by Lennart. */

/***************** BFILE via LZMA compression ********************/
/*
 * Uses the single-header lzma.h library (Ciremun/lzma.h), which
 * wraps Igor Pavlov's LZMA SDK.
 *
 * To use, #define LZMA_IMPLEMENTATION before including lzma.h,
 * then compile with this file.
 *
 * Wire format:
 *   "LZ2"          -- 3-byte magic / version tag
 *   <5 bytes>      -- LZMA properties header
 *   <stream>       -- LZMA compressed bitstream with end-of-stream marker
 */

#define LZMA_IN_BUF_SIZE 4096

struct BFILE_lzma_dec {
  BFILE          mets;
  BFILE         *bfile;    /* underlying BFILE */
  CLzmaDec       dec;
  SizeT          read_pos; /* read cursor in dec.dic */
  SizeT          in_pos;   /* read cursor in in_buf */
  SizeT          in_len;   /* valid bytes in in_buf */
  int            eof;      /* end of LZMA stream reached */
  int            unget;    /* >= 0 if an ungot byte is pending */
  Byte           in_buf[LZMA_IN_BUF_SIZE];
};

struct BFILE_lzma_enc {
  BFILE          mets;
  BFILE         *bfile;    /* underlying BFILE */
  CLzmaEnc      *enc;
  ISeqOutStream  out_stream;
  int            flushed;
};

static size_t
lzma_seq_out_write(const ISeqOutStream *pp, const void *data, size_t size)
{
  struct BFILE_lzma_enc *p = CONTAINER_FROM_VTBL(pp, struct BFILE_lzma_enc, out_stream);
  return writeb((const uint8_t *)data, size, p->bfile);
}

static SRes
lzma_seq_in_read(const ISeqInStream *pp, void *buf, size_t *size)
{
  (void)pp;
  (void)buf;
  *size = 0;
  return SZ_OK;
}

static const ISeqInStream lzma_dummy_in_stream = { lzma_seq_in_read };

/* Fill more decoded bytes into p->dec.dic */
static void
lzma_dec_fill(struct BFILE_lzma_dec *p)
{
  while (p->read_pos == p->dec.dicPos && !p->eof) {
    if (p->dec.dicPos == p->dec.dicBufSize) {
      p->dec.dicPos = 0;
      p->read_pos = 0;
    }
    if (p->in_pos == p->in_len) {
      p->in_pos = 0;
      p->in_len = readb(p->in_buf, LZMA_IN_BUF_SIZE, p->bfile);
    }
    SizeT in_rem = p->in_len - p->in_pos;
    ELzmaStatus status = LZMA_STATUS_NOT_SPECIFIED;
    SRes res = LzmaDec_DecodeToDic(&p->dec, p->dec.dicBufSize,
                                   p->in_buf + p->in_pos, &in_rem,
                                   LZMA_FINISH_ANY, &status);
    if (res != SZ_OK)
      ERR("LzmaDec_DecodeToDic");
    p->in_pos += in_rem;
    if (status == LZMA_STATUS_FINISHED_WITH_MARK) {
      p->eof = 1;
    } else if (status == LZMA_STATUS_NEEDS_MORE_INPUT &&
               in_rem == 0 && p->in_len == 0 &&
               p->read_pos == p->dec.dicPos) {
      ERR("LZMA unexpected EOF");
    }
  }
}

static int
getb_lzma_dec(BFILE *bp)
{
  struct BFILE_lzma_dec *p = (struct BFILE_lzma_dec *)bp;
  CHECKBFILE(bp, getb_lzma_dec);
  if (p->unget >= 0) {
    int c = p->unget;
    p->unget = -1;
    return c;
  }
  if (p->read_pos == p->dec.dicPos) {
    lzma_dec_fill(p);
    if (p->read_pos == p->dec.dicPos)
      return -1;
  }
  return p->dec.dic[p->read_pos++];
}

static void
ungetb_lzma_dec(int c, BFILE *bp)
{
  struct BFILE_lzma_dec *p = (struct BFILE_lzma_dec *)bp;
  CHECKBFILE(bp, getb_lzma_dec);
  if (c < 0)
    return;
  if (p->unget >= 0)
    ERR("ungetb_lzma_dec: double unget");
  p->unget = c;
}

static size_t
readb_lzma_dec(uint8_t *buf, size_t size, BFILE *bp)
{
  struct BFILE_lzma_dec *p = (struct BFILE_lzma_dec *)bp;
  CHECKBFILE(bp, getb_lzma_dec);
  size_t total = 0;

  if (size == 0)
    return 0;

  if (p->unget >= 0) {
    *buf++ = (uint8_t)p->unget;
    p->unget = -1;
    total++;
    size--;
  }

  while (size > 0) {
    if (p->read_pos == p->dec.dicPos) {
      lzma_dec_fill(p);
      if (p->read_pos == p->dec.dicPos)
        break;
    }
    size_t avail = p->dec.dicPos - p->read_pos;
    if (avail > size)
      avail = size;
    memcpy(buf, p->dec.dic + p->read_pos, avail);
    p->read_pos += avail;
    buf += avail;
    total += avail;
    size -= avail;
  }
  return total;
}

static void
closeb_lzma_dec(BFILE *bp)
{
  struct BFILE_lzma_dec *p = (struct BFILE_lzma_dec *)bp;
  CHECKBFILE(bp, getb_lzma_dec);
  LzmaDec_Free(&p->dec, &LZMA_ALLOC);
  closeb(p->bfile);
  FREE(p);
}

static int
isattyb_lzma_dec(BFILE *bp)
{
  struct BFILE_lzma_dec *p = (struct BFILE_lzma_dec *)bp;
  CHECKBFILE(bp, getb_lzma_dec);
  return isattyb(p->bfile);
}

/* ---- streaming compressor ---- */

static void
lzma_enc_feed(struct BFILE_lzma_enc *p, const uint8_t *str, size_t size)
{
  CLzmaEnc *enc = p->enc;
  CMatchFinder *mf = &enc->matchFinderBase;

  while (size > 0) {
    if (MatchFinder_NeedMove(mf))
      MatchFinder_MoveBlock(mf);
    Byte *dest = mf->buffer + GET_AVAIL_BYTES(mf);
    size_t space = (size_t)(mf->bufferBase + mf->blockSize - dest);
    size_t chunk = size < space ? size : space;
    memcpy(dest, str, chunk);
    mf->streamPos += (UInt32)chunk;
    str += chunk;
    size -= chunk;

    while (GET_AVAIL_BYTES(mf) + enc->additionalOffset > (1 << 17) + (1 << 12) + mf->keepSizeAfter) {
      mf->streamEndWasReached = 0;
      enc->needInit = 0;
      MatchFinder_SetLimits(mf);
      if (LzmaEnc_CodeOneBlock(enc, 0, 0) != SZ_OK)
        ERR("LzmaEnc_CodeOneBlock");
      if (enc->finished)
        break;
    }
  }
}

static int
getb_lzma_enc(BFILE *bp)
{
  ERR("getb on lzma compressor");
  return -1;
}

static void
putb_lzma_enc(int b, BFILE *bp)
{
  struct BFILE_lzma_enc *p = (struct BFILE_lzma_enc *)bp;
  CHECKBFILE(bp, getb_lzma_enc);
  uint8_t ch = (uint8_t)b;
  lzma_enc_feed(p, &ch, 1);
}

static size_t
writeb_lzma_enc(const uint8_t *str, size_t size, BFILE *bp)
{
  struct BFILE_lzma_enc *p = (struct BFILE_lzma_enc *)bp;
  CHECKBFILE(bp, getb_lzma_enc);
  lzma_enc_feed(p, str, size);
  return size;
}

static void
flushb_lzma_enc(BFILE *bp)
{
  struct BFILE_lzma_enc *p = (struct BFILE_lzma_enc *)bp;
  CHECKBFILE(bp, getb_lzma_enc);

  if (p->flushed)
    return;
  p->flushed = 1;

  CLzmaEnc *enc = p->enc;
  CMatchFinder *mf = &enc->matchFinderBase;
  mf->streamEndWasReached = 1;
  enc->needInit = 0;
  MatchFinder_SetLimits(mf);
  while (!enc->finished) {
    if (LzmaEnc_CodeOneBlock(enc, 0, 0) != SZ_OK)
      ERR("LzmaEnc_CodeOneBlock flush");
  }
  LzmaEnc_Finish(enc);
  flushb(p->bfile);
}

static void
closeb_lzma_enc(BFILE *bp)
{
  struct BFILE_lzma_enc *p = (struct BFILE_lzma_enc *)bp;
  CHECKBFILE(bp, getb_lzma_enc);
  flushb_lzma_enc(bp);
  LzmaEnc_Destroy(p->enc, &LZMA_ALLOC, &LZMA_ALLOC);
  closeb(p->bfile);
  FREE(p);
}

static int
isattyb_lzma_enc(BFILE *bp)
{
  struct BFILE_lzma_enc *p = (struct BFILE_lzma_enc *)bp;
  CHECKBFILE(bp, getb_lzma_enc);
  return isattyb(p->bfile);
}

/* ---- public constructors ---- */

BFILE *
add_lzma_decompressor(BFILE *file)
{
  struct BFILE_lzma_dec *p = MALLOC(sizeof(struct BFILE_lzma_dec));
  if (!p)
    memerr();
  memset(p, 0, sizeof(struct BFILE_lzma_dec));

  p->mets.getb    = getb_lzma_dec;
  p->mets.ungetb  = ungetb_lzma_dec;
  p->mets.putb    = 0;
  p->mets.flushb  = 0;
  p->mets.closeb  = closeb_lzma_dec;
  p->mets.readb   = readb_lzma_dec;
  p->mets.writeb  = 0;
  p->mets.isattyb = isattyb_lzma_dec;
  p->mets.filesizeb = 0;
  p->bfile        = file;
  p->unget        = -1;

  /* Check the 3-byte magic tag. */
  if (getb(file) != 'L' || getb(file) != 'Z' || getb(file) != '3')
    ERR("Bad LZMA signature");

  Byte props[LZMA_PROPS_SIZE];
  if (readb(props, LZMA_PROPS_SIZE, file) != LZMA_PROPS_SIZE)
    ERR("LZMA header too short");

  LzmaDec_Construct(&p->dec);
  if (LzmaDec_Allocate(&p->dec, props, LZMA_PROPS_SIZE, &LZMA_ALLOC) != SZ_OK)
    ERR("LzmaDec_Allocate");
  LzmaDec_Init(&p->dec);

  return (BFILE *)p;
}

BFILE *
add_lzma_compressor(BFILE *file)
{
  struct BFILE_lzma_enc *p = MALLOC(sizeof(struct BFILE_lzma_enc));
  if (!p)
    memerr();
  memset(p, 0, sizeof(struct BFILE_lzma_enc));

  p->mets.getb    = getb_lzma_enc;
  p->mets.ungetb  = 0;
  p->mets.putb    = putb_lzma_enc;
  p->mets.flushb  = flushb_lzma_enc;
  p->mets.closeb  = closeb_lzma_enc;
  p->mets.readb   = 0;
  p->mets.writeb  = writeb_lzma_enc;
  p->mets.isattyb = isattyb_lzma_enc;
  p->mets.filesizeb = 0;
  p->bfile        = file;
  p->flushed      = 0;

  p->out_stream.Write = lzma_seq_out_write;

  CLzmaEnc *enc = (CLzmaEnc *)LzmaEnc_Create(&LZMA_ALLOC);
  if (!enc)
    memerr();
  p->enc = enc;

  CLzmaEncProps props;
  LzmaEncProps_Init(&props);
  props.writeEndMark = 1;
  if (LzmaEnc_SetProps(enc, &props) != SZ_OK)
    ERR("LzmaEnc_SetProps");

  Byte props_buf[LZMA_PROPS_SIZE];
  SizeT props_size = LZMA_PROPS_SIZE;
  if (LzmaEnc_WriteProperties(enc, props_buf, &props_size) != SZ_OK)
    ERR("LzmaEnc_WriteProperties");

  putsb("LZ3", file);
  writeb(props_buf, props_size, file);

  if (LzmaEnc_Prepare(enc, &p->out_stream, (ISeqInStream *)&lzma_dummy_in_stream, &LZMA_ALLOC, &LZMA_ALLOC) != SZ_OK)
    ERR("LzmaEnc_Prepare");

  CMatchFinder *mf = &enc->matchFinderBase;
  MatchFinder_Init_HighHash(mf);
  MatchFinder_Init_LowHash(mf);
  MatchFinder_Init_4(mf);
  mf->cyclicBufferPos = (mf->pos - CYC_TO_POS_OFFSET);

  return (BFILE *)p;
}

#endif /* WANT_LZMA */
