
/***************** BFILE *******************/
/*
 * BFILE is used to access files.
 * It allows various "transducers" to be added
 * to the processing.
 */

/* Sanity checking */
#define CHECKBFILE(p, f) do { if (p->getb != f) ERR("CHECKBFILE"); } while(0)

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

/***************** BFILE from static buffer *******************/
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
ungetb_buf(int c, BFILE *bp)
{
  struct BFILE_buffer *p = (struct BFILE_buffer *)bp;
  CHECKBFILE(bp, getb_buf);
  if (p->b_pos == 0)
    ERR("ungetb");
  p->b_buffer[--p->b_pos] = (uint8_t)c;
}

void
closeb_buf(BFILE *bp)
{
  CHECKBFILE(bp, getb_buf);
  FREE(bp);
}

/* There is no open().  Only used with statically allocated buffers. */
struct BFILE*
openb_buf(uint8_t *buf, size_t len)
{
  struct BFILE_buffer *p = MALLOC(sizeof(struct BFILE_buffer));;
  if (!p)
    memerr();
  p->mets.getb = getb_buf;
  p->mets.ungetb = ungetb_buf;
  p->mets.putb = 0;
  p->mets.flushb = 0;
  p->mets.closeb = closeb_buf;
  p->b_size = len;
  p->b_pos = 0;
  p->b_buffer = buf;
  return (struct BFILE*)p;
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

/***************** BFILE via simple LZW decompression *******************/

#define DICTSIZE 4096
#define ASCIISIZE 96            /* ' ' - '~', '\n' */

struct BFILE_lzw {
  BFILE    mets;
  BFILE    *bfile;              /* underlying BFILE */
  int      unget;               /* storage for a single ungetb */
  char     *table[DICTSIZE];    /* dictionary */
  int      table_size;          /* next dictionary slot */
  char     *ptr;                /* pointer into output string */
  int      old;                 /* previous code word */
  int      ch;                  /* previous first character */
  char     buf[DICTSIZE+1];     /* buffer holding output string */
  int      rdstate;             /* state of 3 bytes to 2 codewords transducer */
  int      rdres;               /* saved transducer bits */
};

/* Get a code word.  It's 12 bits, so 2 codewords are spread over 3 bytes.
 * XXX This has 4096 hardcoded.
*/
int
getcode_lzw(struct BFILE_lzw *p)
{
  int r;

  if (p->rdstate == 0) {
    r = getb(p->bfile);
    if (r < 0)
      return -1;
    r |= getb(p->bfile) << 8;
    p->rdres = r >> 12;         /* save 4 bits */
    p->rdstate = 1;
    return r & 0xfff;
  } else {
    r = p->rdres;
    r |= getb(p->bfile) << 4;
    p->rdstate = 0;
    return r;
  }
}

char *
str_lzw(const char *s, int c)
{
  int l = strlen(s);
  char *p = MALLOC(l + 1 + 1);
  if (!p)
    memerr();
  strcpy(p, s);
  p[l] = c;
  p[l+1] = '\0';
  return p;
}

int
getb_lzw(BFILE *bp)
{
  struct BFILE_lzw *p = (struct BFILE_lzw*)bp;
  CHECKBFILE(bp, getb_lzw);
  char *s;
  int c, n, l;

  /* Do we have an ungetb character? */
  if (p->unget >= 0) {
    c = p->unget;
    p->unget = -1;
    return c;
  }
  /* Are we in the middle of emitting a string? */
  if (p->ptr) {
    c = *p->ptr++;
    if (c) {
      //PRINT("c='%c'\n", c);
      return c;
    }
    p->ptr = 0;
  }
  n = getcode_lzw(p);
  if (n < 0)
    return -1;
  if (n >= DICTSIZE)
    ERR("getcode_lzw 1");
  s = p->table[n];
  if (!s) {
    char *os = p->table[p->old];
    strcpy(p->buf, os);
    l = strlen(os);
    p->buf[l++] = p->ch;
    p->buf[l] = '\0';
  } else {
    strcpy(p->buf, s);
  }
  p->ptr = p->buf;
  p->ch = p->buf[0];
  if (p->table_size < DICTSIZE) {
    p->table[p->table_size++] = str_lzw(p->table[p->old], p->ch);
  }
  p->old = n;
  return *p->ptr++;
}

void
ungetb_lzw(int c, BFILE *bp)
{
  struct BFILE_lzw *p = (struct BFILE_lzw*)bp;
  CHECKBFILE(bp, getb_lzw);
  if (p->unget >= 0)
    ERR("ungetb_lzw");
  p->unget = c;
}

void
closeb_lzw(BFILE *bp)
{
  struct BFILE_lzw *p = (struct BFILE_lzw*)bp;
  CHECKBFILE(bp, getb_lzw);
  int i;

  for (i = 0; i < DICTSIZE; i++) {
    if (p->table[i])
      FREE(p->table[i]);
  }
  closeb(p->bfile);
  FREE(p);
}

BFILE *
add_lzw_decompressor(BFILE *file)
{
  struct BFILE_lzw *p = MALLOC(sizeof(struct BFILE_lzw));
  int i;
  
  if (!p)
    memerr();
  memset(p, 0, sizeof(struct BFILE_lzw));
  p->mets.getb = getb_lzw;
  p->mets.ungetb = ungetb_lzw;
  p->mets.putb = 0;             /* no compressor yet. */
  p->mets.flushb = 0;
  p->mets.closeb = closeb_lzw;
  p->bfile = file;
  p->unget = -1;

  /* initialize dictionary with printable ASCII */
  for(i = 0; i < ASCIISIZE-1; i++) {
    p->table[i] = str_lzw("", i + ' ');
  }
  p->table[i++] = str_lzw("", '\n');
  p->table_size = i;

  /* set up decompressore state */
  p->old = getcode_lzw(p);
  strcpy(p->buf, p->table[p->old]);
  p->ch = p->buf[0];
  p->ptr = p->buf;
  
  return (BFILE *)p;
}

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
