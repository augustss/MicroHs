
/***************** BFILE *******************/
/*
 * BFILE is used to access files.
 * It allows various "transducers" to be added
 * to the processing.
 */

/* BFILE will have different implementations, they all have these methods */
typedef struct BFILE {
  int (*getb)(struct BFILE*);
  void (*ungetb)(int, struct BFILE*);
  void (*putb)(int, struct BFILE*);
  void (*closeb)(struct BFILE*);
} BFILE;

static inline int
getb(struct BFILE *p)
{
  return p->getb(p);
}

static inline void
ungetb(int c, struct BFILE *p)
{
  p->ungetb(c, p);
}

static inline void
putb(int c, struct BFILE *p)
{
  p->putb(c, p);
}

static inline void
closeb(struct BFILE *p)
{
  p->closeb(p);
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
  if (p->b_pos >= p->b_size)
    return -1;
  return p->b_buffer[p->b_pos++];
}

void
ungetb_buf(int c, BFILE *bp)
{
  struct BFILE_buffer *p = (struct BFILE_buffer *)bp;
  if (p->b_pos == 0)
    ERR("ungetb");
  p->b_buffer[--p->b_pos] = (uint8_t)c;
}

void
closeb_buf(BFILE *bp)
{
  (void)bp;                     /* shut up warning */
}

/* There is no open().  Only used with statically allocated buffers. */

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
  return fgetc(p->file);
}

void
ungetb_file(int c, BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  ungetc(c, p->file);
}

void
putb_file(int c, BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  (void)fputc(c, p->file);
}

void
closeb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  FREE(p);
}

BFILE *
openb_FILE(FILE *f)
{
  struct BFILE_file *p = MALLOC(sizeof (struct BFILE_file));
  if (!p)
    memerr();
  p->mets.getb   = getb_file;
  p->mets.ungetb = ungetb_file;
  p->mets.putb   = putb_file;
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
    r = p->bfile->getb(p->bfile);
    if (r < 0)
      return -1;
    r |= p->bfile->getb(p->bfile) << 8;
    p->rdres = r >> 12;         /* save 4 bits */
    p->rdstate = 1;
    return r & 0xfff;
  } else {
    r = p->rdres;
    r |= p->bfile->getb(p->bfile) << 4;
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
  char *s;
  int c, n;

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
    int l = strlen(os);
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
  if (p->unget >= 0)
    ERR("ungetb_lzw");
  p->unget = c;
}

void
closeb_lzw(BFILE *bp)
{
  struct BFILE_lzw *p = (struct BFILE_lzw*)bp;

  for (int i = 0; i < DICTSIZE; i++) {
    if (p->table[i])
      FREE(p->table[i]);
  }
  p->bfile->closeb(p->bfile);
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

int
getb_utf8(BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;
  int c1, c2, c3, c4;

  /* Do we have an ungetb character? */
  if (p->unget >= 0) {
    c1 = p->unget;
    p->unget = -1;
    return c1;
  }
  c1 = p->bfile->getb(p->bfile);
  if (c1 < 0)
    return -1;
  if ((c1 & 0x80) == 0)
    return c1;
  c2 = p->bfile->getb(p->bfile);
  if (c2 < 0)
    return -1;
  if ((c1 & 0xe0) == 0xc0)
    return ((c1 & 0x1f) << 6) | (c2 & 0x3f);
  c3 = p->bfile->getb(p->bfile);
  if (c3 < 0)
    return -1;
  if ((c1 & 0xf0) == 0xe0)
    return ((c1 & 0x0f) << 12) | ((c2 & 0x3f) << 6) | (c3 & 0x3f);
  c4 = p->bfile->getb(p->bfile);
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
  if (p->unget >= 0)
    ERR("ungetb_utf8");
  p->unget = c;
}

void
putb_utf8(int c, BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8 *)bp;
  if (c < 0)
    ERR("putb_utf8: < 0");
  if (c < 0x80) {
    p->bfile->putb(c, p->bfile);
    return;
  }
  if (c < 0x800) {
    p->bfile->putb(((c >> 6 )       ) | 0xc0, p->bfile);
    p->bfile->putb(((c      ) & 0x3f) | 0x80, p->bfile);
    return;
  }
  if (c < 0x10000) {
    p->bfile->putb(((c >> 12)       ) | 0xe0, p->bfile);
    p->bfile->putb(((c >> 6 ) & 0x3f) | 0x80, p->bfile);
    p->bfile->putb(((c      ) & 0x3f) | 0x80, p->bfile);
    return;
  }
  if (c < 0x110000) {
    p->bfile->putb(((c >> 18)       ) | 0xf0, p->bfile);
    p->bfile->putb(((c >> 12) & 0x3f) | 0x80, p->bfile);
    p->bfile->putb(((c >> 6 ) & 0x3f) | 0x80, p->bfile);
    p->bfile->putb(((c      ) & 0x3f) | 0x80, p->bfile);
    return;
  }
  ERR("putb_utf8");
}

void
closeb_utf8(BFILE *bp)
{
  struct BFILE_utf8 *p = (struct BFILE_utf8*)bp;

  p->bfile->closeb(p->bfile);
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
  p->mets.closeb = closeb_utf8;
  p->bfile = file;
  p->unget = -1;

  return (BFILE*)p;
}
