/* Copyright 2023 Lennart Augustsson
 * See LICENSE file for full license.
 */
#include <inttypes.h>

#include "config-unix-64.h"
//#include "config-micro-64.h"

#if WANT_STDIO
#include <stdio.h>
#include <locale.h>
#endif  /* WANT_STDIO */
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#if WANT_MATH
#include <math.h>
#endif  /* WANT_MATH */

#if WANT_MD5
#include "md5.h"
#endif

#define VERSION "v5.1\n"

typedef intptr_t value_t;       /* Make value the same size as pointers, since they are in a union */
#define PRIvalue PRIdPTR
typedef uintptr_t uvalue_t;     /* Make unsigned value the same size as pointers, since they are in a union */
#define PRIuvalue PRIuPTR
typedef uintptr_t heapoffs_t;   /* Heap offsets */
#define PRIheap PRIuPTR
typedef uintptr_t tag_t;        /* Room for tag, low order bit indicates AP/not-AP */
typedef intptr_t stackptr_t;    /* Index into stack */
/* These types can be changed for 32 bit platforms. */
typedef uint64_t counter_t;     /* Statistics counter, can be smaller since overflow doesn't matter */
#define PRIcounter PRIu64
typedef uint64_t bits_t;        /* One word of bits */

/* We cast all FFI functions to this type.  It's reasonably portable */
typedef void (*funptr_t)(void);

#if !defined(PCOMMA)
#define PCOMMA "'"
#endif  /* !defined(PCOMMA) */

#if !defined(GETRAW)
int GETRAW(void) { return -1; }
#endif  /* !defined(getraw) */

#if !defined(GETTIMEMILLI)
uint64_t GETTIMEMILLI(void) { return 0; }
#endif  /* !define(GETTIMEMILLI) */

#if !defined(TMPNAME)
char* TMPNAME(const char* pre, const char* post) {
  char *s = malloc(strlen(pre) + 3 + strlen(post) + 1);
  strcpy(s, pre);
  strcat(s, "TMP");
  strcat(s, post);
  return s;
}
#endif

#if !defined(INLINE)
#define INLINE inline
#endif  /* !define(INLINE) */

#if !defined(NORETURN)
#define NORETURN __attribute__ ((noreturn))
#endif /* !defined(NORETURN) */

/***************************************/

/* Keep permanent nodes for LOW_INT <= i < HIGH_INT */
#define LOW_INT (-10)
#define HIGH_INT 256

#define HEAP_CELLS 50000000
#define STACK_SIZE 100000

#if !defined(ERR)
#if WANT_STDIO
#define ERR(s)    do { fprintf(stderr,"ERR: "s"\n");   exit(1); } while(0)
#define ERR1(s,a) do { fprintf(stderr,"ERR: "s"\n",a); exit(1); } while(0)
#else  /* WANT_STDIO */
#define ERR(s) exit(1)
#define ERR1(s,a) exit(1)
#endif  /* WANT_STDIO */
#endif  /* !define(ERR) */

enum node_tag { T_FREE, T_IND, T_AP, T_INT, T_DBL, T_PTR, T_BADDYN, T_S, T_K, T_I, T_B, T_C,
                T_A, T_Y, T_SS, T_BB, T_CC, T_P, T_R, T_O, T_U, T_Z,
                T_ADD, T_SUB, T_MUL, T_QUOT, T_REM, T_SUBR, T_UQUOT, T_UREM, T_NEG,
                T_AND, T_OR, T_XOR, T_INV, T_SHL, T_SHR, T_ASHR,
                T_EQ, T_NE, T_LT, T_LE, T_GT, T_GE, T_ULT, T_ULE, T_UGT, T_UGE,
                T_TOPTR, T_TOINT, T_TODBL,
#if WANT_FLOAT
                T_FADD, T_FSUB, T_FMUL, T_FDIV, T_FNEG, T_ITOF,
                T_FEQ, T_FNE, T_FLT, T_FLE, T_FGT, T_FGE, T_FSHOW, T_FREAD,
#endif
                T_ERROR, T_NODEFAULT, T_NOMATCH, T_SEQ, T_EQUAL, T_COMPARE, T_RNF,
                T_IO_BIND, T_IO_THEN, T_IO_RETURN,
                T_IO_SERIALIZE, T_IO_DESERIALIZE,
                T_IO_STDIN, T_IO_STDOUT, T_IO_STDERR, T_IO_GETARGS, T_IO_DROPARGS,
                T_IO_PERFORMIO, T_IO_GETTIMEMILLI, T_IO_PRINT, T_IO_CATCH,
                T_IO_CCALL, T_DYNSYM,
                T_NEWCASTRING, T_PEEKCASTRING, T_PEEKCASTRINGLEN,
                T_STR,
                T_LAST_TAG,
};
const char* tag_names[] = {
  "FREE", "IND", "AP", "INT", "DBL", "PTR", "BADDYN", "S", "K", "I", "B", "C",
  "A", "Y", "SS", "BB", "CC", "P", "R", "O", "U", "Z",
  "ADD", "SUB", "MUL", "QUOT", "REM", "SUBR", "UQUOT", "UREM", "NEG",
  "AND", "OR", "XOR", "INV", "SHL", "SHR", "ASHR",
  "EQ", "NE", "LT", "LE", "GT", "GE", "ULT", "ULE", "UGT", "UGE",
  "TOPTR", "TOINT", "TODBL",
#if WANT_FLOAT
  "FADD", "FSUB", "FMUL", "FDIV", "FNEG", "ITOF",
  "FEQ", "FNE", "FLT", "FLE", "FGT", "FGE", "FSHOW", "FREAD",
#endif
  "ERROR", "NODEFAULT", "NOMATCH", "SEQ", "EQUAL", "COMPARE", "RNF",
  "IO_BIND", "IO_THEN", "IO_RETURN",
  "IO_SERIALIZE", "IO_DESERIALIZE",
  "IO_STDIN", "IO_STDOUT", "IO_STDERR", "IO_GETARGS", "IO_DROPARGS",
  "IO_PERFORMIO", "IO_GETTIMEMILLI", "IO_PRINT", "IO_CATCH",
  "IO_CCALL", "DYNSYM",
  "NEWCASTRING", "PEEKCASTRING", "PEEKCASTRINGLEN",
  "STR",
  "LAST_TAG",
};

typedef struct node {
  union {
    struct node *uufun;
    tag_t        uutag;             /* LSB=1 indicates that this is a tag, LSB=0 that this is a T_AP node */
  } ufun;
  union {
    struct node *uuarg;
    value_t      uuvalue;
    double       uudoublevalue;
    const char  *uustring;
    void        *uuptr;
  } uarg;
} node;
typedef struct node* NODEPTR;
#define NIL 0
#define HEAPREF(i) &cells[(i)]
#define GETTAG(p) ((p)->ufun.uutag & 1 ? (int)((p)->ufun.uutag >> 1) : T_AP)
#define SETTAG(p,t) do { if (t != T_AP) (p)->ufun.uutag = ((t) << 1) + 1; } while(0)
#define GETVALUE(p) (p)->uarg.uuvalue
#define GETDBLVALUE(p) (p)->uarg.uudoublevalue
#define SETVALUE(p,v) (p)->uarg.uuvalue = v
#define SETDBLVALUE(p,v) (p)->uarg.uudoublevalue = v
#define FUN(p) (p)->ufun.uufun
#define ARG(p) (p)->uarg.uuarg
#define STR(p) (p)->uarg.uustring
#define PTR(p) (p)->uarg.uuptr
#define INDIR(p) ARG(p)
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((heapoffs_t)((n) - cells))
node *cells;                 /* All cells */

counter_t num_reductions = 0;
counter_t num_alloc;
counter_t num_gc = 0;
uint64_t gc_mark_time = 0;
uint64_t run_time = 0;

NODEPTR *stack;
stackptr_t stack_ptr = -1;
#if STACKOVL
#define PUSH(x) do { if (stack_ptr >= stack_size-1) ERR("stack overflow"); stack[++stack_ptr] = (x); } while(0)
#else  /* SANITY */
#define PUSH(x) do {                                                        stack[++stack_ptr] = (x); } while(0)
#endif  /* SANITY */
#define TOP(n) stack[stack_ptr - (n)]
#define POP(n) stack_ptr -= (n)
#define GCCHECK(n) gc_check((n))

heapoffs_t heap_size = HEAP_CELLS; /* number of heap cells */
heapoffs_t heap_start;             /* first location in heap that needs GC */
stackptr_t stack_size = STACK_SIZE;

counter_t num_marked;
counter_t max_num_marked = 0;
counter_t num_free;

#define BITS_PER_WORD (sizeof(bits_t) * 8)
bits_t *free_map;             /* 1 bit per node, 0=free, 1=used */
heapoffs_t free_map_nwords;
heapoffs_t next_scan_index;

NORETURN
void
memerr(void)
{
  ERR("Out of memory");
  exit(1);
}

/***************** BFILE *******************/

/* BFILE will have different implementations, they all have these methods */
typedef struct BFILE {
  int (*getb)(struct BFILE*);
  void (*ungetb)(int c, struct BFILE*);
  void (*closeb)(struct BFILE*);
} BFILE;

/*** BFILE from static buffer ***/
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

#if WANT_STDIO
/*** BFILE via FILE ***/
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
closeb_file(BFILE *bp)
{
  struct BFILE_file *p = (struct BFILE_file *)bp;
  free(p);
}

BFILE *
openb_FILE(FILE *f)
{
  struct BFILE_file *p = malloc(sizeof (struct BFILE_file));
  if (!p)
    memerr();
  p->mets.getb   = getb_file;
  p->mets.ungetb = ungetb_file;
  p->mets.closeb = closeb_file;
  p->file = f;
  return (BFILE*)p;
}
#endif

/*** BFILE via simple LZW decompression ***/

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
  char *p = malloc(l + 1 + 1);
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
  if (p->unget) {
    c = p->unget;
    p->unget = 0;
    return c;
  }
  /* Are we in the middle of emitting a string? */
  if (p->ptr) {
    c = *p->ptr++;
    if (c) {
      //printf("c='%c'\n", c);
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
  if (p->unget)
    ERR("ungetb_lzw");
  p->unget = c;
}

void
closeb_lzw(BFILE *bp)
{
  struct BFILE_lzw *p = (struct BFILE_lzw*)bp;

  for (int i = 0; i < DICTSIZE; i++) {
    if (p->table[i])
      free(p->table[i]);
  }
  p->bfile->closeb(p->bfile);
  free(p);
}

BFILE *
add_lzw_decompressor(BFILE *file)
{
  struct BFILE_lzw *p = calloc(1, sizeof(struct BFILE_lzw));
  int i;
  
  if (!p)
    memerr();
  p->mets.getb = getb_lzw;
  p->mets.ungetb = ungetb_lzw;
  p->mets.closeb = closeb_lzw;
  p->bfile = file;

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

/*****************************************************************************/

struct handler {
  jmp_buf         hdl_buf;      /* env storage */
  struct handler *hdl_old;      /* old handler */
  stackptr_t      hdl_stack;    /* old stack pointer */
  NODEPTR         hdl_exn;      /* used temporarily to pass the exception value */
} *cur_handler = 0;

/* Set FREE bit to 0 */
static INLINE void mark_used(NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  if (i < heap_start)
    return;
#if SANITY
  if (i >= free_map_nwords * BITS_PER_WORD) ERR("mark_used");
#endif
  free_map[i / BITS_PER_WORD] &= ~(1ULL << (i % BITS_PER_WORD));
}

/* Test if FREE bit is 0 */
static INLINE int is_marked_used(NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  if (i < heap_start)
    return 1;
#if SANITY
  if (i >= free_map_nwords * BITS_PER_WORD) ERR("is_marked_used");;
#endif
  return (free_map[i / BITS_PER_WORD] & (1ULL << (i % BITS_PER_WORD))) == 0;
}

static INLINE void mark_all_free(void)
{
  memset(free_map, ~0, free_map_nwords * sizeof(bits_t));
  next_scan_index = heap_start;
}

int glob_argc;
char **glob_argv;

int verbose = 0;

static INLINE NODEPTR
alloc_node(enum node_tag t)
{
#if SANITY
  if (num_free <= 0)
    ERR("alloc_node");
#endif

  heapoffs_t i = next_scan_index / BITS_PER_WORD;
  int k;                        /* will contain bit pos + 1 */
  for(;;) {
    heapoffs_t word = free_map[i];
    k = FFS(word);
    if (k)
      break;
    i++;
#if SANITY
    if (i >= free_map_nwords)
      ERR("alloc_node free_map");
#endif
  }
  heapoffs_t pos = i * BITS_PER_WORD + k - 1; /* first free node */
  NODEPTR n = HEAPREF(pos);
  mark_used(n);
  next_scan_index = pos;

  SETTAG(n, t);
  num_alloc++;
  num_free--;
  return n;
}

static INLINE NODEPTR
new_ap(NODEPTR f, NODEPTR a)
{
  NODEPTR n = alloc_node(T_AP);
  FUN(n) = f;
  ARG(n) = a;
  return n;
}

/* Needed during reduction */
NODEPTR intTable[HIGH_INT - LOW_INT];
NODEPTR combFalse, combTrue, combUnit, combCons;
NODEPTR combCC, combZ, combIOBIND;
NODEPTR combLT, combEQ, combGT;

/* One node of each kind for primitives, these are never GCd. */
/* We use linear search in this, because almost all lookups
 * are among the combinators.
 */
struct {
  char *name;
  enum node_tag tag;
  NODEPTR node;
} primops[] = {
  /* combinators */
  /* sorted by frequency in a typical program */
  { "B", T_B },
  { "O", T_O },
  { "K", T_K },
  { "C'", T_CC },
  { "C", T_C },
  { "A", T_A },
  { "S'", T_SS },
  { "P", T_P },
  { "R", T_R },
  { "I", T_I },
  { "S", T_S },
  { "U", T_U },
  { "Y", T_Y },
  { "B'", T_BB },
  { "Z", T_Z },
  /* primops */
  { "+", T_ADD },
  { "-", T_SUB },
  { "*", T_MUL },
  { "quot", T_QUOT },
  { "rem", T_REM },
  { "uquot", T_UQUOT },
  { "urem", T_UREM },
  { "subtract", T_SUBR },
  { "neg", T_NEG },
  { "and", T_AND },
  { "or", T_OR },
  { "xor", T_XOR },
  { "inv", T_INV },
  { "shl", T_SHL },
  { "shr", T_SHR },
  { "ashr", T_ASHR },
#if WANT_FLOAT
  { "fadd" , T_FADD},
  { "fsub" , T_FSUB},
  { "fmul" , T_FMUL},
  { "fdiv", T_FDIV},
  { "fneg", T_FNEG},
  { "itof", T_ITOF},
  { "feq", T_FEQ},
  { "fne", T_FNE},
  { "flt", T_FLT},
  { "fle", T_FLE},
  { "fgt", T_FGT},
  { "fge", T_FGE},
  { "fshow", T_FSHOW},
  { "fread", T_FREAD},
#endif  /* WANT_FLOAT */
  { "==", T_EQ },
  { "/=", T_NE },
  { "<", T_LT },
  { "u<", T_ULT },
  { "u<=", T_ULE },
  { "u>", T_UGT },
  { "u>=", T_UGE },
  { "<=", T_LE },
  { ">", T_GT },
  { ">=", T_GE },
  { "seq", T_SEQ },
  { "error", T_ERROR },
  { "noDefault", T_NODEFAULT },
  { "noMatch", T_NOMATCH },
  { "equal", T_EQUAL },
  { "compare", T_COMPARE },
  { "rnf", T_RNF },
  /* IO primops */
  { "IO.>>=", T_IO_BIND },
  { "IO.>>", T_IO_THEN },
  { "IO.return", T_IO_RETURN },
  { "IO.serialize", T_IO_SERIALIZE },
  { "IO.print", T_IO_PRINT },
  { "IO.deserialize", T_IO_DESERIALIZE },
  { "IO.stdin", T_IO_STDIN },
  { "IO.stdout", T_IO_STDOUT },
  { "IO.stderr", T_IO_STDERR },
  { "IO.getArgs", T_IO_GETARGS },
  { "IO.dropArgs", T_IO_DROPARGS },
  { "IO.getTimeMilli", T_IO_GETTIMEMILLI },
  { "IO.performIO", T_IO_PERFORMIO },
  { "IO.catch", T_IO_CATCH },
  { "dynsym", T_DYNSYM },
  { "newCAString", T_NEWCASTRING },
  { "peekCAString", T_PEEKCASTRING },
  { "peekCAStringLen", T_PEEKCASTRINGLEN },
  { "toPtr", T_TOPTR },
  { "toInt", T_TOINT },
  { "toDbl", T_TODBL },
};

void
init_nodes(void)
{
  ALLOC_HEAP(heap_size);
  free_map_nwords = (heap_size + BITS_PER_WORD - 1) / BITS_PER_WORD; /* bytes needed for free map */
  free_map = malloc(free_map_nwords * sizeof(bits_t));
  if (!free_map)
    memerr();

  /* Set up permanent nodes */
  heap_start = 0;
#if !FASTTAGS
  for (int j = 0; j < sizeof primops / sizeof primops[0];j++) {
    NODEPTR n = HEAPREF(heap_start++);
    primops[j].node = n;
    //MARK(n) = MARKED;
    SETTAG(n, primops[j].tag);
    switch (primops[j].tag) {
    case T_K: combFalse = n; break;
    case T_A: combTrue = n; break;
    case T_I: combUnit = n; break;
    case T_O: combCons = n; break;
    case T_CC: combCC = n; break;
    case T_Z: combZ = n; break;
    case T_IO_BIND: combIOBIND = n; break;
#if WANT_STDIO
    case T_IO_STDIN:  SETTAG(n, T_PTR); PTR(n) = stdin;  break;
    case T_IO_STDOUT: SETTAG(n, T_PTR); PTR(n) = stdout; break;
    case T_IO_STDERR: SETTAG(n, T_PTR); PTR(n) = stderr; break;
#endif  /* WANT_STDIO */
    default:
      break;
    }
  }
#else
  for(enum node_tag t = T_FREE; t < T_LAST_TAG; t++) {
    NODEPTR n = HEAPREF(heap_start++);
    SETTAG(n, t);
    switch (t) {
    case T_K: combFalse = n; break;
    case T_A: combTrue = n; break;
    case T_I: combUnit = n; break;
    case T_O: combCons = n; break;
    case T_CC: combCC = n; break;
    case T_Z: combZ = n; break;
    case T_IO_BIND: combIOBIND = n; break;
#if WANT_STDIO
    case T_IO_STDIN:  SETTAG(n, T_PTR); PTR(n) = stdin;  break;
    case T_IO_STDOUT: SETTAG(n, T_PTR); PTR(n) = stdout; break;
    case T_IO_STDERR: SETTAG(n, T_PTR); PTR(n) = stderr; break;
#endif
    default:
      break;
    }
    for (int j = 0; j < sizeof primops / sizeof primops[0];j++) {
      if (primops[j].tag == t) {
        primops[j].node = n;
      }
    }
  }
#endif

  /* The representation of the constructors of
   *  data Ordering = LT | EQ | GT
   * do not have single constructors.
   * But we can make compound one, since they are irreducible.
   */
#define NEWAP(c, f, a) do { NODEPTR n = HEAPREF(heap_start++); SETTAG(n, T_AP); FUN(n) = (f); ARG(n) = (a); (c) = n;} while(0)
  NEWAP(combLT, combZ,     combFalse);  /* Z B */
  NEWAP(combEQ, combFalse, combFalse);  /* K K */
  NEWAP(combGT, combFalse, combTrue);   /* K A */
#undef NEWAP

#if INTTABLE
  /* Allocate permanent Int nodes */
  for (int i = LOW_INT; i < HIGH_INT; i++) {
    NODEPTR n = HEAPREF(heap_start++);
    intTable[i - LOW_INT] = n;
    SETTAG(n, T_INT);
    SETVALUE(n, i);
  }
#endif

  /* Round up heap_start to the next bitword boundary to avoid the permanent nodes. */
  heap_start = (heap_start + BITS_PER_WORD - 1) / BITS_PER_WORD * BITS_PER_WORD;

  mark_all_free();

  num_free = heap_size - heap_start;
}

#if GCRED
int red_a, red_k, red_i, red_int;
#endif

//counter_t mark_depth;

/* Mark all used nodes reachable from *np */
void
mark(NODEPTR *np)
{
  NODEPTR n;
#if GCRED
  value_t i;
#endif

  //  mark_depth++;
  //  if (mark_depth % 10000 == 0)
  //    printf("mark depth %"PRIcounter"\n", mark_depth);
  top:
  n = *np;
  if (GETTAG(n) == T_IND) {
#if SANITY
    int loop = 0;
    /* Skip indirections, and redirect start pointer */
    while (GETTAG(n) == T_IND) {
      //      printf("*"); fflush(stdout);
      n = INDIR(n);
      if (loop++ > 10000000) {
        //printf("%p %p %p\n", n, INDIR(n), INDIR(INDIR(n)));
        ERR("IND loop");
      }
    }
    //    if (loop)
    //      printf("\n");
#else  /* SANITY */
    while (GETTAG(n) == T_IND) {
      n = INDIR(n);
    }
#endif  /* SANITY */
    *np = n;
  }
  if (is_marked_used(n)) {
    //    mark_depth--;
    return;
  }
  num_marked++;
  mark_used(n);
#if GCRED
  /* This is really only fruitful just after parsing.  It can be removed. */
  if (GETTAG(n) == T_AP && GETTAG(FUN(n)) == T_AP && GETTAG(FUN(FUN(n))) == T_A) {
    /* Do the A x y --> y reduction */
    NODEPTR y = ARG(n);
    SETTAG(n, T_IND);
    INDIR(n) = y;
    red_a++;
    goto top;
  }
#if 0
  /* This never seems to happen */
  if (GETTAG(n) == T_AP && GETTAG(FUN(n)) == T_AP && GETTAG(FUN(FUN(n))) == T_K) {
    /* Do the K x y --> x reduction */
    NODEPTR x = ARG(FUN(n));
    SETTAG(n, T_IND);
    INDIR(n) = x;
    red_k++;
    goto top;
  }
#endif
  if (GETTAG(n) == T_AP && GETTAG(FUN(n)) == T_I) {
    /* Do the I x --> x reduction */
    NODEPTR x = ARG(n);
    SETTAG(n, T_IND);
    INDIR(n) = x;
    red_i++;
    goto top;
  }
#if INTTABLE
  if (GETTAG(n) == T_INT && LOW_INT <= (i = GETVALUE(n)) && i < HIGH_INT) {
    SETTAG(n, T_IND);
    INDIR(n) = intTable[i - LOW_INT];
    red_int++;
    goto top;
  }
#endif  /* INTTABLE */
#endif  /* GCRED */
  if (GETTAG(n) == T_AP) {
#if 1
    mark(&FUN(n));
    //mark(&ARG(n));
    np = &ARG(n);
    goto top;                   /* Avoid tail recursion */
#else
    mark(&ARG(n));
    np = &FUN(n);
    goto top;                   /* Avoid tail recursion */
#endif
  }
}

/* Perform a garbage collection:
   - First mark from all roots; roots are on the stack.
*/
void
gc(void)
{
  num_gc++;
  num_marked = 0;
#if WANT_STDIO
  if (verbose > 1)
    fprintf(stderr, "gc mark\n");
#endif
  gc_mark_time -= GETTIMEMILLI();
  mark_all_free();
  //  mark_depth = 0;
  for (stackptr_t i = 0; i <= stack_ptr; i++)
    mark(&stack[i]);
  gc_mark_time += GETTIMEMILLI();

  if (num_marked > max_num_marked)
    max_num_marked = num_marked;
  num_free = heap_size - heap_start - num_marked;
  if (num_free < heap_size / 50)
    ERR("heap exhausted");
#if WANT_STDIO
  if (verbose > 1)
    fprintf(stderr, "gc done, %"PRIcounter" free\n", num_free);
#endif  /* !WANT_STDIO */
}

/* Check that there are k nodes available, if not then GC. */
static INLINE void
gc_check(size_t k)
{
  if (k < num_free)
    return;
#if WANT_STDIO
  if (verbose > 1)
    fprintf(stderr, "gc_check: %d\n", (int)k);
#endif
  gc();
}

/*
 * Table of FFI callable functions.
 * (For a more flexible solution use dlopen()/dlsym()/dlclose())
 * The table contains the information needed to do the actual call.
 *   V    void
 *   I    value_t
 *   i    int
 *   D    double
 *   P    void*
 * The types are
 *   V    void    name(void)
 *   i    int     name(void)
 *   I    value_t name(voi)
 *   IV   void    name(value_t)
 *   II   value_t name(value_t)
 *   IIV  void    name(value_t, value_t)
 *   III  value_t name(value_t, value_t)
 *   DD   double  name(double)
 *   Pi   int     name(void*)
 *   PI   value_t name(void*)
 *   PP   void*   name(void*)
 *   iPi  int     name(int, void*)
 *   PPI  value_t name(void*, void*)
 *   PPP  void*   name(void*, void*)
 * more can easily be added.
 */
struct {
  const char *ffi_name;
  const funptr_t ffi_fun;
  enum { FFI_V, FFI_I, FFI_IV, FFI_II, FFI_IIV, FFI_III, FFI_DD, FFI_PI,
         FFI_i, FFI_Pi, FFI_iPi, FFI_PIIPI,
         FFI_PPI, FFI_PP, FFI_PPP, FFI_IPI, FFI_PV, FFI_IP, FFI_PPV,
  } ffi_how;
} ffi_table[] = {
  { "llabs",    (funptr_t)llabs,   FFI_II },
#if WANT_MATH
  { "log",      (funptr_t)log,     FFI_DD },
  { "exp",      (funptr_t)exp,     FFI_DD },
  { "sqrt",     (funptr_t)sqrt,    FFI_DD },
  { "sin",      (funptr_t)sin,     FFI_DD },
  { "cos",      (funptr_t)cos,     FFI_DD },
  { "tan",      (funptr_t)tan,     FFI_DD },
  { "asin",     (funptr_t)asin,    FFI_DD },
  { "acos",     (funptr_t)acos,    FFI_DD },
  { "atan",     (funptr_t)atan,    FFI_DD },
#endif  /* WANT_MATH */
  { "getenv",   (funptr_t)getenv,  FFI_PP },

  { "getRaw",   (funptr_t)GETRAW,  FFI_i },
#if WANT_STDIO
  { "fgetc",    (funptr_t)fgetc,   FFI_Pi },
  { "fputc",    (funptr_t)fputc,   FFI_iPi },
  //  { "cprint",   (funptr_t)cprint,  FFI_PAV },
  //  { "serialize",(funptr_t)serialize,  FFI_PAV },
  //  { "deserialize",(funptr_t)deserialize,  FFI_PA },
  { "fclose",   (funptr_t)fclose,  FFI_Pi },
  { "fflush",   (funptr_t)fflush,  FFI_Pi },
  { "fopen",    (funptr_t)fopen,   FFI_PPP },
  { "fread",    (funptr_t)fread,   FFI_PIIPI },
  { "fwrite",   (funptr_t)fwrite,  FFI_PIIPI },
  { "tmpname",  (funptr_t)TMPNAME, FFI_PPP },
  { "unlink",   (funptr_t)unlink,  FFI_Pi },
  { "system",   (funptr_t)system,  FFI_Pi },
#endif  /* WANT_STDIO */

#if WANT_MD5
  { "md5File",  (funptr_t)md5File, FFI_PPV },
#endif

  //  { "getArgs",   (funptr_t)getArgs,  FFI_A },
  { "getTimeMilli",(funptr_t)GETTIMEMILLI,  FFI_I },
  { "free",     (funptr_t)free,    FFI_PV },
  { "malloc",   (funptr_t)malloc,  FFI_IP }, /* The I is really a size_t */
};

/* Look up an FFI function by name */
value_t
lookupFFIname(const char *name)
{
  for(int i = 0; i < sizeof(ffi_table) / sizeof(ffi_table[0]); i++)
    if (strcmp(ffi_table[i].ffi_name, name) == 0)
      return (value_t)i;
  return -1;
}

NODEPTR
ffiNode(const char *buf)
{
  NODEPTR r;
  value_t i = lookupFFIname(buf);
  if (i < 0) {
    /* lookup failed, generate a node that will dynamically generate an error */
    r = alloc_node(T_BADDYN);
    char *fun = malloc(strlen(buf) + 1);
    strcpy(fun, buf);
    STR(r) = fun;
  } else {
    r = alloc_node(T_IO_CCALL);
    SETVALUE(r, i);
  }
  return r;
}

/* If the next input character is c, then consume it, else leave it alone. */
int
gobble(BFILE *f, int c)
{
  int d = f->getb(f);
  if (c == d) {
    return 1;
  } else {
    f->ungetb(d, f);
    return 0;
  }
}

/* Get a non-terminating character.  ' ' and ')' terminate a token. */
int
getNT(BFILE *f)
{
  int c;
  
  c = f->getb(f);
  if (c == ' ' || c == ')') {
    f->ungetb(c, f);
    return 0;
  } else {
    return c;
  }
}

value_t
parse_int(BFILE *f)
{
  value_t i = 0;
  value_t neg = 1;
  int c = f->getb(f);
  if (c == '-') {
    neg = -1;
    c = f->getb(f);
  }
  for(;;) {
    i = i * 10 + c - '0';
    c = f->getb(f);
    if (c < '0' || c > '9') {
      f->ungetb(c, f);
      break;
    }
  }
  return neg * i;
}

double
parse_double(BFILE *f)
{
  // apparently longest float, when rendered, takes up 24 characters. We add one more for a potential
  // minus sign, and another one for the final null terminator.
  // https://stackoverflow.com/questions/1701055/what-is-the-maximum-length-in-chars-needed-to-represent-any-double-value
  char buf[26];
  for(int j = 0; (buf[j] = getNT(f)); j++)
    ;

  return strtod(buf, NULL);;
}

NODEPTR
mkStrNode(const char *str)
{
  NODEPTR n = alloc_node(T_STR);
  STR(n) = str;
  return n;
}

NODEPTR mkInt(value_t i);
NODEPTR mkDbl(double d);
NODEPTR mkPtr(void* p);

/* Table of labelled nodes for sharing during parsing. */
struct shared_entry {
  heapoffs_t label;
  NODEPTR node;                 /* NIL indicates unused */
} *shared_table;
heapoffs_t shared_table_size;

/* Look for the label in the table.
 * If it's found, return the node.
 * If not found, return the first empty entry.
*/
NODEPTR *
find_label(heapoffs_t label)
{
  int hash = (int)(label % shared_table_size);
  for(int i = hash; ; i++) {
    if (shared_table[i].node == NIL) {
      /* The slot is empty, so claim and return it */
      shared_table[i].label = label;
      return &shared_table[i].node;
    } else if (shared_table[i].label == label) {
      /* Found the label, so return it. */
      return &shared_table[i].node;
    }
    /* Not empty and not found, try next. */
  }
}

NODEPTR
parse(BFILE *f)
{
  NODEPTR r;
  NODEPTR *nodep;
  heapoffs_t l;
  value_t i;
  double d;
  int c;
  char buf[80];                 /* store names of primitives. */

  c = f->getb(f);
  if (c < 0) ERR("parse EOF");
  switch (c) {
  case '(' :
    /* application: (f a) */
    r = alloc_node(T_AP);
    FUN(r) = parse(f);
    if (!gobble(f, ' ')) ERR("parse ' '");
    ARG(r) = parse(f);
    if (!gobble(f, ')')) ERR("parse ')'");
    return r;
  case '&':
    d = parse_double(f);
    r = mkDbl(d);
    return r;
  case '#':
    i = parse_int(f);
    r = mkInt(i);
    return r;
#if 0
  case '-':
    c = f->getb(f);
    neg = -1;
    if ('0' <= c && c <= '9') {
      goto number;
    } else {
      ERR("got -");
    }
  case '0':case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
    /* integer [0-9]+*/
    neg = 1;
  number:
    f->ungetb(c, f);
    i = neg * parse_int(f);
    r = mkInt(i);
    return r;
#endif
  case '_' :
    /* Reference to a shared value: _label */
    l = parse_int(f);  /* The label */
    nodep = find_label(l);
    if (*nodep == NIL) {
      /* Not yet defined, so make it an indirection */
      *nodep = alloc_node(T_IND);
      INDIR(*nodep) = NIL;
    }
    return *nodep;
  case ':' :
    /* Define a shared expression: :label e */
    l = parse_int(f);  /* The label */
    if (!gobble(f, ' ')) ERR("parse ' '");
    nodep = find_label(l);
    if (*nodep == NIL) {
      /* not referenced yet, so create a node */
      *nodep = alloc_node(T_IND);
      INDIR(*nodep) = NIL;
    } else {
      /* Sanity check */
      if (INDIR(*nodep) != NIL) ERR("shared != NIL");
    }
    r = parse(f);
    INDIR(*nodep) = r;
    return r;
  case '"' :
    /* Everything up to the next " is a string.
     * Special characters are encoded as \NNN&,
     * where NNN is the decimal value of the character */
    /* XXX assume there are no NULs in the string, and all fit in a char */
    /* XXX allocation is a hack */
    {
      char *buffer = malloc(10000);
      char *p = buffer;
      for(;;) {
        c = f->getb(f);
        if (c == '"')
          break;
        if (c == '\\') {
          *p++ = (char)parse_int(f);
          if (!gobble(f, '&'))
            ERR("parse string");
        } else {
          *p++ = c;
        }
      }
      *p++ = 0;
      r = mkStrNode(realloc(buffer, p - buffer));
      return r;
    }
  case '^':
    /* An FFI name */
    for (int j = 0; (buf[j] = getNT(f)); j++)
      ;
    r = ffiNode(buf);
    return r;
  default:
    buf[0] = c;
    /* A primitive, keep getting char's until end */
    for (int j = 1; (buf[j] = getNT(f)); j++)
      ;
    /* Look up the primop and use the preallocated node. */
    for (int j = 0; j < sizeof primops / sizeof primops[0]; j++) {
      if (strcmp(primops[j].name, buf) == 0) {
        return primops[j].node;
      }
    }
    ERR1("no primop %s", buf);
  }
}

void
checkversion(BFILE *f)
{
  char *p = VERSION;
  int c;

  while ((c = *p++)) {
    if (c != f->getb(f))
      ERR("version mismatch");
  }
  gobble(f, '\r');                 /* allow extra CR */
}

/* Parse a file */
NODEPTR
parse_top(BFILE *f)
{
  checkversion(f);
  heapoffs_t numLabels = parse_int(f);
  if (!gobble(f, '\n'))
    ERR("size parse");
  gobble(f, '\r');                 /* allow extra CR */
  shared_table_size = 3 * numLabels; /* sparsely populated hashtable */
  shared_table = malloc(shared_table_size * sizeof(struct shared_entry));
  if (!shared_table)
    memerr();
  for(heapoffs_t i = 0; i < shared_table_size; i++)
    shared_table[i].node = NIL;
  NODEPTR n = parse(f);
  free(shared_table);
  return n;
}

#if WANT_STDIO
NODEPTR
parse_FILE(FILE *f)
{
  BFILE *p = openb_FILE(f);
  /* And parse it */
  NODEPTR n = parse_top(p);
  p->closeb(p);
  return n;
}

NODEPTR
parse_file(const char *fn, size_t *psize)
{
  FILE *f = fopen(fn, "r");
  if (!f)
    ERR1("file not found %s", fn);

  /* And parse it */
  NODEPTR n = parse_FILE(f);
  *psize = ftell(f);
  return n;
}
#endif  /* WANT_STDIO */

counter_t num_shared;

/* Two bits per node: marked, shared
 * 0, 0   -- not visited
 * 1, 0   -- visited once
 * 1, 1   -- visited more than once
 * 0, 1   -- printed
 */
bits_t *marked_bits;
bits_t *shared_bits;
static INLINE void set_bit(bits_t *bits, NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  bits[i / BITS_PER_WORD] |= (1ULL << (i % BITS_PER_WORD));
}
#if WANT_STDIO
static INLINE void clear_bit(bits_t *bits, NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  bits[i / BITS_PER_WORD] &= ~(1ULL << (i % BITS_PER_WORD));
}
#endif
static INLINE int test_bit(bits_t *bits, NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  return (bits[i / BITS_PER_WORD] & (1ULL << (i % BITS_PER_WORD))) != 0;
}

#if WANT_STDIO
void printrec(FILE *f, NODEPTR n);

/* Mark all reachable nodes, when a marked node is reached, mark it as shared. */
void
find_sharing(NODEPTR n)
{
 top:
  while (GETTAG(n) == T_IND)
    n = INDIR(n);
  //printf("find_sharing %p %llu ", n, LABEL(n));
  if (GETTAG(n) == T_AP) {
    if (test_bit(shared_bits, n)) {
      /* Alread marked as shared */
      //printf("shared\n");
      ;
    } else if (test_bit(marked_bits, n)) {
      /* Already marked, so now mark as shared */
      //printf("marked\n");
      set_bit(shared_bits, n);
      num_shared++;
    } else {
      /* Mark as visited, and recurse */
      //printf("unmarked\n");
      set_bit(marked_bits, n);
      find_sharing(FUN(n));
      n = ARG(n);
      goto top;
    }
  } else {
    /* Not an application, so do nothing */
    //printf("not T_AP\n");
    ;
  }
}

/* Recursively print an expression.
   This assumes that the shared nodes has been marked as such.
*/
void
printrec(FILE *f, NODEPTR n)
{
  if (test_bit(shared_bits, n)) {
    /* The node is shared */
    if (test_bit(marked_bits, n)) {
      /* Not yet printed, so emit a label */
      fprintf(f, ":%"PRIheap" ", LABEL(n));
      clear_bit(marked_bits, n);  /* mark as printed */
    } else {
      /* This node has already been printed, so just use a reference. */
      fprintf(f, "_%"PRIheap, LABEL(n));
      return;
    }
  }

  switch (GETTAG(n)) {
  case T_IND: /*putc('*', f);*/ printrec(f, INDIR(n)); break;
  case T_AP:
    fputc('(', f);
    printrec(f, FUN(n));
    fputc(' ', f);
    printrec(f, ARG(n));
    fputc(')', f);
    break;
  case T_INT: fprintf(f, "#%"PRIvalue, GETVALUE(n)); break;
  case T_DBL: fprintf(f, "&%.16g", GETDBLVALUE(n)); break;
  case T_PTR:
    if (PTR(n) == stdin)
      fprintf(f, "IO.stdin");
    else if (PTR(n) == stdout)
      fprintf(f, "IO.stdout");
    else if (PTR(n) == stderr)
      fprintf(f, "IO.stderr");
    else
      ERR("Cannot serialize pointers");
    break;
  case T_STR:
    {
      const char *p = STR(n);
      int c;
      fputc('"', f);
      while ((c = *p++)) {
        if (c == '"' || c == '\\' || c < ' ' || c > '~') {
          fprintf(f, "\\%d&", c);
        } else {
          fputc(c, f);
        }
      }
      fputc('"', f);
      break;
    }
  case T_BADDYN: fprintf(f, "^%s", STR(n)); break;
  case T_S: fprintf(f, "S"); break;
  case T_K: fprintf(f, "K"); break;
  case T_I: fprintf(f, "I"); break;
  case T_C: fprintf(f, "C"); break;
  case T_B: fprintf(f, "B"); break;
  case T_A: fprintf(f, "A"); break;
  case T_U: fprintf(f, "U"); break;
  case T_Y: fprintf(f, "Y"); break;
  case T_P: fprintf(f, "P"); break;
  case T_R: fprintf(f, "R"); break;
  case T_O: fprintf(f, "O"); break;
  case T_SS: fprintf(f, "S'"); break;
  case T_BB: fprintf(f, "B'"); break;
  case T_Z: fprintf(f, "Z"); break;
  case T_CC: fprintf(f, "C'"); break;
  case T_ADD: fprintf(f, "+"); break;
  case T_SUB: fprintf(f, "-"); break;
  case T_MUL: fprintf(f, "*"); break;
  case T_QUOT: fprintf(f, "quot"); break;
  case T_REM: fprintf(f, "rem"); break;
  case T_UQUOT: fprintf(f, "uquot"); break;
  case T_UREM: fprintf(f, "urem"); break;
  case T_SUBR: fprintf(f, "subtract"); break;
  case T_NEG: fprintf(f, "neg"); break;
  case T_AND: fprintf(f, "and"); break;
  case T_OR: fprintf(f, "or"); break;
  case T_XOR: fprintf(f, "xor"); break;
  case T_INV: fprintf(f, "inv"); break;
  case T_SHL: fprintf(f, "shl"); break;
  case T_SHR: fprintf(f, "shr"); break;
  case T_ASHR: fprintf(f, "ashr"); break;
#if WANT_FLOAT
  case T_FADD: fprintf(f, "fadd"); break;
  case T_FSUB: fprintf(f, "fsub"); break;
  case T_FMUL: fprintf(f, "fmul"); break;
  case T_FDIV: fprintf(f, "fdiv"); break;
  case T_FNEG: fprintf(f, "fneg"); break;
  case T_ITOF: fprintf(f, "itof"); break;
  case T_FEQ: fprintf(f, "feq"); break;
  case T_FNE: fprintf(f, "fne"); break;
  case T_FLT: fprintf(f, "flt"); break;
  case T_FLE: fprintf(f, "fle"); break;
  case T_FGT: fprintf(f, "fgt"); break;
  case T_FGE: fprintf(f, "fge"); break;
  case T_FSHOW: fprintf(f, "fshow"); break;
  case T_FREAD: fprintf(f, "fread"); break;
#endif
  case T_EQ: fprintf(f, "=="); break;
  case T_NE: fprintf(f, "/="); break;
  case T_LT: fprintf(f, "<"); break;
  case T_LE: fprintf(f, "<="); break;
  case T_GT: fprintf(f, ">"); break;
  case T_GE: fprintf(f, ">="); break;
  case T_ULT: fprintf(f, "u<"); break;
  case T_ULE: fprintf(f, "u<="); break;
  case T_UGT: fprintf(f, "u>"); break;
  case T_UGE: fprintf(f, "u>="); break;
  case T_ERROR: fprintf(f, "error"); break;
  case T_NODEFAULT: fprintf(f, "noDefault"); break;
  case T_NOMATCH: fprintf(f, "noMatch"); break;
  case T_EQUAL: fprintf(f, "equal"); break;
  case T_COMPARE: fprintf(f, "compare"); break;
  case T_RNF: fprintf(f, "rnf"); break;
  case T_SEQ: fprintf(f, "seq"); break;
  case T_IO_BIND: fprintf(f, "IO.>>="); break;
  case T_IO_THEN: fprintf(f, "IO.>>"); break;
  case T_IO_RETURN: fprintf(f, "IO.return"); break;
  case T_IO_SERIALIZE: fprintf(f, "IO.serialize"); break;
  case T_IO_PRINT: fprintf(f, "IO.print"); break;
  case T_IO_DESERIALIZE: fprintf(f, "IO.deserialize"); break;
  case T_IO_GETARGS: fprintf(f, "IO.getArgs"); break;
  case T_IO_DROPARGS: fprintf(f, "IO.dropArgs"); break;
  case T_IO_GETTIMEMILLI: fprintf(f, "IO.getTimeMilli"); break;
  case T_IO_PERFORMIO: fprintf(f, "IO.performIO"); break;
  case T_IO_CCALL: fprintf(f, "^%s", ffi_table[GETVALUE(n)].ffi_name); break;
  case T_IO_CATCH: fprintf(f, "IO.catch"); break;
  case T_DYNSYM: fprintf(f, "dynsym"); break;
  case T_NEWCASTRING: fprintf(f, "newCAString"); break;
  case T_PEEKCASTRING: fprintf(f, "peekCAString"); break;
  case T_PEEKCASTRINGLEN: fprintf(f, "peekCAStringLen"); break;
  case T_TOINT: fprintf(f, "toInt"); break;
  case T_TOPTR: fprintf(f, "toPtr"); break;
  case T_TODBL: fprintf(f, "toDbl"); break;
  default: ERR("print tag");
  }
}

/* Serialize a graph to file. */
void
print(FILE *f, NODEPTR n, int header)
{
  num_shared = 0;
  marked_bits = calloc(free_map_nwords, sizeof(bits_t));
  if (!marked_bits)
    memerr();
  shared_bits = calloc(free_map_nwords, sizeof(bits_t));
  if (!shared_bits)
    memerr();
  find_sharing(n);
  if (header)
    fprintf(f, "%s%"PRIcounter"\n", VERSION, num_shared);
  printrec(f, n);
  free(marked_bits);
  free(shared_bits);
}

/* Show a graph. */
void
pp(FILE *f, NODEPTR n)
{
  print(f, n, 0);
  fprintf(f, "\n");
}
#endif  /* WANT_STDIO */

NODEPTR
mkInt(value_t i)
{
#if INTTABLE
  if (LOW_INT <= i && i < HIGH_INT) {
    return intTable[i - LOW_INT];
  }
#endif

  NODEPTR n;
  n = alloc_node(T_INT);
  SETVALUE(n, i);
  return n;
}

NODEPTR
mkDbl(double d)
{
  NODEPTR n;
  n = alloc_node(T_DBL);
  SETDBLVALUE(n, d);
  return n;
}

NODEPTR
mkPtr(void* p)
{
  NODEPTR n;
  n = alloc_node(T_PTR);
  PTR(n) = p;
  return n;
}

static INLINE NODEPTR
mkNil(void)
{
  return combFalse;
}

static INLINE NODEPTR
mkCons(NODEPTR x, NODEPTR xs)
{
  return new_ap(new_ap(combCons, x), xs);
}

size_t
strNodes(size_t len)
{
  /* Each character will need a CHAR node and a CONS node, a CONS uses 2 T_AP nodes */
  len *= (1 + 2);
  /* And each string will need a NIL */
  len += 1;
  return len;
}

/* Turn a C string into a combinator string */
NODEPTR
mkString(const char *astr, size_t len)
{
  NODEPTR n, nc;
  const unsigned char *str = (unsigned char*)astr; /* no sign bits, please */

  n = mkNil();
  for(size_t i = len; i > 0; i--) {
    nc = mkInt(str[i-1]);
    n = mkCons(nc, n);
  }
  return n;
}

NODEPTR
mkStringC(const char *str)
{
  return mkString(str, strlen(str));
}

void eval(NODEPTR n);

/* Evaluate and skip indirections. */
static INLINE NODEPTR
evali(NODEPTR n)
{
  /* Need to push and pop in case GC happens */
  PUSH(n);
  eval(n);
  n = TOP(0);
  POP(1);
  while (GETTAG(n) == T_IND)
    n = INDIR(n);
  return n;
}

/* Follow indirections */
static INLINE NODEPTR
indir(NODEPTR n)
{
  while (GETTAG(n) == T_IND)
    n = INDIR(n);
  return n;
}

/* Evaluate to an INT */
static INLINE value_t
evalint(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (GETTAG(n) != T_INT) {
    ERR1("evalint, bad tag %d", GETTAG(n));
  }
#endif
  return GETVALUE(n);
}

/* Evaluate to a Double */
static INLINE double
evaldbl(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (GETTAG(n) != T_DBL) {
    ERR1("evaldbl, bad tag %d", GETTAG(n));
  }
#endif
  return GETDBLVALUE(n);
}

/* Evaluate to a T_PTR */
void *
evalptr(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (GETTAG(n) != T_PTR) {
    ERR1("evalptr, bad tag %d", GETTAG(n));
  }
#endif
  return PTR(n);
}

/* Evaluate a string, returns a newly allocated buffer. */
/* XXX this is cheating, should use continuations */
char *
evalstring(NODEPTR n)
{
  size_t sz = 1000;
  size_t offs;
  char *name = malloc(sz);
  value_t c;
  NODEPTR x;

  if (!name)
    memerr();
  for (offs = 0;;) {
    if (offs >= sz - 1) {
      sz *= 2;
      name = realloc(name, sz);
      if (!name)
        memerr();
    }
    n = evali(n);
    if (GETTAG(n) == T_K)            /* Nil */
      break;
    else if (GETTAG(n) == T_AP && GETTAG(x = indir(FUN(n))) == T_AP && GETTAG(indir(FUN(x))) == T_O) { /* Cons */
      c = evalint(ARG(x));
      if (c < 0 || c > 127)
	ERR("invalid char");    /* Only allow ASCII */
      name[offs++] = (char)c;
      n = ARG(n);
    } else {
      ERR("evalstring not Nil/Cons");
    }
  }
  name[offs] = 0;
  return name;
}

/* Compares anything, but really only works well on strings.
 * if p < q  return -1
 * if p > q  return 1
 * if p == q return 0
 */
int
compare(NODEPTR p, NODEPTR q)
{
  int r;
  value_t x, y;
  void *f, *g;
  
 top:
  PUSH(q);                      /* save for GC */
  p = evali(p);
  q = evali(TOP(0));
  POP(1);
  enum node_tag ptag = GETTAG(p);
  enum node_tag qtag = GETTAG(q);
  if (ptag != qtag) {
    /* Hack to make Nil < Cons */
    if (ptag == T_K && qtag == T_AP)
      return -1;
    if (ptag == T_AP && qtag == T_K)
      return 1;
    return ptag < qtag ? -1 : 1;
  }
  switch (ptag) {
  case T_AP:
    PUSH(ARG(p));
    PUSH(ARG(q));
    r = compare(FUN(p), FUN(q));
    if (r != 0) {
      POP(2);
      return r;
    }
    q = TOP(0);
    p = TOP(1);
    POP(2);
    goto top;
  case T_INT:
  case T_IO_CCALL:
    x = GETVALUE(p);
    y = GETVALUE(q);
    return x < y ? -1 : x > y ? 1 : 0;
  case T_PTR:
    f = PTR(p);
    g = PTR(q);
    return f < g ? -1 : f > g ? 1 : 0;
  default:
    return 0;
  }
}

bits_t *rnf_bits;

void
rnf_rec(NODEPTR n)
{
 top:
  if (test_bit(rnf_bits, n))
    return;
  set_bit(rnf_bits, n);
  n = evali(n);
  if (GETTAG(n) == T_AP) {
    PUSH(ARG(n));               /* protect from GC */
    rnf_rec(FUN(n));
    n = TOP(0);
    POP(1);
    goto top;
  }
}

/* This is a yucky hack */
int doing_rnf = 0;

void
rnf(value_t noerr, NODEPTR n)
{
  /* Mark visited nodes to avoid getting stuck in loops. */
  rnf_bits = calloc(free_map_nwords, sizeof(bits_t));
  if (!rnf_bits)
    memerr();
  if (doing_rnf)
    ERR("recursive rnf()");
  doing_rnf = (int)noerr;
  rnf_rec(n);
  doing_rnf = 0;
  free(rnf_bits);
}

NODEPTR execio(NODEPTR n);

/* Evaluate a node, returns when the node is in WHNF. */
void
eval(NODEPTR n)
{
  stackptr_t stk = stack_ptr;
  NODEPTR x, y, z, w;
  value_t xi, yi, r;
#if WANT_FLOAT
  double xd, yd, rd;
#endif  /* WANT_FLOAT */
  char *msg;
  heapoffs_t l;

/* Reset stack pointer and return. */
#define RET do { stack_ptr = stk; return; } while(0)
/* Check that there are at least n arguments, return if not. */
#define CHECK(n) do { if (stack_ptr - stk < (n)) RET; } while(0)

#define SETIND(n, x) do { SETTAG((n), T_IND); INDIR((n)) = (x); } while(0)
#define GOIND(x) do { SETIND(n, (x)); goto ind; } while(0)
#define GOAP(f,a) do { FUN((n)) = (f); ARG((n)) = (a); goto ap; } while(0)
/* CHKARGN checks that there are at least N arguments.
 * It also
 *  - sets n to the "top" node
 *  - set x, y, ... to the arguments
 *  - pops N stack elements
 * NOTE: No GC is allowed after these, since the stack has been popped.
 */
#define CHKARG0 do { } while(0)
#define CHKARG1 do { CHECK(1); POP(1); n = TOP(-1); x = ARG(n); } while(0)
#define CHKARG2 do { CHECK(2); POP(2); n = TOP(-1); y = ARG(n); x = ARG(TOP(-2)); } while(0)
#define CHKARG3 do { CHECK(3); POP(3); n = TOP(-1); z = ARG(n); y = ARG(TOP(-2)); x = ARG(TOP(-3)); } while(0)
#define CHKARG4 do { CHECK(4); POP(4); n = TOP(-1); w = ARG(n); z = ARG(TOP(-2)); y = ARG(TOP(-3)); x = ARG(TOP(-4)); } while(0)

/* Alloc a possible GC action, e, between setting x and popping */
#define CHKARGEV1(e)  do { CHECK(1); x = ARG(TOP(0)); e; POP(1); n = TOP(-1); } while(0)

#define SETINT(n,r)    do { SETTAG((n), T_INT); SETVALUE((n), (r)); } while(0)
#define SETDBL(n,d)    do { SETTAG((n), T_DBL); SETDBLVALUE((n), (d)); } while(0)
#define OPINT1(e)      do { CHECK(1); xi = evalint(ARG(TOP(0)));                            e; POP(1); n = TOP(-1); } while(0);
#define OPINT2(e)      do { CHECK(2); xi = evalint(ARG(TOP(0))); yi = evalint(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define OPDBL1(e)      do { CHECK(1); xd = evaldbl(ARG(TOP(0)));                            e; POP(1); n = TOP(-1); } while(0);
#define OPDBL2(e)      do { CHECK(2); xd = evaldbl(ARG(TOP(0))); yd = evaldbl(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define ARITHUN(op)    do { OPINT1(r = op xi); SETINT(n, r); RET; } while(0)
#define ARITHBIN(op)   do { OPINT2(r = xi op yi); SETINT(n, r); RET; } while(0)
#define ARITHBINU(op)  do { OPINT2(r = (value_t)((uvalue_t)xi op (uvalue_t)yi)); SETINT(n, r); RET; } while(0)
#define FARITHUN(op)   do { OPDBL1(rd = op xd); SETDBL(n, rd); RET; } while(0)
#define FARITHBIN(op)  do { OPDBL2(rd = xd op yd); SETDBL(n, rd); RET; } while(0)
#define CMP(op)        do { OPINT2(r = xi op yi); GOIND(r ? combTrue : combFalse); } while(0)
#define CMPF(op)       do { OPDBL2(r = xd op yd); GOIND(r ? combTrue : combFalse); } while(0)
#define CMPU(op)       do { OPINT2(r = (uvalue_t)xi op (uvalue_t)yi); GOIND(r ? combTrue : combFalse); } while(0)

  for(;;) {
    num_reductions++;
#if FASTTAGS
    l = LABEL(n);
    enum node_tag tag = l < T_IO_BIND ? l : GETTAG(n);
#else   /* FASTTAGS */
    enum node_tag tag = GETTAG(n);
#endif  /* FASTTAGS */
    switch (tag) {
    ind:
      num_reductions++;
    case T_IND:  n = INDIR(n); break;

    ap:
      num_reductions++;
    case T_AP:   PUSH(n); n = FUN(n); break;

    case T_STR:  GCCHECK(strNodes(strlen(STR(n)))); GOIND(mkStringC(STR(n)));
    case T_INT:  RET;
    case T_DBL:  RET;
    case T_PTR:  RET;
    case T_BADDYN: ERR1("FFI unknown %s", STR(n));

    case T_S:    GCCHECK(2); CHKARG3; GOAP(new_ap(x, z), new_ap(y, z));                     /* S x y z = x z (y z) */
    case T_SS:   GCCHECK(3); CHKARG4; GOAP(new_ap(x, new_ap(y, w)), new_ap(z, w));          /* S' x y z w = x (y w) (z w) */
    case T_K:                CHKARG2; GOIND(x);                                             /* K x y = *x */
    case T_A:                CHKARG2; GOIND(y);                                             /* A x y = *y */
    case T_U:                CHKARG2; GOAP(y, x);                                           /* U x y = y x */
    case T_I:                CHKARG1; GOIND(x);                                             /* I x = *x */
    case T_Y:                CHKARG1; GOAP(x, n);                                           /* n@(Y x) = x n */
    case T_B:    GCCHECK(1); CHKARG3; GOAP(x, new_ap(y, z));                                /* B x y z = x (y z) */
    case T_BB:   GCCHECK(2); CHKARG4; GOAP(new_ap(x, y), new_ap(z, w));                     /* B' x y z w = x y (z w) */
    case T_Z:                CHKARG3; GOAP(x, y);                                           /* Z x y z = x y */
    case T_C:    GCCHECK(1); CHKARG3; GOAP(new_ap(x, z), y);                                /* C x y z = x z y */
    case T_CC:   GCCHECK(2); CHKARG4; GOAP(new_ap(x, new_ap(y, w)), z);                     /* C' x y z w = x (y w) z */
    case T_P:    GCCHECK(1); CHKARG3; GOAP(new_ap(z, x), y);                                /* P x y z = z x y */
    case T_R:    GCCHECK(1); CHKARG3; GOAP(new_ap(y, z), x);                                /* R x y z = y z x */
    case T_O:    GCCHECK(1); CHKARG4; GOAP(new_ap(w, x), y);                                /* O x y z w = w x y */

    case T_ADD:  ARITHBIN(+);
    case T_SUB:  ARITHBIN(-);
    case T_MUL:  ARITHBIN(*);
    case T_QUOT: ARITHBIN(/);
    case T_REM:  ARITHBIN(%);
    case T_SUBR: OPINT2(r = yi - xi); SETINT(n, r); RET;
    case T_UQUOT: ARITHBINU(/);
    case T_UREM:  ARITHBINU(%);
    case T_NEG:  ARITHUN(-);
    case T_AND:  ARITHBIN(&);
    case T_OR:   ARITHBIN(|);
    case T_XOR:  ARITHBIN(^);
    case T_INV:  ARITHUN(~);
    case T_SHL:  ARITHBIN(<<);
    case T_SHR:  ARITHBINU(>>);
    case T_ASHR: ARITHBIN(>>);

#if WANT_FLOAT
    case T_FADD: FARITHBIN(+);
    case T_FSUB: FARITHBIN(-);
    case T_FMUL: FARITHBIN(*);
    case T_FDIV: FARITHBIN(/);
    case T_FNEG: FARITHUN(-);
    case T_ITOF: OPINT1(rd = (double)xi); SETDBL(n, rd); RET;
    case T_FEQ: CMPF(==);
    case T_FNE: CMPF(!=);
    case T_FLT: CMPF(<);
    case T_FLE: CMPF(<=);
    case T_FGT: CMPF(>);
    case T_FGE: CMPF(>=);
    case T_FREAD:
      CHECK(1);
      msg = evalstring(ARG(TOP(0)));
      xd = strtod(msg, NULL);
      free(msg);

      POP(1);
      n = TOP(-1);
      
      GOIND(mkDbl(xd));

    case T_FSHOW:
      // check that the double exists
      CHECK(1);
      // evaluate it
      xd = evaldbl(ARG(TOP(0)));
      // turn it into a string
      char str[30];
      /* Using 16 decimals will lose some precision.
       * 17 would keep the precision, but it frequently looks very ugly.
       */
      (void)snprintf(str, 25, "%.16g", xd);
      if (!strchr(str, '.') && !strchr(str, 'e') && !strchr(str, 'E')) {
        /* There is no decimal point and no exponent, so add a decimal point */
        strcat(str, ".0");
      }

      // turn it into a mhs string
      GCCHECK(strNodes(strlen(str)));
      NODEPTR s = mkStringC(str);

      // remove the double from the stack
      POP(1);
      n = TOP(-1);
      // update n to be s
      GOIND(s);
#endif  /* WANT_FLOAT */

    /* Retag a word sized value, keeping the value bits */
#define CONV(t) do { CHECK(1); x = evali(ARG(TOP(0))); GCCHECK(1); y = alloc_node(t); SETVALUE(y, GETVALUE(x)); POP(1); n = TOP(-1); GOIND(y); } while(0)
    case T_TODBL: CONV(T_DBL);
    case T_TOINT: CONV(T_INT);
    case T_TOPTR: CONV(T_PTR);
#undef CONV

    case T_EQ:   CMP(==);
    case T_NE:   CMP(!=);
    case T_LT:   CMP(<);
    case T_LE:   CMP(<=);
    case T_GT:   CMP(>);
    case T_GE:   CMP(>=);
    case T_ULT:  CMPU(<);
    case T_ULE:  CMPU(<=);
    case T_UGT:  CMPU(>);
    case T_UGE:  CMPU(>=);

    case T_NOMATCH:
      if (doing_rnf) RET;
      {
      CHECK(3);
      msg = evalstring(ARG(TOP(0)));
      xi = evalint(ARG(TOP(1)));
      yi = evalint(ARG(TOP(2)));
      int sz = strlen(msg) + 100;
      char *res = malloc(sz);
#if WANT_STDIO
      snprintf(res, sz, "no match at %s, line %"PRIvalue", col %"PRIvalue, msg, xi, yi);
#else  /* WANT_STDIO */
      strcpy(res, "no match");
#endif  /* WANT_STDIO */
      POP(2);
      GCCHECK(strNodes(strlen(res)));
      ARG(TOP(0)) = mkStringC(res);
      free(res);
      free(msg);
      goto err;                 /* XXX not right message if the error is caught */
      }
    case T_NODEFAULT:
      if (doing_rnf) RET;
      {
      CHECK(1);
      msg = evalstring(ARG(TOP(0)));
      int sz = strlen(msg) + 100;
      char *res = malloc(sz);

#if WANT_STDIO
      snprintf(res, sz, "no default for %s", msg);
#else  /* WANT_STDIO */
      strcpy(res, "no default");
#endif  /* WANT_STDIO */
      GCCHECK(strNodes(strlen(res)));
      ARG(TOP(0)) = mkStringC(res);
      free(res);
      free(msg);
      goto err;                 /* XXX not right message if the error is caught */
      }
    case T_ERROR:
      if (doing_rnf) RET;
    err:
      if (cur_handler) {
        /* Pass the string to the handler */
        CHKARG1;
        cur_handler->hdl_exn = x;
        longjmp(cur_handler->hdl_buf, 1);
      } else {
        /* No handler, so just die. */
        CHKARGEV1(msg = evalstring(x));
#if WANT_STDIO
        fprintf(stderr, "mhs: %s\n", msg);
        exit(1);
#else  /* WANT_STDIO */
        ERR1("error: %s", msg);
#endif  /* WANT_STDIO */
      }
    case T_SEQ:  CHECK(2); eval(ARG(TOP(0))); POP(2); n = TOP(-1); y = ARG(n); GOIND(y); /* seq x y = eval(x); y */

    case T_EQUAL:
      CHECK(2); r = compare(ARG(TOP(0)), ARG(TOP(1))); POP(2); n = TOP(-1); GOIND(r==0 ? combTrue : combFalse);
    case T_COMPARE:
      CHECK(2); r = compare(ARG(TOP(0)), ARG(TOP(1))); POP(2); n = TOP(-1); GOIND(r < 0 ? combLT : r > 0 ? combGT : combEQ);

    case T_RNF:
      if (doing_rnf) RET;
      CHECK(2);
      xi = evalint(ARG(TOP(0)));
      rnf(xi, ARG(TOP(1))); POP(2); n = TOP(-1); GOIND(combUnit);

    case T_IO_PERFORMIO:
      if (doing_rnf) RET;
      CHKARGEV1(x = execio(x)); GOIND(x);

    case T_IO_BIND:
    case T_IO_THEN:
    case T_IO_RETURN:
    case T_IO_SERIALIZE:
    case T_IO_PRINT:
    case T_IO_DESERIALIZE:
    case T_IO_GETARGS:
    case T_IO_DROPARGS:
    case T_IO_GETTIMEMILLI:
    case T_IO_CCALL:
    case T_IO_CATCH:
    case T_NEWCASTRING:
    case T_PEEKCASTRING:
    case T_PEEKCASTRINGLEN:
      RET;

    case T_DYNSYM:
      /* A dynamic FFI lookup */
      CHECK(1);
      msg = evalstring(ARG(TOP(0)));
      GCCHECK(1);
      x = ffiNode(msg);
      free(msg);
      POP(1);
      n = TOP(-1);
      GOIND(x);

#if 0
    case T_ISINT:
      CHECK(1);
      x = evali(ARG(TOP(0)));
      n = TOP(0);
      POP(1);
      GOIND(GETTAG(x) == T_INT ? combTrue : combFalse);

    case T_ISIO:
      CHECK(1);
      x = evali(ARG(TOP(0)));
      n = TOP(0);
      POP(1);
      l = GETTAG(x);
      GOIND(T_IO_BIND <= l && l <= T_IO_FLUSH ? combTrue : combFalse);
#endif
    default:
      ERR1("eval tag %d", GETTAG(n));
    }
  }
}

/* This is the interpreter for the IO monad operations. */
/* It takes a monadic expression and returns the unwrapped expression (unevaluated). */
NODEPTR
execio(NODEPTR n)
{
  stackptr_t stk = stack_ptr;
  NODEPTR f, x;
  int c;
  char *name;
#if WANT_STDIO
  void *ptr;
  int hdr;
#endif  /* WANT_STDIO */

/* IO operations need all arguments, anything else should not happen. */
#define CHECKIO(n) do { if (stack_ptr - stk != (n+1)) {ERR("CHECKIO");}; } while(0)
#define RETIO(p) do { stack_ptr = stk; return (p); } while(0)
#define GCCHECKSAVE(p, n) do { PUSH(p); GCCHECK(n); (p) = TOP(0); POP(1); } while(0)

 top:
  n = evali(n);
  PUSH(n);
  for(;;) {
    num_reductions++;
    switch (GETTAG(n)) {
    case T_IND:
      n = INDIR(n);
      TOP(0) = n;
      break;
    case T_AP:
      n = FUN(n);
      PUSH(n);
      break;
    case T_IO_BIND:
      CHECKIO(2);
      {
        /* Use associativity to avoid deep execio recursion. */
        /* (m >>= g) >>= h      ===  m >>= (\ x -> g x >>= h) */
        /* BIND ((BIND m) g) h  ===  BIND m (\ x -> BIND (g x) h) == (BIND m) (((C' BIND) g) h)*/
        NODEPTR bm;
        NODEPTR bmg = evali(ARG(TOP(1)));
        GCCHECKSAVE(bmg, 4);
        if (GETTAG(bmg) == T_AP && GETTAG(bm = indir(FUN(bmg))) == T_AP && GETTAG(indir(FUN(bm))) == T_IO_BIND) {
          NODEPTR g = ARG(bmg);
          NODEPTR h = ARG(TOP(2));
          n = new_ap(bm, new_ap(new_ap(new_ap(combCC, combIOBIND), g), h));
          POP(3);
          goto top;
        }
      }

      x = execio(ARG(TOP(1)));  /* first argument, unwrapped */

      /* Do a GC check, make sure we keep the x live */
      GCCHECKSAVE(x, 1);

      f = ARG(TOP(2));          /* second argument, the continuation */
      n = new_ap(f, x);
      POP(3);
      goto top;
    case T_IO_THEN:
      CHECKIO(2);
      (void)execio(ARG(TOP(1))); /* first argument, unwrapped, ignored */
      n = ARG(TOP(2));          /* second argument, the continuation */
      POP(3);
      goto top;
    case T_IO_RETURN:
      CHECKIO(1);
      n = ARG(TOP(1));
      RETIO(n);
#if WANT_STDIO
    case T_IO_PRINT:
      hdr = 0;
      goto ser;
    case T_IO_SERIALIZE:
      hdr = 1;
    ser:
      CHECKIO(2);
      ptr = evalptr(ARG(TOP(1)));
      x = evali(ARG(TOP(2)));
      //x = ARG(TOP(1));
      print(ptr, x, hdr);
      fprintf(ptr, "\n");
      RETIO(combUnit);
    case T_IO_DESERIALIZE:
      CHECKIO(1);
      ptr = evalptr(ARG(TOP(1)));
      gc();                     /* parser runs without GC */
      n = parse_FILE(ptr);
      RETIO(n);
#endif
    case T_IO_GETARGS:
      CHECKIO(0);
      {
      /* compute total number of characters */
        size_t size = 0;
        for(int i = 0; i < glob_argc; i++) {
          size += strNodes(strlen(glob_argv[i]));
        }
        /* The returned list will need a CONS for each string, and a NIL */
        size += glob_argc * 2 + 1;
        GCCHECK(size);
        /*
        printf("total size %d:", size);
        for(int i = 0; i < glob_argc; i++)
          printf(" %s", glob_argv[i]);
        printf("\n");
        */
        n = mkNil();
        for(int i = glob_argc-1; i >= 0; i--) {
          n = mkCons(mkString(glob_argv[i], strlen(glob_argv[i])), n);
        }
      }
      RETIO(n);
    case T_IO_DROPARGS:
      CHECKIO(1);
      c = (int)evalint(ARG(TOP(1)));
      if (c > glob_argc)
        c = glob_argc;
      glob_argc -= c;
      glob_argv += c;
      RETIO(combUnit);
    case T_IO_CCALL:
      {
        int a = (int)GETVALUE(n);
        funptr_t f = ffi_table[a].ffi_fun;
        value_t ri, xi, yi, zi;
        double rd, xd;
        void *xp, *yp, *wp, *rp;
#define INTARG(n) evalint(ARG(TOP(n)))
#define PTRARG(n) evalptr(ARG(TOP(n)))
#define DBLARG(n) evaldbl(ARG(TOP(n)))
#define FFIV(n) CHECKIO(n)
#define FFI(n)  CHECKIO(n); GCCHECK(1)
        /* This isn't great, but this is MicroHs, so it's good enough. */
        switch (ffi_table[a].ffi_how) {
        case FFI_V:   FFIV(0);                                      (*                               f)();                     RETIO(combUnit);
        case FFI_I:   FFI (0);                                 ri = (*(value_t (*)(void            ))f)();      n = mkInt(ri); RETIO(n);
        case FFI_i:   FFI (0);                                 ri = (*(int     (*)(void            ))f)();      n = mkInt(ri); RETIO(n);
        case FFI_IV:  FFIV(1); xi = INTARG(1);                      (*(void    (*)(value_t         ))f)(xi);                   RETIO(combUnit);
        case FFI_II:  FFI (1); xi = INTARG(1);                 ri = (*(value_t (*)(value_t         ))f)(xi);    n = mkInt(ri); RETIO(n);
        case FFI_IIV: FFIV(2); xi = INTARG(1); yi = INTARG(2);      (*(void    (*)(value_t, value_t))f)(xi,yi);                RETIO(combUnit);
        case FFI_III: FFI (2); xi = INTARG(1); yi = INTARG(2); ri = (*(value_t (*)(value_t, value_t))f)(xi,yi); n = mkInt(ri); RETIO(n);
        case FFI_DD:  FFI (1); xd = DBLARG(1);                 rd = (*(double  (*)(double          ))f)(xd);    n = mkDbl(rd); RETIO(n);
        case FFI_PI:  FFI (1); xp = PTRARG(1);                 ri = (*(value_t (*)(void*           ))f)(xp);    n = mkInt(ri); RETIO(n);
        case FFI_Pi:  FFI (1); xp = PTRARG(1);                 ri = (*(int     (*)(void*           ))f)(xp);    n = mkInt(ri); RETIO(n);
        case FFI_IP:  FFI (1); xi = INTARG(1);                 rp = (*(void*   (*)(value_t         ))f)(xi);    n = mkPtr(rp); RETIO(n);
        case FFI_PP:  FFI (1); xp = PTRARG(1);                 rp = (*(void*   (*)(void*           ))f)(xp);    n = mkPtr(rp); RETIO(n);
        case FFI_PV:  FFI (1); xp = PTRARG(1);                      (*(void    (*)(void*           ))f)(xp);                   RETIO(combUnit);
        case FFI_PPI: FFI (2); xp = PTRARG(1);yp = PTRARG(2);  ri = (*(value_t (*)(void*, void*    ))f)(xp,yp); n = mkInt(ri); RETIO(n);
        case FFI_PPV: FFI (2); xp = PTRARG(1);yp = PTRARG(2);       (*(void    (*)(void*, void*    ))f)(xp,yp);                RETIO(combUnit);
        case FFI_PPP: FFI (2); xp = PTRARG(1);yp = PTRARG(2);  rp = (*(void*   (*)(void*, void*    ))f)(xp,yp); n = mkPtr(rp); RETIO(n);
        case FFI_IPI: FFI (2); xi = INTARG(1);yp = PTRARG(2);  ri = (*(value_t (*)(value_t, void*  ))f)(xi,yp); n = mkInt(ri); RETIO(n);
        case FFI_iPi: FFI (2); xi = INTARG(1);yp = PTRARG(2);  ri = (*(int     (*)(int,   void*    ))f)(xi,yp); n = mkInt(ri); RETIO(n);
        case FFI_PIIPI:FFI (4);xp = PTRARG(1);yi = INTARG(2); zi = INTARG(3); wp = PTRARG(4);
          ri = (*(int     (*)(void*, int, int, void*    ))f)(xp,yi,zi,wp); n = mkInt(ri); RETIO(n);
        default: ERR("T_IO_CCALL");
        }
      }

    case T_IO_CATCH:
      {
        struct handler *h = malloc(sizeof *h);
        if (!h)
          memerr();
        CHECKIO(2);
        h->hdl_old = cur_handler;
        h->hdl_stack = stack_ptr;
        cur_handler = h;
        if (setjmp(h->hdl_buf)) {
          /* An exception occurred: */
          stack_ptr = h->hdl_stack;
          x = h->hdl_exn;       /* exception value */
          GCCHECKSAVE(x, 1);
          f = ARG(TOP(2));      /* second argument, handler */
          n = new_ap(f, x);
          cur_handler = h->hdl_old;
          free(h);
          POP(3);
          goto top;
        } else {
          /* Normal execution: */
          n = execio(ARG(TOP(1))); /* execute first argument */
          cur_handler = h->hdl_old; /* restore old handler */
          free(h);
          RETIO(n);             /* return result */
        }
      }

    case T_NEWCASTRING:
      CHECKIO(1);
      name = evalstring(ARG(TOP(1)));
      GCCHECK(1);
      n = alloc_node(T_PTR);
      PTR(n) = name;
      RETIO(n);

    case T_PEEKCASTRING:
      {
      CHECKIO(1);
      name = evalptr(ARG(TOP(1)));
      size_t size = strlen(name);
      GCCHECK(strNodes(size));
      RETIO(mkString(name, size));
      }

    case T_PEEKCASTRINGLEN:
      {
      CHECKIO(2);
      size_t size = evalint(ARG(TOP(2)));
      name = evalptr(ARG(TOP(1)));
      GCCHECK(strNodes(size));
      RETIO(mkString(name, size));
      }

    default:
      ERR1("execio tag %d", GETTAG(n));
    }
  }
}

heapoffs_t
memsize(const char *p)
{
  heapoffs_t n = atoi(p);
  while (isdigit(*p))
    p++;
  switch (*p) {
  case 'k': case 'K': n *= 1000; break;
  case 'm': case 'M': n *= 1000000; break;
  case 'g': case 'G': n *= 1000000000; break;
  default: break;
  }
  return n;
}

extern uint8_t *combexpr;
extern int combexprlen;

int
main(int argc, char **argv)
{
  char *inname = 0;
  char **av;
  NODEPTR prog;
  int inrts;
#if WANT_STDIO
  char *outname = 0;
  size_t file_size;
#endif
  
#if 0
  /* MINGW doesn't do buffering right */
  setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
  setvbuf(stderr, NULL, _IONBF, BUFSIZ);
#endif

  argc--, argv++;
  glob_argv = argv;
  for (av = argv, inrts = 0; argc--; argv++) {
    char *p = *argv;
    if (inrts) {
      if (strcmp(p, "-RTS") == 0) {
        inrts = 0;
      } else {
        if (strcmp(p, "-v") == 0)
          verbose++;
        else if (strncmp(p, "-H", 2) == 0)
          heap_size = memsize(&p[2]);
        else if (strncmp(p, "-K", 2) == 0)
          stack_size = memsize(&p[2]);
        else if (strncmp(p, "-r", 2) == 0)
          inname = &p[2];
#if WANT_STDIO
        else if (strncmp(p, "-o", 2) == 0)
          outname = &p[2];
#endif  /* WANT_STDIO */
        else
          ERR("Usage: eval [+RTS [-v] [-Hheap-size] [-Kstack-size] [-rFILE] [-oFILE] -RTS] arg ...");
      }
    } else {
      if (strcmp(p, "+RTS") == 0) {
        inrts = 1;
      } else {
        *av++ = p;
      }
    }
  }
  glob_argc = av - glob_argv;

  if (inname == 0)
    inname = "out.comb";

  init_nodes();
  stack = malloc(sizeof(NODEPTR) * stack_size);
  if (!stack)
    memerr();

  if (combexpr) {
    int c;
    struct BFILE_buffer ibf = { { getb_buf, ungetb_buf, closeb_buf }, combexprlen, 0, combexpr };
    BFILE *bf = (BFILE*)&ibf;
    c = bf->getb(bf);
    /* Compressed combinators start with a 'Z', otherwise 'v' (for version) */
    if (c == 'Z') {
      /* add compressor transducer */
      bf = add_lzw_decompressor(bf);
    } else {
      /* put it back, we need it */
      bf->ungetb(c, bf);
    }
    prog = parse_top(bf);
    bf->closeb(bf);
  } else {
#if WANT_STDIO
    prog = parse_file(inname, &file_size);
#else
    ERR("no stdio");
#endif
  }

  PUSH(prog); gc(); prog = TOP(0); POP(1);
#if WANT_STDIO
  heapoffs_t start_size = num_marked;
  if (outname) {
    /* Save GCed file (smaller), and exit. */
    FILE *out = fopen(outname, "w");
    if (!out)
      ERR1("cannot open output file %s", outname);
    print(out, prog, 1);
    fclose(out);
    exit(0);
  }
  if (verbose > 2) {
    //pp(stdout, prog);
    print(stdout, prog, 1);
  }
#endif
  run_time -= GETTIMEMILLI();
  NODEPTR res = execio(prog);
  res = evali(res);
  run_time += GETTIMEMILLI();
#if WANT_STDIO
  if (0) {
    FILE *out = fopen("prog.comb", "w");
    print(out, prog, 1);
    fclose(out);
  }
  if (verbose) {
    if (verbose > 1) {
      printf("\nmain returns ");
      pp(stdout, res);
      printf("node size=%"PRIheap", heap size bytes=%"PRIheap"\n", (heapoffs_t)NODE_SIZE, heap_size * NODE_SIZE);
    }
    setlocale(LC_NUMERIC, "");  /* Make %' work on platforms that support it */
    printf("%"PCOMMA"15"PRIheap" combinator file size\n", (heapoffs_t)file_size);
    printf("%"PCOMMA"15"PRIheap" cells at start\n", start_size);
    printf("%"PCOMMA"15"PRIheap" cells heap size (%"PCOMMA""PRIheap" bytes)\n", heap_size, heap_size * NODE_SIZE);
    printf("%"PCOMMA"15"PRIcounter" cells allocated (%"PCOMMA".1f Mbyte/s)\n", num_alloc, num_alloc * NODE_SIZE / ((double)run_time / 1000) / 1000000);
    printf("%"PCOMMA"15"PRIcounter" GCs\n", num_gc);
    printf("%"PCOMMA"15"PRIcounter" max cells used\n", max_num_marked);
    printf("%"PCOMMA"15"PRIcounter" reductions (%"PCOMMA".1f Mred/s)\n", num_reductions, num_reductions / ((double)run_time / 1000) / 1000000);
    printf("%15.2fs total expired time\n", (double)run_time / 1000);
    printf("%15.2fs total gc time\n", (double)gc_mark_time / 1000);
#if GCRED && 0
    printf(" GC reductions A=%d, K=%d, I=%d, int=%d\n", red_a, red_k, red_i, red_int);
#endif
  }
#endif  /* WANT_STDIO */
  exit(0);
}
