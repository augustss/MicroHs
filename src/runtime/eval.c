/* Copyright 2023 Lennart Augustsson
 * See LICENSE file for full license.
 */
#include <inttypes.h>
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

#define VERSION "v6.1\n"

typedef intptr_t value_t;       /* Make value the same size as pointers, since they are in a union */
#define PRIvalue PRIdPTR
typedef uintptr_t uvalue_t;     /* Make unsigned value the same size as pointers, since they are in a union */
#define PRIuvalue PRIuPTR
typedef uintptr_t heapoffs_t;   /* Heap offsets */
#define PRIheap PRIuPTR
typedef uintptr_t tag_t;        /* Room for tag, low order bit indicates AP/not-AP */
typedef intptr_t stackptr_t;    /* Index into stack */

typedef uintptr_t counter_t;    /* Statistics counter, can be smaller since overflow doesn't matter */
#define PRIcounter PRIuPTR
typedef uintptr_t bits_t;       /* One word of bits */
#if WORD_SIZE == 64
typedef double flt_t;
#else
typedef float flt_t;
#endif

/* We cast all FFI functions to this type.  It's reasonably portable */
typedef void (*funptr_t)(void);

#if !defined(MALLOC)
#define MALLOC malloc
#endif

#if !defined(FREE)
#define FREE free
#endif

#if !defined(EXIT)
#define EXIT exit
#endif

#if !defined(PRINT)
#define PRINT printf
#endif

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
  char *s = MALLOC(strlen(pre) + 3 + strlen(post) + 1);
  strcpy(s, pre);
  strcat(s, "TMP");
  strcat(s, post);
  return s;
}
#endif

// #if !defined(MALLOC)
// #define MALLOC malloc
// #endif

#if !defined(INLINE)
#define INLINE inline
#endif  /* !define(INLINE) */

#if !defined(NORETURN)
#define NORETURN __attribute__ ((noreturn))
#endif /* !defined(NORETURN) */

value_t
iswindows(void)
{
#if defined(ISWINDOWS)
  return 1;
#else
  return 0;
#endif
}

/***************************************/

/* Keep permanent nodes for LOW_INT <= i < HIGH_INT */
#define LOW_INT (-10)
#define HIGH_INT 256

#include "config.h"

#if !defined(HEAP_CELLS)
#define HEAP_CELLS 50000000
#endif

#if !defined(STACK_SIZE)
#define STACK_SIZE 100000
#endif

#if !defined(ERR)
#if WANT_STDIO
#define ERR(s)    do { fprintf(stderr,"ERR: "s"\n");   EXIT(1); } while(0)
#define ERR1(s,a) do { fprintf(stderr,"ERR: "s"\n",a); EXIT(1); } while(0)
#else  /* WANT_STDIO */
#define ERR(s) EXIT(1)
#define ERR1(s,a) EXIT(1)
#endif  /* WANT_STDIO */
#endif  /* !define(ERR) */

enum node_tag { T_FREE, T_IND, T_AP, T_INT, T_DBL, T_PTR, T_BADDYN, T_ARR,
                T_S, T_K, T_I, T_B, T_C,
                T_A, T_Y, T_SS, T_BB, T_CC, T_P, T_R, T_O, T_U, T_Z,
                T_K2, T_K3, T_K4, T_CCB,
                T_ADD, T_SUB, T_MUL, T_QUOT, T_REM, T_SUBR, T_UQUOT, T_UREM, T_NEG,
                T_AND, T_OR, T_XOR, T_INV, T_SHL, T_SHR, T_ASHR,
                T_EQ, T_NE, T_LT, T_LE, T_GT, T_GE, T_ULT, T_ULE, T_UGT, T_UGE,
                T_PEQ, T_PNULL, T_PADD, T_PSUB,
                T_TOPTR, T_TOINT, T_TODBL,
#if WANT_FLOAT
                T_FADD, T_FSUB, T_FMUL, T_FDIV, T_FNEG, T_ITOF,
                T_FEQ, T_FNE, T_FLT, T_FLE, T_FGT, T_FGE, T_FSHOW, T_FREAD,
#endif
                T_ARR_ALLOC, T_ARR_SIZE, T_ARR_READ, T_ARR_WRITE, T_ARR_EQ,
                T_ERROR, T_NODEFAULT, T_NOMATCH, T_SEQ, T_EQUAL, T_COMPARE, T_RNF,
                T_TICK,
                T_IO_BIND, T_IO_THEN, T_IO_RETURN,
                T_IO_SERIALIZE, T_IO_DESERIALIZE,
                T_IO_STDIN, T_IO_STDOUT, T_IO_STDERR, T_IO_GETARGS,
                T_IO_PERFORMIO, T_IO_GETTIMEMILLI, T_IO_PRINT, T_IO_CATCH,
                T_IO_CCALL, T_DYNSYM,
                T_NEWCASTRINGLEN, T_PEEKCASTRING, T_PEEKCASTRINGLEN,
                T_STR,
                T_LAST_TAG,
};
#if 0
static const char* tag_names[] = {
  "FREE", "IND", "AP", "INT", "DBL", "PTR", "BADDYN", "ARR",
  "S", "K", "I", "B", "C",
  "A", "Y", "SS", "BB", "CC", "P", "R", "O", "U", "Z",
  "ADD", "SUB", "MUL", "QUOT", "REM", "SUBR", "UQUOT", "UREM", "NEG",
  "AND", "OR", "XOR", "INV", "SHL", "SHR", "ASHR",
  "EQ", "NE", "LT", "LE", "GT", "GE", "ULT", "ULE", "UGT", "UGE",
  "TOPTR", "TOINT", "TODBL",
#if WANT_FLOAT
  "FADD", "FSUB", "FMUL", "FDIV", "FNEG", "ITOF",
  "FEQ", "FNE", "FLT", "FLE", "FGT", "FGE", "FSHOW", "FREAD",
#endif
  "ARR_ALLOC", "ARR_SIZE", "ARR_READ", "ARR_WRITE", "ARR_EQ",
  "ERROR", "NODEFAULT", "NOMATCH", "SEQ", "EQUAL", "COMPARE", "RNF",
  "TICK",
  "IO_BIND", "IO_THEN", "IO_RETURN",
  "IO_SERIALIZE", "IO_DESERIALIZE",
  "IO_STDIN", "IO_STDOUT", "IO_STDERR", "IO_GETARGS",
  "IO_PERFORMIO", "IO_GETTIMEMILLI", "IO_PRINT", "IO_CATCH",
  "IO_CCALL", "DYNSYM",
  "NEWCASTRINGLEN", "PEEKCASTRING", "PEEKCASTRINGLEN",
  "STR",
  "LAST_TAG",
};
#endif

struct ioarray;

typedef struct node {
  union {
    struct node *uufun;
    tag_t        uutag;             /* LSB=1 indicates that this is a tag, LSB=0 that this is a T_AP node */
  } ufun;
  union {
    struct node    *uuarg;
    value_t         uuvalue;
    flt_t           uufloatvalue;
    const char     *uustring;
    void           *uuptr;
    struct ioarray *uuarray;
  } uarg;
} node;
typedef struct node* NODEPTR;
#define NIL 0
#define HEAPREF(i) &cells[(i)]
#define GETTAG(p) ((p)->ufun.uutag & 1 ? (int)((p)->ufun.uutag >> 1) : T_AP)
#define SETTAG(p,t) do { if (t != T_AP) (p)->ufun.uutag = ((t) << 1) + 1; } while(0)
#define GETVALUE(p) (p)->uarg.uuvalue
#define GETDBLVALUE(p) (p)->uarg.uufloatvalue
#define SETVALUE(p,v) (p)->uarg.uuvalue = v
#define SETDBLVALUE(p,v) (p)->uarg.uufloatvalue = v
#define FUN(p) (p)->ufun.uufun
#define ARG(p) (p)->uarg.uuarg
#define STR(p) (p)->uarg.uustring
#define PTR(p) (p)->uarg.uuptr
#define ARR(p) (p)->uarg.uuarray
#define INDIR(p) ARG(p)
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = MALLOC(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((heapoffs_t)((n) - cells))
node *cells;                 /* All cells */

/*
 * Arrays are allocated with MALLOC()/FREE().
 * During GC they are marked, and all elements in the array are
 * recursively marked.
 * At the end of the the mark phase there is a scan of all
 * arrays, and the unmarked ones are freed.
 */
struct ioarray {
  struct ioarray *next;         /* all ioarrays are linked together */
  int marked;                   /* marked during GC */
  size_t size;                  /* number of elements in the array */
  NODEPTR array[1];             /* actual size may be bigger */
};
struct ioarray *array_root = 0;

counter_t num_reductions = 0;
counter_t num_alloc;
counter_t num_gc = 0;
uintptr_t gc_mark_time = 0;
uintptr_t run_time = 0;

NODEPTR *stack;
stackptr_t stack_ptr = -1;
#if STACKOVL
#define PUSH(x) do { if (stack_ptr >= stack_size-1) ERR("stack overflow"); stack[++stack_ptr] = (x); } while(0)
#else  /* STACKOVL */
#define PUSH(x) do {                                                        stack[++stack_ptr] = (x); } while(0)
#endif  /* STACKOVL */
#define TOP(n) stack[stack_ptr - (n)]
#define POP(n) stack_ptr -= (n)
#define POPTOP() stack[stack_ptr--]
#define GCCHECK(n) gc_check((n))

heapoffs_t heap_size = HEAP_CELLS; /* number of heap cells */
heapoffs_t heap_start;             /* first location in heap that needs GC */
stackptr_t stack_size = STACK_SIZE;

counter_t num_marked;
counter_t max_num_marked = 0;
counter_t num_free;
counter_t num_arr_alloc;
counter_t num_arr_free;

#define BITS_PER_WORD (sizeof(bits_t) * 8)
bits_t *free_map;             /* 1 bit per node, 0=free, 1=used */
heapoffs_t free_map_nwords;
heapoffs_t next_scan_index;

int want_gc_red = 0;

NORETURN
void
memerr(void)
{
  ERR("Out of memory");
  EXIT(1);
}

struct ioarray*
arr_alloc(size_t sz, NODEPTR e)
{
  struct ioarray *arr = MALLOC(sizeof(struct ioarray) + (sz-1) * sizeof(NODEPTR));
  if (!arr)
    memerr();
  arr->next = array_root;
  array_root = arr;
  arr->marked = 0;
  arr->size = sz;
  for(size_t i = 0; i < sz; i++)
    arr->array[i] = e;
  //PRINT("arr_alloc(%d, %p) = %p\n", (int)sz, e, arr);
  num_arr_alloc++;
  return arr;
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
  if (p->unget) {
    c = p->unget;
    p->unget = 0;
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
      FREE(p->table[i]);
  }
  p->bfile->closeb(p->bfile);
  FREE(p);
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

struct tick_entry {
  const char *tick_name;
  counter_t tick_count;
} *tick_table = 0;
size_t tick_table_size;
size_t tick_index;

/* Allocate a new tick table entry and return the index. */
size_t
add_tick_table(const char *name)
{
  if (!tick_table) {
    tick_table_size = 100;
    tick_table = malloc(tick_table_size * sizeof(struct tick_entry));
    if (!tick_table)
      memerr();
    tick_index = 0;
  }
  if (tick_index >= tick_table_size) {
    tick_table_size *= 2;
    tick_table = realloc(tick_table, tick_table_size * sizeof(struct tick_entry));
    if (!tick_table)
      memerr();
  }
  tick_table[tick_index].tick_name = name;
  tick_table[tick_index].tick_count = 0;
  return tick_index++;
}


/* Called with the tick index. */
static inline void
dotick(value_t i)
{
  tick_table[i].tick_count++;
}

void
dump_tick_table(FILE *f)
{
  if (!tick_table) {
    fprintf(f, "Tick table empty\n");
    return;
  }
  for (size_t i = 0; i < tick_index; i++) {
    counter_t n = tick_table[i].tick_count;
    if (n)
      fprintf(f, "%-60s %10"PRIcounter"\n", tick_table[i].tick_name, n);
  }
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
    if (i >= free_map_nwords) {
#if 0
      fprintf(stderr, "wordsize=%d, num_free=%u next_scan_index=%u i=%u free_map_nwords=%u\n", (int)BITS_PER_WORD,
              (unsigned int)num_free, (unsigned int)next_scan_index, (unsigned int)i, (unsigned int)free_map_nwords);
#endif
      ERR("alloc_node: free_map");
    }
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
NODEPTR combFalse, combTrue, combUnit, combCons, combPair;
NODEPTR combCC, combZ, combIOBIND;
NODEPTR combLT, combEQ, combGT;

/* One node of each kind for primitives, these are never GCd. */
/* We use linear search in this, because almost all lookups
 * are among the combinators.
 */
struct {
  char *name;
  enum node_tag tag;
  enum node_tag flipped;        /* What should (C op) reduce to? defaults to T_FREE */
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
  { "K2", T_K2 },
  { "K3", T_K3 },
  { "K4", T_K4 },
  { "C'B", T_CCB },
/* primops */
  { "+", T_ADD, T_ADD },
  { "-", T_SUB, T_SUBR },
  { "*", T_MUL, T_MUL },
  { "quot", T_QUOT },
  { "rem", T_REM },
  { "uquot", T_UQUOT },
  { "urem", T_UREM },
  { "subtract", T_SUBR, T_SUB },
  { "neg", T_NEG },
  { "and", T_AND, T_AND },
  { "or", T_OR, T_OR },
  { "xor", T_XOR, T_XOR },
  { "inv", T_INV },
  { "shl", T_SHL },
  { "shr", T_SHR },
  { "ashr", T_ASHR },
#if WANT_FLOAT
  { "f+" , T_FADD, T_FADD},
  { "f-" , T_FSUB, T_FSUB},
  { "f*" , T_FMUL, T_FMUL},
  { "f/", T_FDIV},
  { "fneg", T_FNEG},
  { "itof", T_ITOF},
  { "f==", T_FEQ, T_FEQ},
  { "f/=", T_FNE, T_FNE},
  { "f<", T_FLT},
  { "f<=", T_FLE},
  { "f>", T_FGT},
  { "f>=", T_FGE},
  { "fshow", T_FSHOW},
  { "fread", T_FREAD},
#endif  /* WANT_FLOAT */
  { "ord", T_I },
  { "chr", T_I },
  { "==", T_EQ, T_EQ },
  { "/=", T_NE, T_NE },
  { "<", T_LT, T_GT },
  { "u<", T_ULT, T_UGT },
  { "u<=", T_ULE, T_UGE },
  { "u>", T_UGT, T_ULT },
  { "u>=", T_UGE, T_ULE },
  { "<=", T_LE, T_GE },
  { ">", T_GT, T_LT },
  { ">=", T_GE, T_LE },
  { "p==", T_PEQ, T_PEQ },
  { "pnull", T_PNULL },
  { "pcast", T_I },
  { "p+", T_PADD },
  { "p-", T_PSUB },
  { "seq", T_SEQ },
  { "error", T_ERROR },
  { "noDefault", T_NODEFAULT },
  { "noMatch", T_NOMATCH },
  { "equal", T_EQUAL, T_EQUAL },
  { "sequal", T_EQUAL, T_EQUAL },
  { "compare", T_COMPARE },
  { "scmp", T_COMPARE },
  { "icmp", T_COMPARE },
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
  { "IO.getTimeMilli", T_IO_GETTIMEMILLI },
  { "IO.performIO", T_IO_PERFORMIO },
  { "IO.catch", T_IO_CATCH },
  { "A.alloc", T_ARR_ALLOC },
  { "A.size", T_ARR_SIZE },
  { "A.read", T_ARR_READ },
  { "A.write", T_ARR_WRITE },
  { "A.==", T_ARR_EQ },
  { "dynsym", T_DYNSYM },
  { "newCAStringLen", T_NEWCASTRINGLEN },
  { "peekCAString", T_PEEKCASTRING },
  { "peekCAStringLen", T_PEEKCASTRINGLEN },
  { "toPtr", T_TOPTR },
  { "toInt", T_TOINT },
  { "toDbl", T_TODBL },
};

enum node_tag flip_ops[T_LAST_TAG];

void
init_nodes(void)
{
  ALLOC_HEAP(heap_size);
  free_map_nwords = (heap_size + BITS_PER_WORD - 1) / BITS_PER_WORD; /* bytes needed for free map */
  free_map = MALLOC(free_map_nwords * sizeof(bits_t));
  if (!free_map)
    memerr();

  /* Set up permanent nodes */
  heap_start = 0;
#if !FASTTAGS
  for (int j = 0; j < sizeof primops / sizeof primops[0]; j++) {
    NODEPTR n = HEAPREF(heap_start++);
    primops[j].node = n;
    //MARK(n) = MARKED;
    SETTAG(n, primops[j].tag);
    switch (primops[j].tag) {
    case T_K: combFalse = n; break;
    case T_A: combTrue = n; break;
    case T_I: combUnit = n; break;
    case T_O: combCons = n; break;
    case T_P: combPair = n; break;
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
    case T_P: combPair = n; break;
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
  for (unsigned int j = 0; j < sizeof primops / sizeof primops[0]; j++) {
    flip_ops[primops[j].tag] = primops[j].flipped;
  }

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
int red_a, red_k, red_i, red_int, red_flip;
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
  enum node_tag tag;

  //  mark_depth++;
  //  if (mark_depth % 10000 == 0)
  //    PRINT("mark depth %"PRIcounter"\n", mark_depth);
  top:
  n = *np;
  tag = GETTAG(n);
  if (tag == T_IND) {
#if SANITY
    int loop = 0;
    /* Skip indirections, and redirect start pointer */
    while ((tag = GETTAG(n)) == T_IND) {
      //      PRINT("*"); fflush(stdout);
      n = INDIR(n);
      if (loop++ > 10000000) {
        //PRINT("%p %p %p\n", n, INDIR(n), INDIR(INDIR(n)));
        ERR("IND loop");
      }
    }
    //    if (loop)
    //      PRINT("\n");
#else  /* SANITY */
    while ((tag = GETTAG(n)) == T_IND) {
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
  if (want_gc_red) {
    /* This is really only fruitful just after parsing.  It can be removed. */
    if (tag == T_AP && GETTAG(FUN(n)) == T_AP && GETTAG(FUN(FUN(n))) == T_A) {
      /* Do the A x y --> y reduction */
      NODEPTR y = ARG(n);
      SETTAG(n, T_IND);
      INDIR(n) = y;
      red_a++;
      goto top;
    }
#if 0
    /* This never seems to happen */
    if (tag == T_AP && GETTAG(FUN(n)) == T_AP && GETTAG(FUN(FUN(n))) == T_K) {
      /* Do the K x y --> x reduction */
      NODEPTR x = ARG(FUN(n));
      SETTAG(n, T_IND);
      INDIR(n) = x;
      red_k++;
      goto top;
    }
#endif  /* 0 */
    if (tag == T_AP && GETTAG(FUN(n)) == T_I) {
      /* Do the I x --> x reduction */
      NODEPTR x = ARG(n);
      SETTAG(n, T_IND);
      INDIR(n) = x;
      red_i++;
      goto top;
    }
#if 1
    /* This is broken.
     * Probably because it can happen in the middle of the C reduction code.
     */
    if (tag == T_AP && GETTAG(FUN(n)) == T_C) {
      NODEPTR q = ARG(n);
      enum node_tag tt, tf;
      while ((tt = GETTAG(q)) == T_IND)
        q = INDIR(q);
      if ((tf = flip_ops[tt])) {
        /* Do the C op --> flip_op reduction */
        // PRINT("%s -> %s\n", tag_names[tt], tag_names[tf]);
        SETTAG(n, T_IND);
        INDIR(n) = HEAPREF(tf);
        red_flip++;
        return;
        goto top;
      }
    }
  }
#endif
#if INTTABLE
  if (tag == T_INT && LOW_INT <= (i = GETVALUE(n)) && i < HIGH_INT) {
    SETTAG(n, T_IND);
    INDIR(n) = intTable[i - LOW_INT];
    red_int++;
    goto top;
  }
#endif  /* INTTABLE */
#endif  /* GCRED */
  if (tag == T_AP) {
    mark(&FUN(n));
    np = &ARG(n);
    goto top;                   /* Avoid tail recursion */
  } else if (tag == T_ARR) {
    struct ioarray *arr = ARR(n);
    /* It really should never happen that we encounter a marked
     * array, since the parent is marked.
     */
    if (!arr->marked) {
      arr->marked = 1;
      for(size_t i = 0; i < arr->size; i++)
        mark(&arr->array[i]);
    }
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
    PRINT("gc mark\n");
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

  for (struct ioarray **arrp = &array_root; *arrp; ) {
    struct ioarray *arr = *arrp;
    if (arr->marked) {
      arr->marked = 0;
      arrp = &arr->next;
    } else {
      *arrp = arr->next;        /* unlink */
      num_arr_free++;
      FREE(arr);                /* and FREE */
    }
  }

#if WANT_STDIO
  if (verbose > 1) {
    PRINT("gc done, %"PRIcounter" free\n", num_free);
    //PRINT(" GC reductions A=%d, K=%d, I=%d, int=%d flip=%d\n", red_a, red_k, red_i, red_int, red_flip);
  }
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
    PRINT("gc_check: %d\n", (int)k);
#endif
  gc();
}

value_t
peekWord(value_t *p)
{
  return *p;
}

void
pokeWord(value_t *p, value_t w)
{
  *p = w;
}

value_t
peekByte(uint8_t *p)
{
  return *p;
}

void
pokeByte(uint8_t *p, value_t w)
{
  *p = (uint8_t)w;
}

/*
 * Table of FFI callable functions.
 * (For a more flexible solution use dlopen()/dlsym()/dlclose())
 * The table contains the information needed to do the actual call.
 *   V    void
 *   I    value_t
 *   i    int
 *   D    flt_t
 *   P    void*
 * The types are
 *   V    void    name(void)
 *   i    int     name(void)
 *   I    value_t name(voi)
 *   IV   void    name(value_t)
 *   II   value_t name(value_t)
 *   IIV  void    name(value_t, value_t)
 *   III  value_t name(value_t, value_t)
 *   DD   flt_t  name(flt_t)
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
  enum { FFI_V, FFI_I, FFI_IV, FFI_II, FFI_IIV, FFI_III, FFI_DD, FFI_DDD, FFI_PI,
         FFI_i, FFI_Pi, FFI_iPi, FFI_PIIPI, FFI_PIV, FFI_IIP,
         FFI_PPI, FFI_PP, FFI_PPP, FFI_IPI, FFI_PV, FFI_IP, FFI_PPV, FFI_PPzV,
  } ffi_how;
} ffi_table[] = {
#if WORD_SIZE == 64
  { "llabs",    (funptr_t)llabs,   FFI_II },
#else  /* WORD_SIZE */
  { "llabs",    (funptr_t)labs,    FFI_II },
#endif  /* WORD_SIZE */
#if WANT_MATH
#if WORD_SIZE == 64
  { "log",      (funptr_t)log,     FFI_DD },
  { "exp",      (funptr_t)exp,     FFI_DD },
  { "sqrt",     (funptr_t)sqrt,    FFI_DD },
  { "sin",      (funptr_t)sin,     FFI_DD },
  { "cos",      (funptr_t)cos,     FFI_DD },
  { "tan",      (funptr_t)tan,     FFI_DD },
  { "asin",     (funptr_t)asin,    FFI_DD },
  { "acos",     (funptr_t)acos,    FFI_DD },
  { "atan",     (funptr_t)atan,    FFI_DD },
  { "atan2",    (funptr_t)atan2,   FFI_DDD },
#else  /* WORD_SIZE == 64 */
  { "log",      (funptr_t)logf,    FFI_DD },
  { "exp",      (funptr_t)expf,    FFI_DD },
  { "sqrt",     (funptr_t)sqrtf,   FFI_DD },
  { "sin",      (funptr_t)sinf,    FFI_DD },
  { "cos",      (funptr_t)cosf,    FFI_DD },
  { "tan",      (funptr_t)tanf,    FFI_DD },
  { "asin",     (funptr_t)asinf,   FFI_DD },
  { "acos",     (funptr_t)acosf,   FFI_DD },
  { "atan",     (funptr_t)atanf,   FFI_DD },
  { "atan2",    (funptr_t)atan2f,  FFI_DDD },  
#endif  /* WORD_SIZE == 64 */
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
  { "md5String",(funptr_t)md5String, FFI_PPV },
  { "md5Array", (funptr_t)md5Array, FFI_PPzV },
#endif

  { "iswindows",(funptr_t)iswindows, FFI_I },

  //  { "getArgs",   (funptr_t)getArgs,  FFI_A },
  { "getTimeMilli",(funptr_t)GETTIMEMILLI,  FFI_I },
  { "free",     (funptr_t)FREE,    FFI_PV },
  { "malloc",   (funptr_t)MALLOC,  FFI_IP }, /* The I is really a size_t */
  { "calloc",   (funptr_t)MALLOC,  FFI_IIP },
  { "peekWord", (funptr_t)peekWord,FFI_PI },
  { "pokeWord", (funptr_t)pokeWord,FFI_PIV },
  { "peekByte", (funptr_t)peekByte,FFI_PI },
  { "pokeByte", (funptr_t)pokeByte,FFI_PIV },
  { "memcpy",   (funptr_t)memcpy,  FFI_PPzV },
  { "memmove",  (funptr_t)memmove, FFI_PPzV },
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
    char *fun = MALLOC(strlen(buf) + 1);
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

#if WANT_FLOAT
flt_t
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
#endif

NODEPTR
mkStrNode(const char *str)
{
  NODEPTR n = alloc_node(T_STR);
  STR(n) = str;
  return n;
}

NODEPTR mkInt(value_t i);
NODEPTR mkFlt(flt_t d);
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

char *
parse_string(BFILE *f)
{
  size_t sz = 20;
  char *buffer = MALLOC(sz);
  size_t i;
  int c;

  if (!buffer)
    memerr();
  for(i = 0;;) {
    c = f->getb(f);
    if (c == '"')
      break;
    if (i >= sz) {
      sz *= 2;
      buffer = realloc(buffer, sz);
      if (!buffer)
        memerr();
    }
    if (c == '\\') {
      buffer[i++] = (char)parse_int(f);
      if (!gobble(f, '&'))
        ERR("parse string");
    } else {
      buffer[i++] = c;
    }
  }
  buffer[i++] = 0;
  return realloc(buffer, i);
}

NODEPTR
parse(BFILE *f)
{
  NODEPTR r;
  NODEPTR *nodep;
  heapoffs_t l;
  value_t i;
  flt_t d;
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
    r = mkFlt(d);
    return r;
  case '#':
    i = parse_int(f);
    r = mkInt(i);
    return r;
  case '[':
    {
      size_t sz = (size_t)parse_int(f);
      if (!gobble(f, ']')) ERR("parse arr 1");
      struct ioarray *arr = arr_alloc(sz, NIL);
      for (size_t i = 0; i < sz; i++) {
        if (!gobble(f, ' ')) ERR("parse arr 2");
        arr->array[i] = parse(f);
      }
      r = alloc_node(T_ARR);
      ARR(r) = arr;
      return r;
    }
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
    return mkStrNode(parse_string(f));
  case '!':
    if (!gobble(f, '"'))
      ERR("parse !");
    i = add_tick_table(parse_string(f));
    r = alloc_node(T_TICK);
    SETVALUE(r, (value_t)i);
    return r;
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
  shared_table = MALLOC(shared_table_size * sizeof(struct shared_entry));
  if (!shared_table)
    memerr();
  for(heapoffs_t i = 0; i < shared_table_size; i++)
    shared_table[i].node = NIL;
  NODEPTR n = parse(f);
  FREE(shared_table);
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
  //PRINT("find_sharing %p %llu ", n, LABEL(n));
  if (GETTAG(n) == T_AP) {
    if (test_bit(shared_bits, n)) {
      /* Alread marked as shared */
      //PRINT("shared\n");
      ;
    } else if (test_bit(marked_bits, n)) {
      /* Already marked, so now mark as shared */
      //PRINT("marked\n");
      set_bit(shared_bits, n);
      num_shared++;
    } else {
      /* Mark as visited, and recurse */
      //PRINT("unmarked\n");
      set_bit(marked_bits, n);
      find_sharing(FUN(n));
      n = ARG(n);
      goto top;
    }
  } else {
    /* Not an application, so do nothing */
    //PRINT("not T_AP\n");
    ;
  }
}

void
print_string(FILE *f, const char *p)
{
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
  case T_ARR:
    /* Arrays serialize as '[sz] e_1 ... e_sz' */
    fprintf(f, "[%u]", (unsigned)ARR(n)->size);
    for(size_t i = 0; i < ARR(n)->size; i++) {
      fputc(' ', f);
      printrec(f, ARR(n)->array[i]);
    }
    break;
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
    print_string(f, STR(n));
    break;
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
  case T_K2: fprintf(f, "K2"); break;
  case T_K3: fprintf(f, "K3"); break;
  case T_K4: fprintf(f, "K4"); break;
  case T_CC: fprintf(f, "C'"); break;
  case T_CCB: fprintf(f, "C'B"); break;
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
  case T_FADD: fprintf(f, "f+"); break;
  case T_FSUB: fprintf(f, "f-"); break;
  case T_FMUL: fprintf(f, "f*"); break;
  case T_FDIV: fprintf(f, "f/"); break;
  case T_FNEG: fprintf(f, "fneg"); break;
  case T_ITOF: fprintf(f, "itof"); break;
  case T_FEQ: fprintf(f, "f=="); break;
  case T_FNE: fprintf(f, "f/="); break;
  case T_FLT: fprintf(f, "f<"); break;
  case T_FLE: fprintf(f, "f<="); break;
  case T_FGT: fprintf(f, "f>"); break;
  case T_FGE: fprintf(f, "f>="); break;
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
  case T_PEQ: fprintf(f, "p=="); break;
  case T_PNULL: fprintf(f, "pnull"); break;
  case T_PADD: fprintf(f, "p+"); break;
  case T_PSUB: fprintf(f, "p-"); break;
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
  case T_IO_GETTIMEMILLI: fprintf(f, "IO.getTimeMilli"); break;
  case T_IO_PERFORMIO: fprintf(f, "IO.performIO"); break;
  case T_IO_CCALL: fprintf(f, "^%s", ffi_table[GETVALUE(n)].ffi_name); break;
  case T_IO_CATCH: fprintf(f, "IO.catch"); break;
  case T_ARR_ALLOC: fprintf(f, "A.alloc");
  case T_ARR_SIZE: fprintf(f, "A.size");
  case T_ARR_READ: fprintf(f, "A.read");
  case T_ARR_WRITE: fprintf(f, "A.write");
  case T_ARR_EQ: fprintf(f, "A.==");
  case T_DYNSYM: fprintf(f, "dynsym"); break;
  case T_NEWCASTRINGLEN: fprintf(f, "newCAStringLen"); break;
  case T_PEEKCASTRING: fprintf(f, "peekCAString"); break;
  case T_PEEKCASTRINGLEN: fprintf(f, "peekCAStringLen"); break;
  case T_TOINT: fprintf(f, "toInt"); break;
  case T_TOPTR: fprintf(f, "toPtr"); break;
  case T_TODBL: fprintf(f, "toDbl"); break;
  case T_TICK:
    fprintf(f, "!");
    print_string(f, tick_table[GETVALUE(n)].tick_name);
    break;
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
  FREE(marked_bits);
  FREE(shared_bits);
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
mkFlt(flt_t d)
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
  n = POPTOP();
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

/* Evaluate to a Flt_T */
static INLINE flt_t
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
evalstring(NODEPTR n, value_t *lenp)
{
  size_t sz = 1000;
  size_t offs;
  char *name = MALLOC(sz);
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
      PUSH(n);                  /* protect from GC */
      c = evalint(ARG(x));
      n = POPTOP();
      if (c < 0 || c > 127)
	ERR("invalid char");    /* Only allow ASCII */
      name[offs++] = (char)c;
      n = ARG(n);
    } else {
      ERR("evalstring not Nil/Cons");
    }
  }
  name[offs] = 0;
  if (lenp)
    *lenp = (value_t)offs;
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
  case T_ARR:
    return ARR(p) < ARR(q) ? -1 : ARR(p) > ARR(q) ? 1 : 0;
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
    n = POPTOP();
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
  FREE(rnf_bits);
}

NODEPTR execio(NODEPTR n);

/* Evaluate a node, returns when the node is in WHNF. */
void
eval(NODEPTR an)
{
  NODEPTR n = an;
  stackptr_t stk = stack_ptr;
  NODEPTR x, y, z, w;
  value_t xi, yi, r;
  void *xp, *yp;
#if WANT_FLOAT
  flt_t xd, yd, rd;
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
#define CHKARGEV1(e)   do { CHECK(1); x = ARG(TOP(0)); e; POP(1); n = TOP(-1); } while(0)

#define SETINT(n,r)    do { SETTAG((n), T_INT); SETVALUE((n), (r)); } while(0)
#define SETDBL(n,d)    do { SETTAG((n), T_DBL); SETDBLVALUE((n), (d)); } while(0)
#define SETPTR(n,r)    do { SETTAG((n), T_PTR); PTR(n) = (r); } while(0)
#define OPINT1(e)      do { CHECK(1); xi = evalint(ARG(TOP(0)));                            e; POP(1); n = TOP(-1); } while(0);
#define OPINT2(e)      do { CHECK(2); xi = evalint(ARG(TOP(0))); yi = evalint(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define OPDBL1(e)      do { CHECK(1); xd = evaldbl(ARG(TOP(0)));                            e; POP(1); n = TOP(-1); } while(0);
#define OPDBL2(e)      do { CHECK(2); xd = evaldbl(ARG(TOP(0))); yd = evaldbl(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define OPPTR2(e)      do { CHECK(2); xp = evalptr(ARG(TOP(0))); yp = evalptr(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define ARITHUN(op)    do { OPINT1(r = op xi); SETINT(n, r); RET; } while(0)
#define ARITHBIN(op)   do { OPINT2(r = xi op yi); SETINT(n, r); RET; } while(0)
#define ARITHBINU(op)  do { OPINT2(r = (value_t)((uvalue_t)xi op (uvalue_t)yi)); SETINT(n, r); RET; } while(0)
#define FARITHUN(op)   do { OPDBL1(rd = op xd); SETDBL(n, rd); RET; } while(0)
#define FARITHBIN(op)  do { OPDBL2(rd = xd op yd); SETDBL(n, rd); RET; } while(0)
#define CMP(op)        do { OPINT2(r = xi op yi); GOIND(r ? combTrue : combFalse); } while(0)
#define CMPF(op)       do { OPDBL2(r = xd op yd); GOIND(r ? combTrue : combFalse); } while(0)
#define CMPU(op)       do { OPINT2(r = (uvalue_t)xi op (uvalue_t)yi); GOIND(r ? combTrue : combFalse); } while(0)
#define CMPP(op)       do { OPPTR2(r = xp op yp); GOIND(r ? combTrue : combFalse); } while(0)

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
    case T_ARR:  RET;
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
    case T_K2:               CHKARG3; GOIND(x);                                             /* K2 x y z = *x */
    case T_K3:               CHKARG4; GOIND(x);                                             /* K3 x y z w = *x */
    case T_K4:               CHECK(5); POP(5); n = TOP(-1); x = ARG(TOP(-5)); GOIND(x);     /* K4 x y z w v = *x */
    case T_CCB:  GCCHECK(2); CHKARG4; GOAP(new_ap(x, z), new_ap(y, w));                     /* C'B x y z w = x z (y w) */

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
    case T_ITOF: OPINT1(rd = (flt_t)xi); SETDBL(n, rd); RET;
    case T_FEQ: CMPF(==);
    case T_FNE: CMPF(!=);
    case T_FLT: CMPF(<);
    case T_FLE: CMPF(<=);
    case T_FGT: CMPF(>);
    case T_FGE: CMPF(>=);
    case T_FREAD:
      CHECK(1);
      msg = evalstring(ARG(TOP(0)), 0);
#if WORD_SIZE == 64
      xd = strtod(msg, NULL);
#else
      xd = strtof(msg, NULL);
#endif
      FREE(msg);

      POP(1);
      n = TOP(-1);
      
      GOIND(mkFlt(xd));

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
        /* XXX wrong for inf and NaN */
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

    case T_PEQ:  CMPP(==);
    case T_PNULL: SETTAG(n, T_PTR); PTR(n) = 0; RET;
    case T_PADD: CHECK(2); xp = evalptr(ARG(TOP(0))); yi = evalint(ARG(TOP(1))); POP(2); n = TOP(-1); SETPTR(n, (char*)xp + yi); RET;
    case T_PSUB: CHECK(2); xp = evalptr(ARG(TOP(0))); yp = evalptr(ARG(TOP(1))); POP(2); n = TOP(-1); SETINT(n, (char*)xp - (char*)yp); RET;

    case T_ARR_EQ:
      {
        CHECK(2);
        x = evali(ARG(TOP(0)));
        struct ioarray *arr = ARR(x);
        y = evali(ARG(TOP(1)));
        POP(2);
        n = TOP(-1);
        GOIND(arr == ARR(y) ? combTrue : combFalse);
      }

    case T_NOMATCH:
      if (doing_rnf) RET;
      {
      CHECK(3);
      msg = evalstring(ARG(TOP(0)), 0);
      xi = evalint(ARG(TOP(1)));
      yi = evalint(ARG(TOP(2)));
      int sz = strlen(msg) + 100;
      char *res = MALLOC(sz);
#if WANT_STDIO
      snprintf(res, sz, "no match at %s, line %"PRIvalue", col %"PRIvalue, msg, xi, yi);
#else  /* WANT_STDIO */
      strcpy(res, "no match");
#endif  /* WANT_STDIO */
      POP(2);
      GCCHECK(strNodes(strlen(res)));
      ARG(TOP(0)) = mkStringC(res);
      FREE(res);
      FREE(msg);
      goto err;                 /* XXX not right message if the error is caught */
      }
    case T_NODEFAULT:
      if (doing_rnf) RET;
      {
      CHECK(1);
      msg = evalstring(ARG(TOP(0)), 0);
      int sz = strlen(msg) + 100;
      char *res = MALLOC(sz);

#if WANT_STDIO
      snprintf(res, sz, "no default for %s", msg);
#else  /* WANT_STDIO */
      strcpy(res, "no default");
#endif  /* WANT_STDIO */
      GCCHECK(strNodes(strlen(res)));
      ARG(TOP(0)) = mkStringC(res);
      FREE(res);
      FREE(msg);
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
        CHKARGEV1(msg = evalstring(x, 0));
#if WANT_STDIO
        ERR1("mhs: %s\n", msg);
        EXIT(1);
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
    case T_IO_GETTIMEMILLI:
    case T_IO_CCALL:
    case T_IO_CATCH:
    case T_NEWCASTRINGLEN:
    case T_PEEKCASTRING:
    case T_PEEKCASTRINGLEN:
    case T_ARR_ALLOC:
    case T_ARR_SIZE:
    case T_ARR_READ:
    case T_ARR_WRITE:
      RET;

    case T_DYNSYM:
      /* A dynamic FFI lookup */
      CHECK(1);
      msg = evalstring(ARG(TOP(0)), 0);
      GCCHECK(1);
      x = ffiNode(msg);
      FREE(msg);
      POP(1);
      n = TOP(-1);
      GOIND(x);

    case T_TICK:
      xi = GETVALUE(n);
      CHKARG1;
      dotick(xi);
      GOIND(x);

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
  char *name;
  value_t len;
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

#if 0
    case T_TICK:
      CHECKIO(2);
      if (GETTAG(ARG(TOP(1))) == T_STR)
        printf("tick: %s\n", STR(ARG(TOP(1))));
      POP(2);
      TOP(0) = ARG(TOP(0));
      break;
#endif

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
      gc();                     /* DUBIOUS: do a GC to get possible GC reductions */
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
        PRINT("total size %d:", size);
        for(int i = 0; i < glob_argc; i++)
          PRINT(" %s", glob_argv[i]);
        PRINT("\n");
        */
        n = mkNil();
        for(int i = glob_argc-1; i >= 0; i--) {
          n = mkCons(mkString(glob_argv[i], strlen(glob_argv[i])), n);
        }
      }
      RETIO(n);

    case T_IO_CCALL:
      {
        int a = (int)GETVALUE(n);
        funptr_t f = ffi_table[a].ffi_fun;
        value_t ri, xi, yi, zi;
#if WANT_FLOAT
        flt_t rd, xd, yd;
#endif  /* WANT_FLOAT */
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
#if WANT_FLOAT
        case FFI_DD:  FFI (1); xd = DBLARG(1);                 rd = (*(flt_t   (*)(flt_t           ))f)(xd);    n = mkFlt(rd); RETIO(n);
        case FFI_DDD: FFI (2); xd = DBLARG(1); yd = DBLARG(2); rd = (*(flt_t   (*)(flt_t,   flt_t  ))f)(xd,yd); n = mkFlt(rd); RETIO(n);
#endif  /* WANT_FLOAT */
        case FFI_PI:  FFI (1); xp = PTRARG(1);                 ri = (*(value_t (*)(void*           ))f)(xp);    n = mkInt(ri); RETIO(n);
        case FFI_Pi:  FFI (1); xp = PTRARG(1);                 ri = (*(int     (*)(void*           ))f)(xp);    n = mkInt(ri); RETIO(n);
        case FFI_IP:  FFI (1); xi = INTARG(1);                 rp = (*(void*   (*)(value_t         ))f)(xi);    n = mkPtr(rp); RETIO(n);
        case FFI_PP:  FFI (1); xp = PTRARG(1);                 rp = (*(void*   (*)(void*           ))f)(xp);    n = mkPtr(rp); RETIO(n);
        case FFI_PV:  FFI (1); xp = PTRARG(1);                      (*(void    (*)(void*           ))f)(xp);                   RETIO(combUnit);
        case FFI_PPI: FFI (2); xp = PTRARG(1);yp = PTRARG(2);  ri = (*(value_t (*)(void*, void*    ))f)(xp,yp); n = mkInt(ri); RETIO(n);
        case FFI_PIV: FFI (2); xp = PTRARG(1);yi = INTARG(2);       (*(void    (*)(void*, value_t  ))f)(xp,yi);                RETIO(combUnit);
        case FFI_PPV: FFI (2); xp = PTRARG(1);yp = PTRARG(2);       (*(void    (*)(void*, void*    ))f)(xp,yp);                RETIO(combUnit);
        case FFI_IIP: FFI (2); xi = INTARG(1);yi = INTARG(2);  rp = (*(void*   (*)(value_t,value_t ))f)(xi,yi); n = mkPtr(rp); RETIO(n);
        case FFI_PPP: FFI (2); xp = PTRARG(1);yp = PTRARG(2);  rp = (*(void*   (*)(void*, void*    ))f)(xp,yp); n = mkPtr(rp); RETIO(n);
        case FFI_IPI: FFI (2); xi = INTARG(1);yp = PTRARG(2);  ri = (*(value_t (*)(value_t, void*  ))f)(xi,yp); n = mkInt(ri); RETIO(n);
        case FFI_iPi: FFI (2); xi = INTARG(1);yp = PTRARG(2);  ri = (*(int     (*)(int,   void*    ))f)(xi,yp); n = mkInt(ri); RETIO(n);
        case FFI_PPzV:FFI (3); xp = PTRARG(1);yp = PTRARG(2); zi = INTARG(3); (*(void    (*)(void*, void*, size_t))f)(xp,yp,zi); RETIO(combUnit);
        case FFI_PIIPI:FFI (4);xp = PTRARG(1);yi = INTARG(2); zi = INTARG(3); wp = PTRARG(4);
          ri = (*(int     (*)(void*, int, int, void*    ))f)(xp,yi,zi,wp); n = mkInt(ri); RETIO(n);
        default: ERR("T_IO_CCALL");
        }
      }

    case T_IO_CATCH:
      {
        struct handler *h = MALLOC(sizeof *h);
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
          FREE(h);
          POP(3);
          goto top;
        } else {
          /* Normal execution: */
          n = execio(ARG(TOP(1))); /* execute first argument */
          cur_handler = h->hdl_old; /* restore old handler */
          FREE(h);
          RETIO(n);             /* return result */
        }
      }

    case T_NEWCASTRINGLEN:
      CHECKIO(1);
      name = evalstring(ARG(TOP(1)), &len);
      GCCHECK(4);
      n = new_ap(new_ap(combPair, x = alloc_node(T_PTR)), mkInt(len));
      PTR(x) = name;
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

    case T_ARR_ALLOC:
      {
      CHECKIO(2);
      size_t size = evalint(ARG(TOP(1)));
      NODEPTR elem = ARG(TOP(2));
      struct ioarray *arr = arr_alloc(size, elem);
      n = alloc_node(T_ARR);
      ARR(n) = arr;
      RETIO(n);
      }
    case T_ARR_SIZE:
      CHECKIO(1);
      n = evali(ARG(TOP(1)));
      if (GETTAG(n) != T_ARR)
        ERR("bad ARR tag");
      RETIO(mkInt(ARR(n)->size));
    case T_ARR_READ:
      {
      CHECKIO(2);
      size_t i = evalint(ARG(TOP(2)));
      n = evali(ARG(TOP(1)));
      if (GETTAG(n) != T_ARR)
        ERR("bad ARR tag");
      if (i >= ARR(n)->size)
        ERR("ARR_READ");
      RETIO(ARR(n)->array[i]);
      }
    case T_ARR_WRITE:
      {
      CHECKIO(3);
      size_t i = evalint(ARG(TOP(2)));
      n = evali(ARG(TOP(1)));
      if (GETTAG(n) != T_ARR)
        ERR("bad ARR tag");
      if (i >= ARR(n)->size) {
        //PRINT("%d %p %d\n", (int)i, ARR(n), (int)ARR(n)->size);
        ERR("ARR_WRITE");
      }
      ARR(n)->array[i] = ARG(TOP(3));
      RETIO(combUnit);
      }

    default:
      //printf("bad tag %s\n", tag_names[GETTAG(n)]);
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
  int dump_ticks = 0;
  
#if 0
  /* MINGW doesn't do buffering right */
  setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
  setvbuf(stderr, NULL, _IONBF, BUFSIZ);
#endif

#ifdef INITIALIZATION
  main_setup(); // void main_setup(void); will perform extra initialization
                // that is unique to a specific platform, e.g. initialization
                // a HAL
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
        else if (strcmp(p, "-T") == 0)
          dump_ticks = 1;
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
  stack = MALLOC(sizeof(NODEPTR) * stack_size);
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

  /* GC unused stuff, nice for -o */
  PUSH(prog);
  want_gc_red = 1;
  gc();
  want_gc_red = 0;
  prog = POPTOP();

#if WANT_STDIO
  heapoffs_t start_size = num_marked;
  if (outname) {
    /* Save GCed file (smaller), and exit. */
    FILE *out = fopen(outname, "w");
    if (!out)
      ERR1("cannot open output file %s", outname);
    print(out, prog, 1);
    fclose(out);
    EXIT(0);
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
      PRINT("\nmain returns ");
      pp(stdout, res);
      PRINT("node size=%"PRIheap", heap size bytes=%"PRIheap"\n", (heapoffs_t)NODE_SIZE, heap_size * NODE_SIZE);
    }
    setlocale(LC_NUMERIC, "");  /* Make %' work on platforms that support it */
    PRINT("%"PCOMMA"15"PRIheap" combinator file size\n", (heapoffs_t)file_size);
    PRINT("%"PCOMMA"15"PRIheap" cells at start\n", start_size);
    PRINT("%"PCOMMA"15"PRIheap" cells heap size (%"PCOMMA""PRIheap" bytes)\n", heap_size, heap_size * NODE_SIZE);
    PRINT("%"PCOMMA"15"PRIcounter" cells allocated (%"PCOMMA".1f Mbyte/s)\n", num_alloc, num_alloc * NODE_SIZE / ((double)run_time / 1000) / 1000000);
    PRINT("%"PCOMMA"15"PRIcounter" GCs\n", num_gc);
    PRINT("%"PCOMMA"15"PRIcounter" max cells used\n", max_num_marked);
    PRINT("%"PCOMMA"15"PRIcounter" reductions (%"PCOMMA".1f Mred/s)\n", num_reductions, num_reductions / ((double)run_time / 1000) / 1000000);
    PRINT("%"PCOMMA"15"PRIcounter" array alloc\n", num_arr_alloc);
    PRINT("%"PCOMMA"15"PRIcounter" array free\n", num_arr_free);
    PRINT("%15.2fs total expired time\n", (double)run_time / 1000);
    PRINT("%15.2fs total gc time\n", (double)gc_mark_time / 1000);
#if GCRED
    PRINT(" GC reductions A=%d, K=%d, I=%d, int=%d flip=%d\n", red_a, red_k, red_i, red_int, red_flip);
#endif
  }
#endif  /* WANT_STDIO */

  if (dump_ticks) {
    dump_tick_table(stdout);
  }

#ifdef TEARDOWN
  main_teardown(); // do some platform specific teardown
#endif
  EXIT(0);
}

#if WANT_MD5
#include "md5.c"
#endif  /* WANT_MD5 */
