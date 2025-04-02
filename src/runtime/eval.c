/* Copyright 2023 Lennart Augustsson
 * See LICENSE file for full license.
 */
#if !defined(WANT_GMP)
#define WANT_GMP 0
#endif

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
#if WANT_DIR
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif  /* WANT_DIR */
#if WANT_TIME
#include <time.h>
#endif
#if WANT_GMP
#include <gmp.h>
#endif

#if WANT_MD5
#include "md5.h"
#endif

#if !defined(WANT_LZ77)
#define WANT_LZ77 1
#endif

#if !defined(WANT_RLE)
#define WANT_RLE 1
#endif

#if !defined(WANT_BWT)
#define WANT_BWT 1
#endif

#if WANT_LZ77
size_t lz77d(uint8_t *src, size_t srclen, uint8_t **bufp);
size_t lz77c(uint8_t *src, size_t srclen, uint8_t **bufp);
#endif

#include "mhsffi.h"
struct ffi_entry ffi_table[];
int num_ffi;
#define FFI_IX(i) ((i) < num_ffi ? ffi_table[i] : xffi_table[i - num_ffi])

//#include "config.h"

#define VERSION "v7.2\n"

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

#if !defined(MALLOC)
#define MALLOC malloc
#endif

#if !defined(REALLOC)
#define REALLOC realloc
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

#if !defined(MAIN)
#define MAIN int main(int argc, char **argv)
#endif

#if !defined(PCOMMA)
#define PCOMMA "'"
#endif  /* !defined(PCOMMA) */

#if !defined(GETRAW)
int GETRAW(void) { return -1; }
#endif  /* !defined(getraw) */

#if !defined(GETTIMEMILLI)
value_t GETTIMEMILLI(void) { return 0; }
#endif  /* !define(GETTIMEMILLI) */

#if !defined(TMPNAME)
/* This is a really bad implementation, since it doesn't check for anything. */
char* TMPNAME(const char* pre, const char* post) {
  char *s = MALLOC(strlen(pre) + 3 + strlen(post) + 1);
  strcpy(s, pre);
  strcat(s, "TMP");
  strcat(s, post);
  return s;
}
#endif

#if !defined(FFS)
/* This is pretty bad, could use deBruijn multiplication instead. */
int
FFS(bits_t x)
{
  int i;
  if (!x)
    return 0;
  for(i = 1; !(x & 1); x >>= 1, i++)
    ;
  return i;
}
#endif  /* !defined(FFS) */

#if defined(__has_builtin)

#if __has_builtin(__builtin_popcountl)
#define BUILTIN_POPCOUNT
#endif

#endif

#if !defined(POPCOUNT)
uvalue_t POPCOUNT(uvalue_t x) {
#if defined(BUILTIN_POPCOUNT)
  return __builtin_popcountl(x);
#else
  uvalue_t count = 0;
  while (x) {
    x = x & (x - 1); // clear lowest 1 bit
    count += 1;
  }
  return count;
#endif
}
#endif

#if defined(__GNUC__)
#define BUILTIN_CLZ
#elif defined(__clang__)

#if __has_builtin(__builtin_clzl)
#define BUILTIN_CLZ
#endif

#endif


#if !defined(CLZ)
uvalue_t CLZ(uvalue_t x) {
#if defined(BUILTIN_CLZ)
  if (x == 0) return WORD_SIZE;
  return __builtin_clzl(x);
#else
  value_t count = WORD_SIZE;
  while (x) {
    x = x >> 1;
    count -= 1;
  }
  return count;
#endif
}
#endif

#if defined(__has_builtin)

#if __has_builtin(__builtin_ctzl)
#define BUILTIN_CTZ
#endif

#endif


#if !defined(CTZ)
uvalue_t CTZ(uvalue_t x) {
  if (x == 0) return WORD_SIZE;
#if defined(BUILTIN_CLZ)
  return __builtin_ctzl(x);
#else
  uvalue_t count = 0;
  while ((x & 1) == 0) {
    x = x >> 1;
    count += 1;
  }
  return count;
#endif
}
#endif

#if !defined(WANT_ARGS)
#define WANT_ARGS 1
#endif

#if !defined(INLINE)
#define INLINE inline
#endif  /* !define(INLINE) */

#if !defined(NORETURN)
#define NORETURN __attribute__ ((noreturn))
#endif /* !defined(NORETURN) */

#if !defined(COUNT)
#define COUNT(n) ++(n)
#endif

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

enum node_tag { T_FREE, T_IND, T_AP, T_INT, T_DBL, T_PTR, T_FUNPTR, T_FORPTR, T_BADDYN, T_ARR,
                T_S, T_K, T_I, T_B, T_C,
                T_A, T_Y, T_SS, T_BB, T_CC, T_P, T_R, T_O, T_U, T_Z,
                T_K2, T_K3, T_K4, T_CCB,
                T_ADD, T_SUB, T_MUL, T_QUOT, T_REM, T_SUBR, T_UQUOT, T_UREM, T_NEG,
                T_AND, T_OR, T_XOR, T_INV, T_SHL, T_SHR, T_ASHR,
                T_POPCOUNT, T_CLZ, T_CTZ,
                T_EQ, T_NE, T_LT, T_LE, T_GT, T_GE, T_ULT, T_ULE, T_UGT, T_UGE, T_ICMP, T_UCMP,
                T_FPADD, T_FP2P, T_FPNEW, T_FPFIN, // T_FPSTR,
                T_FP2BS, T_BS2FP,
                T_TOPTR, T_TOINT, T_TODBL, T_TOFUNPTR,
                T_BININT2, T_BININT1, T_UNINT1,
                T_BINDBL2, T_BINDBL1, T_UNDBL1,
                T_BINBS2, T_BINBS1,
#if WANT_FLOAT
                T_FADD, T_FSUB, T_FMUL, T_FDIV, T_FNEG, T_ITOF,
                T_FEQ, T_FNE, T_FLT, T_FLE, T_FGT, T_FGE, T_FSHOW, T_FREAD,
#endif
                T_ARR_ALLOC, T_ARR_COPY, T_ARR_SIZE, T_ARR_READ, T_ARR_WRITE, T_ARR_EQ,
                T_RAISE, T_SEQ, T_EQUAL, T_COMPARE, T_RNF,
                T_TICK,
                T_IO_BIND, T_IO_THEN, T_IO_RETURN,
                T_IO_CCBIND,
                T_IO_SERIALIZE, T_IO_DESERIALIZE,
                T_IO_STDIN, T_IO_STDOUT, T_IO_STDERR, T_IO_GETARGREF,
                T_IO_PERFORMIO, T_IO_PRINT, T_CATCH,
                T_IO_CCALL, T_IO_GC, T_DYNSYM,
                T_NEWCASTRINGLEN, T_PACKCSTRING, T_PACKCSTRINGLEN,
                T_BSAPPEND, T_BSEQ, T_BSNE, T_BSLT, T_BSLE, T_BSGT, T_BSGE, T_BSCMP,
                T_BSPACK, T_BSUNPACK, T_BSREPLICATE, T_BSLENGTH, T_BSSUBSTR, T_BSINDEX,
                T_BSFROMUTF8, T_BSTOUTF8, T_BSHEADUTF8, T_BSTAILUTF8,
                T_BSAPPENDDOT,
                T_LAST_TAG,
};
#if 0
static const char* tag_names[] = {
  "FREE", "IND", "AP", "INT", "DBL", "PTR", "FUNPTR", "FORPTR", "BADDYN", "ARR",
  "S", "K", "I", "B", "C",
  "A", "Y", "SS", "BB", "CC", "P", "R", "O", "U", "Z",
  "K2", "K3", "K4", "CCB",
  "ADD", "SUB", "MUL", "QUOT", "REM", "SUBR", "UQUOT", "UREM", "NEG",
  "AND", "OR", "XOR", "INV", "SHL", "SHR", "ASHR",
  "POPCOUNT", "CLZ", "CTZ",
  "EQ", "NE", "LT", "LE", "GT", "GE", "ULT", "ULE", "UGT", "UGE",
  "FPADD", "FP2P", "FPNEW", "FPFIN",
  "TOPTR", "TOINT", "TODBL", "TOFUNPTR",
  "BININT2", "BININT1", "UNINT1",
  "BINDBL2", "BINDBL1", "UNDBL1",
#if WANT_FLOAT
  "FADD", "FSUB", "FMUL", "FDIV", "FNEG", "ITOF",
  "FEQ", "FNE", "FLT", "FLE", "FGT", "FGE", "FSHOW", "FREAD",
#endif
  "ARR_ALLOC", "ARR_COPY", "ARR_SIZE", "ARR_READ", "ARR_WRITE", "ARR_EQ",
  "RAISE", "SEQ", "EQUAL", "COMPARE", "RNF",
  "TICK",
  "IO_BIND", "IO_THEN", "IO_RETURN",
  "C'BIND",
  "IO_SERIALIZE", "IO_DESERIALIZE",
  "IO_STDIN", "IO_STDOUT", "IO_STDERR", "IO_GETARGREF",
  "IO_PERFORMIO", "IO_PRINT", "CATCH",
  "IO_CCALL", "IO_GC", "DYNSYM",
  "NEWCASTRINGLEN", "PACKCSTRING", "PACKCSTRINGLEN",
  "BSFROMUTF8",
  "STR",
  "LAST_TAG",
};
#endif

struct ioarray;
struct bytestring;
struct forptr;

typedef struct node {
  union {
    struct node *uufun;
    tag_t        uutag;             /* LSB=1 indicates that this is a tag, LSB=0 that this is a T_AP node */
  } ufun;
  union {
    struct node    *uuarg;
    value_t         uuvalue;
    flt_t           uufloatvalue;
    const char     *uucstring;
    void           *uuptr;
    HsFunPtr        uufunptr;
    struct ioarray *uuarray;
    struct forptr  *uuforptr;      /* foreign pointers and byte arrays */
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
#define CSTR(p) (p)->uarg.uucstring
#define PTR(p) (p)->uarg.uuptr
#define FUNPTR(p) (p)->uarg.uufunptr
#define FORPTR(p) (p)->uarg.uuforptr
#define BSTR(p) (p)->uarg.uuforptr->payload
#define ARR(p) (p)->uarg.uuarray
#define INDIR(p) ARG(p)
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = MALLOC(n * sizeof(node)); } while(0)
#define LABEL(n) ((heapoffs_t)((n) - cells))
node *cells;                 /* All cells */

/*
 * byte arrays
 */
struct bytestring {
  size_t size;
  void *string;
};

/*
 * Arrays are allocated with MALLOC()/FREE().
 * During GC they are marked, and all elements in the array are
 * recursively marked.
 * At the end of the the mark phase there is a scan of all
 * arrays, and the unmarked ones are freed.
 */
struct ioarray {
  struct ioarray *next;         /* all ioarrays are linked together */
  int permanent;                /* this array should never be GC-ed */
  size_t marked;                /* marked during GC */
  size_t size;                  /* number of elements in the array */
  NODEPTR array[1];             /* actual size may be bigger */
};
struct ioarray *array_root = 0; /* root of all allocated arrays, linked by next */

enum fptype {
  FP_FORPTR = 0,                /* a regular foreign pointer to unknown memory */
  FP_BSTR,                      /* a bytestring */
  FP_MPZ,                       /* a GMP MPZ pointer */
};

/*
 * A Haskell ForeignPtr has a normal pointer, and a finalizer
 * function that is to be called when there are no more references
 * to the ForeignPtr.
 * A complication is that using plusForeignPtr creates a new
 * ForeignPtr that must share the same finalizer.
 * There is one struct forptr for each ForeignPtr.  It has pointer
 * to the actual data, and to a struct final which is shared between
 * all ForeignPtrs that have been created with plusForeignPtr.
 * During GC the used bit is set for any references to the forptr.
 * The scan phase will traverse the struct final chain and run
 * the finalizer, and free associated structs.
 */
struct final {
  struct final  *next;      /* the next finalizer */
  HsFunPtr       final;     /* function to call to release resource */
  void          *arg;       /* argument to final when called */
  size_t         size;      /* size of memory, if known, otherwise NOSIZE */
#define NOSIZE ~0           /* used as the size in payload for actual foreign pointers */
  struct forptr *back;      /* back pointer to the first forptr */
  short          marked;    /* mark bit for GC */
  enum fptype    fptype;    /* what kind of foreign pointer */
};

/*
 * Foreign pointers are also used to represent bytestrings.
 * The difference between a foreign pointer and a bytestring
 * is that we can serialize the latter.
 * The size field is non-zero only for bytestrings.
 */
struct forptr {
  struct forptr *next;       /* the next ForeignPtr that shares the same finalizer */
  struct final  *finalizer;  /* the finalizer for this ForeignPtr */
  struct bytestring payload; /* the actual pointer to allocated data, and maybe a size */
  //  char          *desc;
};
struct final *final_root = 0;   /* root of all allocated foreign pointers, linked by next */

counter_t num_reductions = 0;
counter_t num_alloc = 0;
counter_t num_gc = 0;
uintptr_t gc_mark_time = 0;
uintptr_t gc_scan_time = 0;
uintptr_t run_time = 0;

#define MAXSTACKDEPTH 0
#if MAXSTACKDEPTH
stackptr_t max_stack_depth = 0;
counter_t max_c_stack = 0;
counter_t cur_c_stack = 0;
#define MAXSTACK if (stack_ptr > max_stack_depth) max_stack_depth = stack_ptr
#else
#define MAXSTACK
#endif

NODEPTR *topnode;
NODEPTR atptr;

NODEPTR *stack;
stackptr_t stack_ptr = -1;
#if STACKOVL
#define PUSH(x) do { if (stack_ptr >= stack_size-1) stackerr(); stack[++stack_ptr] = (x); MAXSTACK; } while(0)
#else  /* STACKOVL */
#define PUSH(x) do {                                            stack[++stack_ptr] = (x); MAXSTACK; } while(0)
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
counter_t num_fin_alloc;
counter_t num_fin_free;
counter_t num_bs_alloc;
counter_t num_bs_alloc_max;
counter_t num_bs_free;
counter_t num_bs_bytes;
counter_t num_bs_inuse;
counter_t num_bs_inuse_max;

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

NORETURN
void
stackerr(void)
{
  ERR("stack overflow");
  EXIT(1);
}

/***************************************/

#include "bfile.c"

/***************************************/

struct ioarray*
arr_alloc(size_t sz, NODEPTR e)
{
  struct ioarray *arr = MALLOC(sizeof(struct ioarray) + (sz-1) * sizeof(NODEPTR));
  size_t i;

  if (!arr)
    memerr();
  arr->next = array_root;
  array_root = arr;
  arr->marked = 0;
  arr->permanent = 0;
  arr->size = sz;
  for(i = 0; i < sz; i++)
    arr->array[i] = e;
  //PRINT("arr_alloc(%d, %p) = %p\n", (int)sz, e, arr);
  num_arr_alloc++;
  return arr;
}

struct ioarray*
arr_copy(struct ioarray *oarr)
{
  size_t sz = oarr->size;
  struct ioarray *arr = MALLOC(sizeof(struct ioarray) + (sz-1) * sizeof(NODEPTR));

  if (!arr)
    memerr();
  arr->next = array_root;
  array_root = arr;
  arr->marked = 0;
  arr->permanent = 0;
  arr->size = sz;
  memcpy(arr->array, oarr->array, sz * sizeof(NODEPTR));
  num_arr_alloc++;
  return arr;
}

/*****************************************************************************/

#if WANT_TICK
struct tick_entry {
  struct bytestring tick_name;
  counter_t tick_count;
} *tick_table = 0;
size_t tick_table_size;
size_t tick_index;

/* Allocate a new tick table entry and return the index. */
size_t
add_tick_table(struct bytestring name)
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
    tick_table = REALLOC(tick_table, tick_table_size * sizeof(struct tick_entry));
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
      fprintf(f, "%-60s %10"PRIcounter"\n", (char *)tick_table[i].tick_name.string, n);
  }
}
#endif

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

/* Set FREE bit to 1, used to undo marking in GC */
static INLINE void mark_unused(NODEPTR n)
{
  heapoffs_t i = LABEL(n);
#if SANITY
  if (i < heap_start)
    ERR("Unmarking invalid heap address.");
  if (i >= free_map_nwords * BITS_PER_WORD) ERR("mark_used");
#endif
  free_map[i / BITS_PER_WORD] |= 1ULL << (i % BITS_PER_WORD);
}

/* Test if FREE bit is 0 */
static INLINE int is_marked_used(NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  if (i < heap_start)
    return 1;
#if SANITY
  if (i >= free_map_nwords * BITS_PER_WORD)
    ERR("is_marked_used");
#endif
  return (free_map[i / BITS_PER_WORD] & (1ULL << (i % BITS_PER_WORD))) == 0;
}

static INLINE void mark_all_free(void)
{
  memset(free_map, ~0, free_map_nwords * sizeof(bits_t));
  next_scan_index = heap_start;
}

#if WANT_ARGS
/* This single element array hold a list of the program arguments. */
struct ioarray *argarray;
#endif  /* WANT_ARGS */

int verbose = 0;
int gcbell = 0;

static INLINE NODEPTR
alloc_node(enum node_tag t)
{
  heapoffs_t i = next_scan_index / BITS_PER_WORD;
  int k;                        /* will contain bit pos + 1 */
  heapoffs_t pos;
  NODEPTR n;
  heapoffs_t word;

  /* This can happen if we run out of memory when parsing. */
  if (num_free <= 0)
    ERR("alloc_node");

  for(;;) {
    word = free_map[i];
    if (word)
      break;
    i++;
#if SANITY
    if (i >= free_map_nwords) {
#if 0
      fprintf(stderr, "wordsize=%u, num_free=%u next_scan_index=%u i=%u free_map_nwords=%u\n", (uint)BITS_PER_WORD,
              (uint)num_free, (uint)next_scan_index, (uint)i, (uint)free_map_nwords);
#endif
      ERR("alloc_node: free_map");
    }
#endif
  }
  k = FFS(word);
  pos = i * BITS_PER_WORD + k - 1; /* first free node */
  n = HEAPREF(pos);
  // mark_used(n); // equivalent to:
  free_map[i] = word & (word-1);
  next_scan_index = pos;

  SETTAG(n, t);
  COUNT(num_alloc);
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
NODEPTR combK, combTrue, combUnit, combCons, combPair;
NODEPTR combCC, combZ, combIOBIND, combIORETURN, combIOCCBIND, combB, combC;
NODEPTR combLT, combEQ, combGT;
NODEPTR combShowExn, combU, combK2, combK3;
NODEPTR combBININT1, combBININT2, combUNINT1;
NODEPTR combBINDBL1, combBINDBL2, combUNDBL1;
NODEPTR combBINBS1, combBINBS2;
NODEPTR comb_stdin, comb_stdout, comb_stderr;
#define combFalse combK

/* One node of each kind for primitives, these are never GCd. */
/* We use linear search in this, because almost all lookups
 * are among the combinators.
 */
struct {
  const char *name;
  const enum node_tag tag;
  const enum node_tag flipped;        /* What should (C op) reduce to? defaults to T_FREE */
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
  { "popcount", T_POPCOUNT },
  { "clz", T_CLZ },
  { "ctz", T_CTZ },
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

  { "bs++", T_BSAPPEND },
  { "bs++.", T_BSAPPENDDOT },
  { "bs==", T_BSEQ, T_BSEQ },
  { "bs/=", T_BSNE, T_BSNE },
  { "bs<", T_BSLT },
  { "bs<=", T_BSLE },
  { "bs>", T_BSGT },
  { "bs>=", T_BSGE },
  { "bscmp", T_BSCMP },
  { "bspack", T_BSPACK },
  { "bsunpack", T_BSUNPACK },
  { "bsreplicate", T_BSREPLICATE },
  { "bslength", T_BSLENGTH },
  { "bssubstr", T_BSSUBSTR },
  { "bsindex", T_BSINDEX },

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
  { "fp+", T_FPADD },
  { "fp2p", T_FP2P },
  { "fpnew", T_FPNEW },
  { "fpfin", T_FPFIN },
  //  { "fpstr", T_FPSTR },
  { "fp2bs", T_FP2BS },
  { "bs2fp", T_BS2FP },
  { "seq", T_SEQ },
  { "equal", T_EQUAL, T_EQUAL },
  { "sequal", T_EQUAL, T_EQUAL },
  { "compare", T_COMPARE },
  { "scmp", T_COMPARE },
  { "icmp", T_ICMP },
  { "ucmp", T_UCMP },
  { "rnf", T_RNF },
  { "fromUTF8", T_BSFROMUTF8 },
  { "toUTF8", T_BSTOUTF8 },
  { "headUTF8", T_BSHEADUTF8 },
  { "tailUTF8", T_BSTAILUTF8 },
  /* IO primops */
  { "IO.>>=", T_IO_BIND },
  { "IO.>>", T_IO_THEN },
  { "IO.return", T_IO_RETURN },
  { "IO.C'BIND", T_IO_CCBIND },
  { "IO.serialize", T_IO_SERIALIZE },
  { "IO.print", T_IO_PRINT },
  { "IO.deserialize", T_IO_DESERIALIZE },
  { "IO.stdin", T_IO_STDIN },
  { "IO.stdout", T_IO_STDOUT },
  { "IO.stderr", T_IO_STDERR },
  { "IO.getArgRef", T_IO_GETARGREF },
  { "IO.performIO", T_IO_PERFORMIO },
  { "IO.gc", T_IO_GC },
  { "raise", T_RAISE },
  { "catch", T_CATCH },
  { "A.alloc", T_ARR_ALLOC },
  { "A.copy", T_ARR_COPY },
  { "A.size", T_ARR_SIZE },
  { "A.read", T_ARR_READ },
  { "A.write", T_ARR_WRITE },
  { "A.==", T_ARR_EQ },
  { "dynsym", T_DYNSYM },
  { "newCAStringLen", T_NEWCASTRINGLEN },
  { "packCString", T_PACKCSTRING },
  { "packCStringLen", T_PACKCSTRINGLEN },
  { "toPtr", T_TOPTR },
  { "toInt", T_TOINT },
  { "toDbl", T_TODBL },
  { "toFunPtr", T_TOFUNPTR },
};

#if GCRED
enum node_tag flip_ops[T_LAST_TAG];
#endif

#if WANT_STDIO
/* Create a dummy foreign pointer for the standard stdio handles. */
/* These handles are never gc():d. */
void
mk_std(NODEPTR n, FILE *f)
{
  struct final *fin = calloc(1, sizeof(struct final));
  struct forptr *fp = calloc(1, sizeof(struct forptr));
  if (!fin || !fp)
    memerr();
  BFILE *bf = add_utf8(add_FILE(f));
  SETTAG(n, T_FORPTR);
  FORPTR(n) = fp;
  fin->arg = bf;
  fin->back = fp;
  fp->payload.string = bf;
  fp->finalizer = fin;
}
#endif

void
init_nodes(void)
{
  enum node_tag t;
  size_t j;
  NODEPTR n;

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
    case T_K: combK = n; break;
    case T_A: combTrue = n; break;
    case T_I: combUnit = n; break;
    case T_O: combCons = n; break;
    case T_P: combPair = n; break;
    case T_CC: combCC = n; break;
    case T_B: combB = n; break;
    case T_C: combC = n; break;
    case T_Z: combZ = n; break;
    case T_U: combU = n; break;
    case T_K2: combK2 = n; break;
    case T_K3: combK3 = n; break;
    case T_IO_BIND: combIOBIND = n; break;
    case T_IO_RETURN: combIORETURN = n; break;
    case T_IO_CCBIND: combIOCCBIND = n; break;
    case T_BININT1: combBININT1 = n; break;
    case T_BININT2: combBININT2 = n; break;
    case T_UNINT1: combUNINT1 = n; break;
    case T_BINDBL1: combBINDBL1 = n; break;
    case T_BINDBL2: combBINDBL2 = n; break;
    case T_UNDBL1: combUNDBL1 = n; break;
    case T_BINBS1: combBINBS1 = n; break;
    case T_BINBS2: combBINBS2 = n; break;
#if WANT_STDIO
    case T_IO_STDIN:  comb_stdin  = n; mk_std(n, stdin);  break;
    case T_IO_STDOUT: comb_stdout = n; mk_std(n, stdout); break;
    case T_IO_STDERR: comb_stderr = n; mk_std(n, stderr); break;
#endif  /* WANT_STDIO */
    default:
      break;
    }
  }
#else
  for(t = T_FREE; t < T_LAST_TAG; t++) {
    NODEPTR n = HEAPREF(heap_start++);
    SETTAG(n, t);
    switch (t) {
    case T_K: combK = n; break;
    case T_A: combTrue = n; break;
    case T_I: combUnit = n; break;
    case T_O: combCons = n; break;
    case T_P: combPair = n; break;
    case T_CC: combCC = n; break;
    case T_B: combB = n; break;
    case T_C: combC = n; break;
    case T_Z: combZ = n; break;
    case T_U: combU = n; break;
    case T_K2: combK2 = n; break;
    case T_K3: combK3 = n; break;
    case T_IO_BIND: combIOBIND = n; break;
    case T_IO_RETURN: combIORETURN = n; break;
    case T_IO_CCBIND: combIOCCBIND = n; break;
    case T_BININT1: combBININT1 = n; break;
    case T_BININT2: combBININT2 = n; break;
    case T_UNINT1: combUNINT1 = n; break;
    case T_BINDBL1: combBINDBL1 = n; break;
    case T_BINDBL2: combBINDBL2 = n; break;
    case T_UNDBL1: combUNDBL1 = n; break;
    case T_BINBS1: combBINBS1 = n; break;
    case T_BINBS2: combBINBS2 = n; break;
#if WANT_STDIO
    case T_IO_STDIN:  comb_stdin  = n; mk_std(n, stdin);  break;
    case T_IO_STDOUT: comb_stdout = n; mk_std(n, stdout); break;
    case T_IO_STDERR: comb_stderr = n; mk_std(n, stderr); break;
#endif
    default:
      break;
    }
    for (j = 0; j < sizeof primops / sizeof primops[0];j++) {
      if (primops[j].tag == t) {
        primops[j].node = n;
      }
    }
  }
#endif
#if GCRED
  for (j = 0; j < sizeof primops / sizeof primops[0]; j++) {
    flip_ops[primops[j].tag] = primops[j].flipped;
  }
#endif

  /* The representation of the constructors of
   *  data Ordering = LT | EQ | GT
   * do not have single constructors.
   * But we can make compound one, since they are irreducible.
   */
#define NEWAP(c, f, a) do { n = HEAPREF(heap_start++); SETTAG(n, T_AP); FUN(n) = (f); ARG(n) = (a); (c) = n;} while(0)
  NEWAP(combLT, combZ,     combFalse);  /* Z B */
  NEWAP(combEQ, combFalse, combFalse);  /* K K */
  NEWAP(combGT, combFalse, combTrue);   /* K A */
  {
    /* The displaySomeException compiles to (U (U (K2 A))) */
    NODEPTR x;
    NEWAP(x, combK2, combTrue);        /* (K2 A) */
    NEWAP(x, combU, x);                /* (U (K2 A)) */
    NEWAP(combShowExn, combU, x);      /* (U (U (K2 A))) */
  }
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
int red_bb, red_k4, red_k3, red_k2, red_ccb, red_z, red_r;

//counter_t mark_depth;
//counter_t max_mark_depth = 0;

/* Mark all used nodes reachable from *np, updating *np. */
void
mark(NODEPTR *np)
{
  stackptr_t stk = stack_ptr;
  NODEPTR n;
  NODEPTR *to_push = 0;         /* silence warning by initializing */
#if GCRED
  value_t val;
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
  if (n < cells || n > cells + heap_size)
    ERR("bad n");
  if (is_marked_used(n)) {
    goto fin;
  }
  num_marked++;
  mark_used(n);
  switch (tag) {
#if GCRED
   case T_INT:
#if INTTABLE
    if (LOW_INT <= (val = GETVALUE(n)) && val < HIGH_INT) {
      SETTAG(n, T_IND);
      INDIR(n) = intTable[val - LOW_INT];
      red_int++;
      goto top;
    }
    goto fin;
#endif  /* INTTABLE */
   case T_AP:
      if (want_gc_red) {
        /* This is really only fruitful just after parsing.  It can be removed. */
        if (GETTAG(FUN(n)) == T_AP && GETTAG(FUN(FUN(n))) == T_A) {
          /* Do the A x y --> y reduction */
          NODEPTR y = ARG(n);
          SETTAG(n, T_IND);
          INDIR(n) = y;
          red_a++;
          goto top;
        }
#if 0
        /* This never seems to happen */
        if (GETTAG(FUN(n)) == T_AP && GETTAG(FUN(FUN(n))) == T_K) {
          /* Do the K x y --> x reduction */
          NODEPTR x = ARG(FUN(n));
          SETTAG(n, T_IND);
          INDIR(n) = x;
          red_k++;
          goto top;
        }
#endif  /* 0 */
        if (GETTAG(FUN(n)) == T_I) {
          /* Do the I x --> x reduction */
          NODEPTR x = ARG(n);
          SETTAG(n, T_IND);
          INDIR(n) = x;
          red_i++;
          goto top;
        }
#if 0
        /* This is broken.
         * Probably because it can happen in the middle of the C reduction code.
         */
        DO NOT ENABLE
        if (GETTAG(FUN(n)) == T_C) {
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
            goto fin;
          }
        }
#endif
      }
#else   /* GCRED */
   case T_AP:
#endif  /* GCRED */
    /* Avoid tail recursion */
    np = &FUN(n);
    to_push = &ARG(n);
    break;
   case T_ARR:
    {
      struct ioarray *arr = ARR(n);

      // arr->marked records marking progress through arr.
      if (arr->marked >= arr->size) {
        goto fin;
      }
      // We unmark the array as a whole and push it as long
      // as there's more entries to scan.
      mark_unused(n);
      num_marked--;
      to_push = np;
      np = &arr->array[arr->marked++];
      break;
    }
   case T_FORPTR:
     FORPTR(n)->finalizer->marked = 1;
     goto fin;

   default:
     goto fin;
  }

  if (!is_marked_used(*to_push)) {
    //  mark_depth++;
    PUSH((NODEPTR)to_push);
  }
  goto top;
 fin:
  //  if (mark_depth > max_mark_depth) {
  //    max_mark_depth = mark_depth;
  //  }
  //  mark_depth--;
  if (stack_ptr > stk) {
    np = (NODEPTR *)POPTOP();
    goto top;
  }
  return;
}

/* Perform a garbage collection:
   - First mark from all roots; roots are on the stack.
*/
void
gc(void)
{
  stackptr_t i;

  num_gc++;
  num_marked = 0;
#if WANT_STDIO
  if (verbose > 1)
    PRINT("gc mark\n");
#endif
  gc_mark_time -= GETTIMEMILLI();
  mark_all_free();
  // mark everything reachable from the stack
  for (i = 0; i <= stack_ptr; i++)
    mark(&stack[i]);
  // mark everything reachable from permanent array nodes
  for (struct ioarray *arr = array_root; arr; arr = arr->next) {
    if (arr->permanent) {
      for (i = 0; i < arr->size; i++)
        mark(&arr->array[i]);
    }
  }
  gc_mark_time += GETTIMEMILLI();

  if (num_marked > max_num_marked)
    max_num_marked = num_marked;
  num_free = heap_size - heap_start - num_marked;
  if (num_free < heap_size / 50)
    ERR("heap exhausted");

  gc_scan_time -= GETTIMEMILLI();
  /* Free unused arrays */
  for (struct ioarray **arrp = &array_root; *arrp; ) {
    struct ioarray *arr = *arrp;
    if (arr->marked || arr->permanent) {
      arr->marked = 0;
      arrp = &arr->next;
    } else {
      *arrp = arr->next;        /* unlink */
      num_arr_free++;
      FREE(arr);                /* and FREE */
    }
  }

  /* Run finalizers on unused foreign pointers. */
  for (struct final **finp = &final_root; *finp; ) {
    struct final *fin = *finp;
    if (fin->marked) {
      fin->marked = 0;
      finp = &fin->next;
    } else {
      /* Unused, run finalizer and free all associated memory */
      if (fin->size == NOSIZE) {
        num_fin_free++;
      } else {
        num_bs_free++;
        num_bs_inuse -= fin->size;
        if (num_bs_alloc - num_bs_free > num_bs_alloc_max)
          num_bs_alloc_max = num_bs_alloc - num_bs_free;
      }
      void (*f)(void *) = (void (*)(void *))fin->final;
      //printf("forptr free fin=%p, f=%p", fin, f);
      //fflush(stdout);
      if (f) {
        //printf("finalizer fin=%p final=%p\n", fin, f);
        (*f)(fin->arg);
      }
      for (struct forptr *p = fin->back; p; ) {
        struct forptr *q = p->next;
        //printf("free fp=%p\n", p);
        //printf(" p=%p desc=%s", p, p->desc ? p->desc : "NONE");
        //fflush(stdout);
        FREE(p);
        //memset(p, 0x55, sizeof *p);
        p = q;
      }
      //printf("\n");
      *finp = fin->next;
      //printf("free fin=%p\n", fin);
      FREE(fin);
      //memset(fin, 0x77, sizeof *fin);
    }
  }
  gc_scan_time += GETTIMEMILLI();

#if WANT_STDIO
  if (verbose > 1) {
    PRINT("gc done, %"PRIcounter" free\n", num_free);
    //PRINT(" GC reductions A=%d, K=%d, I=%d, int=%d flip=%d\n", red_a, red_k, red_i, red_int, red_flip);
  }
  if (gcbell) {
    fputc('\007', stderr);      /* ring the bell */
    fflush(stderr);
  }
#endif  /* !WANT_STDIO */

#if 0
  /* For debugging only: mark all free cells */
  for(int n = 0; n < heap_size; n++) {
    NODEPTR p = HEAPREF(n);
    if (!is_marked_used(p)) {
      SETTAG(p, T_FREE);
    }
  }
#endif
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

static INLINE
value_t
peekWord(value_t *p)
{
  return *p;
}

static INLINE
void
pokeWord(value_t *p, value_t w)
{
  *p = w;
}

static INLINE
void *
peekPtr(void **p)
{
  return *p;
}

static INLINE
void
pokePtr(void **p, void *w)
{
  *p = w;
}

static INLINE
uvalue_t
peek_uint8(uint8_t *p)
{
  return *p;
}

static INLINE
void
poke_uint8(uint8_t *p, value_t w)
{
  *p = (uint8_t)w;
}

static INLINE
uvalue_t
peek_uint16(uint16_t *p)
{
  return *p;
}

static INLINE
void
poke_uint16(uint16_t *p, value_t w)
{
  *p = (uint16_t)w;
}

#if WORD_SIZE >= 32
static INLINE
uvalue_t
peek_uint32(uint32_t *p)
{
  return *p;
}

static INLINE
void
poke_uint32(uint32_t *p, value_t w)
{
  *p = (uint32_t)w;
}
#endif  /* WORD_SIZE >= 32 */

#if WORD_SIZE >= 64
static INLINE
uvalue_t
peek_uint64(uint64_t *p)
{
  return *p;
}

static INLINE
void
poke_uint64(uint64_t *p, value_t w)
{
  *p = (uint64_t)w;
}
#endif  /* WORD_SIZE >= 64 */

static INLINE
value_t
peek_int8(int8_t *p)
{
  return *p;
}

static INLINE
void
poke_int8(int8_t *p, value_t w)
{
  *p = (int8_t)w;
}

static INLINE
value_t
peek_int16(int16_t *p)
{
  return *p;
}

static INLINE
void
poke_int16(int16_t *p, value_t w)
{
  *p = (int16_t)w;
}

#if WORD_SIZE >= 32
static INLINE
value_t
peek_int32(int32_t *p)
{
  return *p;
}

static INLINE
void
poke_int32(int32_t *p, value_t w)
{
  *p = (int32_t)w;
}
#endif  /* WORD_SIZE >= 32 */

#if WORD_SIZE >= 64
static INLINE
value_t
peek_int64(int64_t *p)
{
  return *p;
}

static INLINE
void
poke_int64(int64_t *p, value_t w)
{
  *p = (int64_t)w;
}
#endif  /* WORD_SIZE >= 64 */

static INLINE
value_t
peek_int(int *p)
{
  return *p;
}

static INLINE
void
poke_int(int *p, value_t w)
{
  *p = (int)w;
}

static INLINE
value_t
peek_uint(unsigned int *p)
{
  return *p;
}

static INLINE
void
poke_uint(unsigned int *p, value_t w)
{
  *p = (unsigned int)w;
}

static INLINE
value_t
peek_short(short *p)
{
  return *p;
}

static INLINE
void
poke_short(short *p, value_t w)
{
  *p = (short)w;
}

static INLINE
value_t
peek_ushort(unsigned short *p)
{
  return *p;
}

static INLINE
void
poke_ushort(unsigned short *p, value_t w)
{
  *p = (unsigned short)w;
}

static INLINE
value_t
peek_long(long *p)
{
  return *p;
}

static INLINE
void
poke_long(long *p, value_t w)
{
  *p = (long)w;
}

static INLINE
value_t
peek_ulong(unsigned long *p)
{
  return *p;
}

static INLINE
void
poke_ulong(unsigned long *p, value_t w)
{
  *p = (unsigned long)w;
}

static INLINE
value_t
peek_llong(long long *p)
{
  return *p;
}

static INLINE
void
poke_llong(long long *p, value_t w)
{
  *p = (long long)w;
}

static INLINE
value_t
peek_ullong(unsigned long long *p)
{
  return *p;
}

static INLINE
void
poke_ullong(unsigned long long *p, value_t w)
{
  *p = (unsigned long long)w;
}

#if WANT_FLOAT
static INLINE
flt_t
peek_flt(flt_t *p)
{
  return *p;
}

static INLINE
void
poke_flt(flt_t *p, flt_t w)
{
  *p = w;
}
#endif  /* WANT_FLOAT */

/* Look up an FFI function by name */
value_t
lookupFFIname(const char *name)
{
  size_t i;

  for(i = 0; ffi_table[i].ffi_name; i++)
    if (strcmp(ffi_table[i].ffi_name, name) == 0)
      return (value_t)i;
  if (xffi_table) {
    for(i = 0; xffi_table[i].ffi_name; i++)
      if (strcmp(xffi_table[i].ffi_name, name) == 0)
        return (value_t)(i + num_ffi);
  }
  return -1;
}

NODEPTR
ffiNode(const char *buf)
{
  NODEPTR r;
  value_t i = lookupFFIname(buf);
  char *fun;

  if (i < 0) {
    /* lookup failed, generate a node that will dynamically generate an error */
    r = alloc_node(T_BADDYN);
    fun = MALLOC(strlen(buf) + 1);
    strcpy(fun, buf);
    CSTR(r) = fun;
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
  int d = getb(f);
  if (c == d) {
    return 1;
  } else {
    ungetb(d, f);
    return 0;
  }
}

/* Get a non-terminating character.  ' ' and '\n' terminates a token. */
int
getNT(BFILE *f)
{
  int c;

  c = getb(f);
  if (c == ' ' || c == '\n') {
    return 0;
  } else {
    return c;
  }
}

value_t
parse_int(BFILE *f)
{
  // Parse using uvalue_t, which wraps on overflow.
  uvalue_t i = 0;
  int neg = 1;
  int c = getb(f);
  if (c == '-') {
    neg = -1;
    c = getb(f);
  }
  for(;;) {
    i = i * 10 + (c - '0');
    c = getb(f);
    if (c < '0' || c > '9') {
      ungetb(c, f);
      break;
    }
  }
  // Multiply by neg without triggering undefined behavior.
  return (value_t)(((uvalue_t)neg) * i);
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

  return strtod(buf, NULL);
}
#endif

struct forptr *mkForPtr(struct bytestring bs);
NODEPTR mkFunPtr(HsFunPtr p);

/* Create a forptr that has a free() finalizer. */
struct forptr *
mkForPtrFree(struct bytestring str)
{
  struct forptr *fp = mkForPtr(str);         /* Create a foreign pointer */
  fp->finalizer->final = (HsFunPtr)FREE;     /* and set the finalizer to just free it */
  return fp;
}

NODEPTR
mkStrNode(struct bytestring str)
{
  NODEPTR n = alloc_node(T_FORPTR);
  struct forptr *fp = mkForPtrFree(str);
  FORPTR(n) = fp;
  fp->finalizer->fptype = FP_BSTR;
  //printf("mkForPtr n=%p fp=%p %d %s payload.string=%p\n", n, fp, (int)FORPTR(n)->payload.size, (char*)FORPTR(n)->payload.string, FORPTR(n)->payload.string);
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
  int i;

  for(i = (int)label; ; i++) {
    i %= shared_table_size;
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

/* The memory allocated here is never freed.
 * This could be fixed by using a forptr and a
 * finalizer for read UTF-8 strings.
 * Fix this if there is a lot of deserialization.
 */
struct bytestring
parse_string(BFILE *f)
{
  struct bytestring bs;
  size_t sz = 20;
  uint8_t *buffer = MALLOC(sz);
  size_t i;
  int c;

  if (!buffer)
    memerr();
  for(i = 0;;) {
    c = getb(f);
    if (c == '"')
      break;
    if (i >= sz - 1) {
      sz *= 2;
      buffer = REALLOC(buffer, sz);
      if (!buffer)
        memerr();
    }
#if 0
    if (c == '\\') {
      buffer[i++] = (uint8_t)parse_int(f);
      if (!gobble(f, '&'))
        ERR("parse string");
    } else {
      buffer[i++] = c;
    }
#else
    /* See src/MicroHs/ExpPrint.hs for how strings are encoded. */
    switch (c) {
    case '\\':
      c = getb(f);
      if (c == '?')
        c = 0x7f;
      else if (c == '_')
        c = 0xff;
      break;
    case '^':
      c = getb(f);
      if (c < 0x40)
        c &= 0x1f;
      else
        c = (c & 0x1f) | 0x80;
      break;
    case '|':
      c = getb(f);
      c |= 0x80;
      break;
    default:
      /* Unencoded */
      ;
    }
    buffer[i++] = c;
#endif
  }
  buffer[i] = 0;                /* add a trailing 0 in case we need a C string */
  buffer = REALLOC(buffer, i + 1);

  bs.size = i;
  bs.string = buffer;
  //printf("parse_string %d %s\n", (int)bs.size, (char*)bs.string);
  return bs;
}

struct forptr *new_mpz(void);

NODEPTR
parse(BFILE *f)
{
  stackptr_t stk = stack_ptr;
  NODEPTR r, x, y;
  NODEPTR *nodep;
  heapoffs_t l;
  value_t i;
  int c;
  size_t j;
  char buf[80];                 /* store names of primitives. */

  for(;;) {
    c = getb(f);
    if (c < 0) ERR("parse EOF");
    switch (c) {
    case ' ':
    case '\n':
      continue;
    }
    if (num_free < 3)
      ERR("out of heap reading code");
    GCCHECK(1);
    switch(c) {
    case '@':
      x = TOP(0);
      y = TOP(1);
      POP(2);
      PUSH(new_ap(y, x));
      break;
    case '}':
      x = TOP(0);
      POP(1);
      if (stack_ptr != stk)
        ERR("parse: stack");
      return x;
#if WANT_GMP
    case '%':
      {
        struct bytestring bs = parse_string(f); /* get all the digits, terminated by " */
        struct forptr *fp = new_mpz();          /* a new mpz */
        mpz_ptr op = fp->payload.string;        /* get actual pointer */
        mpz_set_str(op, bs.string, 10);         /* convert to an mpz */
        free(bs.string);
        r = alloc_node(T_FORPTR);
        FORPTR(r) = fp;
        PUSH(r);
        break;
      }
#endif
    case '&':
#if WANT_FLOAT
      r = mkFlt(parse_double(f));
#else
      while (getNT(f))          /* skip the float constant */
        ;
      r = alloc_node(T_DBL);
      SETVALUE(r, 0);
#endif
      PUSH(r);
      break;
    case '#':
      i = parse_int(f);
      r = mkInt(i);
      PUSH(r);
      break;
    case '[':
      {
        size_t sz;
        struct ioarray *arr;
        size_t i;
        sz = (size_t)parse_int(f);
        if (!gobble(f, ']')) ERR("parse arr 1");
        arr = arr_alloc(sz, NIL);
        for (i = 0; i < sz; i++) {
          arr->array[i] = TOP(sz - i - 1);
        }
        r = alloc_node(T_ARR);
        ARR(r) = arr;
        POP(sz);
        PUSH(r);
        break;
      }
    case '_':
      /* Reference to a shared value: _label */
      l = parse_int(f);  /* The label */
      nodep = find_label(l);
      if (*nodep == NIL) {
        /* Not yet defined, so make it an indirection */
        *nodep = alloc_node(T_IND);
        INDIR(*nodep) = NIL;
      }
      PUSH(*nodep);
      break;
    case ':':
      /* Define a shared expression: :label e */
      l = parse_int(f);  /* The label */
      if (!gobble(f, ' ')) ERR("parse ' '");
      nodep = find_label(l);
      x = TOP(0);
      if (*nodep == NIL) {
        /* not referenced yet, so add a direct reference */
        *nodep = x;
      } else {
        /* Sanity check */
        if (INDIR(*nodep) != NIL) ERR("shared != NIL");
        INDIR(*nodep) = x;
      }
      break;
    case '"':
      /* Everything up to the next " is a string.
       * Special characters are encoded as \NNN&,
       * where NNN is the decimal value of the character */
      PUSH(mkStrNode(parse_string(f)));
      break;
#if WANT_TICK
    case '!':
      if (!gobble(f, '"'))
        ERR("parse !");
      i = add_tick_table(parse_string(f));
      r = alloc_node(T_TICK);
      SETVALUE(r, (value_t)i);
      PUSH(r);
      break;
#endif
    case '^':
      /* An FFI name */
      for (j = 0; (buf[j] = getNT(f)); j++)
        ;
      r = ffiNode(buf);
      PUSH(r);
      break;
    case ';':
      /* <name is a C function pointer to name */
      for (j = 0; (buf[j] = getNT(f)); j++)
        ;
      if (strcmp(buf, "0") == 0) {
        PUSH(mkFunPtr((HsFunPtr)0));
      } else if (strcmp(buf, "closeb") == 0) {
        PUSH(mkFunPtr((HsFunPtr)closeb));
      } else {
        ERR1("unknown funptr '%s'", buf);
      }
      break;
    default:
      buf[0] = c;
      /* A primitive, keep getting char's until end */
      for (j = 1; (buf[j] = getNT(f)); j++)
        ;
      /* Look up the primop and use the preallocated node. */
      for (j = 0; j < sizeof primops / sizeof primops[0]; j++) {
        if (strcmp(primops[j].name, buf) == 0) {
          r = primops[j].node;
          goto found;
        }
      }
      ERR1("no primop %s", buf);
    found:
      PUSH(r);
      break;
    }
  }
}

void
checkversion(BFILE *f)
{
  char *p = VERSION;
  int c;

  while ((c = *p++)) {
    if (c != getb(f))
      ERR("version mismatch");
  }
  (void)gobble(f, '\r');                 /* allow extra CR */
}

/* Parse a file */
NODEPTR
parse_top(BFILE *f)
{
  heapoffs_t numLabels, i;
  NODEPTR n;
  checkversion(f);
  numLabels = parse_int(f);
  if (!gobble(f, '\n'))
    ERR("size parse");
  gobble(f, '\r');                 /* allow extra CR */
  shared_table_size = 3 * numLabels; /* sparsely populated hashtable */
  shared_table = MALLOC(shared_table_size * sizeof(struct shared_entry));
  if (!shared_table)
    memerr();
  for(i = 0; i < shared_table_size; i++)
    shared_table[i].node = NIL;
  n = parse(f);
  FREE(shared_table);
  return n;
}

#if WANT_STDIO
NODEPTR
parse_file(const char *fn, size_t *psize)
{
  FILE *f = fopen(fn, "r");
  if (!f)
    ERR1("file not found %s", fn);

  /* And parse it */
  BFILE *p = add_FILE(f);
  NODEPTR n = parse_top(p);
  *psize = ftell(f);
  closeb(p);
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
struct print_bits {
  bits_t *marked_bits;
  bits_t *shared_bits;
};
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

size_t strNodes(size_t len);
NODEPTR mkStringC(char *str);

#if WANT_STDIO
#if WORD_SIZE == 64
#define CONVDBL "%.16g"
#elif WORD_SIZE == 32
#define CONVDBL "%.8g"
#endif
void
convdbl(char *str, flt_t x)
{
  /* Using 16 decimals will lose some precision.
   * 17 would keep the precision, but it frequently looks very ugly.
   */
  (void)snprintf(str, 25, CONVDBL, x);
  if (strcmp(str, "nan") != 0 && strcmp(str, "-nan") != 0 &&
      strcmp(str, "inf") != 0 && strcmp(str, "-inf") != 0 &&
      !strchr(str, '.') && !strchr(str, 'e') && !strchr(str, 'E')) {
    /* There is no decimal point and no exponent, so add a decimal point */
    strcat(str, ".0");
  }
}

NODEPTR
dblToString(flt_t x)
{
  char str[30];
  convdbl(str, x);
  // turn it into a mhs string
  GCCHECK(strNodes(strlen(str)));
  return mkStringC(str);
}

void
putdblb(flt_t x, BFILE *p)
{
  char str[30];
  convdbl(str, x);
  putsb(str, p);
}

void printrec(BFILE *f, struct print_bits *pb, NODEPTR n, int prefix);

/* Mark all reachable nodes, when a marked node is reached, mark it as shared. */
void
find_sharing(struct print_bits *pb, NODEPTR n)
{
 top:
  while (GETTAG(n) == T_IND) {
    n = INDIR(n);
  }
  if (n < cells || n >= cells + heap_size) abort();
  //PRINT("find_sharing %p %llu ", n, LABEL(n));
  tag_t tag = GETTAG(n);
  if (tag == T_AP || tag == T_ARR || tag == T_FORPTR) {
    if (test_bit(pb->shared_bits, n)) {
      /* Alread marked as shared */
      //PRINT("shared\n");
      ;
    } else if (test_bit(pb->marked_bits, n)) {
      /* Already marked, so now mark as shared */
      //PRINT("marked\n");
      set_bit(pb->shared_bits, n);
      num_shared++;
    } else {
      /* Mark as visited, and recurse */
      //PRINT("unmarked\n");
      set_bit(pb->marked_bits, n);
      switch(tag) {
      case T_AP:
        find_sharing(pb, FUN(n));
        n = ARG(n);
        goto top;
      case T_ARR:
        for(size_t i = 0; i < ARR(n)->size; i++) {
          find_sharing(pb, ARR(n)->array[i]);
        }
        break;
      default:
        break;
      }
    }
  } else {
    /* Not an sharable node, so do nothing */
    //PRINT("not T_AP\n");
    ;
  }
}

void
print_string(BFILE *f, struct bytestring bs)
{
  uint8_t *str = bs.string;
  putb('"', f);
  for (size_t i = 0; i < bs.size; i++) {
    int c = str[i];
#if 0
    if (c == '"' || c == '\\' || c < ' ' || c > '~') {
      putb('\\', f);
      putdecb(c, f);
      putb('&', f);
    } else {
      putb(c, f);
    }
#else
    if (c < 0 || c > 0xff)
      ERR("print_string");
    if (c < 0x20) {
      putb('^', f); putb(c + 0x20, f);
    } else if (c == '"' || c == '^' || c == '|' || c == '\\') {
      putb('\\', f); putb(c, f);
    } else if (c < 0x7f) {
      putb(c, f);
    } else if (c == 0x7f) {
      putb('\\', f); putb('?', f);
    } else if (c < 0xa0) {
      putb('^', f); putb(c - 0x80 + 0x40, f);
    } else if (c < 0xff) {
      putb('|', f); putb(c - 0x80, f);
    } else {                    /* must be c == 0xff */
      putb('\\', f); putb('_', f);
    }
#endif
  }
  putb('"', f);
}

/*
 * Recursively print an expression.
 * This assumes that the shared nodes has been marked as such.
 * The prefix flag is used to get a readable dump.
 */
void
printrec(BFILE *f, struct print_bits *pb, NODEPTR n, int prefix)
{
  int share = 0;

  while (GETTAG(n) == T_IND) {
    //putb('*', f);
    n = INDIR(n);
  }

  if (test_bit(pb->shared_bits, n)) {
    /* The node is shared */
    if (test_bit(pb->marked_bits, n)) {
      /* Not yet printed, so emit a label */
      if (prefix) {
        putb(':', f);
        putdecb((value_t)LABEL(n), f);
        putb(' ', f);
      } else {
        share = 1;
      }
      clear_bit(pb->marked_bits, n);  /* mark as printed */
    } else {
      /* This node has already been printed, so just use a reference. */
      putb('_', f);
      putdecb((value_t)LABEL(n), f);
      if (!prefix)
        putb(' ', f);
      return;
    }
  }

  //if (n == atptr) putb('@', f);
  switch (GETTAG(n)) {
  case T_AP:
    if (prefix) {
      putb('(', f);
      printrec(f, pb, FUN(n), prefix);
      putb(' ', f);
      printrec(f, pb, ARG(n), prefix);
      putb(')', f);
    } else {
      printrec(f, pb, FUN(n), prefix);
      printrec(f, pb, ARG(n), prefix);
      putb('@', f);
    }
    break;
  case T_INT: putb('#', f); putdecb(GETVALUE(n), f); break;
  case T_DBL: putb('&', f); putdblb(GETDBLVALUE(n), f); break;
  case T_ARR:
    if (prefix) {
      /* Arrays serialize as '[sz] e_1 ... e_sz' */
      putb('[', f);
      putdecb((value_t)ARR(n)->size, f);
      putb(']', f);
      for(size_t i = 0; i < ARR(n)->size; i++) {
        putb(' ', f);
        printrec(f, pb, ARR(n)->array[i], prefix);
      }
    } else {
      /* Arrays serialize as 'e_1 ... e_sz [sz]' */
      for(size_t i = 0; i < ARR(n)->size; i++) {
        printrec(f, pb, ARR(n)->array[i], prefix);
      }
      putb('[', f);
      putdecb((value_t)ARR(n)->size, f);
      putb(']', f);
    }
    break;
  case T_PTR:
    if (prefix) {
      char b[200]; sprintf(b,"PTR<%p>",PTR(n));
      putsb(b, f);
    } else {
      ERR("Cannot serialize pointers");
    }
    break;
  case T_FUNPTR:
    /* There are a few function pointers that happen without user FFI.
     * We need to be able to serialize these.
     * XXX Make a table if we need more.
     */
    if (FUNPTR(n) == 0) {
      putsb(";0 ", f);
    } else if (FUNPTR(n) == (HsFunPtr)closeb) {
      putsb(";closeb ", f);
    } else {
      ERR1("Cannot serialize function pointers %p", FUNPTR(n));
    }
    break;
  case T_FORPTR:
    if (n == comb_stdin)
      putsb("IO.stdin", f);
    else if (n == comb_stdout)
      putsb("IO.stdout", f);
    else if (n == comb_stderr)
      putsb("IO.stderr", f);
#if WANT_GMP
    else if (FORPTR(n)->finalizer->fptype == FP_MPZ) {
      /* Serialize as %99999" */
      mpz_ptr op = FORPTR(n)->payload.string; /* get the mpz */
      int sz = mpz_sizeinbase(op, 10);        /* maximum length */
      char *s = malloc(sz + 2);
      if (!s)
        memerr();
      (void)mpz_get_str(s, 10, op);           /* convert to a string */
      putsb("%", f);
      putsb(s, f);
      putsb("\"", f);                         /* so we can use parse_string */
      free(s);
    }
#endif  /* WANT_GMP */
    else if (FORPTR(n)->finalizer->fptype == FP_BSTR) {
      print_string(f, FORPTR(n)->payload);
    } else {
      ERR("Cannot serialize foreign pointers");
    }
    break;
  case T_IO_CCALL: putb('^', f); putsb(FFI_IX(GETVALUE(n)).ffi_name, f); break;
  case T_BADDYN: putb('^', f); putsb(CSTR(n), f); break;
  case T_S: putsb("S", f); break;
  case T_K: putsb("K", f); break;
  case T_I: putsb("I", f); break;
  case T_C: putsb("C", f); break;
  case T_B: putsb("B", f); break;
  case T_A: putsb("A", f); break;
  case T_U: putsb("U", f); break;
  case T_Y: putsb("Y", f); break;
  case T_P: putsb("P", f); break;
  case T_R: putsb("R", f); break;
  case T_O: putsb("O", f); break;
  case T_SS: putsb("S'", f); break;
  case T_BB: putsb("B'", f); break;
  case T_Z: putsb("Z", f); break;
  case T_K2: putsb("K2", f); break;
  case T_K3: putsb("K3", f); break;
  case T_K4: putsb("K4", f); break;
  case T_CC: putsb("C'", f); break;
  case T_CCB: putsb("C'B", f); break;
  case T_ADD: putsb("+", f); break;
  case T_SUB: putsb("-", f); break;
  case T_MUL: putsb("*", f); break;
  case T_QUOT: putsb("quot", f); break;
  case T_REM: putsb("rem", f); break;
  case T_UQUOT: putsb("uquot", f); break;
  case T_UREM: putsb("urem", f); break;
  case T_SUBR: putsb("subtract", f); break;
  case T_NEG: putsb("neg", f); break;
  case T_AND: putsb("and", f); break;
  case T_OR: putsb("or", f); break;
  case T_XOR: putsb("xor", f); break;
  case T_INV: putsb("inv", f); break;
  case T_SHL: putsb("shl", f); break;
  case T_SHR: putsb("shr", f); break;
  case T_ASHR: putsb("ashr", f); break;
  case T_POPCOUNT: putsb("popcount", f); break;
  case T_CLZ: putsb("clz", f); break;
  case T_CTZ: putsb("ctz", f); break;
#if WANT_FLOAT
  case T_FADD: putsb("f+", f); break;
  case T_FSUB: putsb("f-", f); break;
  case T_FMUL: putsb("f*", f); break;
  case T_FDIV: putsb("f/", f); break;
  case T_FNEG: putsb("fneg", f); break;
  case T_ITOF: putsb("itof", f); break;
  case T_FEQ: putsb("f==", f); break;
  case T_FNE: putsb("f/=", f); break;
  case T_FLT: putsb("f<", f); break;
  case T_FLE: putsb("f<=", f); break;
  case T_FGT: putsb("f>", f); break;
  case T_FGE: putsb("f>=", f); break;
  case T_FSHOW: putsb("fshow", f); break;
  case T_FREAD: putsb("fread", f); break;
#endif
  case T_BSAPPEND: putsb("bs++", f); break;
  case T_BSAPPENDDOT: putsb("bs++.", f); break;
  case T_BSEQ: putsb("bs==", f); break;
  case T_BSNE: putsb("bs/=", f); break;
  case T_BSLT: putsb("bs<", f); break;
  case T_BSLE: putsb("bs<=", f); break;
  case T_BSGT: putsb("bs>", f); break;
  case T_BSGE: putsb("bs>=", f); break;
  case T_BSCMP: putsb("bscmp", f); break;
  case T_BSPACK: putsb("bspack", f); break;
  case T_BSUNPACK: putsb("bsunpack", f); break;
  case T_BSREPLICATE: putsb("bsreplicate", f); break;
  case T_BSLENGTH: putsb("bslength", f); break;
  case T_BSSUBSTR: putsb("bssubstr", f); break;
  case T_BSINDEX: putsb("bsindex", f); break;
  case T_EQ: putsb("==", f); break;
  case T_NE: putsb("/=", f); break;
  case T_LT: putsb("<", f); break;
  case T_LE: putsb("<=", f); break;
  case T_GT: putsb(">", f); break;
  case T_GE: putsb(">=", f); break;
  case T_ULT: putsb("u<", f); break;
  case T_ULE: putsb("u<=", f); break;
  case T_UGT: putsb("u>", f); break;
  case T_UGE: putsb("u>=", f); break;
  case T_ICMP: putsb("icmp", f); break;
  case T_UCMP: putsb("ucmp", f); break;
  case T_FPADD: putsb("fp+", f); break;
  case T_FP2P: putsb("fp2p", f); break;
  case T_FPNEW: putsb("fpnew", f); break;
  case T_FPFIN: putsb("fpfin", f); break;
    //  case T_FPSTR: putsb("fpstr", f); break;
  case T_FP2BS: putsb("fp2bs", f); break;
  case T_BS2FP: putsb("bs2fp", f); break;
  case T_EQUAL: putsb("equal", f); break;
  case T_COMPARE: putsb("compare", f); break;
  case T_RNF: putsb("rnf", f); break;
  case T_SEQ: putsb("seq", f); break;
  case T_IO_BIND: putsb("IO.>>=", f); break;
  case T_IO_THEN: putsb("IO.>>", f); break;
  case T_IO_RETURN: putsb("IO.return", f); break;
  case T_IO_CCBIND: putsb("IO.C'BIND", f); break;
  case T_IO_SERIALIZE: putsb("IO.serialize", f); break;
  case T_IO_PRINT: putsb("IO.print", f); break;
  case T_IO_DESERIALIZE: putsb("IO.deserialize", f); break;
  case T_IO_GETARGREF: putsb("IO.getArgRef", f); break;
  case T_IO_PERFORMIO: putsb("IO.performIO", f); break;
  case T_IO_GC: putsb("IO.gc", f); break;
  case T_RAISE: putsb("raise", f); break;
  case T_CATCH: putsb("catch", f); break;
  case T_ARR_ALLOC: putsb("A.alloc", f); break;
  case T_ARR_COPY: putsb("A.copy", f); break;
  case T_ARR_SIZE: putsb("A.size", f); break;
  case T_ARR_READ: putsb("A.read", f); break;
  case T_ARR_WRITE: putsb("A.write", f); break;
  case T_ARR_EQ: putsb("A.==", f); break;
  case T_DYNSYM: putsb("dynsym", f); break;
  case T_NEWCASTRINGLEN: putsb("newCAStringLen", f); break;
  case T_PACKCSTRING: putsb("packCString", f); break;
  case T_PACKCSTRINGLEN: putsb("packCStringLen", f); break;
  case T_TOINT: putsb("toInt", f); break;
  case T_TOPTR: putsb("toPtr", f); break;
  case T_TODBL: putsb("toDbl", f); break;
  case T_TOFUNPTR: putsb("toFunPtr", f); break;
  case T_BSFROMUTF8: putsb("fromUTF8", f); break;
  case T_BSTOUTF8: putsb("toUTF8", f); break;
  case T_BSHEADUTF8: putsb("headUTF8", f); break;
  case T_BSTAILUTF8: putsb("tailUTF8", f); break;
  case T_TICK:
    putb('!', f);
    print_string(f, tick_table[GETVALUE(n)].tick_name);
    break;
  default: ERR("print tag");
  }
  if (!prefix) {
    if (GETTAG(n) != T_AP)
      putb(' ', f);
    if (share) {
      putb(':', f);
      putdecb((value_t)LABEL(n), f);
      putb(' ', f);
    }
  }
}

/* Serialize a graph to file. */
void
printb(BFILE *f, NODEPTR n, int header)
{
  struct print_bits pb;
  num_shared = 0;
  pb.marked_bits = calloc(free_map_nwords, sizeof(bits_t));
  if (!pb.marked_bits)
    memerr();
  pb.shared_bits = calloc(free_map_nwords, sizeof(bits_t));
  if (!pb.shared_bits)
    memerr();
  find_sharing(&pb, n);
  if (header) {
    putsb(VERSION, f);
    putdecb(num_shared, f);
    putb('\n', f);
  }
  printrec(f, &pb, n, !header);
  if (header) {
    putb('}', f);
  }
  FREE(pb.marked_bits);
  FREE(pb.shared_bits);
}

/* Show a graph. */
void
pp(FILE *f, NODEPTR n)
{
  BFILE *bf = add_FILE(f);
  printb(bf, n, 0);
  putb('\n', bf);
  freeb_file(bf);
}

#if 0
void
ppmsg(const char *msg, NODEPTR n)
{
  printf("%s", msg);
  pp(stdout, n);
  printf("\n");
}
#endif

void
dump(const char *msg, NODEPTR at)
{
#if 0
  atptr = at;
  printf("dump: %s\n", msg);
  pp(stdout, *topnode);
#endif
}

#else  /* WANT_STDIO */
NODEPTR
dblToString(flt_t x)
{
  return mkStringC("no dblToString");
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

NODEPTR
mkFunPtr(void (*p)(void))
{
  NODEPTR n;
  n = alloc_node(T_FUNPTR);
  FUNPTR(n) = p;
  return n;
}

struct forptr*
mkForPtr(struct bytestring bs)
{
  struct final *fin = calloc(1, sizeof(struct final));
  struct forptr *fp = calloc(1, sizeof(struct forptr));
  if (!fin || !fp)
    memerr();
  if (bs.size == NOSIZE) {
    num_fin_alloc++;
  } else {
    num_bs_alloc++;
    num_bs_inuse += bs.size;
    num_bs_bytes += bs.size;
    if (num_bs_inuse > num_bs_inuse_max)
      num_bs_inuse_max = num_bs_inuse;
  }
  //printf("mkForPtr p=%p fin=%p fp=%p\n", p, fin, fp);
  fin->next = final_root;
  final_root = fin;
  fin->final = 0;
  fin->arg = bs.string;
  fin->size = bs.size;          /* The size is not really needed */
  fin->back = fp;
  fin->marked = 0;
  fp->next = 0;
  fp->payload = bs;
  fp->finalizer = fin;
  //  fp->desc = 0;
  return fp;
}

struct forptr*
mkForPtrP(void *p)
{
  struct bytestring bs = { NOSIZE, p };
  return mkForPtr(bs);
}

struct forptr*
addForPtr(struct forptr *ofp, int s)
{
  struct forptr *fp = malloc(sizeof(struct forptr));
  struct final *fin = ofp->finalizer;
  if (!fp)
    memerr();
  fp->next = ofp;
  fin->back = fp;
  if (ofp->payload.size != NOSIZE)
    fp->payload.size = ofp->payload.size - s;
  fp->payload.string = (uint8_t*)ofp->payload.string + s;
  fp->finalizer = fin;
  return fp;
}

struct forptr*
bssubstr(struct forptr *fp, value_t offs, value_t len)
{
  struct forptr *res = addForPtr(fp, offs);
  res->payload.size = len;
  return res;
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

/* Turn a C string into a combinator string.
 * Does NOT do UTF decoding.
 */
NODEPTR
mkString(struct bytestring bs)
{
  NODEPTR n, nc;
  size_t i;
  const unsigned char *str = bs.string; /* no sign bits, please */

  n = mkNil();
  for(i = bs.size; i > 0; i--) {
    nc = mkInt(str[i-1]);
    n = mkCons(nc, n);
  }
  return n;
}

NODEPTR
mkStringC(char *str)
{
  struct bytestring bs = { strlen(str), str };
  return mkString(bs);
}

NODEPTR
mkStringU(struct bytestring bs)
{
  BFILE *ubuf = add_utf8(openb_rd_buf(bs.string, bs.size));
  NODEPTR n, *np, nc;

  //printf("mkStringU %d %s\n", (int)bs.size, (char*)bs.string);

  n = mkNil();
  np = &n;
  for(;;) {
    int c = getb(ubuf);
    if (c < 0)
      break;
    nc = mkInt(c);
    *np = mkCons(nc, *np);
    np = &ARG(*np);
  }
  closeb(ubuf);
  return n;
}

NODEPTR
bsunpack(struct bytestring bs)
{
  NODEPTR n, *np, nc;
  size_t i;

  n = mkNil();
  np = &n;
  for(i = 0; i < bs.size; i++) {
    nc = mkInt(((uint8_t *)bs.string)[i]);
    *np = mkCons(nc, *np);
    np = &ARG(*np);
  }
  return n;
}

/* XXX This should somehow be merged with other utf8 decoders */
value_t
headutf8(struct bytestring bs, void **ret)
{
  uint8_t *p = bs.string;
  if (bs.size == 0)
    ERR("headUTF8 0");
  int c1 = *p++;
  if ((c1 & 0x80) == 0) {
    if (ret)
      *ret = p;
    return c1;
  }
  if (bs.size == 1)
    ERR("headUTF8 1");
  int c2 = *p++;
  if ((c1 & 0xe0) == 0xc0) {
    if (ret)
      *ret = p;
    return ((c1 & 0x1f) << 6) | (c2 & 0x3f);
  }
  if (bs.size == 2)
    ERR("headUTF8 2");
  int c3 = *p++;
  if ((c1 & 0xf0) == 0xe0) {
    if (ret)
      *ret = p;
    return ((c1 & 0x0f) << 12) | ((c2 & 0x3f) << 6) | (c3 & 0x3f);
  }
  if (bs.size == 3)
    ERR("headUTF8 3");
  int c4 = *p++;
  if ((c1 & 0xf8) == 0xf0) {
    if (ret)
      *ret = p;
    return ((c1 & 0x07) << 18) | ((c2 & 0x3f) << 12) | ((c3 & 0x3f) << 6) | (c4 & 0x3f);
  }
  ERR("headUTF8 4");
}

NODEPTR evali(NODEPTR n);

/* Follow indirections */
static INLINE NODEPTR
indir(NODEPTR *np)
{
  NODEPTR n = *np;
  while (GETTAG(n) == T_IND)
    n = INDIR(n);
  *np = n;
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

/* Evaluate to a T_FUNPTR */
HsFunPtr
evalfunptr(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (GETTAG(n) != T_FUNPTR) {
    ERR1("evalfunptr, bad tag %d", GETTAG(n));
  }
#endif
  return FUNPTR(n);
}

/* Evaluate to a T_FORPTR */
struct forptr *
evalforptr(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (GETTAG(n) != T_FORPTR) {
    ERR1("evalforptr, bad tag %d", GETTAG(n));
  }
#endif
  return FORPTR(n);
}

/* Evaluate to a bytestring */
struct forptr *
evalbstr(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (GETTAG(n) != T_FORPTR || FORPTR(n)->finalizer->fptype != FP_BSTR) {
    ERR1("evalbstr, bad tag %d", GETTAG(n));
  }
#endif
  return FORPTR(n);
}

/* Evaluate a string, returns a newly allocated buffer.
 * XXX this is cheating, should use continuations.
 * XXX the malloc()ed string is leaked if we yield in here.
 * Caller is responsible to free().
 * Does UTF-8 encoding.
 */
struct bytestring
evalstring(NODEPTR n)
{
  size_t sz = 100;
  char *buf = MALLOC(sz);
  size_t offs;
  uvalue_t c;
  NODEPTR x;
  struct bytestring bs;

  if (!buf)
    memerr();
  for (offs = 0;;) {
    if (offs >= sz - 4) {
      sz *= 2;
      buf = REALLOC(buf, sz);
      if (!buf)
        memerr();
    }
    PUSH(n);                    /* protect the list from GC */
    n = evali(n);
    POP(1);
    if (GETTAG(n) == T_K)       /* Nil */
      break;
    else if (GETTAG(n) == T_AP && GETTAG(x = indir(&FUN(n))) == T_AP && GETTAG(indir(&FUN(x))) == T_O) { /* Cons */
      PUSH(n);                  /* protect from GC */
      c = evalint(ARG(x));
      n = POPTOP();
      if ((c & 0x1ff800) == 0xd800) {
        // c is a surrogate
        c = 0xfffd; // replacement character
      }
      if (c < 0x80) {
        buf[offs++] = (char)c;
      } else if (c < 0x800) {
        buf[offs++] = ((c >> 6 )       ) | 0xc0;
        buf[offs++] = ((c      ) & 0x3f) | 0x80;
      } else if (c < 0x10000) {
        buf[offs++] = ((c >> 12)       ) | 0xe0;
        buf[offs++] = ((c >> 6 ) & 0x3f) | 0x80;
        buf[offs++] = ((c      ) & 0x3f) | 0x80;
      } else if (c < 0x110000) {
        buf[offs++] = ((c >> 18)       ) | 0xf0;
        buf[offs++] = ((c >> 12) & 0x3f) | 0x80;
        buf[offs++] = ((c >> 6 ) & 0x3f) | 0x80;
        buf[offs++] = ((c      ) & 0x3f) | 0x80;
      } else {
	ERR("invalid char");
      }
      n = ARG(n);
    } else {
      ERR("evalstring not Nil/Cons");
    }
  }
  buf[offs] = 0;                /* in case we use it as a C string */
  bs.size = offs;
  bs.string = buf;
  return bs;
}

/* Does not do UTF-8 encoding */
struct bytestring
evalbytestring(NODEPTR n)
{
  size_t sz = 100;
  uint8_t *buf = MALLOC(sz);
  size_t offs;
  uvalue_t c;
  NODEPTR x;
  struct bytestring bs;

  if (!buf)
    memerr();
  for (offs = 0;;) {
    if (offs >= sz - 1) {
      sz *= 2;
      buf = REALLOC(buf, sz);
      if (!buf)
        memerr();
    }
    PUSH(n);                    /* protect list from GC */
    n = evali(n);
    POP(1);
    if (GETTAG(n) == T_K)       /* Nil */
      break;
    else if (GETTAG(n) == T_AP && GETTAG(x = indir(&FUN(n))) == T_AP && GETTAG(indir(&FUN(x))) == T_O) { /* Cons */
      PUSH(n);                  /* protect from GC */
      c = evalint(ARG(x));
      n = POPTOP();
      buf[offs++] = c;
      n = ARG(n);
    } else {
      ERR("evalbytestring not Nil/Cons");
    }
  }
  buf[offs] = 0;                /* in case we use it as a C string */
  bs.size = offs;
  bs.string = buf;
  return bs;
}

struct bytestring
bsreplicate(size_t size, uint8_t value)
{
  struct bytestring bs;
  bs.size = size;
  bs.string = MALLOC(size);
  if (!bs.string)
    memerr();
  memset(bs.string, value, size);
  return bs;
}

struct bytestring
bsappend(struct bytestring p, struct bytestring q)
{
  struct bytestring r;
  r.size = p.size + q.size;
  r.string = MALLOC(r.size);
  if (!r.string)
    memerr();
  memcpy(r.string, p.string, p.size);
  memcpy((uint8_t *)r.string + p.size, q.string, q.size);
  return r;
}

struct bytestring
bsappenddot(struct bytestring p, struct bytestring q)
{
  struct bytestring r;
  r.size = p.size + q.size + 1;
  r.string = MALLOC(r.size);
  if (!r.string)
    memerr();
  memcpy(r.string, p.string, p.size);
  memcpy((uint8_t *)r.string + p.size, ".", 1);
  memcpy((uint8_t *)r.string + p.size + 1, q.string, q.size);
  return r;
}

/*
 * Compare bytestrings.
 * We can't use memcmp() directly for two reasons:
 *  - the two strings can have different lengths
 *  - the return value is only guaranteed to be ==0 or !=0
 */
int
bscompare(struct bytestring bsp, struct bytestring bsq)
{
  uint8_t *p = bsp.string;
  uint8_t *q = bsq.string;
  size_t len = bsp.size < bsq.size ? bsp.size : bsq.size;
  while (len--) {
    int r = (int)*p++ - (int)*q++;
    if (r) {
      /* Unequal bytes found. */
      if (r < 0)
        return -1;
      if (r > 0)
        return 1;
      return 0;
    }
  }
  /* Got to the end of the shorter string. */
  /* The shorter string is considered smaller. */
  if (bsp.size < bsq.size)
    return -1;
  if (bsp.size > bsq.size)
    return 1;
  return 0;
}

/* Compares anything, but really only works well on strings.
 * if p < q  return -1
 * if p > q  return 1
 * if p == q return 0
 *
 * As we compare we update the argument pointers with any
 * progress we make, in case we are interruped and resume from the top.
 *
 * XXX This is a rather dodgy comparison, since we are comparing
 * functions, and the same data type could plausibly get different
 * functions in the Scott encoding.
 * But we only use it for lists, and it seems to work fine.
 */
int
compare(NODEPTR cmp)
{
  stackptr_t stk = stack_ptr;
#define CRET(x) do { stack_ptr = stk; return (x); } while(0)
  value_t x, y;
  flt_t xd, yd;
  void *f, *g;
  void (*ff)(void), (*fg)(void);
  NODEPTR p, q;
  NODEPTR *ap, *aq;
  enum node_tag ptag, qtag;
  int r;

  /* Since FUN(cmp) can be shared, allocate a copy for it. */
  GCCHECK(1);
  FUN(cmp) = new_ap(FUN(FUN(cmp)), ARG(FUN(cmp)));
  aq = &ARG(cmp);
  ap = &ARG(FUN(cmp));

  PUSH(*ap);
  PUSH(*aq);
  for(;;) {
    if (stk == stack_ptr)
      return 0;
    q = evali(TOP(0));
    p = evali(TOP(1));
    POP(2);
    if (stk == stack_ptr) {
      /* We have made some progress, save this in the compare node. */
      *ap = p;
      *aq = q;
    }

    ptag = GETTAG(p);
    qtag = GETTAG(q);
    if (ptag != qtag) {
      /* Hack to make Nil < Cons */
      if (ptag == T_K && qtag == T_AP)
        CRET(-1);
      if (ptag == T_AP && qtag == T_K)
        CRET(1);
      CRET(ptag < qtag ? -1 : 1);
    }
    switch (ptag) {
    case T_AP:
      PUSH(ARG(p));             /* compare arg part later */
      PUSH(ARG(q));
      PUSH(FUN(p));             /* compare fun part now */
      PUSH(FUN(q));
      break;
    case T_INT:
    case T_IO_CCALL:
      x = GETVALUE(p);
      y = GETVALUE(q);
      if (x < y)
        CRET(-1);
      if (x > y)
        CRET(1);
      break;
    case T_DBL:
      xd = GETDBLVALUE(p);
      yd = GETDBLVALUE(q);
      if (xd < yd)
        CRET(-1);
      if (xd > yd)
        CRET(1);
      break;
    case T_PTR:
      f = PTR(p);
      g = PTR(q);
      if (f < g)
        CRET(-1);
      if (f > g)
        CRET(1);
      break;
    case T_FUNPTR:
      ff = FUNPTR(p);
      fg = FUNPTR(q);
      if ((intptr_t)ff < (intptr_t)fg)
        CRET(-1);
      if ((intptr_t)ff > (intptr_t)fg)
        CRET(1);
      break;
    case T_FORPTR:
      {
      struct forptr *fp = FORPTR(p);
      struct forptr *fq = FORPTR(q);
#if WANT_GMP
      if (fp->finalizer->fptype == FP_MPZ && fq->finalizer->fptype == FP_MPZ) {
        int i = mpz_cmp(fp->payload.string, fq->payload.string);
        if (i < 0)
          CRET(-1);
        if (i > 0)
          CRET(1);
      } else
#endif
      if (fp->finalizer->fptype == FP_BSTR && fq->finalizer->fptype == FP_BSTR) {
        r = bscompare(BSTR(p), BSTR(q));
        if (r)
          CRET(r);
      } else {
        f = fp->payload.string;
        g = fq->payload.string;
        if (f < g)
          CRET(-1);
        if (f > g)
          CRET(1);
      }
      }
      break;
    case T_ARR:
      if (ARR(p) < ARR(q))
        CRET(-1);
      if (ARR(p) > ARR(q))
        CRET(1);
      break;
    default:
      break;
    }
  }
#undef CRET
}

void
rnf_rec(bits_t *done, NODEPTR n)
{
 top:
  if (test_bit(done, n))
    return;
  set_bit(done, n);
  n = evali(n);
  if (GETTAG(n) == T_AP) {
    PUSH(ARG(n));               /* protect from GC */
    rnf_rec(done, FUN(n));
    n = POPTOP();
    goto top;
  }
}

/* Used to detect calls to error while we are already in a call to error. */
int in_raise = 0;

/* This is a yucky hack */
int doing_rnf = 0;

void
rnf(value_t noerr, NODEPTR n)
{
  /* Mark visited nodes to avoid getting stuck in loops. */
  bits_t *done = calloc(free_map_nwords, sizeof(bits_t));
  if (!done)
    memerr();
  if (doing_rnf)
    ERR("recursive rnf()");
  doing_rnf = (int)noerr;
  rnf_rec(done, n);
  doing_rnf = 0;
  FREE(done);
}

void execio(NODEPTR *);

/* Evaluate a node, returns when the node is in WHNF. */
NODEPTR
evali(NODEPTR an)
{
  NODEPTR n = an;
  stackptr_t stk = stack_ptr;
  NODEPTR x, y, z, w;
  value_t xi, yi, r;
  struct forptr *xfp;
#if WANT_FLOAT
  flt_t xd, rd;
#endif  /* WANT_FLOAT */
  char *msg;
#if FASTTAGS
  heapoffs_t l;
#endif
  enum node_tag tag;
  struct ioarray *arr;
  struct bytestring xbs, ybs, rbs;

#if MAXSTACKDEPTH
  counter_t old_cur_c_stack = cur_c_stack;
  if (++cur_c_stack > max_c_stack)
    max_c_stack = cur_c_stack;
#endif

/* Reset stack pointer and return. */
#define RET do { goto ret; } while(0)
/* Check that there are at least n arguments, return if not. */
#define HASNARGS(n) (stack_ptr - stk >= (n))
#define CHECK(n) do { if (!HASNARGS(n)) RET; } while(0)

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
#define SETFUNPTR(n,r) do { SETTAG((n), T_FUNPTR); FUNPTR(n) = (r); } while(0)
#define SETFORPTR(n,r) do { SETTAG((n), T_FORPTR); FORPTR(n) = (r); } while(0)
#define SETBSTR(n,r)   do { SETTAG((n), T_FORPTR); FORPTR(n) = (r); FORPTR(n)->finalizer->fptype = FP_BSTR; } while(0)
#define OPINT1(e)      do { CHECK(1); xi = evalint(ARG(TOP(0)));                            e; POP(1); n = TOP(-1); } while(0);
#define OPPTR2(e)      do { CHECK(2); xp = evalptr(ARG(TOP(0))); yp = evalptr(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define CMPP(op)       do { OPPTR2(r = xp op yp); GOIND(r ? combTrue : combFalse); } while(0)

 top:
  COUNT(num_reductions);
#if FASTTAGS
  l = LABEL(n);
  tag = l < T_IO_BIND ? l : GETTAG(n);
#else   /* FASTTAGS */
  tag = GETTAG(n);
#endif  /* FASTTAGS */
  switch (tag) {
  ind:
  case T_IND:  n = INDIR(n); goto top;

  ap:
  case T_AP:   PUSH(n); n = FUN(n); goto top;

  case T_INT:  RET;
  case T_DBL:  RET;
  case T_PTR:  RET;
  case T_FUNPTR: RET;
  case T_FORPTR: RET;
  case T_ARR:  RET;
  case T_BADDYN: ERR1("FFI unknown %s", CSTR(n));

  /*
   * Some of these reductions, (e.g., Z x y = K (x y)) are there to avoid
   * that increase in arity that some "optimizations" in Abstract.hs
   * stop reductions from happening.  This can be important for "full laziness".
   * In practice, these reductions almost never happen, but there they are anyway. :)
   */
  case T_S:    GCCHECK(2); CHKARG3; GOAP(new_ap(x, z), new_ap(y, z));                     /* S x y z = x z (y z) */
  case T_SS:   GCCHECK(3); CHKARG4; GOAP(new_ap(x, new_ap(y, w)), new_ap(z, w));          /* S' x y z w = x (y w) (z w) */
  case T_K:                CHKARG2; GOIND(x);                                             /* K x y = *x */
  case T_A:                CHKARG2; GOIND(y);                                             /* A x y = *y */
  case T_U:                CHKARG2; GOAP(y, x);                                           /* U x y = y x */
  case T_I:                CHKARG1; GOIND(x);                                             /* I x = *x */
  case T_Y:                CHKARG1; GOAP(x, n);                                           /* n@(Y x) = x n */
  case T_B:    GCCHECK(1); CHKARG3; GOAP(x, new_ap(y, z));                                /* B x y z = x (y z) */
  case T_BB:   if (!HASNARGS(4)) {
               GCCHECK(1); CHKARG2; red_bb++; GOAP(combB, new_ap(x, y)); } else {         /* B' x y = B (x y) */
               GCCHECK(2); CHKARG4; GOAP(new_ap(x, y), new_ap(z, w)); }                   /* B' x y z w = x y (z w) */
  case T_Z:    if (!HASNARGS(3)) {
               GCCHECK(1); CHKARG2; red_z++; GOAP(combK, new_ap(x, y)); } else {          /* Z x y = K (x y) */
                           CHKARG3; GOAP(x, y); }                                         /* Z x y z = x y */
  case T_C:    GCCHECK(1); CHKARG3; GOAP(new_ap(x, z), y);                                /* C x y z = x z y */
  case T_CC:   GCCHECK(2); CHKARG4; GOAP(new_ap(x, new_ap(y, w)), z);                     /* C' x y z w = x (y w) z */
  case T_P:    GCCHECK(1); CHKARG3; GOAP(new_ap(z, x), y);                                /* P x y z = z x y */
  case T_R:    if(!HASNARGS(3)) {
               GCCHECK(1); CHKARG2; red_r++; GOAP(new_ap(combC, y), x); } else {          /* R x y = C y x */
               GCCHECK(1); CHKARG3; GOAP(new_ap(y, z), x); }                              /* R x y z = y z x */
  case T_O:    GCCHECK(1); CHKARG4; GOAP(new_ap(w, x), y);                                /* O x y z w = w x y */
  case T_K2:   if (!HASNARGS(3)) {
                           CHKARG2; red_k2++; GOAP(combK, x); } else {                    /* K2 x y = K x */
                           CHKARG3; GOIND(x); }                                           /* K2 x y z = *x */
  case T_K3:   if (!HASNARGS(4)) {
                           CHKARG2; red_k3++; GOAP(combK2, x); } else {                   /* K3 x y = K2 x */
                           CHKARG4; GOIND(x); }                                           /* K3 x y z w = *x */
  case T_K4:   if (!HASNARGS(5)) {
                           CHKARG2; red_k4++; GOAP(combK3, x); } else {                   /* K4 x y = K3 x */
                           CHECK(5); POP(5); n = TOP(-1); x = ARG(TOP(-5)); GOIND(x); }   /* K4 x y z w v = *x */
  case T_CCB:  if (!HASNARGS(4)) {
               GCCHECK(2); CHKARG3; red_ccb++; GOAP(new_ap(combB, new_ap(x, z)), y); } else {  /* C'B x y z = B (x z) y */
               GCCHECK(2); CHKARG4; GOAP(new_ap(x, z), new_ap(y, w)); }                   /* C'B x y z w = x z (y w) */

    /*
     * Strict primitives require evaluating the arguments before we can proceed.
     * The easiest way to do this is to just recursively call evali() for each argument.
     * The drawback of this is that it uses a lot of C stack.  (E.g., recompiling MicroHs
     * uses a stack depth of 1800).
     * Instead we use the following scheme:
     *  When we find a strict binary (int) primitive we push T_BININT2,
     *  set n=second argument.
     *  Continue evaluation of n.
     *  When n is finally evaluated and we are about to return we check if the stack top is T_BININT2.
     *  If so, change the stack top to T_BININT1,
     *  set n=first argument.
     *  Continue evaluation of n.
     *  When n is finally evaluated and we are about to return we check if the stack top is T_BININT1.
     *  If so, we know that both arguments are now evaluated, and we perform the strict operation.
     *
     * On my desktop machine this is about 3% slower, on my laptop (Apple M1) it is about 3% faster.
     *
     * Pictorially for BININT
     *  Before the code below:
     *  ----
     *  | --------> @
     *  ----       / \
     *  | ------> @   y
     *  ----     / \
     *  n ----> ADD x
     *
     * After
     *  ----
     *  | --------> @
     *  ----       / \
     *  | ------> @   y
     *  ----     / \
     *  | ->BI2 ADD x
     *  ----        ^
     *  n ----------|
     *
     *  x becomes an INT, stack is not empty, BININT2 found on top
     *  ----
     *  | --------> @
     *  ----       / \
     *  | ------> @   y
     *  ----     / \
     *  | ->BI2 ADD INT
     *  ----        ^
     *  n ----------|
     *
     *  After
     *  ----
     *  | --------> @
     *  ----       / \
     *  | ------> @   y
     *  ----     / \    \
     *  | ->BI1 ADD INT  |
     *  ----             |
     *  n ---------------|
     *
     *  y becomes an INT, stack is not empty, BININT1 found on top
     *  do arithmetic
     *  ----
     *  | --------> @
     *  ----       / \
     *  | ------> @   INT
     *  ----     / \    \
     *  | ->BI1 ADD INT  |
     *  ----             |
     *  n ---------------|
     *
     *  ----
     *  n -------> INT(x+y)
     */
  case T_ADD:
  case T_SUB:
  case T_MUL:
  case T_QUOT:
  case T_REM:
  case T_SUBR:
  case T_UQUOT:
  case T_UREM:
  case T_AND:
  case T_OR:
  case T_XOR:
  case T_SHL:
  case T_SHR:
  case T_ASHR:
  case T_EQ:
  case T_NE:
  case T_LT:
  case T_LE:
  case T_GT:
  case T_GE:
  case T_ICMP:
  case T_ULT:
  case T_ULE:
  case T_UGT:
  case T_UGE:
  case T_UCMP:
    CHECK(2);
    n = ARG(TOP(1));
    if (GETTAG(n) == T_INT) {
      n = ARG(TOP(0));
      PUSH(combBININT1);
      if (GETTAG(n) == T_INT)
        goto binint1;
    } else {
      PUSH(combBININT2);
    }
    goto top;
  case T_NEG:
  case T_INV:
  case T_POPCOUNT:
  case T_CLZ:
  case T_CTZ:
    CHECK(1);
    n = ARG(TOP(0));
    PUSH(combUNINT1);
    goto top;

#if WANT_FLOAT
  case T_FADD:
  case T_FSUB:
  case T_FMUL:
  case T_FDIV:
  case T_FEQ:
  case T_FNE:
  case T_FLT:
  case T_FLE:
  case T_FGT:
  case T_FGE:
    CHECK(2);
    n = ARG(TOP(1));
    PUSH(combBINDBL2);
    goto top;
  case T_FNEG:
    CHECK(1);
    n = ARG(TOP(0));
    PUSH(combUNDBL1);
    goto top;

  case T_ITOF: OPINT1(rd = (flt_t)xi); SETDBL(n, rd); RET;
  case T_FREAD:
    CHECK(1);
    msg = evalstring(ARG(TOP(0))).string;
#if WORD_SIZE == 64
    xd = strtod(msg, NULL);
#elif WORD_SIZE == 32
    xd = strtof(msg, NULL);
#else  /* WORD_SIZE */
#error Unknown WORD_SIZE
#endif  /* WORD_SIZE */
    FREE(msg);
    POP(1);
    n = TOP(-1);
    SETDBL(n, xd);
    RET;

  case T_FSHOW:
    CHECK(1);
    xd = evaldbl(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    GOIND(dblToString(xd));
#endif  /* WANT_FLOAT */

  case T_BSAPPEND:
  case T_BSAPPENDDOT:
  case T_BSEQ:
  case T_BSNE:
  case T_BSLT:
  case T_BSLE:
  case T_BSGT:
  case T_BSGE:
  case T_BSCMP:
    CHECK(2);
    n = ARG(TOP(1));
    PUSH(combBINBS2);
    goto top;

  /* Retag a word sized value, keeping the value bits */
#define CONV(t) do { CHECK(1); x = evali(ARG(TOP(0))); n = POPTOP(); SETTAG(n, t); SETVALUE(n, GETVALUE(x)); RET; } while(0)
  case T_TODBL: CONV(T_DBL);
  case T_TOINT: CONV(T_INT);
  case T_TOPTR: CONV(T_PTR);
  case T_TOFUNPTR: CONV(T_FUNPTR);
#undef CONV

  case T_FPADD: CHECK(2); xfp = evalforptr(ARG(TOP(0))); yi = evalint(ARG(TOP(1))); POP(2); n = TOP(-1); SETFORPTR(n, addForPtr(xfp, yi)); RET;
  case T_FP2P:
    CHECK(1);
    xfp = evalforptr(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETPTR(n, xfp->payload.string);
    RET;

  case T_FP2BS:
    CHECK(2);
    xfp = evalforptr(ARG(TOP(0)));
    xi = evalint(ARG(TOP(1)));
    POP(2);
    n = TOP(-1);
    xfp->payload.size = xi;
    SETBSTR(n, xfp);
    RET;

  case T_BS2FP:
    CHECK(1);
    xfp = evalbstr(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETFORPTR(n, xfp);
    RET;

  case T_ARR_EQ:
    {
      CHECK(2);
      x = evali(ARG(TOP(0)));
      arr = ARR(x);
      y = evali(ARG(TOP(1)));
      POP(2);
      n = TOP(-1);
      GOIND(arr == ARR(y) ? combTrue : combFalse);
    }

  case T_BSTOUTF8:
    {
      CHECK(1);
      n = ARG(TOP(0));
      /* Zap the pointer to the list so it can be GC:ed.
       * The actual list is protected from GC by evalbytestring().
       */
      // ARG(TOP(0)) = combK;
      struct bytestring bs = evalstring(n);
      POP(1);
      n = TOP(-1);
      SETBSTR(n, mkForPtrFree(bs));
      RET;
    }

  case T_BSHEADUTF8:
    CHECK(1);
    xfp = evalbstr(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETINT(n, headutf8(xfp->payload, (void**)0));
    RET;

  case T_BSTAILUTF8:
    CHECK(1);
    xfp = evalbstr(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    { void *out;
      (void)headutf8(xfp->payload, &out);           /* skip one UTF8 character */
      xi = (char*)out - (char*)xfp->payload.string; /* offset */
      yi = xfp->payload.size - xi;                  /* remaining length */
      SETBSTR(n, bssubstr(xfp, xi, yi));            /* make a substring */
    }
    RET;

  case T_BSFROMUTF8:
    if (doing_rnf) RET;
    CHECK(1);

    xfp = evalbstr(ARG(TOP(0)));
    GCCHECK(strNodes(xfp->payload.size));
    POP(1);
    n = TOP(-1);
    //printf("T_FROMUTF8 x = %p fp=%p payload.string=%p\n", x, x->uarg.uuforptr, x->uarg.uuforptr->payload.string);
    GOIND(mkStringU(xfp->payload));

  case T_BSUNPACK:
    if (doing_rnf) RET;
    CHECK(1);
    struct forptr *xfp = evalbstr(ARG(TOP(0)));
    GCCHECK(strNodes(xfp->payload.size));
    POP(1);
    n = TOP(-1);
    GOIND(bsunpack(xfp->payload));

  case T_BSPACK:
    CHECK(1);
    n = ARG(TOP(0));
    /* Zap the pointer to the list so it can be GC:ed.
     * The actual list is protected from GC by evalbytestring().
     */
    ARG(TOP(0)) = combK;
    struct bytestring rbs = evalbytestring(n);
    POP(1);
    n = TOP(-1);
    SETBSTR(n, mkForPtrFree(rbs));
    RET;

  case T_BSREPLICATE:
    CHECK(2);
    xi = evalint(ARG(TOP(0)));
    yi = evalint(ARG(TOP(1)));
    POP(2);
    n = TOP(-1);
    SETBSTR(n, mkForPtrFree(bsreplicate(xi, yi)));
    RET;

  case T_BSLENGTH:
    CHECK(1);
    xfp = evalbstr(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETINT(n, xfp->payload.size);
    RET;

  case T_BSSUBSTR:
    CHECK(3);
    xfp = evalbstr(ARG(TOP(0)));
    xi = evalint(ARG(TOP(1)));
    yi = evalint(ARG(TOP(2)));
    POP(3);
    n = TOP(-1);
    SETBSTR(n, bssubstr(xfp, xi, yi));
    RET;

  case T_BSINDEX:
    CHECK(2);
    xfp = evalbstr(ARG(TOP(0)));
    xi = evalint(ARG(TOP(1)));
    POP(2);
    n = TOP(-1);
    SETINT(n, ((uint8_t *)xfp->payload.string)[xi]);
    RET;

  case T_RAISE:
    if (doing_rnf) RET;
    if (cur_handler) {
      /* Pass the exception to the handler */
      CHKARG1;
      cur_handler->hdl_exn = x;
      longjmp(cur_handler->hdl_buf, 1);
    } else {
      /* No handler:
       * First convert the exception to a string by calling displaySomeException.
       * The display function compiles to combShowExn, so we need to build
       * (combShowExn x) and evaluate it.
       */
      if (in_raise) {
        ERR("recursive error");
        EXIT(1);
      }
      in_raise = 1;
      CHECK(1);
      GCCHECK(1);
      //TOP(0) = new_ap(combShowExn, TOP(0));
      FUN(TOP(0)) = combShowExn; /* TOP(0) = (combShowExn exn) */
      x = evali(TOP(0));        /* evaluate it */
      msg = evalstring(x).string;   /* and convert to a C string */
      POP(1);
#if WANT_STDIO
      /* A horrible hack until we get proper exceptions */
      if (strcmp(msg, "ExitSuccess") == 0) {
        EXIT(0);
      } else {
        fprintf(stderr, "mhs: %s\n", msg);
        EXIT(1);
      }
#else  /* WANT_STDIO */
      ERR1("mhs error: %s", msg);
#endif  /* WANT_STDIO */
    }


  case T_SEQ:  CHECK(2); evali(ARG(TOP(0))); POP(2); n = TOP(-1); y = ARG(n); GOIND(y); /* seq x y = eval(x); y */

  case T_EQUAL:
    CHECK(2); r = compare(TOP(1)); POP(2); n = TOP(-1); GOIND(r==0 ? combTrue : combFalse);
  case T_COMPARE:
    CHECK(2); r = compare(TOP(1)); POP(2); n = TOP(-1); GOIND(r < 0 ? combLT : r > 0 ? combGT : combEQ);

  case T_RNF:
    if (doing_rnf) RET;
    CHECK(2);
    xi = evalint(ARG(TOP(0)));
    rnf(xi, ARG(TOP(1))); POP(2); n = TOP(-1); GOIND(combUnit);

  case T_IO_PERFORMIO:
    CHECK(1);
    if (doing_rnf) RET;
    execio(&ARG(TOP(0)));       /* run IO action */
    x = ARG(TOP(0));            /* should be RETURN e */
    if (GETTAG(x) != T_AP || GETTAG(FUN(x)) != T_IO_RETURN)
      ERR("PERFORMIO");
    POP(1);
    n = TOP(-1);
    GOIND(ARG(x));

  case T_IO_CCBIND:           /* We should never have to reduce this */
  case T_IO_BIND:
  case T_IO_THEN:
  case T_IO_RETURN:
  case T_IO_SERIALIZE:
  case T_IO_PRINT:
  case T_IO_DESERIALIZE:
  case T_IO_GETARGREF:
  case T_IO_CCALL:
  case T_CATCH:
  case T_NEWCASTRINGLEN:
  case T_PACKCSTRING:
  case T_PACKCSTRINGLEN:
  case T_ARR_ALLOC:
  case T_ARR_COPY:
  case T_ARR_SIZE:
  case T_ARR_READ:
  case T_ARR_WRITE:
  case T_FPNEW:
  case T_FPFIN:
    //  case T_FPSTR:
  case T_IO_GC:
    RET;

  case T_DYNSYM:
    /* A dynamic FFI lookup */
    CHECK(1);
    msg = evalstring(ARG(TOP(0))).string;
    GCCHECK(1);
    x = ffiNode(msg);
    FREE(msg);
    POP(1);
    n = TOP(-1);
    GOIND(x);

#if WANT_TICK
  case T_TICK:
    xi = GETVALUE(n);
    CHKARG1;
    dotick(xi);
    GOIND(x);
#endif

  default:
    ERR1("eval tag %d", GETTAG(n));
  }


 ret:
  if (stack_ptr != stk) {
    // In this case, n was an AP that got pushed and potentially
    // updated.
    uvalue_t xu, yu, ru;
#if WANT_FLOAT
    flt_t xd, yd, rd;
#endif  /* WANT_FLOAT */
    NODEPTR p;

    tag = GETTAG(TOP(0));
    switch (tag) {
    case T_BININT2:
      n = ARG(TOP(1));
      TOP(0) = combBININT1;
      goto top;

    case T_BININT1:
      /* First argument */
#if SANITY
      if (GETTAG(n) != T_INT)
        ERR("BININT 0");
#endif  /* SANITY */
    binint1:
      xu = (uvalue_t)GETVALUE(n);
      /* Second argument */
      y = ARG(TOP(2));
      while (GETTAG(y) == T_IND)
        y = INDIR(y);
#if SANITY
      if (GETTAG(y) != T_INT)
        ERR("BININT 1");
#endif  /* SANITY */
      yu = (uvalue_t)GETVALUE(y);
      p = FUN(TOP(1));
      POP(3);
      n = TOP(-1);
    binint:
      switch (GETTAG(p)) {
      case T_IND:   p = INDIR(p); goto binint;
      case T_ADD:   ru = xu + yu; break;
      case T_SUB:   ru = xu - yu; break;
      case T_MUL:   ru = xu * yu; break;
      case T_QUOT:  ru = (uvalue_t)((value_t)xu / (value_t)yu); break;
      case T_REM:   ru = (uvalue_t)((value_t)xu % (value_t)yu); break;
      case T_SUBR:  ru = yu - xu; break;
      case T_UQUOT: ru = xu / yu; break;
      case T_UREM:  ru = xu % yu; break;
      case T_AND:   ru = xu & yu; break;
      case T_OR:    ru = xu | yu; break;
      case T_XOR:   ru = xu ^ yu; break;
      case T_SHL:   ru = xu << yu; break;
      case T_SHR:   ru = xu >> yu; break;
      case T_ASHR:  ru = (uvalue_t)((value_t)xu >> yu); break;

      case T_EQ:    GOIND(xu == yu ? combTrue : combFalse);
      case T_NE:    GOIND(xu != yu ? combTrue : combFalse);
      case T_ULT:   GOIND(xu <  yu ? combTrue : combFalse);
      case T_ULE:   GOIND(xu <= yu ? combTrue : combFalse);
      case T_UGT:   GOIND(xu >  yu ? combTrue : combFalse);
      case T_UGE:   GOIND(xu >= yu ? combTrue : combFalse);
      case T_UCMP:  GOIND(xu <  yu ? combLT   : xu > yu ? combGT : combEQ);
      case T_LT:    GOIND((value_t)xu <  (value_t)yu ? combTrue : combFalse);
      case T_LE:    GOIND((value_t)xu <= (value_t)yu ? combTrue : combFalse);
      case T_GT:    GOIND((value_t)xu >  (value_t)yu ? combTrue : combFalse);
      case T_GE:    GOIND((value_t)xu >= (value_t)yu ? combTrue : combFalse);
      case T_ICMP:  GOIND((value_t)xu <  (value_t)yu ? combLT   : (value_t)xu > (value_t)yu ? combGT : combEQ);

      default:
        //fprintf(stderr, "tag=%d\n", GETTAG(FUN(TOP(0))));
        ERR("BININT");
      }
      SETINT(n, (value_t)ru);
      goto ret;

    case T_UNINT1:
      /* The argument */
#if SANITY
      if (GETTAG(n) != T_INT)
        ERR("UNINT 0");
#endif
      xu = (uvalue_t)GETVALUE(n);
      p = FUN(TOP(1));
      POP(2);
      n = TOP(-1);
    unint:
      switch (GETTAG(p)) {
      case T_IND:      p = INDIR(p); goto unint;
      case T_NEG:      ru = -xu; break;
      case T_INV:      ru = ~xu; break;
      case T_POPCOUNT: ru = POPCOUNT(xu); break;
      case T_CLZ:      ru = CLZ(xu); break;
      case T_CTZ:      ru = CTZ(xu); break;
      default:
        //fprintf(stderr, "tag=%d\n", GETTAG(FUN(TOP(0))));
        ERR("UNINT");
      }
      SETINT(n, (value_t)ru);
      goto ret;

#if WANT_FLOAT
    case T_BINDBL2:
      n = ARG(TOP(1));
      TOP(0) = combBINDBL1;
      goto top;

    case T_BINDBL1:
      /* First argument */
#if SANITY
      if (GETTAG(n) != T_DBL)
        ERR("BINDBL 0");
#endif  /* SANITY */
      xd = GETDBLVALUE(n);
      /* Second argument */
      y = ARG(TOP(2));
      while (GETTAG(y) == T_IND)
        y = INDIR(y);
#if SANITY
      if (GETTAG(y) != T_DBL)
        ERR("BINDBL 1");
#endif  /* SANITY */
      yd = GETDBLVALUE(y);
      p = FUN(TOP(1));
      POP(3);
      n = TOP(-1);
    bindbl:
      switch (GETTAG(p)) {
      case T_IND:   p = INDIR(p); goto bindbl;
      case T_FADD:  rd = xd + yd; break;
      case T_FSUB:  rd = xd - yd; break;
      case T_FMUL:  rd = xd * yd; break;
      case T_FDIV:  rd = xd / yd; break;

      case T_FEQ:   GOIND(xd == yd ? combTrue : combFalse);
      case T_FNE:   GOIND(xd != yd ? combTrue : combFalse);
      case T_FLT:   GOIND(xd <  yd ? combTrue : combFalse);
      case T_FLE:   GOIND(xd <= yd ? combTrue : combFalse);
      case T_FGT:   GOIND(xd >  yd ? combTrue : combFalse);
      case T_FGE:   GOIND(xd >= yd ? combTrue : combFalse);

      default:
        //fprintf(stderr, "tag=%d\n", GETTAG(FUN(TOP(0))));
        ERR("BINDBL");
      }
      SETDBL(n, rd);
      goto ret;

    case T_UNDBL1:
      /* The argument */
#if SANITY
      if (GETTAG(n) != T_DBL)
        ERR("UNDBL 0");
#endif
      xd = GETDBLVALUE(n);
      p = FUN(TOP(1));
      POP(2);
      n = TOP(-1);
    undbl:
      switch (GETTAG(p)) {
      case T_IND:   p = INDIR(p); goto undbl;
      case T_FNEG:  rd = -xd; break;
      default:
        //fprintf(stderr, "tag=%d\n", GETTAG(FUN(TOP(0))));
        ERR("UNDBL");
      }
      SETDBL(n, rd);
      goto ret;
#endif  /* WANT_FLOAT */

    case T_BINBS2:
      n = ARG(TOP(1));
      TOP(0) = combBINBS1;
      goto top;

    case T_BINBS1:
      /* First argument */
#if SANITY
      if (GETTAG(n) != T_FORPTR || FORPTR(n)->finalizer->fptype != FP_BSTR)
        ERR("BINBS 0");
#endif  /* SANITY */
      xbs = BSTR(n);
      /* Second argument */
      y = ARG(TOP(2));
      while (GETTAG(y) == T_IND)
        y = INDIR(y);
#if SANITY
      if (GETTAG(y) != T_FORPTR || FORPTR(y)->finalizer->fptype != FP_BSTR)
        ERR("BINBS 1");
#endif  /* SANITY */
      ybs = BSTR(y);
      p = FUN(TOP(1));
      POP(3);
      n = TOP(-1);
    binbs:
      switch (GETTAG(p)) {
      case T_IND:    p = INDIR(p); goto binbs;

      case T_BSAPPEND: rbs = bsappend(xbs, ybs); break;
      case T_BSAPPENDDOT: rbs = bsappenddot(xbs, ybs); break;
      case T_BSEQ:   GOIND(bscompare(xbs, ybs) == 0 ? combTrue : combFalse);
      case T_BSNE:   GOIND(bscompare(xbs, ybs) != 0 ? combTrue : combFalse);
      case T_BSLT:   GOIND(bscompare(xbs, ybs) <  0 ? combTrue : combFalse);
      case T_BSLE:   GOIND(bscompare(xbs, ybs) <= 0 ? combTrue : combFalse);
      case T_BSGT:   GOIND(bscompare(xbs, ybs) >  0 ? combTrue : combFalse);
      case T_BSGE:   GOIND(bscompare(xbs, ybs) >= 0 ? combTrue : combFalse);
      case T_BSCMP:  r = bscompare(xbs, ybs); GOIND(r < 0 ? combLT : r > 0 ? combGT : combEQ);

      default:
        //fprintf(stderr, "tag=%d\n", GETTAG(FUN(TOP(0))));
        ERR("BINBS");
      }
      SETBSTR(n, mkForPtrFree(rbs));
      goto ret;

    default:
      stack_ptr = stk;
      n = TOP(-1);
    }
  }
#if MAXSTACKDEPTH
  cur_c_stack = old_cur_c_stack; /* reset rather than counting down, in case of longjump */
#endif
  return n;
}

/* This is the interpreter for the IO monad operations.
 *
 * Assuming every graph rewrite is atomic we want the graph
 * to always represent the rest of the program to run.
 * To this end, we need to mutate the graph every time
 * an IO operation has been performed to make sure we don't
 * execute it again.
 * To have a cell that is safe to mutate, we allocate a new
 * application on entry to execio().
 * Given the call execio(np) we allocate this graph, top = BIND (*np) RETURN.
 * I.e.
 *          @
 *         / \
 *        @  RETURN
 *       / \
 *    BIND (*np) 
 * and make np point to it.
 * This graph will be updated continuously as we execute IO actions.
 *
 * Invariant: the second argument to this BIND is always either RETURN
 * or a C'BIND.  The second argument to C'BIND has the same invariant.
 * C'BIND has the reduction rule (which is normally never used): 
 *   C'BIND x y z = BIND (x z) y
 *
 * This is the cycle of the execio loop:
 *  again:
 *   given top = BIND n q
 *   eval(n)
 *   case n
 *     BIND r s:  rewrite to top := BIND r (C'BIND s q)  -- (r >>= s) >>= q  -->  r >>= (\ x -> s x >>= q)
 *     THEN r s:  ... K s ...
 *     otherwise: res = execute n
 *       case q
 *         RETURN:     rewrite to  top := RETURN res;  return to caller
 *         C'BIND r s: rewrite to  top := BIND (r res) s; goto again
 */
void
execio(NODEPTR *np)
{
  stackptr_t stk = stack_ptr;
  NODEPTR f, x, n, q, r, s, res, top1;
  char *cstr;
  struct handler *h;
#if WANT_STDIO
  void *ptr;
  int hdr;
#endif  /* WANT_STDIO */
  NODEPTR top;

/* IO operations need all arguments, anything else should not happen. */
#define CHECKIO(n) do { if (stack_ptr - stk != (n)+1) {/*printf("\nLINE=%d\n", __LINE__);*/ ERR("CHECKIO");}; } while(0)
/* #define RETIO(p) do { stack_ptr = stk; return (p); } while(0)*/
#define GCCHECKSAVE(p, n) do { PUSH(p); GCCHECK(n); (p) = TOP(0); POP(1); } while(0)
#define RETIO(p) do { stack_ptr = stk; res = (p); goto rest; } while(0)
#define IOASSERT(p,s) do { if (!(p)) ERR("IOASSERT " s); } while(0)

  GCCHECK(2);
  top = new_ap(new_ap(combIOBIND, *np), combIORETURN);
  *np = top;

 start:
  //dump("start", top);
  IOASSERT(stack_ptr == stk, "start");
  //ppmsg("n before = ", ARG(FUN(top)));
  n = evali(ARG(FUN(top)));     /* eval(n) */
  //ppmsg("n after  = ", n);
  if (GETTAG(n) == T_AP && GETTAG(top1 = indir(&FUN(n))) == T_AP) {
    switch (GETTAG(indir(&FUN(top1)))) {
    case T_IO_BIND:
      GCCHECKSAVE(n, 2);
      s = ARG(n);
    bind:
      q = ARG(top);
      r = ARG(top1);
      ARG(FUN(top)) = r;
      ARG(top) = x = new_ap(new_ap(combIOCCBIND, s), q);
      goto start;
    case T_IO_THEN:
      GCCHECKSAVE(n, 3);
      s = new_ap(combFalse, ARG(n));
      goto bind;
    default:
      break;
    }
  }
  goto execute;

 rest:                          /* result is in res */
  //ppmsg("res=", res);
  q = ARG(top);
  //ppmsg("q=", q);
  if (GETTAG(q) == T_IO_RETURN) {
    /* execio is done */
    FUN(top) = combIORETURN;
    ARG(top) = res;
    IOASSERT(stack_ptr == stk, "stk");
    return;
  }
  /* not done, it must be a C'BIND */
  GCCHECKSAVE(res, 1);
  IOASSERT(GETTAG(q) == T_AP && GETTAG(FUN(q)) == T_AP && GETTAG(FUN(FUN(q))) == T_IO_CCBIND, "rest-AP");
  r = ARG(FUN(q));
  s = ARG(q);
  ARG(FUN(top)) = new_ap(r, res);
  ARG(top) = s;
  goto start;

 execute:
  PUSH(n);
  for(;;) {
    COUNT(num_reductions);
    //printf("execute switch %s\n", tag_names[GETTAG(n)]);
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
      ERR("T_IO_BIND");
    case T_IO_THEN:
      ERR("T_IO_THEN");
    case T_IO_CCBIND:
      ERR("T_IO_CCBIND");
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
      ptr = (struct BFILE*)evalptr(ARG(TOP(1)));
      x = evali(ARG(TOP(2)));
      //x = ARG(TOP(1));
      printb(ptr, x, hdr);
      putb('\n', ptr);
      RETIO(combUnit);
    case T_IO_DESERIALIZE:
      CHECKIO(1);
      ptr = (struct BFILE*)evalptr(ARG(TOP(1)));
      gc();                     /* make sure we have room.  GC during parse is dodgy. */
      n = parse_top(ptr);
      RETIO(n);
#endif
#if WANT_ARGS
    case T_IO_GETARGREF:
      CHECKIO(0);
      n = alloc_node(T_ARR);
      ARR(n) = argarray;
      RETIO(n);
#endif

    case T_IO_CCALL:
      {
        int a = (int)GETVALUE(n);
        funptr_t f = FFI_IX(a).ffi_fun;
        GCCHECK(1);             /* room for placeholder */
        PUSH(mkFlt(0.0));       /* placeholder for result, protected from GC */
        f(stk);                 /* call FFI function */
        n = TOP(0);             /* pop actual result */
        RETIO(n);               /* and this is the result */
      }

    case T_CATCH:
      {
        h = MALLOC(sizeof *h);
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
          ARG(FUN(top)) = n;
          goto start;
        } else {
          /* Normal execution: */
          n = ARG(TOP(1));          /* first argument, should be evaluated (but not overwritten) */
          PUSH(n);
          execio(&TOP(0));          /* execute first argument */
          cur_handler = h->hdl_old; /* restore old handler */
          FREE(h);
          n = TOP(0);
          POP(1);
          IOASSERT(GETTAG(n) == T_AP && GETTAG(FUN(n)) == T_IO_RETURN, "CATCH");
          RETIO(ARG(n));             /* return result */
        }
      }

    case T_NEWCASTRINGLEN:
      {
      CHECKIO(1);
      n = ARG(TOP(1));
      /* Zap the pointer to the list so it can be GC:ed.
       * The actual list is protected from GC by evalbytestring().
       */
      // ARG(TOP(1)) = combK;
      struct bytestring bs = evalbytestring(n);
      GCCHECK(4);
      n = new_ap(new_ap(combPair, x = alloc_node(T_PTR)), mkInt(bs.size));
      PTR(x) = bs.string;
      RETIO(n);
      }

    case T_PACKCSTRING:
      {
      size_t size;
      CHECKIO(1);
      cstr = evalptr(ARG(TOP(1)));
      size = strlen(cstr);
      char *str = MALLOC(size);
      memcpy(str, cstr, size);
      struct bytestring bs = { size, str };
      RETIO(mkStrNode(bs));
      }

    case T_PACKCSTRINGLEN:
      {
      size_t size;
      CHECKIO(2);
      cstr = evalptr(ARG(TOP(1)));
      size = evalint(ARG(TOP(2)));
      char *str = MALLOC(size);
      memcpy(str, cstr, size);
      struct bytestring bs = { size, str };
      RETIO(mkStrNode(bs));
      }

    case T_ARR_ALLOC:
      {
      size_t size;
      NODEPTR elem;
      struct ioarray *arr;
      CHECKIO(2);
      GCCHECK(1);
      size = evalint(ARG(TOP(1)));
      elem = ARG(TOP(2));
      arr = arr_alloc(size, elem);
      n = alloc_node(T_ARR);
      ARR(n) = arr;
      RETIO(n);
      }
    case T_ARR_COPY:
      {
      struct ioarray *arr;
      CHECKIO(1);
      GCCHECK(1);
      n = evali(ARG(TOP(1)));
      if (GETTAG(n) != T_ARR)
        ERR("T_ARR_COPY tag");
      arr = arr_copy(ARR(n));
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
      size_t i;
      CHECKIO(2);
      i = evalint(ARG(TOP(2)));
      n = evali(ARG(TOP(1)));
      if (GETTAG(n) != T_ARR)
        ERR("bad ARR tag");
      if (i >= ARR(n)->size)
        ERR("ARR_READ");
      RETIO(ARR(n)->array[i]);
      }
    case T_ARR_WRITE:
      {
      size_t i;
      CHECKIO(3);
      i = evalint(ARG(TOP(2)));
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

    case T_FPNEW:
      {
        CHECKIO(1);
        //printf("T_FPNEW\n");
        void *xp = evalptr(ARG(TOP(1)));
        //printf("T_FPNEW xp=%p\n", xp);
        n = alloc_node(T_FORPTR);
        SETFORPTR(n, mkForPtrP(xp));
        RETIO(n);
      }
    case T_FPFIN:
      {
        CHECKIO(2);
        //printf("T_FPFIN\n");
        struct forptr *xfp = evalforptr(ARG(TOP(2)));
        //printf("T_FPFIN xfp=%p\n", xfp);
        HsFunPtr yp = evalfunptr(ARG(TOP(1)));
        //printf("T_FPFIN yp=%p\n", yp);
        xfp->finalizer->final = yp;
        RETIO(combUnit);
      }

#if 0
    case T_FPSTR:
      {
        CHECKIO(2);
        //printf("T_FPFIN\n");
        struct forptr *xfp = evalforptr(ARG(TOP(2)));
        //printf("T_FPFIN xfp=%p\n", xfp);
        struct bytestring bs = evalstring(ARG(TOP(1)));
        //printf("T_FPFIN yp=%p\n", yp);
        xfp->desc = bs.string;
        RETIO(combUnit);
      }
#endif

    case T_IO_GC:
      CHECKIO(0);
      //printf("gc()\n");
      gc();
      RETIO(combUnit);

    default:
      //printf("bad tag %s\n", tag_names[GETTAG(n)]);
      ERR1("execio tag %d", GETTAG(n));
    }
  }
}

#if WANT_ARGS
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
#endif

extern uint8_t *combexpr;
extern int combexprlen;

MAIN
{
  NODEPTR prog;
#if WANT_ARGS
  char *inname = 0;
  char **av;
  char *progname;
  char **gargv;
  int gargc;
  int inrts;
#if WANT_TICK
  int dump_ticks = 0;
#endif
#endif
#if WANT_STDIO
  char *outname = 0;
  size_t file_size = 0;
#endif

#if 0
  /* MINGW doesn't do buffering right */
  setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
  setvbuf(stderr, NULL, _IONBF, BUFSIZ);
#endif

#ifdef INITIALIZATION
  main_setup(); /* Do platform specific start-up. */
#endif

#if WANT_ARGS
  progname = argv[0];
  argc--, argv++;
  gargv = argv;
  for (av = argv, inrts = 0; argc--; argv++) {
    char *p = *argv;
    if (inrts) {
      if (strcmp(p, "-RTS") == 0) {
        inrts = 0;
      } else {
        if (strcmp(p, "-v") == 0)
          verbose++;
#if WANT_TICK
        else if (strcmp(p, "-T") == 0)
          dump_ticks = 1;
#endif
        else if (strncmp(p, "-H", 2) == 0)
          heap_size = memsize(&p[2]);
        else if (strncmp(p, "-K", 2) == 0)
          stack_size = memsize(&p[2]);
        else if (strncmp(p, "-r", 2) == 0)
          inname = &p[2];
#if WANT_STDIO
        else if (strncmp(p, "-o", 2) == 0)
          outname = &p[2];
        else if (strcmp(p, "-B") == 0)
          gcbell++;
#endif  /* WANT_STDIO */
        else
          ERR("Usage: eval [+RTS [-v] [-B] [-T] [-Hheap-size] [-Kstack-size] [-rFILE] [-oFILE] -RTS] arg ...");
      }
    } else {
      if (strcmp(p, "+RTS") == 0) {
        inrts = 1;
      } else {
        *av++ = p;
      }
    }
  }
  gargc = av - gargv;

  if (inname == 0)
    inname = "out.comb";
#endif

  init_nodes();
  stack = MALLOC(sizeof(NODEPTR) * stack_size);
  if (!stack)
    memerr();

#if WANT_ARGS
  /* Initialize an IORef (i.e., single element IOArray
   * to contain the list of program arguments.
   * The 0th element is the program name, and the rest
   * are the non RTS arguments.
   */
  {
    NODEPTR n;
    /* No GC checks, the heap is empty. */
    n = mkNil();
    for(int i = gargc-1; i >= 0; i--) {
      n = mkCons(mkStringC(gargv[i]), n);
    }
    n = mkCons(mkStringC(progname), n);
    argarray = arr_alloc(1, n);      /* An IORef contains a single element array */
    argarray->permanent = 1;         /* never GC the arguments, because a T_IO_GETARGREF can reach argarray */
  }
#endif  /* WANT_ARGS */

  if (combexpr) {
    int c;
    BFILE *bf = openb_rd_buf(combexpr, combexprlen);
    c = getb(bf);
    /* Compressed combinators start with a 'Z' or 'z', otherwise 'v' (for version) */
    if (c == 'z') {
      /* add LZ77 compressor transducer */
      bf = add_lz77_decompressor(bf);
    } else {
      /* put it back, we need it */
      ungetb(c, bf);
    }
    prog = parse_top(bf);
    closeb(bf);
#if WANT_STDIO
    file_size = combexprlen;
#endif
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
    struct BFILE *bf = add_FILE(out);
    printb(bf, prog, 1);
    closeb(bf);
    EXIT(0);
  }
  if (verbose > 2) {
    pp(stdout, prog);
  }
#endif
  run_time -= GETTIMEMILLI();
  PUSH(prog);
  topnode = &TOP(0);
  execio(&TOP(0));
  prog = TOP(0);
  POP(1);
#if SANITY
  if (GETTAG(prog) != T_AP || GETTAG(FUN(prog)) != T_IO_RETURN)
    ERR("main execio");
  NODEPTR res = evali(ARG(prog));
  if (GETTAG(res) != T_I)
    ERR("main execio I");
#endif
  /* Flush standard handles in case there is some BFILE buffering */
  flushb((BFILE*)FORPTR(comb_stdout)->payload.string);
  flushb((BFILE*)FORPTR(comb_stderr)->payload.string);
  gc();                      /* Run finalizers */
  run_time += GETTIMEMILLI();

#if WANT_STDIO
  if (verbose) {
    if (verbose > 1) {
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
    PRINT("%"PCOMMA"15"PRIcounter" foreign alloc\n", num_fin_alloc);
    PRINT("%"PCOMMA"15"PRIcounter" foreign free\n", num_fin_free);
    PRINT("%"PCOMMA"15"PRIcounter" bytestring alloc (max %"PCOMMA""PRIcounter")\n", num_bs_alloc, num_bs_alloc_max);
    PRINT("%"PCOMMA"15"PRIcounter" bytestring alloc bytes (max %"PCOMMA""PRIcounter")\n", num_bs_bytes, num_bs_inuse_max);
    PRINT("%"PCOMMA"15"PRIcounter" bytestring free\n", num_bs_free);
#if MAXSTACKDEPTH
    PRINT("%"PCOMMA"15d max stack depth\n", (int)max_stack_depth);
    PRINT("%"PCOMMA"15d max C stack depth\n", (int)max_c_stack);
#endif
    // PRINT("%"PCOMMA"15"PRIcounter" max mark depth\n", max_mark_depth);
    PRINT("%15.2fs total expired time\n", (double)run_time / 1000);
    PRINT("%15.2fs total gc time (%.2f + %.2f)\n",
          (double)(gc_mark_time + gc_scan_time) / 1000,
          (double)gc_mark_time / 1000,
          (double)gc_scan_time / 1000);
#if GCRED
    PRINT(" GC reductions A=%d, K=%d, I=%d, int=%d flip=%d\n", red_a, red_k, red_i, red_int, red_flip);
    PRINT(" special reductions B'=%d K4=%d K3=%d K2=%d C'B=%d, Z=%d, R=%d\n", red_bb, red_k4, red_k3, red_k2, red_ccb, red_z, red_r);
#endif
  }
#endif  /* WANT_STDIO */

#if WANT_TICK
  if (dump_ticks) {
    dump_tick_table(stdout);
  }
#endif

#ifdef TEARDOWN
  main_teardown(); /* do some platform specific teardown */
#endif
  EXIT(0);
}

#if WANT_MD5
#include "md5.c"
#endif  /* WANT_MD5 */

#if WANT_LZ77
#include "lz77.c"
#endif

/*********************/
/* FFI adapters      */

#define MHS_FROM(name, set, type) \
void \
name(stackptr_t stk, int n, type x) \
{ \
  NODEPTR r = TOP(0);           /* The pre-allocated cell for the result, */ \
  CHECKIO(n+1);                 /* Check that we actually had the right number of arguments. */ \
  set(r, x);                    /* Put result in pre-allocated cell. */ \
}
MHS_FROM(mhs_from_FloatW, SETDBL, flt_t);
MHS_FROM(mhs_from_Int, SETINT, value_t);
MHS_FROM(mhs_from_Word, SETINT, uvalue_t);
MHS_FROM(mhs_from_Word8, SETINT, uvalue_t);
MHS_FROM(mhs_from_Ptr, SETPTR, void*);
MHS_FROM(mhs_from_ForPtr, SETFORPTR, struct forptr *);
MHS_FROM(mhs_from_FunPtr, SETFUNPTR, HsFunPtr);
MHS_FROM(mhs_from_CChar, SETINT, char);
MHS_FROM(mhs_from_CSChar, SETINT, signed char);
MHS_FROM(mhs_from_CUChar, SETINT, unsigned char);
MHS_FROM(mhs_from_CShort, SETINT, short);
MHS_FROM(mhs_from_CUShort, SETINT, unsigned short);
MHS_FROM(mhs_from_CInt, SETINT, int);
MHS_FROM(mhs_from_CUInt, SETINT, unsigned int);
MHS_FROM(mhs_from_CLong, SETINT, long);
MHS_FROM(mhs_from_CULong, SETINT, unsigned long);
MHS_FROM(mhs_from_CLLong, SETINT, long long);
MHS_FROM(mhs_from_CULLong, SETINT, unsigned long long);
MHS_FROM(mhs_from_CSize, SETINT, size_t);
#if WANT_TIME
MHS_FROM(mhs_from_CTime, SETINT, time_t);
#endif
// MHS_FROM(mhs_from_CSSize, SETINT, ssize_t);
MHS_FROM(mhs_from_CIntPtr, SETINT, intptr_t);
MHS_FROM(mhs_from_CUIntPtr, SETINT, uintptr_t);
void
mhs_from_Unit(stackptr_t stk, int n)
{
  CHECKIO(n+1);                 /* Check that we actually had the right number of arguments. */
  TOP(0) = combUnit;            /* Put result on top of stack */
}

#define MHS_TO(name, eval, type) \
type name(stackptr_t stk, int n) \
{ \
  return eval(ARG(TOP(n+2)));                /* The stack has a reserved cell, and the FFI node on top of the arguments */ \
}
MHS_TO(mhs_to_FloatW, evaldbl, flt_t);
MHS_TO(mhs_to_Int, evalint, value_t);
MHS_TO(mhs_to_Word, evalint, uvalue_t);
MHS_TO(mhs_to_Word8, evalint, uint8_t);
MHS_TO(mhs_to_Ptr, evalptr, void*);
MHS_TO(mhs_to_FunPtr, evalfunptr, HsFunPtr);
MHS_TO(mhs_to_CChar, evalint, char);
MHS_TO(mhs_to_CSChar, evalint, signed char);
MHS_TO(mhs_to_CUChar, evalint, unsigned char);
MHS_TO(mhs_to_CShort, evalint, short);
MHS_TO(mhs_to_CUShort, evalint, unsigned short);
MHS_TO(mhs_to_CInt, evalint, int);
MHS_TO(mhs_to_CUInt, evalint, unsigned int);
MHS_TO(mhs_to_CLong, evalint, long);
MHS_TO(mhs_to_CULong, evalint, unsigned long);
MHS_TO(mhs_to_CLLong, evalint, long long);
MHS_TO(mhs_to_CULLong, evalint, unsigned long long);
MHS_TO(mhs_to_CSize, evalint, size_t);
#if WANT_TIME
MHS_TO(mhs_to_CTime, evalint, time_t);
#endif
// MHS_TO(mhs_to_CSSize, evalint, ssize_t);
MHS_TO(mhs_to_CIntPtr, evalint, intptr_t);
MHS_TO(mhs_to_CUIntPtr, evalint, uintptr_t);


/* The rest of this file was generated by the compiler, with some minor edits with #if. */
void mhs_GETRAW(int s) { mhs_from_Int(s, 0, GETRAW()); }
void mhs_GETTIMEMILLI(int s) { mhs_from_Int(s, 0, GETTIMEMILLI()); }
#if WANT_MATH
#if WORD_SIZE == 64
void mhs_acos(int s) { mhs_from_FloatW(s, 1, acos(mhs_to_FloatW(s, 0))); }
void mhs_asin(int s) { mhs_from_FloatW(s, 1, asin(mhs_to_FloatW(s, 0))); }
void mhs_atan(int s) { mhs_from_FloatW(s, 1, atan(mhs_to_FloatW(s, 0))); }
void mhs_atan2(int s) { mhs_from_FloatW(s, 2, atan2(mhs_to_FloatW(s, 0), mhs_to_FloatW(s, 1))); }
void mhs_cos(int s) { mhs_from_FloatW(s, 1, cos(mhs_to_FloatW(s, 0))); }
void mhs_exp(int s) { mhs_from_FloatW(s, 1, exp(mhs_to_FloatW(s, 0))); }
void mhs_log(int s) { mhs_from_FloatW(s, 1, log(mhs_to_FloatW(s, 0))); }
void mhs_sin(int s) { mhs_from_FloatW(s, 1, sin(mhs_to_FloatW(s, 0))); }
void mhs_sqrt(int s) { mhs_from_FloatW(s, 1, sqrt(mhs_to_FloatW(s, 0))); }
void mhs_tan(int s) { mhs_from_FloatW(s, 1, tan(mhs_to_FloatW(s, 0))); }
#elif WORD_SIZE == 32  /* WORD_SIZE */
void mhs_acos(int s) { mhs_from_FloatW(s, 1, acosf(mhs_to_FloatW(s, 0))); }
void mhs_asin(int s) { mhs_from_FloatW(s, 1, asinf(mhs_to_FloatW(s, 0))); }
void mhs_atan(int s) { mhs_from_FloatW(s, 1, atanf(mhs_to_FloatW(s, 0))); }
void mhs_atan2(int s) { mhs_from_FloatW(s, 2, atan2f(mhs_to_FloatW(s, 0), mhs_to_FloatW(s, 1))); }
void mhs_cos(int s) { mhs_from_FloatW(s, 1, cosf(mhs_to_FloatW(s, 0))); }
void mhs_exp(int s) { mhs_from_FloatW(s, 1, expf(mhs_to_FloatW(s, 0))); }
void mhs_log(int s) { mhs_from_FloatW(s, 1, logf(mhs_to_FloatW(s, 0))); }
void mhs_sin(int s) { mhs_from_FloatW(s, 1, sinf(mhs_to_FloatW(s, 0))); }
void mhs_sqrt(int s) { mhs_from_FloatW(s, 1, sqrtf(mhs_to_FloatW(s, 0))); }
void mhs_tan(int s) { mhs_from_FloatW(s, 1, tanf(mhs_to_FloatW(s, 0))); }
#else
#error Unknown WORD_SIZE
#endif  /* WORD_SIZE */
#endif  /* WANT_MATH */

#if WANT_STDIO
void mhs_add_FILE(int s) { mhs_from_Ptr(s, 1, add_FILE(mhs_to_Ptr(s, 0))); }
void mhs_add_utf8(int s) { mhs_from_Ptr(s, 1, add_utf8(mhs_to_Ptr(s, 0))); }
void mhs_closeb(int s) { closeb(mhs_to_Ptr(s, 0)); mhs_from_Unit(s, 1); }
void mhs_addr_closeb(int s) { mhs_from_FunPtr(s, 0, (HsFunPtr)&closeb); }
void mhs_flushb(int s) { flushb(mhs_to_Ptr(s, 0)); mhs_from_Unit(s, 1); }
void mhs_fopen(int s) { mhs_from_Ptr(s, 2, fopen(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }

void mhs_getb(int s) { mhs_from_Int(s, 1, getb(mhs_to_Ptr(s, 0))); }
void mhs_putb(int s) { putb(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
void mhs_ungetb(int s) { ungetb(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
void mhs_openwrbuf(int s) { mhs_from_Ptr(s, 0, openb_wr_buf()); }
void mhs_openrdbuf(int s) { mhs_from_Ptr(s, 2, openb_rd_buf(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
void mhs_getbuf(int s) { get_buf(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2));  mhs_from_Unit(s, 3); }
void mhs_system(int s) { mhs_from_Int(s, 1, system(mhs_to_Ptr(s, 0))); }
void mhs_tmpname(int s) { mhs_from_Ptr(s, 2, TMPNAME(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }
void mhs_unlink(int s) { mhs_from_Int(s, 1, unlink(mhs_to_Ptr(s, 0))); }
void mhs_readb(int s) { mhs_from_Int(s, 3, readb(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1), mhs_to_Ptr(s, 2))); }
void mhs_writeb(int s) { mhs_from_Int(s, 3, writeb(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1), mhs_to_Ptr(s, 2))); }
#endif  /* WANT_STDIO */

#if WANT_MD5
void mhs_md5Array(int s) { md5Array(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2)); mhs_from_Unit(s, 3); }
void mhs_md5BFILE(int s) { md5BFILE(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
void mhs_md5String(int s) { md5String(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
#endif  /* WANT_MD5 */

#if WANT_LZ77
void mhs_add_lz77_compressor(int s) { mhs_from_Ptr(s, 1, add_lz77_compressor(mhs_to_Ptr(s, 0))); }
void mhs_add_lz77_decompressor(int s) { mhs_from_Ptr(s, 1, add_lz77_decompressor(mhs_to_Ptr(s, 0))); }
void mhs_lz77c(int s) { mhs_from_CSize(s, 3, lz77c(mhs_to_Ptr(s, 0), mhs_to_CSize(s, 1), mhs_to_Ptr(s, 2))); }
#endif  /* WANT_LZ77 */

#if WANT_RLE
void mhs_add_rle_compressor(int s) { mhs_from_Ptr(s, 1, add_rle_compressor(mhs_to_Ptr(s, 0))); }
void mhs_add_rle_decompressor(int s) { mhs_from_Ptr(s, 1, add_rle_decompressor(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_RLE */

#if WANT_BWT
void mhs_add_bwt_compressor(int s) { mhs_from_Ptr(s, 1, add_bwt_compressor(mhs_to_Ptr(s, 0))); }
void mhs_add_bwt_decompressor(int s) { mhs_from_Ptr(s, 1, add_bwt_decompressor(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_BWT */

void mhs_calloc(int s) { mhs_from_Ptr(s, 2, calloc(mhs_to_CSize(s, 0), mhs_to_CSize(s, 1))); }
void mhs_free(int s) { free(mhs_to_Ptr(s, 0)); mhs_from_Unit(s, 1); }
void mhs_addr_free(int s) { mhs_from_FunPtr(s, 0, (HsFunPtr)&FREE); }
void mhs_getenv(int s) { mhs_from_Ptr(s, 1, getenv(mhs_to_Ptr(s, 0))); }
void mhs_iswindows(int s) { mhs_from_Int(s, 0, iswindows()); }
void mhs_malloc(int s) { mhs_from_Ptr(s, 1, MALLOC(mhs_to_CSize(s, 0))); }
void mhs_memcpy(int s) { memcpy(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_CSize(s, 2)); mhs_from_Unit(s, 3); }
void mhs_memmove(int s) { memmove(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_CSize(s, 2)); mhs_from_Unit(s, 3); }
void mhs_peekPtr(int s) { mhs_from_Ptr(s, 1, peekPtr(mhs_to_Ptr(s, 0))); }
void mhs_peekWord(int s) { mhs_from_Word(s, 1, peekWord(mhs_to_Ptr(s, 0))); }
void mhs_pokePtr(int s) { pokePtr(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
void mhs_pokeWord(int s) { pokeWord(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); mhs_from_Unit(s, 2); }

void mhs_peek_uint8(int s) { mhs_from_Word(s, 1, peek_uint8(mhs_to_Ptr(s, 0))); }
void mhs_poke_uint8(int s) { poke_uint8(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); mhs_from_Unit(s, 2); }
void mhs_peek_uint16(int s) { mhs_from_Word(s, 1, peek_uint16(mhs_to_Ptr(s, 0))); }
void mhs_poke_uint16(int s) { poke_uint16(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); mhs_from_Unit(s, 2); }
#if WORD_SIZE >= 32
void mhs_peek_uint32(int s) { mhs_from_Word(s, 1, peek_uint32(mhs_to_Ptr(s, 0))); }
void mhs_poke_uint32(int s) { poke_uint32(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); mhs_from_Unit(s, 2); }
#endif  /* WORD_SIZE */
#if WORD_SIZE >= 64
void mhs_peek_uint64(int s) { mhs_from_Word(s, 1, peek_uint64(mhs_to_Ptr(s, 0))); }
void mhs_poke_uint64(int s) { poke_uint64(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); mhs_from_Unit(s, 2); }
#endif  /* WORD_SIZE */
void mhs_peek_uint(int s) { mhs_from_Word(s, 1, peek_uint(mhs_to_Ptr(s, 0))); }
void mhs_poke_uint(int s) { poke_uint(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); mhs_from_Unit(s, 2); }

void mhs_peek_int8(int s) { mhs_from_Int(s, 1, peek_int8(mhs_to_Ptr(s, 0))); }
void mhs_poke_int8(int s) { poke_int8(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); mhs_from_Unit(s, 2); }
void mhs_peek_int16(int s) { mhs_from_Int(s, 1, peek_int16(mhs_to_Ptr(s, 0))); }
void mhs_poke_int16(int s) { poke_int16(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); mhs_from_Unit(s, 2); }
#if WORD_SIZE >= 32
void mhs_peek_int32(int s) { mhs_from_Int(s, 1, peek_int32(mhs_to_Ptr(s, 0))); }
void mhs_poke_int32(int s) { poke_int32(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); mhs_from_Unit(s, 2); }
#endif  /* WORD_SIZE */
#if WORD_SIZE >= 64
void mhs_peek_int64(int s) { mhs_from_Int(s, 1, peek_int64(mhs_to_Ptr(s, 0))); }
void mhs_poke_int64(int s) { poke_int64(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); mhs_from_Unit(s, 2); }
#endif  /* WORD_SIZE */
void mhs_peek_int(int s) { mhs_from_Int(s, 1, peek_int(mhs_to_Ptr(s, 0))); }
void mhs_poke_int(int s) { poke_int(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); mhs_from_Unit(s, 2); }
void mhs_peek_llong(int s) { mhs_from_CLLong(s, 1, peek_llong(mhs_to_Ptr(s, 0))); }
void mhs_peek_long(int s) { mhs_from_CLong(s, 1, peek_long(mhs_to_Ptr(s, 0))); }
void mhs_peek_ullong(int s) { mhs_from_CULLong(s, 1, peek_ullong(mhs_to_Ptr(s, 0))); }
void mhs_peek_ulong(int s) { mhs_from_CULong(s, 1, peek_ulong(mhs_to_Ptr(s, 0))); }
void mhs_poke_llong(int s) { poke_llong(mhs_to_Ptr(s, 0), mhs_to_CLLong(s, 1)); mhs_from_Unit(s, 2); }
void mhs_poke_long(int s) { poke_long(mhs_to_Ptr(s, 0), mhs_to_CLong(s, 1)); mhs_from_Unit(s, 2); }
void mhs_poke_ullong(int s) { poke_ullong(mhs_to_Ptr(s, 0), mhs_to_CULLong(s, 1)); mhs_from_Unit(s, 2); }
void mhs_poke_ulong(int s) { poke_ulong(mhs_to_Ptr(s, 0), mhs_to_CULong(s, 1)); mhs_from_Unit(s, 2); }
#if WANT_FLOAT
void mhs_peek_flt(int s) { mhs_from_FloatW(s, 1, peek_flt(mhs_to_Ptr(s, 0))); }
void mhs_poke_flt(int s) { poke_flt(mhs_to_Ptr(s, 0), mhs_to_FloatW(s, 1)); mhs_from_Unit(s, 2); }
#endif  /* WANT_FLOAT */
void mhs_sizeof_int(int s) { mhs_from_Int(s, 0, sizeof(int)); }
void mhs_sizeof_llong(int s) { mhs_from_Int(s, 0, sizeof(long long)); }
void mhs_sizeof_long(int s) { mhs_from_Int(s, 0, sizeof(long)); }
#if WANT_DIR
void mhs_closedir(int s) { mhs_from_Int(s, 1, closedir(mhs_to_Ptr(s, 0))); }
void mhs_opendir(int s) { mhs_from_Ptr(s, 1, opendir(mhs_to_Ptr(s, 0))); }
void mhs_readdir(int s) { mhs_from_Ptr(s, 1, readdir(mhs_to_Ptr(s, 0))); }
void mhs_c_d_name(int s) { mhs_from_Ptr(s, 1, ((struct dirent *)(mhs_to_Ptr(s, 0)))->d_name); }
void mhs_chdir(int s) { mhs_from_Int(s, 1, chdir(mhs_to_Ptr(s, 0))); }
void mhs_mkdir(int s) { mhs_from_Int(s, 2, mkdir(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
void mhs_getcwd(int s) { mhs_from_Ptr(s, 2, getcwd(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
#endif  /* WANT_DIR */

/* Use this to detect if we have (and want) GMP or not. */
void mhs_want_gmp(int s) { mhs_from_Int(s, 0, WANT_GMP); }

#if WANT_GMP
void
free_mpz(void *p)
{
  /*  printf("free_mpz %p\n", p);*/
  mpz_clear(p);                 /* free any extra storage */
  FREE(p);                      /* and free the mpz itself */
}

/* Allocate an initialize a GMP integer */
struct forptr *
new_mpz(void)
{
#if 0
  {
    static int done = 0;
    if (!done) {
      printf("GMP\n");
      done = 1;
    }
  }
#endif
  mpz_ptr p = MALLOC(sizeof(*p));
  if (!p)
    memerr();
  mpz_init(p);
  struct forptr *fp = mkForPtrP(p);
  fp->finalizer->final = (HsFunPtr)free_mpz;
  fp->finalizer->fptype = FP_MPZ;
  /*  printf("new_mpz %p %p\n", p, fp); */
  return fp;
}

#if 0
void
print_mpz(mpz_ptr p)
{
  mpz_out_str(stdout, 10, p);
}
#endif

void mhs_new_mpz(int s) { mhs_from_ForPtr(s, 0, new_mpz()); }

/* Stubs for GMP functions */
void mhs_mpz_abs(int s) { mpz_abs(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
void mhs_mpz_add(int s) { mpz_add(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); mhs_from_Unit(s, 3); }
void mhs_mpz_and(int s) { mpz_and(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); mhs_from_Unit(s, 3); }
void mhs_mpz_cmp(int s) { mhs_from_Int(s, 2, mpz_cmp(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }
void mhs_mpz_get_d(int s) { mhs_from_FloatW(s, 1, mpz_get_d(mhs_to_Ptr(s, 0))); }
void mhs_mpz_get_si(int s) { mhs_from_Int(s, 1, mpz_get_si(mhs_to_Ptr(s, 0))); }
void mhs_mpz_get_ui(int s) { mhs_from_Word(s, 1, mpz_get_ui(mhs_to_Ptr(s, 0))); }
void mhs_mpz_init_set_si(int s) { mpz_init_set_si(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); mhs_from_Unit(s, 2); }
void mhs_mpz_init_set_ui(int s) { mpz_init_set_ui(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); mhs_from_Unit(s, 2); }
void mhs_mpz_ior(int s) { mpz_ior(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); mhs_from_Unit(s, 3); }
void mhs_mpz_mul(int s) { mpz_mul(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); mhs_from_Unit(s, 3); }
void mhs_mpz_mul_2exp(int s) { mpz_mul_2exp(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2)); mhs_from_Unit(s, 3); }
void mhs_mpz_neg(int s) { mpz_neg(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
void mhs_mpz_popcount(int s) {
  mpz_ptr a = mhs_to_Ptr(s, 0);
  if (mpz_sgn(a) < 0) {
    mpz_t neg_a;
    mpz_init(neg_a);
    mpz_neg(neg_a, a);
    mhs_from_Int(s, 1, -mpz_popcount(neg_a));
    mpz_clear(neg_a);
  } else {
    mhs_from_Int(s, 1, mpz_popcount(a));
  }
}
void mhs_mpz_sub(int s) { mpz_sub(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); mhs_from_Unit(s, 3); }
void mhs_mpz_fdiv_q_2exp(int s) { mpz_fdiv_q_2exp(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2)); mhs_from_Unit(s, 3); }
void mhs_mpz_tdiv_qr(int s) { mpz_tdiv_qr(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2), mhs_to_Ptr(s, 3)); mhs_from_Unit(s, 4); }
void mhs_mpz_tstbit(int s) { mhs_from_Int(s, 2, mpz_tstbit(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
void mhs_mpz_xor(int s) { mpz_xor(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); mhs_from_Unit(s, 3); }
#endif  /* WANT_GMP */

struct ffi_entry ffi_table[] = {
{ "GETRAW", mhs_GETRAW},
{ "GETTIMEMILLI", mhs_GETTIMEMILLI},
#if WANT_MATH
{ "acos", mhs_acos},
{ "asin", mhs_asin},
{ "atan", mhs_atan},
{ "atan2", mhs_atan2},
{ "cos", mhs_cos},
{ "exp", mhs_exp},
{ "log", mhs_log},
{ "sin", mhs_sin},
{ "sqrt", mhs_sqrt},
{ "tan", mhs_tan},
#endif  /* WANT_MATH */

#if WANT_STDIO
{ "add_FILE", mhs_add_FILE},
{ "add_utf8", mhs_add_utf8},
{ "closeb", mhs_closeb},
{ "&closeb", mhs_addr_closeb},
{ "flushb", mhs_flushb},
{ "fopen", mhs_fopen},
{ "getb", mhs_getb},
{ "putb", mhs_putb},
{ "ungetb", mhs_ungetb},
{ "openb_wr_buf", mhs_openwrbuf},
{ "openb_rd_buf", mhs_openrdbuf},
{ "get_buf", mhs_getbuf},
{ "system", mhs_system},
{ "tmpname", mhs_tmpname},
{ "unlink", mhs_unlink},
{ "readb", mhs_readb},
{ "writeb", mhs_writeb},
#endif  /* WANT_STDIO */

#if WANT_MD5
{ "md5Array", mhs_md5Array},
{ "md5BFILE", mhs_md5BFILE},
{ "md5String", mhs_md5String},
#endif  /* WANT_MD5 */

#if WANT_LZ77
{ "add_lz77_compressor", mhs_add_lz77_compressor},
{ "add_lz77_decompressor", mhs_add_lz77_decompressor},
{ "lz77c", mhs_lz77c},
#endif  /* WANT_LZ77 */

#if WANT_RLE
{ "add_rle_compressor", mhs_add_rle_compressor},
{ "add_rle_decompressor", mhs_add_rle_decompressor},
#endif  /* WANT_RLE */

#if WANT_BWT
{ "add_bwt_compressor", mhs_add_bwt_compressor},
{ "add_bwt_decompressor", mhs_add_bwt_decompressor},
#endif  /* WANT_RLE */

{ "calloc", mhs_calloc},
{ "free", mhs_free},
{ "&free", mhs_addr_free},
{ "getenv", mhs_getenv},
{ "iswindows", mhs_iswindows},
{ "malloc", mhs_malloc},
{ "memcpy", mhs_memcpy},
{ "memmove", mhs_memmove},
{ "peekPtr", mhs_peekPtr},
{ "peekWord", mhs_peekWord},
{ "pokePtr", mhs_pokePtr},
{ "pokeWord", mhs_pokeWord},

{ "peek_uint8", mhs_peek_uint8},
{ "poke_uint8", mhs_poke_uint8},
{ "peek_uint16", mhs_peek_uint16},
{ "poke_uint16", mhs_poke_uint16},
#if WORD_SIZE >= 32
{ "peek_uint32", mhs_peek_uint32},
{ "poke_uint32", mhs_poke_uint32},
#endif  /* WORD_SIZE >= 32 */
#if WORD_SIZE >= 64
{ "peek_uint64", mhs_peek_uint64},
{ "poke_uint64", mhs_poke_uint64},
#endif  /* WORD_SIZE >= 64 */
{ "peek_uint", mhs_peek_uint},
{ "poke_uint", mhs_poke_uint},

{ "peek_int8", mhs_peek_int8},
{ "poke_int8", mhs_poke_int8},
{ "peek_int16", mhs_peek_int16},
{ "poke_int16", mhs_poke_int16},
#if WORD_SIZE >= 32
{ "peek_int32", mhs_peek_int32},
{ "poke_int32", mhs_poke_int32},
#endif  /* WORD_SIZE >= 32 */
#if WORD_SIZE >= 64
{ "peek_int64", mhs_peek_int64},
{ "poke_int64", mhs_poke_int64},
#endif  /* WORD_SIZE >= 64 */
{ "peek_int", mhs_peek_int},
{ "poke_int", mhs_poke_int},
{ "peek_llong", mhs_peek_llong},
{ "peek_long", mhs_peek_long},
{ "peek_ullong", mhs_peek_ullong},
{ "peek_ulong", mhs_peek_ulong},
{ "poke_llong", mhs_poke_llong},
{ "poke_long", mhs_poke_long},
{ "poke_ullong", mhs_poke_ullong},
{ "poke_ulong", mhs_poke_ulong},
#if WANT_FLOAT
{ "poke_flt", mhs_poke_flt},
{ "poke_flt", mhs_poke_flt},
#endif  /* WANT_FLOAT */
{ "sizeof_int", mhs_sizeof_int},
{ "sizeof_llong", mhs_sizeof_llong},
{ "sizeof_long", mhs_sizeof_long},
#if WANT_DIR
{ "c_d_name", mhs_c_d_name},
{ "closedir", mhs_closedir},
{ "opendir", mhs_opendir},
{ "readdir", mhs_readdir},
{ "chdir", mhs_chdir},
{ "mkdir", mhs_mkdir},
{ "getcwd", mhs_getcwd},
#endif  /* WANT_DIR */
{ "want_gmp", mhs_want_gmp},
#if WANT_GMP
{ "new_mpz", mhs_new_mpz},
{ "mpz_abs", mhs_mpz_abs},
{ "mpz_add", mhs_mpz_add},
{ "mpz_and", mhs_mpz_and},
{ "mpz_cmp", mhs_mpz_cmp},
{ "mpz_get_d", mhs_mpz_get_d},
{ "mpz_get_si", mhs_mpz_get_si},
{ "mpz_get_ui", mhs_mpz_get_ui},
{ "mpz_init_set_si", mhs_mpz_init_set_si},
{ "mpz_init_set_ui", mhs_mpz_init_set_ui},
{ "mpz_ior", mhs_mpz_ior},
{ "mpz_mul", mhs_mpz_mul},
{ "mpz_mul_2exp", mhs_mpz_mul_2exp},
{ "mpz_neg", mhs_mpz_neg},
{ "mpz_popcount", mhs_mpz_popcount},
{ "mpz_sub", mhs_mpz_sub},
{ "mpz_fdiv_q_2exp", mhs_mpz_fdiv_q_2exp},
{ "mpz_tdiv_qr", mhs_mpz_tdiv_qr},
{ "mpz_tstbit", mhs_mpz_tstbit},
{ "mpz_xor", mhs_mpz_xor},
#endif  /* WANT_GMP */
{ 0,0 }
};

int num_ffi = sizeof(ffi_table) / sizeof(ffi_table[0]);
