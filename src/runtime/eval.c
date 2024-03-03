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

#if !defined(WANT_LZ77)
#define WANT_LZ77 1
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

#define VERSION "v7.0\n"

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

enum node_tag { T_FREE, T_IND, T_AP, T_INT, T_DBL, T_PTR, T_FUNPTR, T_BADDYN, T_ARR,
                T_S, T_K, T_I, T_B, T_C,
                T_A, T_Y, T_SS, T_BB, T_CC, T_P, T_R, T_O, T_U, T_Z,
                T_K2, T_K3, T_K4, T_CCB,
                T_ADD, T_SUB, T_MUL, T_QUOT, T_REM, T_SUBR, T_UQUOT, T_UREM, T_NEG,
                T_AND, T_OR, T_XOR, T_INV, T_SHL, T_SHR, T_ASHR,
                T_EQ, T_NE, T_LT, T_LE, T_GT, T_GE, T_ULT, T_ULE, T_UGT, T_UGE,
                T_PEQ, T_PNULL, T_PADD, T_PSUB,
                T_TOPTR, T_TOINT, T_TODBL,
                T_BININT2, T_BININT1, T_UNINT1,
                T_BINDBL2, T_BINDBL1, T_UNDBL1,
#if WANT_FLOAT
                T_FADD, T_FSUB, T_FMUL, T_FDIV, T_FNEG, T_ITOF,
                T_FEQ, T_FNE, T_FLT, T_FLE, T_FGT, T_FGE, T_FSHOW, T_FREAD,
#endif
                T_ARR_ALLOC, T_ARR_SIZE, T_ARR_READ, T_ARR_WRITE, T_ARR_EQ,
                T_ERROR, T_NODEFAULT, T_NOMATCH, T_SEQ, T_EQUAL, T_COMPARE, T_RNF,
                T_TICK,
                T_IO_BIND, T_IO_THEN, T_IO_RETURN,
                T_IO_CCBIND,
                T_IO_SERIALIZE, T_IO_DESERIALIZE,
                T_IO_STDIN, T_IO_STDOUT, T_IO_STDERR, T_IO_GETARGREF,
                T_IO_PERFORMIO, T_IO_GETTIMEMILLI, T_IO_PRINT, T_IO_CATCH,
                T_IO_CCALL, T_DYNSYM,
                T_NEWCASTRINGLEN, T_PEEKCASTRING, T_PEEKCASTRINGLEN,
                T_FROMUTF8,
                T_STR,
                T_LAST_TAG,
};
#if 0
static const char* tag_names[] = {
  "FREE", "IND", "AP", "INT", "DBL", "PTR", "BADDYN", "ARR",
  "S", "K", "I", "B", "C",
  "A", "Y", "SS", "BB", "CC", "P", "R", "O", "U", "Z",
  "K2", "K3", "K4", "CCB",
  "ADD", "SUB", "MUL", "QUOT", "REM", "SUBR", "UQUOT", "UREM", "NEG",
  "AND", "OR", "XOR", "INV", "SHL", "SHR", "ASHR",
  "EQ", "NE", "LT", "LE", "GT", "GE", "ULT", "ULE", "UGT", "UGE",
  "PEQ", "PNULL", "PADD", "PSUB",
  "TOPTR", "TOINT", "TODBL",
#if WANT_FLOAT
  "FADD", "FSUB", "FMUL", "FDIV", "FNEG", "ITOF",
  "FEQ", "FNE", "FLT", "FLE", "FGT", "FGE", "FSHOW", "FREAD",
#endif
  "ARR_ALLOC", "ARR_SIZE", "ARR_READ", "ARR_WRITE", "ARR_EQ",
  "ERROR", "NODEFAULT", "NOMATCH", "SEQ", "EQUAL", "COMPARE", "RNF",
  "TICK",
  "IO_BIND", "IO_THEN", "IO_RETURN",
  "C'BIND",
  "IO_SERIALIZE", "IO_DESERIALIZE",
  "IO_STDIN", "IO_STDOUT", "IO_STDERR", "IO_GETARGREF",
  "IO_PERFORMIO", "IO_GETTIMEMILLI", "IO_PRINT", "IO_CATCH",
  "IO_CCALL", "DYNSYM",
  "NEWCASTRINGLEN", "PEEKCASTRING", "PEEKCASTRINGLEN",
  "FROMUTF8",
  "STR",
  "LAST_TAG",
};
#endif

struct ioarray;
struct ustring;

typedef struct node {
  union {
    struct node *uufun;
    tag_t        uutag;             /* LSB=1 indicates that this is a tag, LSB=0 that this is a T_AP node */
  } ufun;
  union {
    struct node    *uuarg;
    value_t         uuvalue;
    flt_t           uufloatvalue;
    struct ustring *uustring;
    const char     *uucstring;
    void           *uuptr;
    HsFunPtr        uufunptr;
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
#define CSTR(p) (p)->uarg.uucstring
#define PTR(p) (p)->uarg.uuptr
#define FUNPTR(p) (p)->uarg.uufunptr
#define ARR(p) (p)->uarg.uuarray
#define INDIR(p) ARG(p)
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = MALLOC(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((heapoffs_t)((n) - cells))
node *cells;                 /* All cells */

/*
 * UTF-8 encoded strings
 */
struct ustring {
  size_t size;
  unsigned char string[1];
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
struct ioarray *array_root = 0;

counter_t num_reductions = 0;
counter_t num_alloc = 0;
counter_t num_gc = 0;
uintptr_t gc_mark_time = 0;
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
#define PUSH(x) do { if (stack_ptr >= stack_size-1) ERR("stack overflow"); stack[++stack_ptr] = (x); MAXSTACK; } while(0)
#else  /* STACKOVL */
#define PUSH(x) do {                                                       stack[++stack_ptr] = (x); MAXSTACK; } while(0)
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

/*****************************************************************************/

#if WANT_TICK
struct tick_entry {
  struct ustring *tick_name;
  counter_t tick_count;
} *tick_table = 0;
size_t tick_table_size;
size_t tick_index;

/* Allocate a new tick table entry and return the index. */
size_t
add_tick_table(struct ustring *name)
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
      fprintf(f, "%-60s %10"PRIcounter"\n", tick_table[i].tick_name->string, n);
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

static INLINE NODEPTR
alloc_node(enum node_tag t)
{
  heapoffs_t i = next_scan_index / BITS_PER_WORD;
  int k;                        /* will contain bit pos + 1 */
  heapoffs_t pos;
  NODEPTR n;

#if 1
  /* This can happen if we run out of memory when parsing. */
  if (num_free <= 0)
    ERR("alloc_node");
#endif

  for(;;) {
    heapoffs_t word = free_map[i];
    k = FFS(word);
    if (k)
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
  pos = i * BITS_PER_WORD + k - 1; /* first free node */
  n = HEAPREF(pos);
  mark_used(n);
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
NODEPTR combFalse, combTrue, combUnit, combCons, combPair;
NODEPTR combCC, combZ, combIOBIND, combIORETURN, combIOCCBIND;
NODEPTR combLT, combEQ, combGT;
NODEPTR combBININT1, combBININT2, combUNINT1;
NODEPTR combBINDBL1, combBINDBL2, combUNDBL1;

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
  { "fromUTF8", T_FROMUTF8 },
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

#if GCRED
enum node_tag flip_ops[T_LAST_TAG];
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
    case T_K: combFalse = n; break;
    case T_A: combTrue = n; break;
    case T_I: combUnit = n; break;
    case T_O: combCons = n; break;
    case T_P: combPair = n; break;
    case T_CC: combCC = n; break;
    case T_Z: combZ = n; break;
    case T_IO_BIND: combIOBIND = n; break;
    case T_IO_RETURN: combIORETURN = n; break;
    case T_IO_CCBIND: combIOCCBIND = n; break;
    case T_BININT1: combBININT1 = n; break;
    case T_BININT2: combBININT2 = n; break;
    case T_UNINT1: combUNINT1 = n; break;
    case T_BINDBL1: combBINDBL1 = n; break;
    case T_BINDBL2: combBINDBL2 = n; break;
    case T_UNDBL1: combUNDBL1 = n; break;
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
  for(t = T_FREE; t < T_LAST_TAG; t++) {
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
    case T_IO_RETURN: combIORETURN = n; break;
    case T_IO_CCBIND: combIOCCBIND = n; break;
    case T_BININT1: combBININT1 = n; break;
    case T_BININT2: combBININT2 = n; break;
    case T_UNINT1: combUNINT1 = n; break;
    case T_BINDBL1: combBINDBL1 = n; break;
    case T_BINDBL2: combBINDBL2 = n; break;
    case T_UNDBL1: combUNDBL1 = n; break;
#if WANT_STDIO
    case T_IO_STDIN:  SETTAG(n, T_PTR); PTR(n) = stdin;  break;
    case T_IO_STDOUT: SETTAG(n, T_PTR); PTR(n) = stdout; break;
    case T_IO_STDERR: SETTAG(n, T_PTR); PTR(n) = stderr; break;
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
    break;
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
#if 1
        /* This is broken.
         * Probably because it can happen in the middle of the C reduction code.
         */
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
      }
#endif
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
      to_push = np;
      np = &arr->array[arr->marked++];
      break;
    }
   default: goto fin;
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
  struct ioarray **arrp, *arr;

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
  for (arr = array_root; arr; arr = arr->next) {
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

  for (arrp = &array_root; *arrp; ) {
    arr = *arrp;
    if (arr->marked || arr->permanent) {
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

void *
peekPtr(void **p)
{
  return *p;
}

void
pokePtr(void **p, void *w)
{
  *p = w;
}

uvalue_t
peek_uint8(uint8_t *p)
{
  return *p;
}

void
poke_uint8(uint8_t *p, value_t w)
{
  *p = (uint8_t)w;
}

uvalue_t
peek_uint16(uint16_t *p)
{
  return *p;
}

void
poke_uint16(uint16_t *p, value_t w)
{
  *p = (uint16_t)w;
}

#if WORD_SIZE >= 32
uvalue_t
peek_uint32(uint32_t *p)
{
  return *p;
}

void
poke_uint32(uint32_t *p, value_t w)
{
  *p = (uint32_t)w;
}
#endif  /* WORD_SIZE >= 32 */

#if WORD_SIZE >= 64
uvalue_t
peek_uint64(uint64_t *p)
{
  return *p;
}

void
poke_uint64(uint64_t *p, value_t w)
{
  *p = (uint64_t)w;
}
#endif  /* WORD_SIZE >= 64 */

value_t
peek_int8(int8_t *p)
{
  return *p;
}

void
poke_int8(int8_t *p, value_t w)
{
  *p = (int8_t)w;
}

value_t
peek_int16(int16_t *p)
{
  return *p;
}

void
poke_int16(int16_t *p, value_t w)
{
  *p = (int16_t)w;
}

#if WORD_SIZE >= 32
value_t
peek_int32(int32_t *p)
{
  return *p;
}

void
poke_int32(int32_t *p, value_t w)
{
  *p = (int32_t)w;
}
#endif  /* WORD_SIZE >= 32 */

#if WORD_SIZE >= 64
value_t
peek_int64(int64_t *p)
{
  return *p;
}

void
poke_int64(int64_t *p, value_t w)
{
  *p = (int64_t)w;
}
#endif  /* WORD_SIZE >= 64 */

value_t
peek_int(int *p)
{
  return *p;
}

void
poke_int(int *p, value_t w)
{
  *p = (int)w;
}

value_t
peek_uint(unsigned int *p)
{
  return *p;
}

void
poke_uint(unsigned int *p, value_t w)
{
  *p = (unsigned int)w;
}

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

NODEPTR
mkStrNode(struct ustring *str)
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

struct ustring *
parse_string(BFILE *f)
{
  size_t sz = 20;
  struct ustring *buffer = MALLOC(sizeof(struct ustring) + sz);
  size_t i;
  int c;

  if (!buffer)
    memerr();
  for(i = 0;;) {
    c = getb(f);
    if (c == '"')
      break;
    if (i >= sz) {
      sz *= 2;
      buffer = realloc(buffer, sizeof(struct ustring) + sz);
      if (!buffer)
        memerr();
    }
    if (c == '\\') {
      buffer->string[i++] = (char)parse_int(f);
      if (!gobble(f, '&'))
        ERR("parse string");
    } else {
      buffer->string[i++] = c;
    }
  }
  buffer->size = i;
  buffer->string[i++] = 0;
  return realloc(buffer, sizeof(struct ustring) + i);
}

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
    case '_' :
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
    case ':' :
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
    case '"' :
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

size_t strNodes(size_t len);
NODEPTR mkStringC(const char *str);

#if WANT_STDIO
void
convdbl(char *str, flt_t x)
{
  /* Using 16 decimals will lose some precision.
   * 17 would keep the precision, but it frequently looks very ugly.
   */
  (void)snprintf(str, 25, "%.16g", x);
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

void printrec(BFILE *f, NODEPTR n, int prefix);

/* Mark all reachable nodes, when a marked node is reached, mark it as shared. */
void
find_sharing(NODEPTR n)
{
 top:
  while (GETTAG(n) == T_IND) {
    n = INDIR(n);
  }
  if (n < cells || n >= cells + heap_size) abort();
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
print_string(BFILE *f, struct ustring *p)
{
  putb('"', f);
  for (size_t i = 0; i < p->size; i++) {
    int c = p->string[i];
    if (c == '"' || c == '\\' || c < ' ' || c > '~') {
      putb('\\', f);
      putdecb(c, f);
      putb('&', f);
    } else {
      putb(c, f);
    }
  }
  putb('"', f);
}

/* Recursively print an expression.
   This assumes that the shared nodes has been marked as such.
*/
void
printrec(BFILE *f, NODEPTR n, int prefix)
{
  int share = 0;

  while (GETTAG(n) == T_IND) {
    //putb('*', f);
    n = INDIR(n);
  }

  if (test_bit(shared_bits, n)) {
    /* The node is shared */
    if (test_bit(marked_bits, n)) {
      /* Not yet printed, so emit a label */
      if (prefix) {
        putb(':', f);
        putdecb((value_t)LABEL(n), f);
        putb(' ', f);
      } else {
        share = 1;
      }
      clear_bit(marked_bits, n);  /* mark as printed */
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
      printrec(f, FUN(n), prefix);
      putb(' ', f);
      printrec(f, ARG(n), prefix);
      putb(')', f);
    } else {
      printrec(f, FUN(n), prefix);
      printrec(f, ARG(n), prefix);
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
        printrec(f, ARR(n)->array[i], prefix);
      }
    } else {
      /* Arrays serialize as 'e_1 ... e_sz [sz]' */
      for(size_t i = 0; i < ARR(n)->size; i++) {
        printrec(f, ARR(n)->array[i], prefix);
      }
      putb('[', f);
      putdecb((value_t)ARR(n)->size, f);
      putb(']', f);
    }
    break;
  case T_PTR:
    if (PTR(n) == stdin)
      putsb("IO.stdin", f);
    else if (PTR(n) == stdout)
      putsb("IO.stdout", f);
    else if (PTR(n) == stderr)
      putsb("IO.stderr", f);
    else
      ERR("Cannot serialize pointers");
    break;
  case T_FUNPTR:
      ERR("Cannot serialize function pointers");
    break;
  case T_STR:
    print_string(f, STR(n));
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
  case T_PEQ: putsb("p==", f); break;
  case T_PNULL: putsb("pnull", f); break;
  case T_PADD: putsb("p+", f); break;
  case T_PSUB: putsb("p-", f); break;
  case T_ERROR: putsb("error", f); break;
  case T_NODEFAULT: putsb("noDefault", f); break;
  case T_NOMATCH: putsb("noMatch", f); break;
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
  case T_IO_GETTIMEMILLI: putsb("IO.getTimeMilli", f); break;
  case T_IO_PERFORMIO: putsb("IO.performIO", f); break;
  case T_IO_CATCH: putsb("IO.catch", f); break;
  case T_ARR_ALLOC: putsb("A.alloc", f); break;
  case T_ARR_SIZE: putsb("A.size", f); break;
  case T_ARR_READ: putsb("A.read", f); break;
  case T_ARR_WRITE: putsb("A.write", f); break;
  case T_ARR_EQ: putsb("A.==", f); break;
  case T_DYNSYM: putsb("dynsym", f); break;
  case T_NEWCASTRINGLEN: putsb("newCAStringLen", f); break;
  case T_PEEKCASTRING: putsb("peekCAString", f); break;
  case T_PEEKCASTRINGLEN: putsb("peekCAStringLen", f); break;
  case T_TOINT: putsb("toInt", f); break;
  case T_TOPTR: putsb("toPtr", f); break;
  case T_TODBL: putsb("toDbl", f); break;
  case T_FROMUTF8: putsb("fromUTF8", f); break;
  case T_TICK:
    putb('!', f);
    print_string(f, tick_table[GETVALUE(n)].tick_name);
    break;
  default: ERR("print tag");
  }
  if (!prefix) {
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
  num_shared = 0;
  marked_bits = calloc(free_map_nwords, sizeof(bits_t));
  if (!marked_bits)
    memerr();
  shared_bits = calloc(free_map_nwords, sizeof(bits_t));
  if (!shared_bits)
    memerr();
  find_sharing(n);
  if (header) {
    putsb(VERSION, f);
    putdecb(num_shared, f);
    putb('\n', f);
  }
  printrec(f, n, !header);
  if (header) {
    putb('}', f);
  }
  FREE(marked_bits);
  FREE(shared_bits);
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

void
ppmsg(const char *msg, NODEPTR n)
{
#if 0
  printf("%s", msg);
  pp(stdout, n);
  printf("\n");
#endif
}

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
  size_t i;
  const unsigned char *str = (unsigned char*)astr; /* no sign bits, please */

  n = mkNil();
  for(i = len; i > 0; i--) {
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

NODEPTR
mkStringU(struct ustring *str)
{
  BFILE *ubuf = add_utf8(openb_buf(str->string, str->size));
  NODEPTR n, *np, nc;

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

/* Evaluate to a T_PTR */
void *
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

/* Evaluate a string, returns a newly allocated buffer. */
/* XXX this is cheating, should use continuations */
/* XXX the malloc()ed string is leaked if we yield in here. */
char *
evalstring(NODEPTR n, value_t *lenp)
{
  size_t sz = 100;
  char *name = MALLOC(sz);
  size_t offs;
  uvalue_t c;
  NODEPTR x;

  if (!name)
    memerr();
  for (offs = 0;;) {
    if (offs >= sz - 4) {
      sz *= 2;
      name = realloc(name, sz);
      if (!name)
        memerr();
    }
    n = evali(n);
    if (GETTAG(n) == T_K)            /* Nil */
      break;
    else if (GETTAG(n) == T_AP && GETTAG(x = indir(&FUN(n))) == T_AP && GETTAG(indir(&FUN(x))) == T_O) { /* Cons */
      PUSH(n);                  /* protect from GC */
      c = (uvalue_t)evalint(ARG(x));
      n = POPTOP();
      /* XXX Encode as UTF8 */
      if (c < 0x80) {
        name[offs++] = (char)c;
      } else if (c < 0x800) {
        name[offs++] = ((c >> 6 )       ) | 0xc0;
        name[offs++] = ((c      ) & 0x3f) | 0x80;
      } else if (c < 0x10000) {
        name[offs++] = ((c >> 12)       ) | 0xe0;
        name[offs++] = ((c >> 6 ) & 0x3f) | 0x80;
        name[offs++] = ((c      ) & 0x3f) | 0x80;
      } else if (c < 0x110000) {
        name[offs++] = ((c >> 18)       ) | 0xf0;
        name[offs++] = ((c >> 12) & 0x3f) | 0x80;
        name[offs++] = ((c >> 6 ) & 0x3f) | 0x80;
        name[offs++] = ((c      ) & 0x3f) | 0x80;
      } else {
	ERR("invalid char");
      }
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

void execio(NODEPTR *);

/* Evaluate a node, returns when the node is in WHNF. */
NODEPTR
evali(NODEPTR an)
{
  NODEPTR n = an;
  stackptr_t stk = stack_ptr;
  NODEPTR x, y, z, w;
  value_t xi, yi, r;
  void *xp, *yp;
#if WANT_FLOAT
  flt_t xd, rd;
#endif  /* WANT_FLOAT */
  char *msg;
#if FASTTAGS
  heapoffs_t l;
#endif
  enum node_tag tag;
  struct ioarray *arr;
  int sz;
  char *res;

#if MAXSTACKDEPTH
  counter_t old_cur_c_stack = cur_c_stack;
  if (++cur_c_stack > max_c_stack)
    max_c_stack = cur_c_stack;
#endif

/* Reset stack pointer and return. */
#define RET do { goto ret; } while(0)
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
#define SETFUNPTR(n,r) do { SETTAG((n), T_FUNPTR); FUNPTR(n) = (r); } while(0)
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

  case T_STR:  RET;
  case T_INT:  RET;
  case T_DBL:  RET;
  case T_PTR:  RET;
  case T_FUNPTR: RET;
  case T_ARR:  RET;
  case T_BADDYN: ERR1("FFI unknown %s", CSTR(n));
    
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
  case T_ULT:
  case T_ULE:
  case T_UGT:
  case T_UGE:
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
    n = ARG(TOP(1));
    PUSH(combBINDBL2);
    goto top;
  case T_FNEG:
    n = ARG(TOP(0));
    PUSH(combUNDBL1);
    goto top;

  case T_ITOF: OPINT1(rd = (flt_t)xi); SETDBL(n, rd); RET;
  case T_FREAD:
    CHECK(1);
    msg = evalstring(ARG(TOP(0)), 0);
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

  /* Retag a word sized value, keeping the value bits */
#define CONV(t) do { CHECK(1); x = evali(ARG(TOP(0))); n = POPTOP(); SETTAG(n, t); SETVALUE(n, GETVALUE(x)); RET; } while(0)
  case T_TODBL: CONV(T_DBL);
  case T_TOINT: CONV(T_INT);
  case T_TOPTR: CONV(T_PTR);
#undef CONV

  case T_PEQ:  CMPP(==);
  case T_PNULL: SETTAG(n, T_PTR); PTR(n) = 0; RET;
  case T_PADD: CHECK(2); xp = evalptr(ARG(TOP(0))); yi = evalint(ARG(TOP(1))); POP(2); n = TOP(-1); SETPTR(n, (char*)xp + yi); RET;
  case T_PSUB: CHECK(2); xp = evalptr(ARG(TOP(0))); yp = evalptr(ARG(TOP(1))); POP(2); n = TOP(-1); SETINT(n, (char*)xp - (char*)yp); RET;

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

  case T_FROMUTF8:
    if (doing_rnf) RET;
    CHECK(1);
    x = evali(ARG(TOP(0)));
    if (GETTAG(x) != T_STR) ERR("FROMUTF8");
    POP(1);
    n = TOP(-1);
    GCCHECK(strNodes(STR(x)->size));
    GOIND(mkStringU(STR(x)));

  case T_NOMATCH:
    if (doing_rnf) RET;
    {
      CHECK(3);
      msg = evalstring(ARG(TOP(0)), 0);
      xi = evalint(ARG(TOP(1)));
      yi = evalint(ARG(TOP(2)));
      sz = strlen(msg) + 100;
      res = MALLOC(sz);
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
      sz = strlen(msg) + 100;
      res = MALLOC(sz);
      
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
      /* A horrible hack until we get proper exceptions */
      if (strcmp(msg, "ExitSuccess") == 0) {
        EXIT(0);
      } else {
        fprintf(stderr, "mhs: %s\n", msg);
        EXIT(1);
      }
#else  /* WANT_STDIO */
      ERR1("error: %s", msg);
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
    x = ARG(TOP(0));           /* should be RETURN e */
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
      case T_LT:    GOIND((value_t)xu <  (value_t)yu ? combTrue : combFalse);
      case T_LE:    GOIND((value_t)xu <= (value_t)yu ? combTrue : combFalse);
      case T_GT:    GOIND((value_t)xu >  (value_t)yu ? combTrue : combFalse);
      case T_GE:    GOIND((value_t)xu >= (value_t)yu ? combTrue : combFalse);

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
      case T_IND:   p = INDIR(p); goto unint;
      case T_NEG:   ru = -xu; break;
      case T_INV:   ru = ~xu; break;
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
 * Given the call execio(np) we allocate this graph, top,:
 *   BIND (*np) RETURN
 * and make np point to it.
 * This graph will be updated continuously as we execite IO action.
 * Invariant: the second argument to this BIND is always either RETURN
 * or a C'BIND.  The second argument to C'BIND has the same invariant.
 * This is the cycle:
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
  char *name;
  value_t len;
  struct handler *h;
#if WANT_STDIO
  void *ptr;
  int hdr;
#endif  /* WANT_STDIO */
  NODEPTR top;

/* IO operations need all arguments, anything else should not happen. */
#define CHECKIO(n) do { if (stack_ptr - stk != (n)+1) {printf("\nLINE=%d\n", __LINE__); ERR("CHECKIO");}; } while(0)
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
  GCCHECK(1);
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

    case T_IO_CATCH:
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
          execio(&ARG(TOP(1))); /* execute first argument */
          cur_handler = h->hdl_old; /* restore old handler */
          FREE(h);
          n = ARG(TOP(1));
          IOASSERT(GETTAG(n) == T_AP && GETTAG(FUN(n)) == T_IO_RETURN, "CATCH");
          RETIO(ARG(n));             /* return result */
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
      size_t size;
      CHECKIO(1);
      name = evalptr(ARG(TOP(1)));
      size = strlen(name);
      GCCHECK(strNodes(size));
      RETIO(mkString(name, size));
      }

    case T_PEEKCASTRINGLEN:
      {
      size_t size;
      CHECKIO(2);
      size = evalint(ARG(TOP(2)));
      name = evalptr(ARG(TOP(1)));
      GCCHECK(strNodes(size));
      RETIO(mkString(name, size));
      }

    case T_ARR_ALLOC:
      {
      size_t size;
      NODEPTR elem;
      struct ioarray *arr;
      CHECKIO(2);
      size = evalint(ARG(TOP(1)));
      elem = ARG(TOP(2));
      arr = arr_alloc(size, elem);
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
#if WANT_TICKS
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
      n = mkCons(mkString(gargv[i], strlen(gargv[i])), n);
    }
    n = mkCons(mkString(progname, strlen(progname)), n);
    argarray = arr_alloc(1, n);      /* An IORef contains a single element array */
    argarray->permanent = 1;         /* never GC the arguments, because a T_IO_GETARGREF can reach argarray */
  }
#endif  /* WANT_ARGS */

  if (combexpr) {
    int c;
    BFILE *bf = openb_buf(combexpr, combexprlen);
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
#endif
#if WANT_STDIO
  NODEPTR res = evali(ARG(prog));
#else
  (void)evali(ARG(prog));
#endif
  run_time += GETTIMEMILLI();
#if WANT_STDIO
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
#if MAXSTACKDEPTH
    PRINT("%"PCOMMA"15d max stack depth\n", (int)max_stack_depth);
    PRINT("%"PCOMMA"15d max C stack depth\n", (int)max_c_stack);
#endif
    // PRINT("%"PCOMMA"15"PRIcounter" max mark depth\n", max_mark_depth);
    PRINT("%15.2fs total expired time\n", (double)run_time / 1000);
    PRINT("%15.2fs total gc time\n", (double)gc_mark_time / 1000);
#if GCRED
    PRINT(" GC reductions A=%d, K=%d, I=%d, int=%d flip=%d\n", red_a, red_k, red_i, red_int, red_flip);
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
MHS_FROM(mhs_from_Double, SETDBL, flt_t);
MHS_FROM(mhs_from_Int, SETINT, value_t);
MHS_FROM(mhs_from_Word, SETINT, uvalue_t);
MHS_FROM(mhs_from_Word8, SETINT, uvalue_t);
MHS_FROM(mhs_from_Ptr, SETPTR, void*);
MHS_FROM(mhs_from_FunPtr, SETFUNPTR, HsFunPtr);
MHS_FROM(mhs_from_CChar, SETINT, char);
MHS_FROM(mhs_from_CSChar, SETINT, signed char);
MHS_FROM(mhs_from_CUChar, SETINT, unsigned char);
MHS_FROM(mhs_from_CSHORT, SETINT, short);
MHS_FROM(mhs_from_CUSHORT, SETINT, unsigned short);
MHS_FROM(mhs_from_CINT, SETINT, int);
MHS_FROM(mhs_from_CUINT, SETINT, unsigned int);
MHS_FROM(mhs_from_CLONG, SETINT, long);
MHS_FROM(mhs_from_CULONG, SETINT, unsigned long);
MHS_FROM(mhs_from_CLLONG, SETINT, long long);
MHS_FROM(mhs_from_CULLONG, SETINT, unsigned long long);
MHS_FROM(mhs_from_CSize, SETINT, size_t);
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
MHS_TO(mhs_to_Double, evaldbl, flt_t);
MHS_TO(mhs_to_Int, evalint, value_t);
MHS_TO(mhs_to_Word, evalint, uvalue_t);
MHS_TO(mhs_to_Word8, evalint, uint8_t);
MHS_TO(mhs_to_Ptr, evalptr, void*);
MHS_TO(mhs_to_FunPtr, evalfunptr, HsFunPtr);
MHS_TO(mhs_to_CChar, evalint, char);
MHS_TO(mhs_to_CSChar, evalint, signed char);
MHS_TO(mhs_to_CUChar, evalint, unsigned char);
MHS_TO(mhs_to_CSHORT, evalint, short);
MHS_TO(mhs_to_CUSHORT, evalint, unsigned short);
MHS_TO(mhs_to_CINT, evalint, int);
MHS_TO(mhs_to_CUINT, evalint, unsigned int);
MHS_TO(mhs_to_CLONG, evalint, long);
MHS_TO(mhs_to_CULONG, evalint, unsigned long);
MHS_TO(mhs_to_CLLONG, evalint, long long);
MHS_TO(mhs_to_CULLONG, evalint, unsigned long long);
MHS_TO(mhs_to_CSize, evalint, size_t);
// MHS_TO(mhs_to_CSSize, evalint, ssize_t);
MHS_TO(mhs_to_CIntPtr, evalint, intptr_t);
MHS_TO(mhs_to_CUIntPtr, evalint, uintptr_t);


/* The rest of this file was generated by the compiler, with some minor edits with #if. */
void mhs_GETRAW(int s) { mhs_from_Int(s, 0, GETRAW()); }
void mhs_GETTIMEMILLI(int s) { mhs_from_Int(s, 0, GETTIMEMILLI()); }
#if WANT_MATH
#if WORD_SIZE == 64
void mhs_acos(int s) { mhs_from_Double(s, 1, acos(mhs_to_Double(s, 0))); }
void mhs_asin(int s) { mhs_from_Double(s, 1, asin(mhs_to_Double(s, 0))); }
void mhs_atan(int s) { mhs_from_Double(s, 1, atan(mhs_to_Double(s, 0))); }
void mhs_atan2(int s) { mhs_from_Double(s, 2, atan2(mhs_to_Double(s, 0), mhs_to_Double(s, 1))); }
void mhs_cos(int s) { mhs_from_Double(s, 1, cos(mhs_to_Double(s, 0))); }
void mhs_exp(int s) { mhs_from_Double(s, 1, exp(mhs_to_Double(s, 0))); }
void mhs_log(int s) { mhs_from_Double(s, 1, log(mhs_to_Double(s, 0))); }
void mhs_sin(int s) { mhs_from_Double(s, 1, sin(mhs_to_Double(s, 0))); }
void mhs_sqrt(int s) { mhs_from_Double(s, 1, sqrt(mhs_to_Double(s, 0))); }
void mhs_tan(int s) { mhs_from_Double(s, 1, tan(mhs_to_Double(s, 0))); }
#elif WORD_SIZE == 32  /* WORD_SIZE */
void mhs_acos(int s) { mhs_from_Double(s, 1, acosf(mhs_to_Double(s, 0))); }
void mhs_asin(int s) { mhs_from_Double(s, 1, asinf(mhs_to_Double(s, 0))); }
void mhs_atan(int s) { mhs_from_Double(s, 1, atanf(mhs_to_Double(s, 0))); }
void mhs_atan2(int s) { mhs_from_Double(s, 2, atan2f(mhs_to_Double(s, 0), mhs_to_Double(s, 1))); }
void mhs_cos(int s) { mhs_from_Double(s, 1, cosf(mhs_to_Double(s, 0))); }
void mhs_exp(int s) { mhs_from_Double(s, 1, expf(mhs_to_Double(s, 0))); }
void mhs_log(int s) { mhs_from_Double(s, 1, logf(mhs_to_Double(s, 0))); }
void mhs_sin(int s) { mhs_from_Double(s, 1, sinf(mhs_to_Double(s, 0))); }
void mhs_sqrt(int s) { mhs_from_Double(s, 1, sqrtf(mhs_to_Double(s, 0))); }
void mhs_tan(int s) { mhs_from_Double(s, 1, tanf(mhs_to_Double(s, 0))); }
#else
#error Unknown WORD_SIZE
#endif  /* WORD_SIZE */
#endif  /* WANT_MATH */

#if WANT_STDIO
void mhs_add_FILE(int s) { mhs_from_Ptr(s, 1, add_FILE(mhs_to_Ptr(s, 0))); }
void mhs_add_utf8(int s) { mhs_from_Ptr(s, 1, add_utf8(mhs_to_Ptr(s, 0))); }
void mhs_closeb(int s) { closeb(mhs_to_Ptr(s, 0)); mhs_from_Unit(s, 1); }
void mhs_flushb(int s) { flushb(mhs_to_Ptr(s, 0)); mhs_from_Unit(s, 1); }
void mhs_fopen(int s) { mhs_from_Ptr(s, 2, fopen(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }
void mhs_getb(int s) { mhs_from_Int(s, 1, getb(mhs_to_Ptr(s, 0))); }
void mhs_putb(int s) { putb(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
void mhs_system(int s) { mhs_from_Int(s, 1, system(mhs_to_Ptr(s, 0))); }
void mhs_tmpname(int s) { mhs_from_Ptr(s, 2, TMPNAME(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }
void mhs_unlink(int s) { mhs_from_Int(s, 1, unlink(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_STDIO */

#if WANT_MD5
void mhs_md5Array(int s) { md5Array(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2)); mhs_from_Unit(s, 3); }
void mhs_md5BFILE(int s) { md5BFILE(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
void mhs_md5String(int s) { md5String(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); mhs_from_Unit(s, 2); }
#endif  /* WANT_MD5 */

#if WANT_LZ77
void mhs_lz77c(int s) { mhs_from_CSize(s, 3, lz77c(mhs_to_Ptr(s, 0), mhs_to_CSize(s, 1), mhs_to_Ptr(s, 2))); }
#endif  /* WANT_LZ77 */

void mhs_calloc(int s) { mhs_from_Ptr(s, 2, calloc(mhs_to_CSize(s, 0), mhs_to_CSize(s, 1))); }
void mhs_free(int s) { free(mhs_to_Ptr(s, 0)); mhs_from_Unit(s, 1); }
void mhs_getenv(int s) { mhs_from_Ptr(s, 1, getenv(mhs_to_Ptr(s, 0))); }
void mhs_iswindows(int s) { mhs_from_Int(s, 0, iswindows()); }
void mhs_malloc(int s) { mhs_from_Ptr(s, 1, malloc(mhs_to_CSize(s, 0))); }
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
{ "flushb", mhs_flushb},
{ "fopen", mhs_fopen},
{ "getb", mhs_getb},
{ "putb", mhs_putb},
{ "system", mhs_system},
{ "tmpname", mhs_tmpname},
{ "unlink", mhs_unlink},
#endif  /* WANT_STDIO */

#if WANT_MD5
{ "md5Array", mhs_md5Array},
{ "md5BFILE", mhs_md5BFILE},
{ "md5String", mhs_md5String},
#endif  /* WANT_MD5 */

#if WANT_LZ77
{ "lz77c", mhs_lz77c},
#endif  /* WANT_LZ77 */

{ "calloc", mhs_calloc},
{ "free", mhs_free},
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
{ 0,0 }
};

int num_ffi = sizeof(ffi_table) / sizeof(ffi_table[0]);
