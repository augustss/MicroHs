/* Copyright 2023 Lennart Augustsson
 * See LICENSE file for full license.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <locale.h>
#include <ctype.h>
#include <setjmp.h>

#define GCRED    1              /* do some reductions during GC */
#define FASTTAGS 1              /* compute tag by pointer subtraction */
#define UNIONPTR 1              /* use compact (2 pointer) layout */
#define INTTABLE 1              /* use fixed table of small INT nodes */
#define SANITY   1              /* do some sanity checks */
#define STACKOVL 1              /* check for stack overflow */
#define GETRAW   1              /* implement raw character get */

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

#if defined(__MINGW32__)
#define ffsl __builtin_ffsll
#endif

#if defined(_MSC_VER)

/* Make Microsoft compiler a little more compatible. */

#pragma warning(disable : 4996)
#pragma intrinsic(_BitScanForward)
static inline int
ffsl(int64_t arg)
{
  unsigned long r;
  if (_BitScanForward64(&r, arg))
    return (int)(r+1);
  else
    return 0;
}
#define PCOMMA ""

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

typedef struct timeval {
    long tv_sec;
    long tv_usec;
} timeval;

int
gettimeofday(struct timeval * tp, struct timezone * tzp)
{
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return 0;
}

int
getraw()
{
  return -1;                    /* too tedious */
}

#else  /* defined(_MSC_VER) */

#include <sys/time.h>

#define PCOMMA "'"

#if GETRAW
#include <termios.h>
#include <unistd.h>

/*
 * Set the terminal in raw mode and read a single character.
 * Return this character, or -1 on any kind of failure.
 */
int
getraw(void)
{
  struct termios old, new;
  char c;
  int r;
  
  if (tcgetattr(0, &old))
    return -1;
  cfmakeraw(&new);
  if (tcsetattr(0, TCSANOW, &new))
    return -1;
  r = read(0, &c, 1);
  (void)tcsetattr(0, TCSANOW, &old);
  if (r == 1)
    return c;
  else
    return -1;
}
#else  /* GETRAW */

int
getraw()
{
  return -1;                    /* not implemented */
}

#endif /* GETRAW */

#endif  /* !defined(_MSC_VER) */


/***************************************/

#define VERSION "v3.5\n"

/* Keep permanent nodes for LOW_INT <= i < HIGH_INT */
#define LOW_INT (-10)
#define HIGH_INT 128

#define HEAP_CELLS 50000000
#define STACK_SIZE 100000

#define ERR(s) do { fprintf(stderr, "ERR: %s\n", s); exit(1); } while(0)

enum node_tag { T_FREE, T_IND, T_AP, T_INT, T_DOUBLE, T_HDL, T_S, T_K, T_I, T_B, T_C,
                T_A, T_Y, T_SS, T_BB, T_CC, T_P, T_O, T_T, T_BK, T_ADD, T_SUB, T_MUL,
                T_QUOT, T_REM, T_SUBR, T_UQUOT, T_UREM,
                T_FADD, T_FSUB, T_FMUL, T_FDIV,
                T_FEQ, T_FNE, T_FLT, T_FLE, T_FGT, T_FGE, T_FSHOW, T_FREAD,
                T_EQ, T_NE, T_LT, T_LE, T_GT, T_GE, T_ULT, T_ULE, T_UGT, T_UGE,
                T_ERROR, T_SEQ, T_EQUAL, T_COMPARE, T_RNF,
                T_IO_BIND, T_IO_THEN, T_IO_RETURN, T_IO_GETCHAR, T_IO_PUTCHAR,
                T_IO_SERIALIZE, T_IO_DESERIALIZE, T_IO_OPEN, T_IO_CLOSE, T_IO_ISNULLHANDLE,
                T_IO_STDIN, T_IO_STDOUT, T_IO_STDERR, T_IO_GETARGS, T_IO_DROPARGS,
                T_IO_PERFORMIO,
                T_IO_GETTIMEMILLI, T_IO_PRINT, T_IO_CATCH,
                T_IO_CCALL, T_IO_GETRAW, T_IO_FLUSH,
                T_STR,
                T_ISINT, T_ISIO,
                T_LAST_TAG,
};

#if NAIVE

/* Naive node representation with minimal unions */
typedef struct node {
  enum node_tag tag;
  union {
    value_t value;
    double doublevalue;
    FILE *file;
    const char *string;
    struct {
      struct node *fun;
      struct node *arg;
    } s;
  } u;
} node;
typedef struct node* NODEPTR;
#define NIL 0
#define HEAPREF(i) &cells[(i)]
#define MARK(p) (p)->mark
#define GETTAG(p) (p)->tag
#define SETTAG(p, t) do { (p)->tag = (t); } while(0)
#define GETVALUE(p) (p)->u.value
// to squeeze a double into value_t we must exactly copy and read the bits
// this is a stm, and not an exp
#define GETDOUBLEVALUE(p) (p)->u.doublevalue
#define SETVALUE(p,v) (p)->u.value = v
#define SETDOUBLEVALUE(p,v) (p)->u.doublevalue = v
#define FUN(p) (p)->u.s.fun
#define ARG(p) (p)->u.s.arg
#define NEXT(p) FUN(p)
#define INDIR(p) FUN(p)
#define HANDLE(p) (p)->u.file
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); if (!cells) memerr(); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((heapoffs_t)((n) - cells))
node *cells;                 /* All cells */

#elif UNIONPTR

typedef struct node {
  union {
    struct node *uufun;
    tag_t uutag;             /* LSB=1 indicates that this is a tag, LSB=0 that this is a T_AP node */
  } ufun;
  union {
    struct node *uuarg;
    value_t uuvalue;
    double uudoublevalue;
    FILE *uufile;
    const char *uustring;
  } uarg;
} node;
typedef struct node* NODEPTR;
#define NIL 0
#define HEAPREF(i) &cells[(i)]
#define GETTAG(p) ((p)->ufun.uutag & 1 ? (int)((p)->ufun.uutag >> 1) : T_AP)
#define SETTAG(p,t) do { if (t != T_AP) (p)->ufun.uutag = ((t) << 1) + 1; } while(0)
#define GETVALUE(p) (p)->uarg.uuvalue
#define GETDOUBLEVALUE(p) (p)->uarg.uudoublevalue
#define SETVALUE(p,v) (p)->uarg.uuvalue = v
#define SETDOUBLEVALUE(p,v) (p)->uarg.uudoublevalue = v
#define FUN(p) (p)->ufun.uufun
#define ARG(p) (p)->uarg.uuarg
#define STR(p) (p)->uarg.uustring
#define INDIR(p) ARG(p)
#define HANDLE(p) (p)->uarg.uufile
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((heapoffs_t)((n) - cells))
node *cells;                 /* All cells */

#else

#error "pick a node type"

#endif

counter_t num_reductions = 0;
counter_t num_alloc;
counter_t num_gc = 0;
double gc_mark_time = 0;
double run_time = 0;

NODEPTR *stack;
stackptr_t stack_ptr = -1;
#if STACKOVL
#define PUSH(x) do { if (stack_ptr >= stack_size-1) ERR("stack overflow"); stack[++stack_ptr] = (x); } while(0)
#else  /* SANITY */
#define PUSH(x) do {                                                       stack[++stack_ptr] = (x); } while(0)
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

typedef struct {
  size_t   b_size;
  size_t   b_pos;
  uint8_t  b_buffer[1];
} BFILE;

struct handler {
  jmp_buf         hdl_buf;      /* env storage */
  struct handler *hdl_old;      /* old handler */
  stackptr_t      hdl_stack;    /* old stack pointer */
  NODEPTR         hdl_exn;     /* used temporarily to pass the exception value */
} *cur_handler = 0;

void
memerr(void)
{
  fprintf(stderr, "Out of memory\n");
  exit(1);
}

BFILE *
alloc_buffer(size_t size)
{
  BFILE *p;
  p = malloc(sizeof(BFILE) + size);
  if (!p)
    memerr();
  p->b_size = size;
  p->b_pos = 0;
  return p;
}

int
getb(BFILE *p)
{
  if (p->b_pos >= p->b_size)
    return -1;
  return p->b_buffer[p->b_pos++];
}

void
ungetb(int c, BFILE *p)
{
  if (p->b_pos == 0)
    ERR("ungetb");
  p->b_buffer[--p->b_pos] = (uint8_t)c;
}

/* Set FREE bit to 0 */
static inline void mark_used(NODEPTR n)
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
static inline int is_marked_used(NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  if (i < heap_start)
    return 1;
#if SANITY
  if (i >= free_map_nwords * BITS_PER_WORD) ERR("is_marked_used");;
#endif
  return (free_map[i / BITS_PER_WORD] & (1ULL << (i % BITS_PER_WORD))) == 0;
}

static inline void mark_all_free(void)
{
  memset(free_map, ~0, free_map_nwords * sizeof(bits_t));
  next_scan_index = heap_start;
}

int glob_argc;
char **glob_argv;

int verbose = 0;

double
gettime()
{
  struct timeval tv;
  (void)gettimeofday(&tv, NULL);
  return tv.tv_sec + tv.tv_usec * 1e-6;
}

static inline NODEPTR
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
    k = ffsl(word);
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

static inline NODEPTR
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
NODEPTR combCC, combBK, combIOBIND;
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
  { "I", T_I },
  { "S", T_S },
  { "T", T_T },
  { "Y", T_Y },
  { "B'", T_BB },
  { "BK", T_BK },
  /* primops */
  { "+", T_ADD },
  { "-", T_SUB },
  { "*", T_MUL },
  { "quot", T_QUOT },
  { "rem", T_REM },
  { "uquot", T_UQUOT },
  { "urem", T_UREM },
  { "subtract", T_SUBR },
  { "fadd" , T_FADD},
  { "fsub" , T_FSUB},
  { "fmul" , T_FMUL},
  { "fdiv", T_FDIV},
  { "feq", T_FEQ},
  { "fne", T_FNE},
  { "flt", T_FLT},
  { "fle", T_FLE},
  { "fgt", T_FGT},
  { "fge", T_FGE},
  { "fshow", T_FSHOW},
  { "fread", T_FREAD},
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
  { "equal", T_EQUAL },
  { "compare", T_COMPARE },
  { "rnf", T_RNF },
  /* IO primops */
  { "IO.>>=", T_IO_BIND },
  { "IO.>>", T_IO_THEN },
  { "IO.return", T_IO_RETURN },
  { "IO.getChar", T_IO_GETCHAR },
  { "IO.getRaw", T_IO_GETRAW },
  { "IO.putChar", T_IO_PUTCHAR },
  { "IO.serialize", T_IO_SERIALIZE },
  { "IO.print", T_IO_PRINT },
  { "IO.deserialize", T_IO_DESERIALIZE },
  { "IO.open", T_IO_OPEN },
  { "IO.close", T_IO_CLOSE },
  { "IO.flush", T_IO_FLUSH },
  { "IO.isNullHandle", T_IO_ISNULLHANDLE },
  { "IO.stdin", T_IO_STDIN },
  { "IO.stdout", T_IO_STDOUT },
  { "IO.stderr", T_IO_STDERR },
  { "IO.getArgs", T_IO_GETARGS },
  { "IO.dropArgs", T_IO_DROPARGS },
  { "IO.getTimeMilli", T_IO_GETTIMEMILLI },
  { "IO.performIO", T_IO_PERFORMIO },
  { "IO.catch", T_IO_CATCH },
  { "isInt", T_ISINT },
  { "isIO", T_ISIO },
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
    case T_BK: combBK = n; break;
    case T_IO_BIND: combIOBIND = n; break;
    case T_IO_STDIN:  SETTAG(n, T_HDL); HANDLE(n) = stdin;  break;
    case T_IO_STDOUT: SETTAG(n, T_HDL); HANDLE(n) = stdout; break;
    case T_IO_STDERR: SETTAG(n, T_HDL); HANDLE(n) = stderr; break;
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
    case T_BK: combBK = n; break;
    case T_IO_BIND: combIOBIND = n; break;
    case T_IO_STDIN:  SETTAG(n, T_HDL); HANDLE(n) = stdin;  break;
    case T_IO_STDOUT: SETTAG(n, T_HDL); HANDLE(n) = stdout; break;
    case T_IO_STDERR: SETTAG(n, T_HDL); HANDLE(n) = stderr; break;
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
  NEWAP(combLT, combBK,    combFalse);  /* BK B */
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
        printf("%p %p %p\n", n, INDIR(n), INDIR(INDIR(n)));
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
  double t;
  
  num_gc++;
  num_marked = 0;
  if (verbose > 1)
    fprintf(stderr, "gc mark\n");
  gc_mark_time -= gettime();
  mark_all_free();
  //  mark_depth = 0;
  for (stackptr_t i = 0; i <= stack_ptr; i++)
    mark(&stack[i]);
  t = gettime();
  gc_mark_time += t;
  if (verbose > 1)
    fprintf(stderr, "gc scan\n");

  if (num_marked > max_num_marked)
    max_num_marked = num_marked;
  num_free = heap_size - heap_start - num_marked;
  if (num_free < heap_size / 50)
    ERR("heap exhausted");
  if (verbose > 1)
    fprintf(stderr, "gc done, %"PRIcounter" free\n", num_free);
}

/* Check that there are k nodes available, if not then GC. */
static inline void
gc_check(size_t k)
{
  if (k < num_free)
    return;
  if (verbose > 1)
    fprintf(stderr, "gc_check: %d\n", (int)k);
  gc();
}

/*
 * Table of FFI callable functions.
 * (For a more flexible solution use dlopen()/dlsym()/dlclose())
 * The table contains the information needed to do the actual call.
 * The types are
 *   V    void name(void)
 *   I    int  name(void)
 *   IV   void name(int)
 *   II   int  name(int)
 *   IIV  void name(int, int)
 *   III  int  name(int, int)
 * more can easily be added.
 */
struct {
  const char *ffi_name;
  const funptr_t ffi_fun;
  enum { FFI_V, FFI_I, FFI_IV, FFI_II, FFI_IIV, FFI_III } ffi_how;
} ffi_table[] = {
  { "llabs", (funptr_t)llabs, FFI_II },
};

/* Look up an FFI function by name */
value_t
lookupFFIname(const char *name)
{
  for(int i = 0; i < sizeof(ffi_table) / sizeof(ffi_table[0]); i++)
    if (strcmp(ffi_table[i].ffi_name, name) == 0)
      return (value_t)i;
  ERR("lookupFFIname");
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

/* Get a non-terminating character.  ' ' and ')' terminate a token. */
int
getNT(BFILE *f)
{
  int c;
  
  c = getb(f);
  if (c == ' ' || c == ')') {
    ungetb(c, f);
    return 0;
  } else {
    return c;
  }
}

value_t
parse_int(BFILE *f)
{
  value_t i = 0;
  int c = getb(f);
  for(;;) {
    i = i * 10 + c - '0';
    c = getb(f);
    if (c < '0' || c > '9') {
      ungetb(c, f);
      break;
    }
  }
  return i;
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
NODEPTR mkDouble(double d);

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
  value_t neg;
  int c;
  char buf[80];                 /* store names of primitives. */

  c = getb(f);
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
  case '-':
    c = getb(f);
    neg = -1;
    if ('0' <= c && c <= '9') {
      goto number;
    } else {
      ERR("got -");
    }
  case '%':
    d = parse_double(f);
    r = mkDouble(d);
    return r;
  case '0':case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
    /* integer [0-9]+*/
    neg = 1;
  number:
    ungetb(c, f);
    i = neg * parse_int(f);
    r = mkInt(i);
    return r;
  case '$':
    /* A primitive, keep getting char's until end */
    for (int j = 0; (buf[j] = getNT(f)); j++)
      ;
    /* Look up the primop and use the preallocated node. */
    for (int j = 0; j < sizeof primops / sizeof primops[0]; j++) {
      if (strcmp(primops[j].name, buf) == 0) {
        return primops[j].node;
      }
    }
    fprintf(stderr, "eval: bad primop %s\n", buf);
    ERR("no primop");
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
        c = getb(f);
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
  case '#':
    /* An FFI name */
    for (int j = 0; (buf[j] = getNT(f)); j++)
      ;
    r = alloc_node(T_IO_CCALL);
    SETVALUE(r, lookupFFIname(buf));
    return r;
  default:
    fprintf(stderr, "parse '%c'\n", c);
    ERR("parse default");
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

NODEPTR
parse_FILE(FILE *f)
{
  size_t size;
  off_t pos;
  
  /* Determine how much is left of the file */
  pos = ftell(f);
  (void)fseek(f, 0, SEEK_END);
  size = (size_t)(ftell(f) - pos);
  (void)fseek(f, pos, SEEK_SET);

  /* Read entire file */
  BFILE *b = alloc_buffer(size);
  if (fread(b->b_buffer, 1, size, f) != size)
    ERR("fread");

  /* And parse it */
  NODEPTR n = parse_top(b);

  free(b);
  return n;
}

NODEPTR
parse_file(const char *fn, size_t *psize)
{
  FILE *f = fopen(fn, "r");
  if (!f)
    ERR("file not found");
  NODEPTR n = parse_FILE(f);
  *psize = ftell(f);
  fclose(f);
  return n;
}


void printrec(FILE *f, NODEPTR n);

counter_t num_shared;

/* Two bits per node: marked, shared
 * 0, 0   -- not visited
 * 1, 0   -- visited once
 * 1, 1   -- visited more than once
 * 0, 1   -- printed
 */
bits_t *marked_bits;
bits_t *shared_bits;
static inline void set_bit(bits_t *bits, NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  bits[i / BITS_PER_WORD] |= (1ULL << (i % BITS_PER_WORD));
}
static inline void clear_bit(bits_t *bits, NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  bits[i / BITS_PER_WORD] &= ~(1ULL << (i % BITS_PER_WORD));
}
static inline int test_bit(bits_t *bits, NODEPTR n)
{
  heapoffs_t i = LABEL(n);
  return (bits[i / BITS_PER_WORD] & (1ULL << (i % BITS_PER_WORD))) != 0;
}

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
  case T_INT: fprintf(f, "%"PRIvalue, GETVALUE(n)); break;
  case T_DOUBLE: fprintf(f, "%f", GETDOUBLEVALUE(n)); break;
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
  case T_HDL:
    if (HANDLE(n) == stdin)
      fprintf(f, "$IO.stdin");
    else if (HANDLE(n) == stdout)
      fprintf(f, "$IO.stdout");
    else if (HANDLE(n) == stderr)
      fprintf(f, "$IO.stderr");
    else
      ERR("Cannot serialize handles");
    break;
  case T_S: fprintf(f, "$S"); break;
  case T_K: fprintf(f, "$K"); break;
  case T_I: fprintf(f, "$I"); break;
  case T_C: fprintf(f, "$C"); break;
  case T_B: fprintf(f, "$B"); break;
  case T_A: fprintf(f, "$A"); break;
  case T_T: fprintf(f, "$T"); break;
  case T_Y: fprintf(f, "$Y"); break;
  case T_P: fprintf(f, "$P"); break;
  case T_O: fprintf(f, "$O"); break;
  case T_SS: fprintf(f, "$S'"); break;
  case T_BB: fprintf(f, "$B'"); break;
  case T_BK: fprintf(f, "$BK"); break;
  case T_CC: fprintf(f, "$C'"); break;
  case T_ADD: fprintf(f, "$+"); break;
  case T_SUB: fprintf(f, "$-"); break;
  case T_MUL: fprintf(f, "$*"); break;
  case T_QUOT: fprintf(f, "$quot"); break;
  case T_REM: fprintf(f, "$rem"); break;
  case T_UQUOT: fprintf(f, "$uquot"); break;
  case T_UREM: fprintf(f, "$urem"); break;
  case T_SUBR: fprintf(f, "$subtract"); break;
  case T_FADD: fprintf(f, "$fadd"); break;
  case T_FSUB: fprintf(f, "$fsub"); break;
  case T_FMUL: fprintf(f, "$fmul"); break;
  case T_FDIV: fprintf(f, "$fdiv"); break;
  case T_FEQ: fprintf(f, "$feq"); break;
  case T_FNE: fprintf(f, "$fne"); break;
  case T_FLT: fprintf(f, "$flt"); break;
  case T_FLE: fprintf(f, "$fle"); break;
  case T_FGT: fprintf(f, "$fgt"); break;
  case T_FGE: fprintf(f, "$fge"); break;
  case T_FSHOW: fprintf(f, "$fshow"); break;
  case T_FREAD: fprintf(f, "$fread"); break;
  case T_EQ: fprintf(f, "$=="); break;
  case T_NE: fprintf(f, "$/="); break;
  case T_LT: fprintf(f, "$<"); break;
  case T_LE: fprintf(f, "$<="); break;
  case T_GT: fprintf(f, "$>"); break;
  case T_GE: fprintf(f, "$>="); break;
  case T_ULT: fprintf(f, "$u<"); break;
  case T_ULE: fprintf(f, "$u<="); break;
  case T_UGT: fprintf(f, "$u>"); break;
  case T_UGE: fprintf(f, "$u>="); break;
  case T_ERROR: fprintf(f, "$error"); break;
  case T_EQUAL: fprintf(f, "$equal"); break;
  case T_COMPARE: fprintf(f, "$compare"); break;
  case T_RNF: fprintf(f, "$rnf"); break;
  case T_SEQ: fprintf(f, "$seq"); break;
  case T_IO_BIND: fprintf(f, "$IO.>>="); break;
  case T_IO_THEN: fprintf(f, "$IO.>>"); break;
  case T_IO_RETURN: fprintf(f, "$IO.return"); break;
  case T_IO_GETCHAR: fprintf(f, "$IO.getChar"); break;
  case T_IO_GETRAW: fprintf(f, "$IO.getRaw"); break;
  case T_IO_PUTCHAR: fprintf(f, "$IO.putChar"); break;
  case T_IO_SERIALIZE: fprintf(f, "$IO.serialize"); break;
  case T_IO_PRINT: fprintf(f, "$IO.print"); break;
  case T_IO_DESERIALIZE: fprintf(f, "$IO.deserialize"); break;
  case T_IO_OPEN: fprintf(f, "$IO.open"); break;
  case T_IO_CLOSE: fprintf(f, "$IO.close"); break;
  case T_IO_FLUSH: fprintf(f, "$IO.flush"); break;
  case T_IO_ISNULLHANDLE: fprintf(f, "$IO.isNullHandle"); break;
  case T_IO_GETARGS: fprintf(f, "$IO.getArgs"); break;
  case T_IO_DROPARGS: fprintf(f, "$IO.dropArgs"); break;
  case T_IO_GETTIMEMILLI: fprintf(f, "$IO.getTimeMilli"); break;
  case T_IO_PERFORMIO: fprintf(f, "$IO.performIO"); break;
  case T_IO_CCALL: fprintf(f, "#%s", ffi_table[GETVALUE(n)].ffi_name); break;
  case T_IO_CATCH: fprintf(f, "$IO.catch"); break;
  case T_ISINT: fprintf(f, "$isInt"); break;
  case T_ISIO: fprintf(f, "$isIO"); break;
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
mkDouble(double d)
{
  NODEPTR n;
  n = alloc_node(T_DOUBLE);
  SETDOUBLEVALUE(n, d);
  return n;
}

static inline NODEPTR
mkNil(void)
{
  return combFalse;
}

static inline NODEPTR
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
mkString(const char *str, size_t len)
{
  NODEPTR n, nc;

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
static inline NODEPTR
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
static inline NODEPTR
indir(NODEPTR n)
{
  while (GETTAG(n) == T_IND)
    n = INDIR(n);
  return n;
}

/* Evaluate to an INT */
static inline value_t
evalint(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (GETTAG(n) != T_INT) {
    fprintf(stderr, "bad tag %d\n", GETTAG(n));
    ERR("evalint");
  }
#endif
  return GETVALUE(n);
}

/* Evaluate to a Double */
static inline double
evaldouble(NODEPTR n)
{
  n = evali(n);
  #if SANITY
  if (GETTAG(n) != T_DOUBLE) {
    fprintf(stderr, "bad tag %d\n", GETTAG(n));
    ERR("evaldouble");
  }
  #endif
  return GETDOUBLEVALUE(n);
}

/* Evaluate to a T_HDL */
FILE *
evalhandleN(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (GETTAG(n) != T_HDL) {
    fprintf(stderr, "bad tag %d\n", GETTAG(n));
    ERR("evalhandle");
  }
#endif
  return HANDLE(n);
}

/* Evaluate to a T_HDL, and check for closed */
FILE *
evalhandle(NODEPTR n)
{
  FILE *hdl;
  hdl = evalhandleN(n);
  if (hdl == 0) {
    fprintf(stderr, "closed file\n");
    ERR("evalhandle");
  }
  return hdl;
}

/* Evaluate a string, returns a newly allocated buffer. */
/* XXX this is cheating, should use continuations */
char *
evalstring(NODEPTR n)
{
  size_t sz = 10000;
  char *p, *name = malloc(sz);
  value_t c;
  NODEPTR x;

  if (!name)
    memerr();
  for (p = name;;) {
    if (p >= name + sz)
      ERR("evalstring too long");
    n = evali(n);
    if (GETTAG(n) == T_K)            /* Nil */
      break;
    else if (GETTAG(n) == T_AP && GETTAG(x = indir(FUN(n))) == T_AP && GETTAG(indir(FUN(x))) == T_O) { /* Cons */
      c = evalint(ARG(x));
      if (c < 0 || c > 127)
	ERR("invalid char");    /* Only allow ASCII */
      *p++ = (char)c;
      n = ARG(n);
    } else {
      ERR("evalstring not Nil/Cons");
    }
  }
  *p = 0;
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
  FILE *f, *g;
  
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
  case T_HDL:
    f = HANDLE(p);
    g = HANDLE(q);
    return f < g ? -1 : f > g ? 1 : 0;
  default:
    return 0;
  }
}

void
rnf_rec(NODEPTR n)
{
 top:
  if (test_bit(marked_bits, n))
    return;
  set_bit(marked_bits, n);
  n = evali(n);
  if (GETTAG(n) == T_AP) {
    rnf_rec(FUN(n));
    n = ARG(n);
    goto top;
  }
}

void
rnf(NODEPTR n)
{
  /* Mark visited nodes to avoid getting stuck in loops. */
  marked_bits = calloc(free_map_nwords, sizeof(bits_t));
  if (!marked_bits)
    memerr();
  rnf_rec(n);
  free(marked_bits);
}

NODEPTR evalio(NODEPTR n);

/* Evaluate a node, returns when the node is in WHNF. */
void
eval(NODEPTR n)
{
  stackptr_t stk = stack_ptr;
  NODEPTR x, y, z, w;
  value_t xi, yi;
  double xd, yd;
  value_t r;
  double rd;
  FILE *hdl;
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
#define SETDOUBLE(n,d) do { SETTAG((n), T_DOUBLE); SETDOUBLEVALUE((n), (d)); } while(0)
#define OPINT2(e)      do { CHECK(2); xi = evalint(ARG(TOP(0))); yi = evalint(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define OPDOUBLE2(e)   do { CHECK(2); xd = evaldouble(ARG(TOP(0))); yd = evaldouble(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define ARITHBIN(op)   do { OPINT2(r = xi op yi); SETINT(n, r); RET; } while(0)
#define ARITHBINU(op)  do { OPINT2(r = (value_t)((uvalue_t)xi op (uvalue_t)yi)); SETINT(n, r); RET; } while(0)
#define FARITHBIN(op)  do { OPDOUBLE2(rd = xd op yd); SETDOUBLE(n, rd); RET; } while(0) // TODO FIXME
#define CMP(op)        do { OPINT2(r = xi op yi); GOIND(r ? combTrue : combFalse); } while(0)
#define CMPF(op)       do { OPDOUBLE2(r = xd op yd); GOIND(r ? combTrue : combFalse); } while(0)
#define CMPU(op)       do { OPINT2(r = (uvalue_t)xi op (uvalue_t)yi); GOIND(r ? combTrue : combFalse); } while(0)

  for(;;) {
    num_reductions++;
#if FASTTAGS
    l = LABEL(n);
#if FASTTAGSCHECK
    if (l < T_IO_BIND) {
      if (l != GETTAG(n)) {
        printf("%lu %lu\n", l, (tag_t)(GETTAG(n)));
        ERR("bad tag");
      }
    }
#endif  /* FASTTAGSCHECK */
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
    case T_DOUBLE: RET;
    case T_HDL:  RET;

    case T_S:    GCCHECK(2); CHKARG3; GOAP(new_ap(x, z), new_ap(y, z));                     /* S x y z = x z (y z) */
    case T_SS:   GCCHECK(3); CHKARG4; GOAP(new_ap(x, new_ap(y, w)), new_ap(z, w));          /* S' x y z w = x (y w) (z w) */
    case T_K:                CHKARG2; GOIND(x);                                             /* K x y = *x */
    case T_A:                CHKARG2; GOIND(y);                                             /* A x y = *y */
    case T_T:                CHKARG2; GOAP(y, x);                                           /* T x y = y x */
    case T_I:                CHKARG1; GOIND(x);                                             /* I x = *x */
    case T_Y:                CHKARG1; GOAP(x, n);                                           /* n@(Y x) = x n */
    case T_B:    GCCHECK(1); CHKARG3; GOAP(x, new_ap(y, z));                                /* B x y z = x (y z) */
    case T_BB:   GCCHECK(2); CHKARG4; GOAP(new_ap(x, y), new_ap(z, w));                     /* B' x y z w = x y (z w) */
    case T_BK:               CHKARG3; GOAP(x, y);                                           /* BK x y z = x y */
    case T_C:    GCCHECK(1); CHKARG3; GOAP(new_ap(x, z), y);                                /* C x y z = x z y */
    case T_CC:   GCCHECK(2); CHKARG4; GOAP(new_ap(x, new_ap(y, w)), z);                     /* C' x y z w = x (y w) z */
    case T_P:    GCCHECK(1); CHKARG3; GOAP(new_ap(z, x), y);                                /* P x y z = z x y */
    case T_O:    GCCHECK(1); CHKARG4; GOAP(new_ap(w, x), y);                                /* O x y z w = w x y */

    case T_ADD:  ARITHBIN(+);
    case T_SUB:  ARITHBIN(-);
    case T_MUL:  ARITHBIN(*);
    case T_QUOT: ARITHBIN(/);
    case T_REM:  ARITHBIN(%);
    case T_SUBR: OPINT2(r = yi - xi); SETINT(n, r); RET;
    case T_UQUOT: ARITHBINU(/);
    case T_UREM:  ARITHBINU(%);

    case T_FADD: FARITHBIN(+);
    case T_FSUB: FARITHBIN(-);
    case T_FMUL: FARITHBIN(*);
    case T_FDIV: FARITHBIN(/);
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
      
      GOIND(mkDouble(xd));

    case T_FSHOW:
      // check that the double exists
      CHECK(1);
      // evaluate it
      xd = evaldouble(ARG(TOP(0)));
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
      NODEPTR s = mkStringC(str);

      // remove the double from the stack
      POP(1);
      n = TOP(-1);
      // update n to be s
      GOIND(s);

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

    case T_ERROR:
      if (cur_handler) {
        /* Pass the string to the handler */
        CHKARG1;
        cur_handler->hdl_exn = x;
        longjmp(cur_handler->hdl_buf, 1);
      } else {
        /* No handler, so just die. */
        CHKARGEV1(msg = evalstring(x));
        fprintf(stderr, "error: %s\n", msg);
        free(msg);
        exit(1);
      }
    case T_SEQ:  CHECK(2); eval(ARG(TOP(0))); POP(2); n = TOP(-1); y = ARG(n); GOIND(y); /* seq x y = eval(x); y */

    case T_EQUAL: r = compare(ARG(TOP(0)), ARG(TOP(1))); POP(2); n = TOP(-1); GOIND(r==0 ? combTrue : combFalse);
    case T_COMPARE: //r = compare(ARG(TOP(0)), ARG(TOP(1))); POP(2); n = TOP(-1); SETINT(n, r); RET;
      r = compare(ARG(TOP(0)), ARG(TOP(1))); POP(2); n = TOP(-1); GOIND(r < 0 ? combLT : r > 0 ? combGT : combEQ);

    case T_RNF: rnf(ARG(TOP(0))); POP(1); n = TOP(-1); GOIND(combUnit);

    case T_IO_ISNULLHANDLE: CHKARGEV1(hdl = evalhandleN(x)); GOIND(hdl == 0 ? combTrue : combFalse);

    case T_IO_PERFORMIO:    CHKARGEV1(x = evalio(x)); GOIND(x);

    case T_IO_BIND:
    case T_IO_THEN:
    case T_IO_RETURN:
    case T_IO_GETCHAR:
    case T_IO_GETRAW:
    case T_IO_PUTCHAR:
    case T_IO_SERIALIZE:
    case T_IO_PRINT:
    case T_IO_DESERIALIZE:
    case T_IO_OPEN:
    case T_IO_CLOSE:
    case T_IO_FLUSH:
    case T_IO_GETARGS:
    case T_IO_DROPARGS:
    case T_IO_GETTIMEMILLI:
    case T_IO_CCALL:
    case T_IO_CATCH:
      RET;

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

    default:
      fprintf(stderr, "bad tag %d\n", GETTAG(n));
      ERR("eval tag");
    }
  }
}

/* This is the interpreter for the IO monad operations. */
/* It takes a monadic expression and returns the unwrapped expression (unevaluated). */
NODEPTR
evalio(NODEPTR n)
{
  stackptr_t stk = stack_ptr;
  NODEPTR f, x;
  int c;
  int hdr;
  FILE *hdl;
  char *name;

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
        /* Use associativity to avoid deep evalio recursion. */
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

      x = evalio(ARG(TOP(1)));  /* first argument, unwrapped */

      /* Do a GC check, make sure we keep the x live */
      GCCHECKSAVE(x, 1);

      f = ARG(TOP(2));          /* second argument, the continuation */
      n = new_ap(f, x);
      POP(3);
      goto top;
    case T_IO_THEN:
      CHECKIO(2);
      (void)evalio(ARG(TOP(1))); /* first argument, unwrapped, ignored */
      n = ARG(TOP(2));          /* second argument, the continuation */
      POP(3);
      goto top;
    case T_IO_RETURN:
      CHECKIO(1);
      n = ARG(TOP(1));
      RETIO(n);
    case T_IO_GETCHAR:
      CHECKIO(1);
      hdl = evalhandle(ARG(TOP(1)));
      GCCHECK(1);
      c = getc(hdl);
      n = mkInt(c);
      RETIO(n);
    case T_IO_GETRAW:
      CHECKIO(0);
      GCCHECK(1);
      c = getraw();
      n = mkInt(c);
      RETIO(n);
    case T_IO_PUTCHAR:
      CHECKIO(2);
      hdl = evalhandle(ARG(TOP(1)));
      c = (int)evalint(ARG(TOP(2)));
      putc(c, hdl);
      RETIO(combUnit);
    case T_IO_PRINT:
      hdr = 0;
      goto ser;
    case T_IO_SERIALIZE:
      hdr = 1;
    ser:
      CHECKIO(2);
      hdl = evalhandle(ARG(TOP(1)));
      x = evali(ARG(TOP(2)));
      //x = ARG(TOP(1));
      print(hdl, x, hdr);
      fprintf(hdl, "\n");
      RETIO(combUnit);
    case T_IO_DESERIALIZE:
      CHECKIO(1);
      hdl = evalhandle(ARG(TOP(1)));
      gc();                     /* parser runs without GC */
      n = parse_FILE(hdl);
      RETIO(n);
    case T_IO_CLOSE:
      CHECKIO(1);
      hdl = evalhandle(ARG(TOP(1)));
      n = evali(ARG(TOP(1)));
      HANDLE(n) = 0;
      fclose(hdl);
      RETIO(combUnit);
    case T_IO_FLUSH:
      CHECKIO(1);
      hdl = evalhandle(ARG(TOP(1)));
      fflush(hdl);
      RETIO(combUnit);
    case T_IO_OPEN:
      CHECKIO(2);
      name = evalstring(ARG(TOP(1)));
      switch (evalint(ARG(TOP(2)))) {
      case 0: hdl = fopen(name, "r"); break;
      case 1: hdl = fopen(name, "w"); break;
      case 2: hdl = fopen(name, "a"); break;
      case 3: hdl = fopen(name, "r+"); break;
      default:
        ERR("IO_OPEN mode");
      }
      free(name);
      GCCHECK(1);
      n = alloc_node(T_HDL);
      HANDLE(n) = hdl;
      RETIO(n);
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
    case T_IO_GETTIMEMILLI:
      CHECKIO(0);
      GCCHECK(1);
      n = alloc_node(T_INT);
      SETVALUE(n, (value_t)(gettime() * 1000));
      RETIO(n);
    case T_IO_CCALL:
      {
        int a = (int)GETVALUE(n);
        funptr_t f = ffi_table[a].ffi_fun;
        value_t r, x, y;
#define INTARG(n) evalint(ARG(TOP(n)))
#define FFIV(n) CHECKIO(n)
#define FFI(n) CHECKIO(n); GCCHECK(1)
        /* This isn't great, but this is MicroHs, so it's good enough. */
        switch (ffi_table[a].ffi_how) {
        case FFI_V:   FFIV(0);                                   (*                               f)();                  RETIO(combUnit);
        case FFI_I:   FFI (0);                               r = (*(value_t (*)(void            ))f)();    n = mkInt(r); RETIO(n);
        case FFI_IV:  FFIV(1); x = INTARG(1);                    (*(void    (*)(value_t         ))f)(x);                 RETIO(combUnit);
        case FFI_II:  FFI (1); x = INTARG(1);                r = (*(value_t (*)(value_t         ))f)(x);   n = mkInt(r); RETIO(n);
        case FFI_IIV: FFIV(1); x = INTARG(1); y = INTARG(2);     (*(void    (*)(value_t, value_t))f)(x,y);               RETIO(combUnit);
        case FFI_III: FFI (1); x = INTARG(1); y = INTARG(2); r = (*(value_t (*)(value_t, value_t))f)(x,y); n = mkInt(r); RETIO(n);
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
          n = evalio(ARG(TOP(1))); /* execute first argument */
          cur_handler = h->hdl_old; /* restore old handler */
          free(h);
          RETIO(n);             /* return result */
        }
      }

    default:
      fprintf(stderr, "bad tag %d\n", GETTAG(n));
      ERR("evalio tag");
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

BFILE *comb_internal;

int
main(int argc, char **argv)
{
  char *fn = 0;
  char **av;
  size_t file_size;
  NODEPTR prog;
  int inrts;
  
  /* MINGW doesn't do buffering right */
  setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
  setvbuf(stderr, NULL, _IONBF, BUFSIZ);

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
          fn = &p[2];
        else
          ERR("Usage: eval [+RTS [-v] [-Hheap-size] [-Kstack-size] [-rFILE] -RTS] arg ...");
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

  if (fn == 0)
    fn = "out.comb";

  init_nodes();
  stack = malloc(sizeof(NODEPTR) * stack_size);
  if (!stack)
    memerr();

  if (comb_internal) {
    prog = parse_top(comb_internal);
  } else {
    prog = parse_file(fn, &file_size);
  }

  PUSH(prog); gc(); prog = TOP(0); POP(1);
  heapoffs_t start_size = num_marked;
  if (verbose > 2) {
    //pp(stdout, prog);
    print(stdout, prog, 1);
  }
  run_time -= gettime();
  NODEPTR res = evalio(prog);
  res = evali(res);
  run_time += gettime();
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
    printf("%"PCOMMA"15"PRIcounter" cells allocated (%"PCOMMA".1f Mbyte/s)\n", num_alloc, num_alloc * NODE_SIZE / run_time / 1000000);
    printf("%"PCOMMA"15"PRIcounter" GCs\n", num_gc);
    printf("%"PCOMMA"15"PRIcounter" max cells used\n", max_num_marked);
    printf("%"PCOMMA"15"PRIcounter" reductions (%"PCOMMA".1f Mred/s)\n", num_reductions, num_reductions / run_time / 1000000);
    printf("%15.2fs total execution time\n", run_time);
    printf("%15.2fs total gc time\n", gc_mark_time);
#if GCRED && 0
    printf(" GC reductions A=%d, K=%d, I=%d, int=%d\n", red_a, red_k, red_i, red_int);
#endif
  }
  exit(0);
}
