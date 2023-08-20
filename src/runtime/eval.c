/* Copyright 2023 Lennart Augustsson
 * See LICENSE file for full license.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <locale.h>
#include <sys/time.h>
#include <ctype.h>

#define FASTTAGS 1
#define UNIONPTR 1

#define VERSION "v2.2\n"

#define HEAP_CELLS 100000
#define STACK_SIZE 10000

#define ERR(s) do { fprintf(stderr, "ERR: %s\n", s); exit(1); } while(0)

enum node_tag { FREE, IND, AP, INT, HDL, S, K, I, B, C, /* 0 - 9 */
                A, Y, SS, BB, CC, P, O, T, ADD, SUB, MUL,  /* 10 - 20 */
                QUOT, REM, SUBR, EQ, NE, LT, LE, GT, GE, ERROR, /* 21-30 */
                IO_BIND, IO_THEN, IO_RETURN, IO_GETCHAR, IO_PUTCHAR, /* 31-35 */
                IO_SERIALIZE, IO_DESERIALIZE, IO_OPEN, IO_CLOSE, IO_ISNULLHANDLE, /* 36-40 */
                IO_STDIN, IO_STDOUT, IO_STDERR, IO_GETARGS, IO_PERFORMIO, /* 41-45 */
                IO_GETTIMEMILLI, IO_PRINT, /* 46 - 47 */
                LAST_TAG,
};

typedef int64_t value_t;

#if NAIVE

/* Naive node representation with minimal unions */
typedef struct node {
  enum node_tag tag;
  union {
    value_t value;
    FILE *file;
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
#define SETVALUE(p,v) (p)->u.value = v
#define FUN(p) (p)->u.s.fun
#define ARG(p) (p)->u.s.arg
#define NEXT(p) FUN(p)
#define INDIR(p) FUN(p)
#define HANDLE(p) (p)->u.file
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((uint64_t)((n) - cells))
node *cells;                 /* All cells */

#elif UNIONPTR

typedef struct node {
  union {
    struct node *uufun;
    uint64_t uutag;             /* LSB=1 indicates that this is a tag, LSB=0 that this is an AP node */
  } ufun;
  union {
    struct node *uuarg;
    value_t uuvalue;
    FILE *uufile;
  } uarg;
} node;
typedef struct node* NODEPTR;
#define NIL 0
#define HEAPREF(i) &cells[(i)]
#define GETTAG(p) ((p)->ufun.uutag & 1 ? (int)((p)->ufun.uutag >> 1) : AP)
#define SETTAG(p,t) do { if (t != AP) (p)->ufun.uutag = ((t) << 1) + 1; } while(0)
#define GETVALUE(p) (p)->uarg.uuvalue
#define SETVALUE(p,v) (p)->uarg.uuvalue = v
#define FUN(p) (p)->ufun.uufun
#define ARG(p) (p)->uarg.uuarg
#define INDIR(p) ARG(p)
#define HANDLE(p) (p)->uarg.uufile
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((uint64_t)((n) - cells))
node *cells;                 /* All cells */

#else

#error "pick a node type"

#endif

int64_t num_reductions = 0;
int64_t num_alloc;
int64_t num_gc = 0;
double gc_scan_time = 0;
double gc_mark_time = 0;
double run_time = 0;

NODEPTR *stack;
int64_t stack_ptr = -1;
#define PUSH(x) do { if (stack_ptr >= stack_size-1) ERR("stack overflow"); stack[++stack_ptr] = (x); } while(0)
#define TOP(n) stack[stack_ptr - (n)]
#define POP(n) stack_ptr -= (n)
#define GCCHECK(n) gc_check((n))

int64_t heap_size = HEAP_CELLS; /* number of heap cells */
int64_t heap_start;             /* first location in heap that needs GC */
int64_t stack_size = STACK_SIZE;

int64_t num_marked;
int64_t max_num_marked = 0;
int64_t num_free;

#define BITS_PER_UINT64 64
uint64_t *free_map;             /* 1 bit per node, 0=free, 1=used */
uint64_t free_map_nwords;
uint64_t next_scan_index;

/* Set FREE bit to 0 */
static inline void mark_used(NODEPTR n)
{
  uint64_t i = LABEL(n);
  if (i < heap_start)
    return;
  //printf("  mark %p\n", n);
  if (i >= free_map_nwords * BITS_PER_UINT64) ERR("mark_used");
  free_map[i / BITS_PER_UINT64] &= ~(1ULL << (i % BITS_PER_UINT64));
}

/* Test if FREE bit is 0 */
static inline int is_marked_used(NODEPTR n)
{
  uint64_t i = LABEL(n);
  if (i < heap_start)
    return 1;
  //printf("ismark %p\n", n);
  if (i >= free_map_nwords * BITS_PER_UINT64) ERR("is_marked_used");;
  return (free_map[i / BITS_PER_UINT64] & (1ULL << (i % BITS_PER_UINT64))) == 0;
}

static inline void mark_all_free(void)
{
  memset(free_map, ~0, free_map_nwords * sizeof(uint64_t));
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
  if (num_free <= 0)
    ERR("alloc_node");

  uint64_t i = next_scan_index / BITS_PER_UINT64;
  int k;
  for(;;) {
    uint64_t word = free_map[i];
    k = ffsl(word);
    if (k)
      break;
    i++;
    if (i >= heap_size)
      ERR("alloc_node free_map");
  }
  uint64_t pos = i * BITS_PER_UINT64 + k - 1; /* first free node */
  NODEPTR n = HEAPREF(pos);
  mark_used(n);
  //printf("%llu %llu %d\n", next_scan_index, pos, t);
  next_scan_index = pos;

  // XXX check if tag is HDL, if so possibly close */
  //  if (TAG(n) != FREE)
  //    ERR("not free");

  //if (MARK(n) == MARKED)
  //  ERR("alloc_node MARKED");

  SETTAG(n, t);
  num_alloc++;
  num_free--;
  return n;
}

static inline NODEPTR
new_ap(NODEPTR f, NODEPTR a)
{
  NODEPTR n = alloc_node(AP);
  FUN(n) = f;
  ARG(n) = a;
  return n;
}

/* Needed during reduction */
NODEPTR combFalse, comTrue, combI, combCons;
NODEPTR combCC, combIOBIND;

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
  { "B", B },
  { "O", O },
  { "K", K },
  { "C'", CC },
  { "C", C },
  { "A", A },
  { "S'", SS },
  { "P", P },
  { "I", I },
  { "S", S },
  { "T", T },
  { "Y", Y },
  { "B'", BB },
  /* primops */
  { "+", ADD },
  { "-", SUB },
  { "*", MUL },
  { "quot", QUOT },
  { "rem", REM },
  { "subtract", SUBR },
  { "==", EQ },
  { "/=", NE },
  { "<", LT },
  { "<=", LE },
  { ">", GT },
  { ">=", GE },
  { "error", ERROR },
  /* IO primops */
  { "IO.>>=", IO_BIND },
  { "IO.>>", IO_THEN },
  { "IO.return", IO_RETURN },
  { "IO.getChar", IO_GETCHAR },
  { "IO.putChar", IO_PUTCHAR },
  { "IO.serialize", IO_SERIALIZE },
  { "IO.print", IO_PRINT },
  { "IO.deserialize", IO_DESERIALIZE },
  { "IO.open", IO_OPEN },
  { "IO.close", IO_CLOSE },
  { "IO.isNullHandle", IO_ISNULLHANDLE },
  { "IO.stdin", IO_STDIN },
  { "IO.stdout", IO_STDOUT },
  { "IO.stderr", IO_STDERR },
  { "IO.getArgs", IO_GETARGS },
  { "IO.getTimeMilli", IO_GETTIMEMILLI },
  { "IO.performIO", IO_PERFORMIO },
};

void
init_nodes(void)
{
  ALLOC_HEAP(heap_size);
  free_map_nwords = (heap_size + BITS_PER_UINT64 - 1) / BITS_PER_UINT64; /* bytes needed for free map */
  free_map = malloc(free_map_nwords * sizeof(uint64_t));

  /* Set up permanent nodes */
  heap_start = 0;
#if !FASTTAGS
  for (int j = 0; j < sizeof primops / sizeof primops[0];j++) {
    NODEPTR n = HEAPREF(heap_start++);
    primops[j].node = n;
    //MARK(n) = MARKED;
    SETTAG(n, primops[j].tag);
    switch (primops[j].tag) {
    case K: combFalse = n; break;
    case A: comTrue = n; break;
    case I: combI = n; break;
    case O: combCons = n; break;
    case CC: combCC = n; break;
    case IO_BIND: combIOBIND = n; break;
    case IO_STDIN:  SETTAG(n, HDL); HANDLE(n) = stdin;  break;
    case IO_STDOUT: SETTAG(n, HDL); HANDLE(n) = stdout; break;
    case IO_STDERR: SETTAG(n, HDL); HANDLE(n) = stderr; break;
    default:
      break;
    }
  }
#else
  for(enum node_tag t = FREE; t < LAST_TAG; t++) {
    NODEPTR n = HEAPREF(heap_start++);
    SETTAG(n, t);
    switch (t) {
    case K: combFalse = n; break;
    case A: comTrue = n; break;
    case I: combI = n; break;
    case O: combCons = n; break;
    case CC: combCC = n; break;
    case IO_BIND: combIOBIND = n; break;
    case IO_STDIN:  SETTAG(n, HDL); HANDLE(n) = stdin;  break;
    case IO_STDOUT: SETTAG(n, HDL); HANDLE(n) = stdout; break;
    case IO_STDERR: SETTAG(n, HDL); HANDLE(n) = stderr; break;
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
  /* Round up heap_start to the next bitword boundary to avoid the permanent nodes. */
  heap_start = (heap_start + BITS_PER_UINT64 - 1) / BITS_PER_UINT64 * BITS_PER_UINT64;

  mark_all_free();

  //for (int64_t i = heap_start; i < heap_size; i++) {
  //  NODEPTR n = HEAPREF(i);
  //  MARK(n) = NOTMARKED;
  //  TAG(n) = FREE;
  //}
  num_free = heap_size - heap_start;
}

#if GCRED
int red_t, red_k, red_i;
#endif

/* Mark all used nodes reachable from *np */
void
mark(NODEPTR *np)
{
  NODEPTR n = *np;

#if GCRED
  top:
#endif
  if (GETTAG(n) == IND) {
    int loop = 0;
    /* Skip indirections, and redirect start pointer */
    while (GETTAG(n) == IND) {
      //      printf("*"); fflush(stdout);
      n = INDIR(n);
      if (loop++ > 10000000) {
        printf("%p %p %p\n", n, INDIR(n), INDIR(INDIR(n)));
        ERR("IND loop");
      }
    }
    //    if (loop)
    //      printf("\n");
    *np = n;
  }
  if (is_marked_used(n)) {
#if SANITY
    if (MARK(n) != MARKED)
      ERR("not marked");
#endif
    return;
  }
#if SANITY
  if (MARK(n) == MARKED) {
    printf("%p %llu\n", n, LABEL(n));
    ERR("marked");
  }
  MARK(n) = MARKED;
#endif
  num_marked++;
  mark_used(n);
#if GCRED
  /* This is really only fruitful just after parsing.  It can be removed. */
  if (GETTAG(n) == AP && GETTAG(FUN(n)) == AP && GETTAG(FUN(FUN(n))) == T) {
    /* Do the T x y --> y reduction */
    NODEPTR y = ARG(n);
    SETTAG(n, IND);
    INDIR(n) = y;
    red_t++;
    goto top;
  }
  if (GETTAG(n) == AP && GETTAG(FUN(n)) == AP && GETTAG(FUN(FUN(n))) == K) {
    /* Do the K x y --> x reduction */
    NODEPTR x = ARG(FUN(n));
    SETTAG(n, IND);
    INDIR(n) = x;
    red_k++;
    goto top;
  }
  if (GETTAG(n) == AP && GETTAG(FUN(n)) == I) {
    /* Do the I x --> x reduction */
    NODEPTR x = ARG(n);
    SETTAG(n, IND);
    INDIR(n) = x;
    red_i++;
    goto top;
  }
#endif
  if (GETTAG(n) == AP) {
    mark(&FUN(n));
    mark(&ARG(n));
  }
}

/* Scan for unmarked nodes and put them on the free list. */
void
scan(void)
{
#if SANITY
  for(int64_t i = heap_start; i < heap_size; i++) {
    NODEPTR n = HEAPREF(i);
    if (MARK(n) == NOTMARKED) {
      if (GETTAG(n) == HDL && HANDLE(n) != 0 &&
         HANDLE(n) != stdin && HANDLE(n) != stdout && HANDLE(n) != stderr) {
        /* A FILE* has become garbage, so close it. */
        fclose(HANDLE(n));
      }
      SETTAG(n, FREE);
      //      NEXT(n) = next_free;
      //      next_free = n;
    } else {
      MARK(n) = NOTMARKED;
    }
  }
#endif
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
  for (int64_t i = 0; i <= stack_ptr; i++)
    mark(&stack[i]);
  t = gettime();
  gc_mark_time += t;
  if (verbose > 1)
    fprintf(stderr, "gc scan\n");
  gc_scan_time -= t;
  scan();
  gc_scan_time += gettime();

  if (num_marked > max_num_marked)
    max_num_marked = num_marked;
  num_free = heap_size - heap_start - num_marked;
  if (num_free < heap_size / 50)
    ERR("heap exhausted");
  if (verbose > 1)
    fprintf(stderr, "gc done, %"PRId64" free\n", num_free);
}

/* Check that there are k nodes available, if not then GC. */
void
gc_check(int k)
{
  if (k < num_free)
    return;
  if (verbose > 1)
    fprintf(stderr, "gc_check: %d\n", k);
  gc();
}

/* If the next input character is c, then consume it, else leave it alone. */
int
gobble(FILE *f, int c)
{
  int d = getc(f);
  if (c == d) {
    return 1;
  } else {
    ungetc(d, f);
    return 0;
  }
}

int64_t
parse_int(FILE *f)
{
  int64_t i = 0;
  int c = getc(f);
  for(;;) {
    i = i * 10 + c - '0';
    c = getc(f);
    if (c < '0' || c > '9') {
      ungetc(c, f);
      break;
    }
  }
  return i;
}

/* Table of labelled nodes for sharing during parsing. */
struct shared_entry {
  int64_t label;
  NODEPTR node;                 /* NIL indicates unused */
} *shared_table;
int64_t shared_table_size;

/* Look for the label in the table.
 * If it's found, return the node.
 * If not found, return the first empty entry.
*/
NODEPTR *
find_label(int64_t label)
{
  int hash = label % shared_table_size;
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
parse(FILE *f)
{
  NODEPTR r;
  NODEPTR *nodep;
  int64_t l;
  value_t i;
  value_t neg;
  int c;
  char buf[80];                 /* store names of primitives. */

  c = getc(f);
  if (c < 0) ERR("parse EOF");
  switch (c) {
  case '(' :
    /* application: (f a) */
    r = alloc_node(AP);
    FUN(r) = parse(f);
    if (!gobble(f, ' ')) ERR("parse ' '");
    ARG(r) = parse(f);
    if (!gobble(f, ')')) ERR("parse ')'");
    return r;
  case '-':
    c = getc(f);
    if ('0' <= c && c <= '9') {
      neg = -1;
      goto number;
    } else {
      ERR("got -");
    }
  case '0':case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
    /* integer [0-9]+*/
    neg = 1;
  number:
    ungetc(c, f);
    i = neg * parse_int(f);
    r = alloc_node(INT);
    SETVALUE(r, i);
    return r;
  case '$':
    /* A primitive, keep getting char's until end */
    for (int j = 0;;) {
      c = getc(f);
      if (c == ' ' || c == ')') {
        ungetc(c, f);
        buf[j] = 0;
        break;
      }
      buf[j++] = c;
    }
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
      *nodep = alloc_node(IND);
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
      *nodep = alloc_node(IND);
      INDIR(*nodep) = NIL;
    } else {
      /* Sanity check */
      if (INDIR(*nodep) != NIL) ERR("shared != NIL");
    }
    r = parse(f);
    INDIR(*nodep) = r;
    return r;
  default:
    fprintf(stderr, "parse '%c'\n", c);
    ERR("parse default");
  }
}

void
checkversion(FILE *f)
{
  char *p = VERSION;
  int c;

  while ((c = *p++)) {
    if (c != fgetc(f))
      ERR("version mismatch");
  }
}

/* Parse a file */
NODEPTR
parse_top(FILE *f)
{
  checkversion(f);
  int64_t numLabels = parse_int(f);
  if (!gobble(f, '\n'))
    ERR("size parse");
  shared_table_size = 3 * numLabels; /* sparsely populated hashtable */
  shared_table = malloc(shared_table_size * sizeof(struct shared_entry));
  for(int64_t i = 0; i < shared_table_size; i++)
    shared_table[i].node = NIL;
  NODEPTR n = parse(f);
  free(shared_table);
  return n;
}

void printrec(FILE *f, NODEPTR n);

int64_t num_shared;

/* Two bits per node: marked, shared
 * 0, 0   -- not visited
 * 1, 0   -- visited once
 * 1, 1   -- visited more than once
 * 0, 1   -- printed
 */
uint64_t *marked_bits;
uint64_t *shared_bits;
static inline void set_bit(uint64_t *bits, NODEPTR n)
{
  uint64_t i = LABEL(n);
  bits[i / BITS_PER_UINT64] |= (1ULL << (i % BITS_PER_UINT64));
}
static inline void clear_bit(uint64_t *bits, NODEPTR n)
{
  uint64_t i = LABEL(n);
  bits[i / BITS_PER_UINT64] &= ~(1ULL << (i % BITS_PER_UINT64));
}
static inline uint64_t test_bit(uint64_t *bits, NODEPTR n)
{
  uint64_t i = LABEL(n);
  return bits[i / BITS_PER_UINT64] & (1ULL << (i % BITS_PER_UINT64));
}

/* Mark all reachable nodes, when a marked node is reached, mark it as shared. */
void
find_sharing(NODEPTR n)
{
  while (GETTAG(n) == IND)
    n = INDIR(n);
  //printf("find_sharing %p %llu ", n, LABEL(n));
  if (GETTAG(n) == AP) {
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
      /* Mark as shared, and recurse */
      //printf("unmarked\n");
      set_bit(marked_bits, n);
      find_sharing(FUN(n));
      find_sharing(ARG(n));
    }
  } else {
    /* Not an application */
    //printf("not AP\n");
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
      fprintf(f, ":%"PRIu64" ", LABEL(n));
      clear_bit(marked_bits, n);  /* mark as printed */
    } else {
      /* This node has already been printed, so just use a reference. */
      fprintf(f, "_%"PRIu64, LABEL(n));
      return;
    }
  }

  switch (GETTAG(n)) {
  case IND: /*putc('*', f);*/ printrec(f, INDIR(n)); break;
  case AP:
    fputc('(', f);
    printrec(f, FUN(n));
    fputc(' ', f);
    printrec(f, ARG(n));
    fputc(')', f);
    break;
  case INT: fprintf(f, "%"PRId64, GETVALUE(n)); break;
  case HDL:
    if (HANDLE(n) == stdin)
      fprintf(f, "$IO.stdin");
    else if (HANDLE(n) == stdout)
      fprintf(f, "$IO.stdout");
    else if (HANDLE(n) == stderr)
      fprintf(f, "$IO.stderr");
    else
      ERR("Cannot serialize handles");
    break;
  case S: fprintf(f, "$S"); break;
  case K: fprintf(f, "$K"); break;
  case I: fprintf(f, "$I"); break;
  case C: fprintf(f, "$C"); break;
  case B: fprintf(f, "$B"); break;
  case A: fprintf(f, "$A"); break;
  case Y: fprintf(f, "$Y"); break;
  case P: fprintf(f, "$P"); break;
  case O: fprintf(f, "$O"); break;
  case SS: fprintf(f, "$S'"); break;
  case BB: fprintf(f, "$B'"); break;
  case CC: fprintf(f, "$C'"); break;
  case ADD: fprintf(f, "$+"); break;
  case SUB: fprintf(f, "$-"); break;
  case MUL: fprintf(f, "$*"); break;
  case QUOT: fprintf(f, "$quot"); break;
  case REM: fprintf(f, "$rem"); break;
  case SUBR: fprintf(f, "$subtract"); break;
  case EQ: fprintf(f, "$=="); break;
  case NE: fprintf(f, "$/="); break;
  case LT: fprintf(f, "$<"); break;
  case LE: fprintf(f, "$<="); break;
  case GT: fprintf(f, "$>"); break;
  case GE: fprintf(f, "$>="); break;
  case ERROR: fprintf(f, "$error"); break;
  case IO_BIND: fprintf(f, "$IO.>>="); break;
  case IO_THEN: fprintf(f, "$IO.>>"); break;
  case IO_RETURN: fprintf(f, "$IO.return"); break;
  case IO_GETCHAR: fprintf(f, "$IO.getChar"); break;
  case IO_PUTCHAR: fprintf(f, "$IO.putChar"); break;
  case IO_SERIALIZE: fprintf(f, "$IO.serialize"); break;
  case IO_PRINT: fprintf(f, "$IO.print"); break;
  case IO_DESERIALIZE: fprintf(f, "$IO.deserialize"); break;
  case IO_OPEN: fprintf(f, "$IO.open"); break;
  case IO_CLOSE: fprintf(f, "$IO.close"); break;
  case IO_ISNULLHANDLE: fprintf(f, "$IO.isNullHandle"); break;
  case IO_GETARGS: fprintf(f, "$IO.getArgs"); break;
  case IO_GETTIMEMILLI: fprintf(f, "$IO.getTimeMilli"); break;
  case IO_PERFORMIO: fprintf(f, "$IO.performIO"); break;
  default: ERR("print tag");
  }
}

/* Serialize a graph to file. */
void
print(FILE *f, NODEPTR n, int header)
{
  num_shared = 0;
  marked_bits = calloc(free_map_nwords, sizeof(uint64_t));
  shared_bits = calloc(free_map_nwords, sizeof(uint64_t));
  if (!marked_bits || !shared_bits)
    ERR("print memory");
  find_sharing(n);
  if (header)
    fprintf(f, "%s%"PRId64"\n", VERSION, num_shared);
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

void eval(NODEPTR n);

/* Evaluate and skip indirections. */
NODEPTR
evali(NODEPTR n)
{
  /* Need to push and pop in case GC happens */
  PUSH(n);
  eval(n);
  n = TOP(0);
  POP(1);
  while (GETTAG(n) == IND)
    n = INDIR(n);
  return n;
}

/* Follow indirections */
NODEPTR
indir(NODEPTR n)
{
  while (GETTAG(n) == IND)
    n = INDIR(n);
  return n;
}

/* Evaluate to an INT */
value_t
evalint(NODEPTR n)
{
  n = evali(n);
  if (GETTAG(n) != INT) {
    fprintf(stderr, "bad tag %d\n", GETTAG(n));
    ERR("evalint");
  }
  return GETVALUE(n);
}

/* Evaluate to a HDL */
FILE *
evalhandleN(NODEPTR n)
{
  n = evali(n);
  if (GETTAG(n) != HDL) {
    fprintf(stderr, "bad tag %d\n", GETTAG(n));
    ERR("evalhandle");
  }
  return HANDLE(n);
}

/* Evaluate to a HDL, and check for closed */
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
    ERR("evalstring malloc");
  for (p = name;;) {
    if (p >= name + sz)
      ERR("evalstring too long");
    n = evali(n);
    if (GETTAG(n) == K)            /* Nil */
      break;
    else if (GETTAG(n) == AP && GETTAG(x = indir(FUN(n))) == AP && GETTAG(indir(FUN(x))) == O) { /* Cons */
      c = evalint(ARG(x));
      if (c < 0 || c > 127)
	ERR("invalid char");
      *p++ = (char)c;
      n = ARG(n);
    } else {
      ERR("evalstring not Nil/Cons");
    }
  }
  *p = 0;
  return name;
}

NODEPTR evalio(NODEPTR n);

/* Evaluate a node, returns when the node is in WHNF. */
void
eval(NODEPTR n)
{
  int64_t stk = stack_ptr;
  NODEPTR f, g, x, k, y;
  value_t r;
  FILE *hdl;
  int64_t l;

/* Reset stack pointer and return. */
#define RET do { stack_ptr = stk; return; } while(0)
/* Check that there are at least n arguments, return if not. */
#define CHECK(n) do { if (stack_ptr - stk <= (n)) RET; } while(0)

#define SETIND(n, x) do { SETTAG((n), IND); INDIR((n)) = (x); } while(0)
#define GOTO num_reductions++; goto

  PUSH(n);
  for(;;) {
    num_reductions++;
    l = LABEL(n);
#if FASTTAG
    if (l < IO_BIND) {
      if (l != GETTAG(n)) {
        printf("%lu %lu\n", l, (uint64_t)(GETTAG(n)));
        ERR("bad tag");
      }
    }
#endif
    enum node_tag tag = l < IO_BIND ? l : GETTAG(n);
    switch (tag) {
    ind:
    case IND:
      n = INDIR(n);
      TOP(0) = n;
      break;
    ap:
    case AP:
      n = FUN(n);
      PUSH(n);
      break;
    case INT:
    case HDL:
      RET;
    case S:                     /* S f g x = f x (g x) */
      CHECK(3);
      GCCHECK(2);
      f = ARG(TOP(1));
      g = ARG(TOP(2));
      x = ARG(TOP(3));
      POP(3);
      n = TOP(0);
      FUN(n) = new_ap(f, x);
      ARG(n) = new_ap(g, x);
      GOTO ap;
      break;
    case SS:                    /* S' k f g x = k (f x) (g x) */
      CHECK(4);
      GCCHECK(3);
      k = ARG(TOP(1));
      f = ARG(TOP(2));
      g = ARG(TOP(3));
      x = ARG(TOP(4));
      POP(4);
      n = TOP(0);
      FUN(n) = new_ap(k, new_ap(f, x));
      ARG(n) = new_ap(g, x);
      GOTO ap;
      break;
    case K:                     /* K x y = * x */
      CHECK(2);
      x = ARG(TOP(1));
      POP(2);
      n = TOP(0);
      SETIND(n, x);
      GOTO ind;
    case A:                     /* A x y = * y */
      CHECK(2);
      y = ARG(TOP(2));
      POP(2);
      n = TOP(0);
      SETIND(n, y);
      GOTO ind;
    case T:                     /* T x y = y x */
      CHECK(2);
      x = ARG(TOP(1));
      y = ARG(TOP(2));
      POP(2);
      n = TOP(0);
      FUN(n) = y;
      ARG(n) = x;
      GOTO ap;
    case I:                     /* I x = * x */
      CHECK(1);
      x = ARG(TOP(1));
      POP(1);
      n = TOP(0);
      SETIND(n, x);
      GOTO ind;
    case Y:                     /* yf@(Y f) = f yf */
      CHECK(1);
      f = ARG(TOP(1));
      POP(1);
      n = TOP(0);
      FUN(n) = f;
      ARG(n) = n;
      GOTO ap;
    case B:                     /* B f g x = f (g x) */
      CHECK(3);
      GCCHECK(1);
      f = ARG(TOP(1));
      g = ARG(TOP(2));
      x = ARG(TOP(3));
      POP(3);
      n = TOP(0);
      FUN(n) = f;
      ARG(n) = new_ap(g, x);
      GOTO ap;
      break;
    case C:                     /* C f g x = f x g */
      CHECK(3);
      GCCHECK(1);
      f = ARG(TOP(1));
      g = ARG(TOP(2));
      x = ARG(TOP(3));
      POP(3);
      n = TOP(0);
      FUN(n) = new_ap(f, x);
      ARG(n) = g;
      GOTO ap;
    case CC:                    /* C' k f g x = k (f x) g */
      CHECK(4);
      GCCHECK(2);
      k = ARG(TOP(1));
      f = ARG(TOP(2));
      g = ARG(TOP(3));
      x = ARG(TOP(4));
      POP(4);
      n = TOP(0);
      FUN(n) = new_ap(k, new_ap(f, x));
      ARG(n) = g;
      GOTO ap;
    case P:                     /* P x y f = f x y */
      CHECK(3);
      GCCHECK(1);
      x = ARG(TOP(1));
      y = ARG(TOP(2));
      f = ARG(TOP(3));
      POP(3);
      n = TOP(0);
      FUN(n) = new_ap(f, x);
      ARG(n) = y;
      GOTO ap;
    case O:                     /* O x y g f = f x y */
      CHECK(4);
      GCCHECK(1);
      x = ARG(TOP(1));
      y = ARG(TOP(2));
      f = ARG(TOP(4));
      POP(4);
      n = TOP(0);
      FUN(n) = new_ap(f, x);
      ARG(n) = y;
      GOTO ap;

#define SETINT(n,r) do { SETTAG((n), INT); SETVALUE((n), (r)); } while(0)
#define ARITH2(op) do { CHECK(2); r = evalint(ARG(TOP(1))) op evalint(ARG(TOP(2))); n = TOP(2); SETINT(n, r); POP(2); } while(0)
    case ADD:
      ARITH2(+);
      RET;
    case SUB:
      ARITH2(-);
      RET;
    case MUL:
      ARITH2(*);
      RET;
    case QUOT:
      ARITH2(/);
      RET;
    case REM:
      ARITH2(%);
      RET;
    case SUBR:
      /* - with arguments reversed */
      CHECK(2); r = evalint(ARG(TOP(2))) - evalint(ARG(TOP(1))); n = TOP(2); SETINT(n, r); POP(2);
      RET;

#define CMP(op) do { CHECK(2); r = evalint(ARG(TOP(1))) op evalint(ARG(TOP(2))); n = TOP(2); SETIND(n, r ? comTrue : combFalse); POP(2); } while(0)
    case EQ:
      CMP(==);
      break;
    case NE:
      CMP(!=);
      break;
    case LT:
      CMP(<);
      break;
    case LE:
      CMP(<=);
      break;
    case GT:
      CMP(>);
      break;
    case GE:
      CMP(>=);
      break;
    case ERROR:
      CHECK(1);
      x = ARG(TOP(1));
      char *msg = evalstring(x);
      fprintf(stderr, "error: %s\n", msg);
      exit(1);
    case IO_ISNULLHANDLE:
      CHECK(1);
      hdl = evalhandleN(ARG(TOP(1)));
      n = TOP(1);
      SETIND(n, hdl == 0 ? comTrue : combFalse);
      POP(1);
      break;
    case IO_BIND:
    case IO_THEN:
    case IO_RETURN:
    case IO_GETCHAR:
    case IO_PUTCHAR:
    case IO_SERIALIZE:
    case IO_PRINT:
    case IO_DESERIALIZE:
    case IO_OPEN:
    case IO_CLOSE:
    case IO_GETARGS:
    case IO_GETTIMEMILLI:
      RET;
    case IO_PERFORMIO:
      CHECK(1);
      x = evalio(ARG(TOP(1)));
      n = TOP(1);
      SETIND(n, x);
      POP(1);
      GOTO ind;
    default:
      fprintf(stderr, "bad tag %d\n", GETTAG(n));
      ERR("eval tag");
    }
  }
}

NODEPTR
mkNil(void)
{
  return combFalse;
}

NODEPTR
mkCons(NODEPTR x, NODEPTR xs)
{
  return new_ap(new_ap(combCons, x), xs);
}

NODEPTR
mkString(const char *str)
{
  NODEPTR n, nc;

  n = mkNil();
  for(int i = strlen(str)-1; i >= 0; i--) {
    nc = alloc_node(INT);
    SETVALUE(nc, str[i]);
    n = mkCons(nc, n);
  }
  return n;
}

/* This is the interpreter for the IO monad operations. */
/* It takes a monadic expression and returns the unwrapped expression (unevaluated). */
NODEPTR
evalio(NODEPTR n)
{
  int64_t stk = stack_ptr;
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
    case IND:
      n = INDIR(n);
      TOP(0) = n;
      break;
    case AP:
      n = FUN(n);
      PUSH(n);
      break;

    case IO_BIND:
      CHECKIO(2);
      {
        /* Use associativity to avoid deep evalio recursion. */
        /* (m >>= g) >>= h      ===  m >>= (\ x -> g x >>= h) */
        /* BIND ((BIND m) g) h  ===  BIND m (\ x -> BIND (g x) h) == (BIND m) (((C' BIND) g) h)*/
        NODEPTR bm;
        NODEPTR bmg = evali(ARG(TOP(1)));
        GCCHECKSAVE(bmg, 4);
        if (GETTAG(bmg) == AP && GETTAG(bm = indir(FUN(bmg))) == AP && GETTAG(indir(FUN(bm))) == IO_BIND) {
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
    case IO_THEN:
      CHECKIO(2);
      (void)evalio(ARG(TOP(1))); /* first argument, unwrapped, ignored */
      n = ARG(TOP(2));          /* second argument, the continuation */
      POP(3);
      goto top;
    case IO_RETURN:
      CHECKIO(1);
      n = ARG(TOP(1));
      POP(1);
      RETIO(n);
    case IO_GETCHAR:
      CHECKIO(1);
      hdl = evalhandle(ARG(TOP(1)));
      GCCHECK(1);
      c = getc(hdl);
      n = alloc_node(INT);
      SETVALUE(n, c);
      RETIO(n);
    case IO_PUTCHAR:
      CHECKIO(2);
      hdl = evalhandle(ARG(TOP(1)));
      c = evalint(ARG(TOP(2)));
      putc(c, hdl);
      RETIO(combI);
    case IO_PRINT:
      hdr = 0;
      goto ser;
    case IO_SERIALIZE:
      hdr = 1;
    ser:
      CHECKIO(2);
      hdl = evalhandle(ARG(TOP(1)));
      x = evali(ARG(TOP(2)));
      //x = ARG(TOP(1));
      print(hdl, x, hdr);
      fprintf(hdl, "\n");
      RETIO(combI);
    case IO_DESERIALIZE:
      CHECKIO(1);
      hdl = evalhandle(ARG(TOP(1)));
      gc();                     /* parser runs without GC */
      n = parse_top(hdl);
      RETIO(n);
    case IO_CLOSE:
      CHECKIO(1);
      hdl = evalhandle(ARG(TOP(1)));
      n = evali(ARG(TOP(1)));
      HANDLE(n) = 0;
      fclose(hdl);
      RETIO(combI);
    case IO_OPEN:
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
      n = alloc_node(HDL);
      HANDLE(n) = hdl;
      RETIO(n);
    case IO_GETARGS:
      CHECKIO(0);
      {
      /* compute total number of characters */
        int size = 0;
        for(int i = 0; i < glob_argc; i++)
          size += strlen(glob_argv[i]);
        /* Each character will need a CHAR node and a CONS node, a CONS uses 2 AP nodes */
        size *= (1 + 2);
        /* And each string will need a NIL */
        size += glob_argc;
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
          n = mkCons(mkString(glob_argv[i]), n);
        }
      }
      RETIO(n);
    case IO_GETTIMEMILLI:
      CHECKIO(0);
      GCCHECK(1);
      n = alloc_node(INT);
      SETVALUE(n, (int64_t)(gettime() * 1000));
      RETIO(n);
    default:
      fprintf(stderr, "bad tag %d\n", GETTAG(n));
      ERR("evalio tag");
    }
  }
}

int64_t
memsize(const char *p)
{
  int64_t n = atoi(p);
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

int
main(int argc, char **argv)
{
  char *fn = 0;
  int64_t file_size;
  
  argc--, argv++;
  while (argc > 0 && argv[0][0] == '-') {
    argc--;
    argv++;
    if (strcmp(argv[-1], "-v") == 0)
      verbose++;
    else if (strncmp(argv[-1], "-H", 2) == 0)
      heap_size = memsize(&argv[-1][2]);
    else if (strncmp(argv[-1], "-K", 2) == 0)
      stack_size = memsize(&argv[-1][2]);
    else if (strncmp(argv[-1], "-r", 2) == 0)
      fn = &argv[-1][2];
    else if (strcmp(argv[-1], "--") == 0)
      break;
    else
      ERR("Usage: eval [-v] [-Hheap-size] [-Kstack-size] [-rFILE] [-- arg ...]");
  }
  glob_argc = argc;
  glob_argv = argv;

  if (fn == 0)
    fn = "out.comb";

  init_nodes();
  stack = malloc(sizeof(NODEPTR) * stack_size);
  if (!stack)
    ERR("stack alloc");
  FILE *f = fopen(fn, "r");
  if (!f)
    ERR("file not found");
  NODEPTR prog = parse_top(f);
  file_size = ftell(f);
  fclose(f);
  PUSH(prog); gc(); prog = TOP(0); POP(1);
  int64_t start_size = num_marked;
  if (verbose > 2) {
    //pp(stdout, prog);
    print(stdout, prog, 1);
  }
  run_time -= gettime();
  NODEPTR res = evalio(prog);
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
      printf("node size=%"PRId64", heap size bytes=%"PRId64"\n", (int64_t)NODE_SIZE, heap_size * NODE_SIZE);
    }
    setlocale(LC_NUMERIC, "");
    printf("%'15"PRId64" combinator file size\n", file_size);
    printf("%'15"PRId64" cells at start\n", start_size);
    printf("%'15"PRId64" heap size\n", heap_size);
    printf("%'15"PRId64" cells allocated\n", num_alloc);
    printf("%'15"PRId64" GCs\n", num_gc);
    printf("%'15"PRId64" max cells used\n", max_num_marked);
    printf("%'15"PRId64" reductions\n", num_reductions);
    printf("%15.2fs total execution time\n", run_time);
    printf("%15.2fs total gc time\n", gc_mark_time + gc_scan_time);
    printf("    %15.2fs mark time\n", gc_mark_time);
    printf("    %15.2fs scan time\n", gc_scan_time);
#if GCRED
    printf(" GC reductions T=%d, K=%d, I=%d\n", red_t, red_k, red_i);
#endif
  }
  exit(0);
}
