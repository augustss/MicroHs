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

#define VERSION "v2.0\n"

/* Node representation:
 * NODE_NAIVE   all fields in a struct, regular pointers
 * NODE_INDEX   use 32 bit indices instead of pointers
 * NODE_SPLIT   split flags, funs, and args
 */
#define NODE_NAIVE

#define HEAP_CELLS 100000
#define STACK_SIZE 10000

#define ERR(s) do { fprintf(stderr, "ERR: %s\n", s); exit(1); } while(0)

enum node_mark { NOTMARKED, MARKED, SHARED, PRINTED }; /* SHARED, PRINTED only for printing */
enum node_tag { FREE, IND, AP, INT, HDL, S, K, I, B, C, T, Y, SS, BB, CC, P, O,
                ADD, SUB, MUL, QUOT, REM, SUBR, EQ, NE, LT, LE, GT, GE, ERROR,
                IO_BIND, IO_THEN, IO_RETURN, IO_GETCHAR, IO_PUTCHAR,
                IO_SERIALIZE, IO_DESERIALIZE,
                IO_OPEN, IO_CLOSE, IO_ISNULLHANDLE,
                IO_STDIN, IO_STDOUT, IO_STDERR,
                IO_GETARGS, IO_PERFORMIO, IO_GETTIMEMILLI, IO_PRINT,
};

typedef int64_t value_t;

#if defined(NODE_NAIVE)
/* Naive node representation with minimal unions */
typedef struct node {
  enum node_mark mark;
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
#define TAG(p) (p)->tag
#define GETVALUE(p) (p)->u.value
#define SETVALUE(p,v) (p)->u.value = v
#define FUN(p) (p)->u.s.fun
#define ARG(p) (p)->u.s.arg
#define NEXT(p) FUN(p)
#define INDIR(p) FUN(p)
#define HANDLE(p) (p)->u.file
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((int)((n) - cells))
node *cells;                 /* All cells */

#elif defined(NODE_INDEX)
/* Naive node representation with minimal unions */
typedef u_int32_t NODEPTR;
typedef struct node {
  enum node_mark mark;
  enum node_tag tag;
  union {
    value_t value;
    FILE *file;
    struct {
      NODEPTR fun;
      NODEPTR arg;
    } s;
  } u;
} node;
#define NIL (-1)
#define HEAPREF(i) (i)
#define MARK(p) cells[p].mark
#define TAG(p) cells[p].tag
#define GETVALUE(p) cells[p].u.value
#define SETVALUE(p,v) cells[p].u.value = v
#define FUN(p) cells[p].u.s.fun
#define ARG(p) cells[p].u.s.arg
#define NEXT(p) FUN(p)
#define INDIR(p) FUN(p)
#define HANDLE(p) cells[p].u.file
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) (n)
node *cells;                 /* All cells */

#elif defined(NODE_SPLIT)
struct node_flags {
  enum node_mark xmark:1;
  enum node_tag xtag:7;
};
typedef u_int32_t NODEPTR;
#define NIL ((NODEPTR)(~0))
#define HEAPREF(i) (i)
#define MARK(p)  ((struct node_flags *)(&flagsmem[p]))->xmark
#define TAG(p)   ((struct node_flags *)(&flagsmem[p]))->xtag
#define GETVALUE(p) ((value_t)funs[p])
#define SETVALUE(p,v) funs[p] = (NODEPTR)(v)
#define FUN(p)   funs[p]
#define ARG(p)   args[p]
#define NEXT(p)  FUN(p)
#define INDIR(p) FUN(p)
#define NODE_SIZE (sizeof(u_int8_t) + 2*sizeof(NODEPTR))
#define ALLOC_HEAP(n) do { flagsmem = calloc(n, sizeof(u_int8_t)); funs = calloc(n, sizeof(NODEPTR)); args = calloc(n, sizeof(NODEPTR)); } while(0)
#define LABEL(n) ((int)(n))
u_int8_t *flagsmem;
NODEPTR *funs;
NODEPTR *args;

#else
#error "Pick a node representation"
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
NODEPTR next_free;              /* Free list */
int64_t stack_size = STACK_SIZE;

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

NODEPTR
alloc_node(enum node_tag t)
{
  NODEPTR n = next_free;
  if (n == NIL) {
    ERR("out of memory");
  }
  if (TAG(n) != FREE)
    abort();
  next_free = NEXT(n);
  TAG(n) = t;
  num_alloc++;
  return n;
}

NODEPTR
new_ap(NODEPTR f, NODEPTR a)
{
  NODEPTR n = alloc_node(AP);
  FUN(n) = f;
  ARG(n) = a;
  return n;
}

/* Needed during reduction */
NODEPTR combK, combT, combI, combO;
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
  { "T", T },
  { "S'", SS },
  { "P", P },
  { "I", I },
  { "S", S },
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

  /* Set up permanent nodes */
  heap_start = 0;
  for (int j = 0; j < sizeof primops / sizeof primops[0];j++) {
    NODEPTR n = HEAPREF(heap_start++);
    primops[j].node = n;
    MARK(n) = NOTMARKED;
    TAG(n) = primops[j].tag;
    switch (primops[j].tag) {
    case K: combK = n; break;
    case T: combT = n; break;
    case I: combI = n; break;
    case O: combO = n; break;
    case CC: combCC = n; break;
    case IO_BIND: combIOBIND = n; break;
    case IO_STDIN:  TAG(n) = HDL; HANDLE(n) = stdin;  break;
    case IO_STDOUT: TAG(n) = HDL; HANDLE(n) = stdout; break;
    case IO_STDERR: TAG(n) = HDL; HANDLE(n) = stderr; break;
    default:
      break;
    }
  }

  /* Set up free list */
  next_free = NIL;
  for (int64_t i = heap_start; i < heap_size; i++) {
    NODEPTR n = HEAPREF(i);
    MARK(n) = NOTMARKED;
    TAG(n) = FREE;
    NEXT(n) = next_free;
    next_free = n;
  }
}

int64_t num_marked;

/* Mark all used nodes reachable from *np */
void
mark(NODEPTR *np)
{
  NODEPTR n = *np;

 top:
  if (TAG(n) == IND) {
    /* Skip indirections, and redirect start pointer */
    while (TAG(n) == IND) {
      n = INDIR(n);
    }
    *np = n;
  }
  if (MARK(n) == MARKED)
    return;
  num_marked++;
  MARK(n) = MARKED;
#if 1
  /* This is really only fruitful just after parsing.  It can be removed. */
  if (TAG(n) == AP && TAG(FUN(n)) == AP && TAG(FUN(FUN(n))) == T) {
    /* Do the T x y --> y reduction */
    NODEPTR y = ARG(n);
    TAG(n) = IND;
    INDIR(n) = y;
    goto top;
  }
#endif
  if (TAG(n) == AP) {
    mark(&FUN(n));
    mark(&ARG(n));
  }
}

/* Scan for unmarked nodes and put them on the free list. */
void
scan(void)
{
  next_free = NIL;
  for(int64_t i = heap_start; i < heap_size; i++) {
    NODEPTR n = HEAPREF(i);
    if (MARK(n) == NOTMARKED) {
      if (TAG(n) == HDL && HANDLE(n) != 0 &&
	  HANDLE(n) != stdin && HANDLE(n) != stdout && HANDLE(n) != stderr) {
        /* A FILE* has become garbage, so close it. */
        fclose(HANDLE(n));
      }
      TAG(n) = FREE;
      NEXT(n) = next_free;
      next_free = n;
    } else {
      MARK(n) = NOTMARKED;
    }
  }
}

int64_t max_num_marked = 0;

/* Perform a garbage collection:
   - First mark from all roots; roots are on the stack.
   - Then scan for unmarked nodes.
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
  int64_t free = heap_size - heap_start - num_marked;
  if (free < heap_size / 50)
    ERR("heap exhausted");
  if (verbose > 1)
    fprintf(stderr, "gc done, %"PRId64" free\n", free);
}

/* Check that there are k nodes available, if not then GC. */
void
gc_check(int kk)
{
  NODEPTR n;
  int k;
  for (k = kk, n = next_free; n != NIL && k > 0; n = NEXT(n), k--)
    ;
  if (n != NIL)
    return;
  if (verbose > 1)
    fprintf(stderr, "gc_check: %d\n", kk);
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

/* Recursively print an expression.
   This assumes that the shared nodes has been marked as such.
*/
void
printrec(FILE *f, NODEPTR n)
{
  if (MARK(n) == PRINTED) {
    /* This node has already been printer, so just use a reference. */
    fprintf(f, "_%d", LABEL(n));
    return;
  } else if (MARK(n) == SHARED) {
    /* This node is shared, mark it as printed now to avoid loops. */
    fprintf(f, ":%d ", LABEL(n));
    MARK(n) = PRINTED;
  }

  switch (TAG(n)) {
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
  case T: fprintf(f, "$T"); break;
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
  case EQ: fprintf(f, "$="); break;
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

int64_t num_shared;

/* Mark all reachable nodes, when a marked node is reached, mark it as shared. */
void
find_sharing(NODEPTR n)
{
  while (TAG(n) == IND)
    n = INDIR(n);
  if (TAG(n) == AP) {
    if (MARK(n) == SHARED) {
      ;
    } else if (MARK(n) == MARKED) {
      MARK(n) = SHARED;
      num_shared++;
    } else {
      MARK(n) = MARKED;
      find_sharing(FUN(n));
      find_sharing(ARG(n));
    }
  }
}

/* Clear all sharing markers after printing. */
void
clear_sharing(NODEPTR n)
{
  while (TAG(n) == IND)
    n = INDIR(n);
  if (MARK(n) == NOTMARKED)
    return;
  if (TAG(n) == AP) {
    MARK(n) = NOTMARKED;
    clear_sharing(FUN(n));
    clear_sharing(ARG(n));
  }
}

/* Serialize a graph to file. */
void
print(FILE *f, NODEPTR n, int header)
{
  num_shared = 0;
  find_sharing(n);
  if (header)
    fprintf(f, "%s%"PRId64"\n", VERSION, num_shared);
  printrec(f, n);
  clear_sharing(n);
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
  while (TAG(n) == IND)
    n = INDIR(n);
  return n;
}

/* Follow indirections */
NODEPTR
indir(NODEPTR n)
{
  while (TAG(n) == IND)
    n = INDIR(n);
  return n;
}

/* Evaluate to an INT */
value_t
evalint(NODEPTR n)
{
  n = evali(n);
  if (TAG(n) != INT) {
    fprintf(stderr, "bad tag %d\n", TAG(n));
    ERR("evalint");
  }
  return GETVALUE(n);
}

/* Evaluate to a HDL */
FILE *
evalhandleN(NODEPTR n)
{
  n = evali(n);
  if (TAG(n) != HDL) {
    fprintf(stderr, "bad tag %d\n", TAG(n));
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
    if (TAG(n) == K)            /* Nil */
      break;
    else if (TAG(n) == AP && TAG(x = indir(FUN(n))) == AP && TAG(indir(FUN(x))) == O) { /* Cons */
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

/* Reset stack pointer and return. */
#define RET do { stack_ptr = stk; return; } while(0)
/* Check that there are at least n arguments, return if not. */
#define CHECK(n) do { if (stack_ptr - stk <= (n)) RET; } while(0)

#define SETIND(n, x) do { TAG((n)) = IND; INDIR((n)) = (x); } while(0)
#define GOTO num_reductions++; goto

  PUSH(n);
  for(;;) {
    num_reductions++;
    switch (TAG(n)) {
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
    case T:                     /* T x y = * y */
      CHECK(2);
      x = ARG(TOP(2));
      POP(2);
      n = TOP(0);
      SETIND(n, x);
      GOTO ind;
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

#define SETINT(n,r) do { TAG((n)) = INT; SETVALUE((n), (r)); } while(0)
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

#define CMP(op) do { CHECK(2); r = evalint(ARG(TOP(1))) op evalint(ARG(TOP(2))); n = TOP(2); SETIND(n, r ? combT : combK); POP(2); } while(0)
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
      SETIND(n, hdl == 0 ? combT : combK);
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
      fprintf(stderr, "bad tag %d\n", TAG(n));
      ERR("eval tag");
    }
  }
}

NODEPTR
mkNil(void)
{
  return combK;
}

NODEPTR
mkCons(NODEPTR x, NODEPTR xs)
{
  return new_ap(new_ap(combO, x), xs);
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
    switch (TAG(n)) {
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
        if (TAG(bmg) == AP && TAG(bm = indir(FUN(bmg))) == AP && TAG(indir(FUN(bm))) == IO_BIND) {
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
      fprintf(stderr, "bad tag %d\n", TAG(n));
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
      ERR("Usage: eval [-v] [-Hheap-size] [-rFILE] [-- arg ...]");
  }
  glob_argc = argc;
  glob_argv = argv;

  if (fn == 0)
    fn = "out.comb";

  init_nodes();
  stack = malloc(sizeof(NODEPTR) * stack_size);
  if (!stack)
    abort();
  FILE *f = fopen(fn, "r");
  if (!f)
    ERR("file not found");
  NODEPTR n = parse_top(f);
  file_size = ftell(f);
  fclose(f);
  PUSH(n); gc(); n = TOP(0); POP(1);
  int64_t start_size = num_marked;
  if (verbose > 2)
    pp(stdout, n);
  run_time -= gettime();
  n = evalio(n);
  run_time += gettime();
  if (verbose) {
    if (verbose > 1) {
      printf("\nmain returns ");
      pp(stdout, n);
      printf("node size=%"PRId64", heap size=%"PRId64"\n", (int64_t)NODE_SIZE, heap_size);
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
  }
  exit(0);
}
