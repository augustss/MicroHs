#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Node representation:
 * NODE_NAIVE   all fields in a struct, regular pointers
 * NODE_SPLIT   split flags, funs, and args
 */
#define NODE_NAIVE

#define HEAP_CELLS 100000
#define STACK_SIZE 10000

#define ERR(s) do { fprintf(stderr, "ERR: %s\n", s); exit(1); } while(0)

enum node_mark { NOTMARKED, MARKED, SHARED, PRINTED }; /* SHARED, PRINTED only for printing */
enum node_tag { FREE, IND, AP, INT, S, K, I, B, C, T, Y, SS, BB, CC, ADD, SUB, MUL, DIV, MOD, SUBR, EQ, NE, LT, LE, GT, GE };

typedef int64_t value_t;

#if defined(NODE_NAIVE)
/* Naive node representation with minimal unions */
typedef struct node {
  enum node_mark mark;
  enum node_tag tag;
  union {
    value_t value;
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
#define NODE_SIZE sizeof(node)
#define ALLOC_HEAP(n) do { cells = malloc(n * sizeof(node)); memset(cells, 0x55, n * sizeof(node)); } while(0)
#define LABEL(n) ((int)((n) - cells))
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

#elif defined(NODE_64) && 0
/* Use bit fiddling for all node fields */
/* Use LSB GC tag, use next 7 bits for tag, remaining 56 bits
 * contains the value or two 28 bit "pointers".
 */
typedef struct {
  long int bits;
} node;
typedef unsigned int NODEPTR;
#define NIL (~0)
#define HEAPREF(i) (i)
#define MARK(p)  (heap[p].bits & 1)
#define TAG(p)   ((heap[p].bits >> 1) & 0x7f)
#define VALUE(p) (heap[p].bits >> 8)
#define FUN(p)   ((NODEPTR)((heap[p].bits >> 8) & 0xfffffffffffff))
#define ARG(p)   ((NODEPTR)((heap[p].bits >> 36) & 0xfffffffffffff))
#define NEXT(p)  FUN(p)
#define INDIR(p) FUN(p)

#else
#error "Pick a node representation"
#endif

int num_reductions = 0;
int num_gc = 0;

NODEPTR stack[STACK_SIZE];
int64_t stack_ptr = -1;
#define PUSH(x) stack[++stack_ptr] = (x)
#define TOP(n) stack[stack_ptr - (n)]
#define POP(n) stack_ptr -= (n)
#define GCCHECK(n) gc_check((n))

int64_t heap_size = HEAP_CELLS; /* number of heap cells */
int64_t heap_start;             /* first location in heap that needs GC */
NODEPTR next_free;          /* Free list */

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

NODEPTR combS, combK, combI, combC, combB, combT, combY, combSS, combCC, combBB;
NODEPTR primADD, primSUB, primMUL, primDIV, primMOD, primSUBR, primEQ, primNE, primLT, primLE, primGT, primGE;

void
init_nodes(void)
{
  ALLOC_HEAP(heap_size);

  /* Set up permanent nodes */
  heap_start = 0;
#define COMB(x) TAG(comb##x = HEAPREF(heap_start++)) = x
#define PRIM(x) TAG(prim##x = HEAPREF(heap_start++)) = x
  COMB(S); COMB(K); COMB(I); COMB(B); COMB(C); COMB(T); COMB(Y); COMB(SS); COMB(CC); COMB(BB);
  PRIM(ADD); PRIM(SUB); PRIM(MUL); PRIM(DIV); PRIM(MOD); PRIM(SUBR);
  PRIM(EQ); PRIM(NE); PRIM(LT); PRIM(LE); PRIM(GT); PRIM(GE);

  /* Set up free list */
  next_free = NIL;
  for (int i = heap_start; i < heap_size; i++) {
    NODEPTR n = HEAPREF(i);
    MARK(n) = NOTMARKED;
    TAG(n) = FREE;
    NEXT(n) = next_free;
    next_free = n;
  }
}

int num_marked;

void
mark(NODEPTR *np)
{
  NODEPTR n = *np;

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
  if (TAG(n) == AP) {
    mark(&FUN(n));
    mark(&ARG(n));
  }
}

void
scan(void)
{
  next_free = NIL;
  for(int i = heap_start; i < heap_size; i++) {
    NODEPTR n = HEAPREF(i);
    if (MARK(n) == NOTMARKED) {
      TAG(n) = FREE;
      NEXT(n) = next_free;
      next_free = n;
    } else {
      MARK(n) = NOTMARKED;
    }
  }
}

void
gc(void)
{
  num_gc++;
  num_marked = 0;
  // fprintf(stderr, "gc mark\n");
  for (int i = 0; i <= stack_ptr; i++)
    mark(&stack[i]);
  // fprintf(stderr, "gc scan\n");
  scan();

  //  fprintf(stderr, "gc done, %d free\n", heap_size - heap_start - num_marked);
}

/* Check that there are k nodes available */
void
gc_check(int k)
{
  NODEPTR n;
  for (n = next_free; n != NIL && k > 0; n = NEXT(n), k--)
    ;
  if (n != NIL)
    return;
  gc();
}

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

NODEPTR *shared;

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

NODEPTR
parse(FILE *f)
{
  NODEPTR r;
  int l;
  value_t i;
  int c = getc(f);

  if (c < 0) ERR("parse EOF");
  switch (c) {
  case 'S' : return gobble(f, '\'') ? combSS : combS;
  case 'K' : return combK;
  case 'I' : return combI;
  case 'C' : return gobble(f, '\'') ? combCC : combC;
  case 'B' : return gobble(f, '\'') ? combBB : combB;
  case 'T' : return combT;
  case 'Y' : return combY;
  case '(' :
    r = alloc_node(AP);
    FUN(r) = parse(f);
    if (!gobble(f, ' ')) ERR("parse ' '");
    ARG(r) = parse(f);
    if (!gobble(f, ')')) ERR("parse ')'");
    return r;
  case '0':case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
    ungetc(c, f);
    i = parse_int(f);
    r = alloc_node(INT);
    SETVALUE(r, i);
    return r;
  case '+' : return primADD;
  case '-' : return gobble(f, '\'') ? primSUBR : primSUB;
  case '*' : return primMUL;
  case '/' : return primDIV;
  case '%' : return primMOD;
  case '=' : return primEQ;
  case '!' : if(gobble(f, '=')) return primNE; else ERR("parse !");
  case '<' : return gobble(f, '=') ? primLE : primLT;
  case '>' : return gobble(f, '=') ? primGE : primGT;
  case '_' :
    l = (int)parse_int(f);  /* The label */
    if (shared[l] == NIL) ERR("shared == NIL");
    return shared[l];
  case ':' :
    l = (int)parse_int(f);  /* The label */
    if (!gobble(f, ' ')) ERR("parse ' '");
    if (shared[l] != NIL) ERR("shared != NIL");
    shared[l] = alloc_node(IND); /* Must have a placeholder for cycles */
    r = parse(f);
    INDIR(shared[l]) = r;
    return r;
  default:
    fprintf(stderr, "parse '%c'\n", c);
    ERR("parse default");
  }
}

NODEPTR
parse_top(FILE *f)
{
  shared = malloc(heap_size * sizeof(NODEPTR));
  for(int i = 0; i < heap_size; i++)
    shared[i] = NIL;
  NODEPTR n = parse(f);
  free(shared);
  return n;
}

void
print(FILE *f, NODEPTR n)
{
  if (MARK(n) == PRINTED) {
    fprintf(f, "_%d", LABEL(n));
    return;
  } else if (MARK(n) == SHARED) {
    fprintf(f, ":%d ", LABEL(n));
    MARK(n) = PRINTED;
  }

  switch (TAG(n)) {
  case IND: print(f, INDIR(n)); break;
  case AP:
    fputc('(', f);
    print(f, FUN(n));
    fputc(' ', f);
    print(f, ARG(n));
    fputc(')', f);
    break;
  case INT: fprintf(f, "%ld", GETVALUE(n)); break;
  case S: fprintf(f, "S"); break;
  case K: fprintf(f, "K"); break;
  case I: fprintf(f, "I"); break;
  case C: fprintf(f, "C"); break;
  case B: fprintf(f, "B"); break;
  case T: fprintf(f, "T"); break;
  case Y: fprintf(f, "Y"); break;
  case SS: fprintf(f, "S'"); break;
  case BB: fprintf(f, "B'"); break;
  case CC: fprintf(f, "C'"); break;
  case ADD: fprintf(f, "+"); break;
  case SUB: fprintf(f, "-"); break;
  case MUL: fprintf(f, "*"); break;
  case DIV: fprintf(f, "/"); break;
  case MOD: fprintf(f, "%%"); break;
  case SUBR: fprintf(f, "-'"); break;
  case EQ: fprintf(f, "="); break;
  case NE: fprintf(f, "!="); break;
  case LT: fprintf(f, "<"); break;
  case LE: fprintf(f, "<="); break;
  case GT: fprintf(f, ">"); break;
  case GE: fprintf(f, ">="); break;
  default: ERR("print tag");
  }
}

void
find_sharing(NODEPTR n)
{
  while (TAG(n) == IND)
    n = INDIR(n);
  if (TAG(n) == AP) {
    if (MARK(n) == MARKED) {
      MARK(n) = SHARED;
    } else {
      MARK(n) = MARKED;
      find_sharing(FUN(n));
      find_sharing(ARG(n));
    }
  }
}

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

void
pp(FILE *f, NODEPTR n)
{
  find_sharing(n);
  print(f, n);
  fprintf(f, "\n");
  clear_sharing(n);
}

void eval(NODEPTR n);

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

void
eval(NODEPTR n)
{
  int64_t stk = stack_ptr;
  NODEPTR f, g, x, k;
  int r;
  
#define RET do { stack_ptr = stk; return; } while(0)
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
      RET;
    case S:
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
    case SS:
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
    case K:
      CHECK(2);
      x = ARG(TOP(1));
      POP(2);
      n = TOP(0);
      SETIND(n, x);
      GOTO ind;
      break;
    case T:
      CHECK(2);
      x = ARG(TOP(2));
      POP(2);
      n = TOP(0);
      SETIND(n, x);
      GOTO ind;
      break;
    case I:
      CHECK(1);
      x = ARG(TOP(1));
      POP(1);
      n = TOP(0);
      SETIND(n, x);
      GOTO ind;
      break;
    case Y:
      CHECK(1);
      f = ARG(TOP(1));
      POP(1);
      n = TOP(0);
      FUN(n) = f;
      ARG(n) = n;
      GOTO ap;
      break;
    case B:
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
    case C:
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
      break;
    case CC:
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
      break;
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
    case DIV:
      ARITH2(/);
      RET;
    case MOD:
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
    default:
      fprintf(stderr, "bad tag %d\n", TAG(n));
      ERR("eval tag");
    }
  }
}

int
main(int argc, char **argv)
{
  if (argc != 2)
    ERR("no file");
  init_nodes();
  FILE *f = fopen(argv[1], "r");
  if (!f)
    ERR("file not found");
  NODEPTR n = parse_top(f);
  fclose(f);
  pp(stdout, n);
  n = evali(n);
  pp(stdout, n);
  printf("node size=%ld, heap size=%ld\n", NODE_SIZE, heap_size); //exit(0);
  printf("%d reductions, %d GCs\n", num_reductions, num_gc);
  exit(0);
}
