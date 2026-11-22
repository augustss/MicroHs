/* Copyright 2023,2024,2025 Lennart Augustsson
 * See LICENSE file for full license.
 */
#include "mhsffi.h"  /* this includes config.h */
#include "extra.c"

/* Extra asserts for debugging */
#if WANT_STDIO
#define XASSERT(p) do { if (!(p)) { fprintf(stderr, "XASSERT line %d: %s\n", __LINE__, #p); exit(1); } } while(0)
#else
#define XASSERT(p) do { if (!(p)) { ERR("XASSERT"); } } while(0)
#endif
//#define XASSERT(p)

//#define ROUND_UP(x, d) ((((x) + (d) - 1) / (d)) * (d))

#if !defined(WANT_GMP)
#define WANT_GMP 0
#endif /* defined(WANT_GMP) */

#if !defined(WANT_IMATH) && !WANT_GMP
#define WANT_IMATH 1
#endif /* defined(WANT_MATH) */

#if WANT_GMP && WANT_IMATH
#error "Cannot have both WANT_GMP and WANT_IMATH"
#endif

#if !defined(WANT_OVERFLOW)
#define WANT_OVERFLOW 0
#endif /* defined(WANT_OVERFLOW) */

#if !defined(WANT_IO_POLL)
#define WANT_IO_POLL 0
#endif /* defined(WANT_IO_POLL) */

#if !defined(WANT_SOCKET)
#define WANT_SOCKET 0
#endif /* defined(WANT_SOCKET) */

#if !defined(WANT_SAMPLE)
#define WANT_SAMPLE 0           /* sampling profiler for hot function nodes */
#endif /* defined(WANT_SAMPLE) */

/* Dynamic profiling: count reductions per definition, see prof_charge().
 * Compile the program with mhs --profile to get it. */
#if !defined(WANT_PROFILE)
#define WANT_PROFILE 0
#endif /* defined(WANT_PROFILE) */

#if WANT_STDIO
#include <stdio.h>
#include <locale.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#if WANT_MATH
#include <math.h>
#endif  /* WANT_MATH */
#if defined(__EMSCRIPTEN__)
#include "emscripten.h"
#ifndef EM_ASM_PTR
#define EM_ASM_PTR(...) (void*)(uintptr_t)EM_ASM_INT(__VA_ARGS__)
#endif
#endif /* __EMSCRIPTEN__ */
#if WANT_DIR
#include <sys/stat.h>
#include <sys/types.h>
#endif  /* WANT_DIR */
#if WANT_TIME
#include <time.h>
#endif
#if WANT_GMP
#include <gmp.h>
#endif  /* WANT_GMP */
#if WANT_IMATH
#include "imgmp.h"
#endif  /* WANT_IMATH */
#if WANT_SIGINT
#include <signal.h>
#endif
#if WANT_IO_POLL
#include <poll.h>
#endif

extern char **environ;          /* should probably be behind some WANT_ */

#if WANT_MD5
#include "md5.h"
#endif

#if !defined(WANT_UTF8)
#define WANT_UTF8 1
#endif

#if !defined(WANT_BUF)
#define WANT_BUF 1
#endif

#if !defined(WANT_CRLF)
#define WANT_CRLF 1
#endif

#if !defined(WANT_BASE64)
#define WANT_BASE64 1
#endif

#if !defined(WANT_LZ77)
#define WANT_LZ77 1
#endif

#if !defined(WANT_LZMA)
#define WANT_LZMA 1
#endif

#if !defined(WANT_RLE)
#define WANT_RLE 1
#endif

#if !defined(WANT_BWT)
#define WANT_BWT 1
#endif

#if !defined(WANT_ENV)
#define WANT_ENV 1
#endif

#if !defined(WANT_ERRNO)
#define WANT_ERRNO 0
#else
#include "ffi_errno.c"
#endif

#if !defined(GET_EXECUTABLE_PATH)
char *get_executable_path(void) { return NULL; }
#endif

#if !defined(YIELD_EXTRA)
#define YIELD_EXTRA do {} while(0)
#endif

#if !defined(MKDIR)
#define MKDIR mkdir
#endif

#if WANT_LZ77
size_t lz77d(uint8_t *src, size_t srclen, uint8_t **bufp);
size_t lz77c(uint8_t *src, size_t srclen, uint8_t **bufp);
#endif

/* The register optimization is disabled for now since it breaks on some platforms. */
#if 0 && defined(__GNUC__) && __GNUC__ >= 14 && defined(__aarch64__)
#define REGISTER(dcl, reg) register dcl asm(#reg)
#else
#define REGISTER(dcl, reg) dcl
#endif

const struct ffi_entry ffi_table[];
int num_ffi;
#define FFI_IX(i) ((i) < num_ffi ? ffi_table[i] : xffi_table[i - num_ffi])

#if WANT_STDIO
#define THREAD_DEBUG 1
#else
#define THREAD_DEBUG 0
#endif


#define VERSION "v9.2\n"

#define PRIvalue PRIdPTR
#define PRIuvalue PRIuPTR
typedef uintptr_t heapoffs_t;   /* Heap offsets */
#define PRIheap PRIuPTR

typedef uintptr_t counter_t;    /* Statistics counter, can be smaller since overflow doesn't matter */
#define PRIcounter PRIuPTR
typedef uintptr_t bits_t;       /* One word of bits */

#if !defined(WANT_ARGS)
#define WANT_ARGS 1
#endif

#if !defined(MALLOC)
#define MALLOC malloc
#endif

#if !defined(REALLOC)
#define REALLOC realloc
#endif

#if !defined(CALLOC)
#define CALLOC calloc
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
#endif  /* !defined(GETRAW) */

#if !defined(GETTIMEMICRO)
value_t GETTIMEMICRO(void) { return 0; }
#endif  /* !define(GETTIMEMICRO) */
#define GETTIMEMILLI() (GETTIMEMICRO() / 1000)

#if !defined(GETCPUTIME)
void GETCPUTIME(long *sec, long *nsec) { *sec = 0; *nsec = 0; }
#endif  /* !define(GETCPUTIME) */

#if !defined(INLINE)
#define INLINE inline
#endif  /* !define(INLINE) */

#if !defined(NOINLINE)
#if defined(__GNUC__) || defined(__clang__)
#define NOINLINE __attribute__((noinline))
#else
#define NOINLINE
#endif
#endif  /* !defined(NOINLINE) */

#if !defined(ALWAYS_INLINE)
#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE inline __attribute__((always_inline))
#else
#define ALWAYS_INLINE inline
#endif
#endif  /* !defined(ALWAYS_INLINE) */

#if !defined(NORETURN)
/*#define NORETURN [[noreturn]]*/
#define NORETURN _Noreturn
#endif /* !defined(NORETURN) */

#if !defined(PACKED)
#if WORD_SIZE == 32
/* Avoid 64 bit alignment to keep nodes to 12 bytes */
#define PACKED __attribute__((packed, aligned(4)))
#else
#define PACKED
#endif  /* WORD_SIZE == 32 */
#endif  /* !defined(PACKED) */

/* tcc doesn't understand noreturn attribute */
#if defined(__TCC__)
#define NOTREACHED return 0
#else
#define NOTREACHED
#endif

#if !defined(SLICE)
#define SLICE 100000
#endif

NORETURN void memerr(void);

void *
mmalloc(size_t s)
{
  if (s == 0) s = 1;            /* avoid malloc() returning NULL */
  void *p = MALLOC(s);
  if (!p)
    memerr();
  return p;
}

void *
mrealloc(void *q, size_t s)
{
  if (s == 0) s = 1;            /* avoid realloc() returning NULL */
  void *p = REALLOC(q, s);
  if (!p)
    memerr();
  return p;
}

void *
mcalloc(size_t n, size_t s)
{
  if (n * s == 0) n = s = 1;    /* avoid calloc() returning NULL */
  void *p = CALLOC(n, s);
  if (!p)
    memerr();
  return p;
}

#if !defined(ERR)
#if WANT_STDIO
#define ERR(s)    do { fprintf(stderr,"ERR: "s"\n");   EXIT(1); } while(0)
#define ERR1(s,a) do { fprintf(stderr,"ERR: "s"\n",a); EXIT(1); } while(0)
#else  /* WANT_STDIO */
#define ERR(s) EXIT(1)
#define ERR1(s,a) EXIT(1)
#endif  /* WANT_STDIO */
#endif  /* !define(ERR) */

#if !defined(TMPNAME)
/* This is a really bad implementation, since it doesn't check for anything. */
char* TMPNAME(const char* pre, const char* suf) {
  ERR("no TMPNAME");
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

/***** popcount *****/
#if defined(__has_builtin)

#if __has_builtin(__builtin_popcountl)
#define BUILTIN_POPCOUNT __builtin_popcountl
#endif

#if __has_builtin(__builtin_popcountll)
#define BUILTIN_POPCOUNT64 __builtin_popcountll
#endif

/* If there are compiler intrinsics to detect over flow, do so */
#if __has_builtin(__builtin_add_overflow) && WANT_OVERFLOW
#define ADD_OVERFLOW(T, r, a, b) do { T vr; if (__builtin_add_overflow((T)(a), (T)(b), &(vr))) raise_rts(exn_overflow); (r) = vr; } while(0)
#endif
#if __has_builtin(__builtin_sub_overflow) && WANT_OVERFLOW
#define SUB_OVERFLOW(T, r, a, b) do { T vr; if (__builtin_sub_overflow((T)(a), (T)(b), &(vr))) raise_rts(exn_overflow); (r) = vr; } while(0)
#endif
#if __has_builtin(__builtin_mul_overflow) && WANT_OVERFLOW
#define MUL_OVERFLOW(T, r, a, b) do { T vr; if (__builtin_mul_overflow((T)(a), (T)(b), &(vr))) raise_rts(exn_overflow); (r) = vr; } while(0)
#endif

#endif

/* If we can't detect overflow, just ignore it. */
#if !defined(ADD_OVERFLOW)
#define ADD_OVERFLOW(T, r, a, b) ((r) = (a) + (b))
#endif
#if !defined(SUB_OVERFLOW)
#define SUB_OVERFLOW(T, r, a, b) ((r) = (a) - (b))
#endif
#if !defined(MUL_OVERFLOW)
#define MUL_OVERFLOW(T, r, a, b) ((r) = (a) * (b))
#endif


#if !defined(POPCOUNT)
uvalue_t POPCOUNT(uvalue_t x) {
#if defined(BUILTIN_POPCOUNT)
  return BUILTIN_POPCOUNT(x);
#else   /* !defined(BUILTIN_POPCOUNT) */
  uvalue_t count = 0;
  while (x) {
    x = x & (x - 1); // clear lowest 1 bit
    count++;
  }
  return count;
#endif   /* !defined(BUILTIN_POPCOUNT) */
}
#endif  /* !defined(POPCOUNT) */

#if !defined(POPCOUNT64)
uvalue_t POPCOUNT64(uint64_t x) {
#if defined(BUILTIN_POPCOUNT64)
  return BUILTIN_POPCOUNT64(x);
#else   /* !defined(BUILTIN_POPCOUNT64) */
  uvalue_t count = 0;
  while (x) {
    x = x & (x - 1); // clear lowest 1 bit
    count++;
  }
  return count;
#endif   /* !defined(BUILTIN_POPCOUNT64) */
}
#endif  /* !defined(POPCOUNT64) */
/***** end popcount *****/

/***** clz *****/
#if defined(__has_builtin)

#if __has_builtin(__builtin_clzl)
#define BUILTIN_CLZ __builtin_clzl
#endif

#if __has_builtin(__builtin_clzll)
#define BUILTIN_CLZ64 __builtin_clzll
#endif

#endif


#if !defined(CLZ)
uvalue_t CLZ(uvalue_t x) {
#if defined(BUILTIN_CLZ)
  if (x == 0) return WORD_SIZE;
  return BUILTIN_CLZ(x);
#else   /* defined(BUILTIN_CLZ) */
  value_t count = WORD_SIZE;
  while (x) {
    x = x >> 1;
    count--;
  }
  return count;
#endif  /* defined(BUILTIN_CLZ) */
}
#endif  /* !defined(CLZ) */

#if !defined(CLZ64)
uvalue_t CLZ64(uint64_t x) {
#if defined(BUILTIN_CLZ64)
  if (x == 0) return 64;
  return BUILTIN_CLZ64(x);
#else   /* defined(BUILTIN_CLZ64) */
  value_t count = 64;
  while (x) {
    x = x >> 1;
    count--;
  }
  return count;
#endif  /* defined(BUILTIN_CLZ64) */
}
#endif  /* !defined(CLZ64) */
/***** end clz *****/

/***** ctz *****/
#if defined(__has_builtin)

#if __has_builtin(__builtin_ctzl)
#define BUILTIN_CTZ __builtin_ctzl
#endif

#if __has_builtin(__builtin_ctzll)
#define BUILTIN_CTZ64 __builtin_ctzll
#endif

#endif  /* defined(__has_builtin) */


#if !defined(CTZ)
uvalue_t CTZ(uvalue_t x) {
  if (x == 0) return WORD_SIZE;
#if defined(BUILTIN_CTZ)
  return BUILTIN_CTZ(x);
#else  /* defined(BUILTIN_CTZ) */
  uvalue_t count = 0;
  while ((x & 1) == 0) {
    x = x >> 1;
    count += 1;
  }
  return count;
#endif  /* defined(BUILTIN_CTZ) */
}
#endif  /* !defined(CTZ) */

#if !defined(CTZ64)
uvalue_t CTZ64(uint64_t x) {
  if (x == 0) return 64;
#if defined(BUILTIN_CTZ64)
  return BUILTIN_CTZ64(x);
#else  /* defined(BUILTIN_CTZ64) */
  uvalue_t count = 0;
  while ((x & 1) == 0) {
    x = x >> 1;
    count++;
  }
  return count;
#endif  /* defined(BUILTIN_CTZ64) */
}
#endif  /* !defined(CTZ) */

/***** end ctz *****/

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

/* Was the runtime compiled with MinGW (rather than, e.g., cl)? */
value_t
ismingw(void)
{
#if defined(__MINGW32__)
  return 1;
#else
  return 0;
#endif
}

value_t
ismacos(void)
{
#if defined(ISMACOS)
  return 1;
#else
  return 0;
#endif
}

value_t
islinux(void)
{
#if defined(ISLINUX)
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
#define STACK_SIZE 300000
#endif

/***************************************/

/* All node tags for nodes with a payload */

enum node_tag { D_NONE,
                X_AP,           /* if we need to indicate an AP node */

                /* NODEPTR with a "virtual" pointer, and special tags */
                D_IND,          /* indirection, pointer payload */
                /* Tags with a payload */
                D_INT, D_INT64, D_DBL, D_FLT32, D_PTR, D_FUNPTR, D_FORPTR,
                D_ARR, D_THID, D_MVAR, D_WEAK,
                D_IO_CCALL,
                D_BADDYN, D_TICK,

                /* Tags with extra info */
                V_CONSTR, V_ARM, V_TAG, V_CASED, V_LOOKD, V_CASES, V_LOOKS,

                T_FIRST_REAL_TAG,
                /* NODEPTR with a "virtual" pointer, and regular tags */
                /* Regular tags */
                T_S, T_K, T_I, T_B, T_C,
                T_A, T_Y, T_SS, T_BB, T_CC, T_P, T_R, T_O, T_U, T_Z,
                T_K2, T_K3, T_K4, T_CCB,
                T_ADD, T_SUB, T_MUL, T_QUOT, T_REM, T_SUBR, T_NEG,
                T_UADD, T_USUB, T_UMUL, T_UQUOT, T_UREM, T_USUBR, T_UNEG,
                T_AND, T_OR, T_XOR, T_INV, T_SHL, T_SHR, T_ASHR,
                T_POPCOUNT, T_CLZ, T_CTZ,
                T_EQ, T_NE, T_LT, T_LE, T_GT, T_GE, T_ULT, T_ULE, T_UGT, T_UGE, T_ICMP, T_UCMP,
                T_ADD64, T_SUB64, T_MUL64, T_QUOT64, T_REM64, T_SUBR64, T_NEG64,
                T_UADD64, T_USUB64, T_UMUL64, T_UQUOT64, T_UREM64, T_USUBR64, T_UNEG64,
                T_AND64, T_OR64, T_XOR64, T_INV64, T_SHL64, T_SHR64, T_ASHR64,
                T_POPCOUNT64, T_CLZ64, T_CTZ64,
                T_EQ64, T_NE64, T_LT64, T_LE64, T_GT64, T_GE64, T_ULT64, T_ULE64, T_UGT64, T_UGE64, T_ICMP64, T_UCMP64,
                T_ITOI64, T_I64TOI, T_UTOU64, T_U64TOU,
                T_FPADD, T_FP2P, T_FPNEW, T_FPFIN,
                T_FP2BS, T_BS2FP,
                T_TOPTR, T_TOINT, T_TODBL, T_TOFLT, T_TOFUNPTR,
                T_FROMDBL, T_FROMFLT,
                T_BININT2, T_BININT1, T_UNINT1,
                T_BININT64_2, T_BININT64_1, T_UNINT64_1,
                T_BINFLT2, T_BINFLT1, T_UNFLT1,
                T_BINDBL2, T_BINDBL1, T_UNDBL1,
                T_BINBS2, T_BINBS1,
                T_ISINT,
                T_FADD, T_FSUB, T_FMUL, T_FDIV, T_FNEG, T_ITOF, T_I64TOF, T_FTOI, T_UTOF,
                T_FEQ, T_FNE, T_FLT, T_FLE, T_FGT, T_FGE,
                T_DADD, T_DSUB, T_DMUL, T_DDIV, T_DNEG, T_ITOD, T_I64TOD, T_DTOI, T_UTOD,
                T_DEQ, T_DNE, T_DLT, T_DLE, T_DGT, T_DGE,
                T_FTOD, T_DTOF,
                T_ARR_ALLOC, T_ARR_COPY, T_ARR_SIZE, T_ARR_READ, T_ARR_WRITE, T_ARR_TRUNC, T_ARR_EQ,
                T_RAISE, T_SEQ, T_RNF,
                T_IO_BIND, T_IO_THEN, T_IO_RETURN,
                T_IO_SERIALIZE, T_IO_DESERIALIZE,
                T_IO_GETARGREF,
                T_IO_PERFORMIO, T_IO_ATOMIC, T_IO_PRINT, T_CATCH, T_CATCHR,
                T_IO_GC, T_IO_STATS,
                T_IO_LAZYBIND, T_IO_STRICT, T_IO_LAZYBINDST,
                T_IO_FORK, T_IO_THID, T_THNUM, T_IO_THROWTO, T_IO_YIELD,
                T_IO_NEWMVAR,
                T_IO_TAKEMVAR, T_IO_PUTMVAR, T_IO_READMVAR,
                T_IO_TRYTAKEMVAR, T_IO_TRYPUTMVAR, T_IO_TRYREADMVAR,
                T_IO_THREADDELAY, T_IO_THREADSTATUS,
                T_IO_GETMASKINGSTATE, T_IO_SETMASKINGSTATE,
                T_PACKCSTRING, T_PACKCSTRINGLEN,
                T_BSAPPEND, T_BSEQ, T_BSNE, T_BSLT, T_BSLE, T_BSGT, T_BSGE, T_BSCMP,
                T_BSUNPACK, T_BSREPLICATE, T_BSLENGTH, T_BSSUBSTR, T_BSINDEX,
                T_BSNEW, T_BSREAD, T_BSWRITE, T_BSFREEZE, T_BSAPPBYTE, T_BSAPPCHAR, 
                T_BSFROMUTF8, T_BSHEADUTF8, T_BSTAILUTF8,
                T_UTF8LENGTH, T_UTF8OFFSET, T_UTF8OFFSETEND, T_UTF8OFFSETFROM, T_UTF8OFFSETENDBEFORE,
                T_UTF8DECODE, T_UTF8DECODEEND,
                T_UTF8REVERSE, T_UTF8SKIPSPACE, T_UTF8SKIPNONSPACE, T_UTF8TRIMSPACEEND,
                T_BSFIND, T_BSRFIND, T_BSASCIICASE, T_BSISASCII, T_BSCONCAT,
                T_BSAPPENDDOT, T_BSGRAB, T_BSGRABLEN,
                T_SPNEW, T_SPDEREF, T_SPFREE,
                T_WKNEWFIN, T_WKNEW, T_WKDEREF, T_WKFINAL,
                T_IO_PP,           /* for debugging */
                T_IO_STDIN, T_IO_STDOUT, T_IO_STDERR,
                T_IO_WAITRDFD, T_IO_WAITWRFD,
                T_BALANCEL, T_BALANCER,  /* experiment: IdentMap balancing */
                T_IDENTEQ,               /* experiment: (==) on Ident */
                T_LAST_TAG,
                T_XXX = 0x7fffffff
};

/* Most entries are initialized from the primops table. */
static const char* tag_names [T_LAST_TAG+1] =
  { [D_NONE] = "NONE", [X_AP] = "AP", [D_IND] = "IND",
    [D_INT] = "INT", [D_INT64] = "INT64", [D_DBL] = "DBL", [D_FLT32] = "FLT32", [D_PTR] = "PTR",
    [D_FUNPTR] = "FUNPTR", [D_FORPTR] = "FORPTR", [D_ARR] = "ARR", [D_THID] = "THID", [D_MVAR] = "MVAR", [D_WEAK] = "WEAK",
    [D_IO_CCALL] = "IO.ccall", [D_BADDYN] = "BADDYN", [D_TICK] = "TICK",
    [V_CONSTR] = "C_", [V_ARM] = "A_", [V_TAG] = "TAG", [V_CASED] = "D_", [V_LOOKD] = "L_", [V_CASES] = "E_", [V_LOOKS] = "M_",
  };
#define TAGNAME(t) tag_names[t]
/* The arity of each tag, from primops[], see arity_virt. */
static uint8_t tag_arity[T_LAST_TAG+1];

/***************************************/

/*
 * A NODEPTR is a basically a pointer to a struct node.
 * But to save memory references, we never point to a constant
 * node that is just a tag, instead we encode the tag straight in
 * the NODEPTR.
 * A struct node is always at least 4 byte aligned, so the two lower
 * bits of a real pointer are always 0.
 * A NODEPTR always has an LSB=0.
 * ...pppp00         real pointer = ...pppp00
 * ...tttt10         tag = ...tttt, basically a node_tag
 */

/* 
 * For a node (i.e., two NODEPTR) the two lower bits of the FUN part
 * decides what kind of node it is.
 * xxx0  yyyy        AP node, xxx0 and yyyy are NODEPTRs
 * xx01  yyyy        unused
 * xx11  yyyy        node with payload (IND, INT, DBL, etc), xx=D_INT,D_DBL,..., yyyy is the payload
 */

/*
 * For a NODEPTR we have
 * ppppppppp00        ppppppppp00 is a real pointer
 * nnnkkkttt10        ttt 11 bits is a node_tag, kkk 11 bits, nnn 8 bits
 *   when ttt=
 *     V_CONSTR       kkk = constructor number, nnn = number of constructor arguments
 *     V_ARM          as above
 *     V_CASED        nnnkkk = index in the table of case arities, see below
 *     V_CASES        kkk = number of constructors, nnn unused
 *     V_LOOKD        as above
 *     V_LOOKS        as above
 *     otherwise      kkk, nnn unused
 * 
 */
typedef uintptr_t tag_t;
typedef union NodePtr {
  tag_t          node_bits;
  struct node   *node_ptr;
  union NodePtr *node_pptr;      /* only used during GC for values on the stack */
} NODEPTR;

const NODEPTR NIL_NODEPTR = { 0 };

/* To fit in a word we should have
 * PTR_TAG_BITS + PTR_TAG_TAG_BITS + PTR_TAG_NCON_BITS + PTR_TAG_NARG_BITS <= size_in_bits(tag_t)
 * We make them add to 32
 */
#define PTR_TAG_BITS 2                                   /* number of reserved tag bits in every pointer */
#define PTR_TAG_MASK ((1 << PTR_TAG_BITS) - 1)           /* 0x3 */
#define PTR_TAG_PTR  0
#define PTR_TAG_TAG  2
#define PTR_TAG_DATA 3
#define PTR_AP_MASK  1
#define PTR_AP_AP    0
#define PTR_TAG_TAG_BITS 11     /* max number of bits in a enum node_tag */
#define PTR_TAG_TAG_MASK ((1 << PTR_TAG_TAG_BITS) - 1)   /* 0x7ff */
#define PTR_TAG_NCON_BITS 11    /* max number of constructors/case-arms */
#define PTR_TAG_NCON_MASK ((1 << PTR_TAG_NCON_BITS) - 1) /* 0x7ff */
#define PTR_TAG_NARG_BITS 8     /* max number of constructor arguments */
#define PTR_TAG_NARG_MASK ((1 << PTR_TAG_NARG_BITS) - 1) /* 0xff */

#define MK_TAG(t) (((tag_t)(t) << PTR_TAG_BITS) | PTR_TAG_TAG)
/* Encode the CASEx/LOOKx/CON/ARM as above */
#define MK_CASE(t, m) MK_TAG(((tag_t)(m) << PTR_TAG_TAG_BITS) | (t))
#define MK_CON(t, k, n) MK_CASE(t, ((tag_t)(n) << PTR_TAG_NCON_BITS) | (k))

/* is it NIL_NODEPTR? */
static INLINE bool
is_NIL(NODEPTR n)
{
  return n.node_bits == 0;
}

/* If the NODEPTR is a real pointer, return it. */
static INLINE struct node*
get_node_ptr(NODEPTR n)
{
  if ((n.node_bits & PTR_TAG_MASK) == 0)
    return n.node_ptr;
  else
    return NULL;
}

/* For a FUN part of a struct node */
static INLINE bool
is_AP(NODEPTR n)
{
  return (n.node_bits & PTR_AP_MASK) == PTR_AP_AP;
}

/* For a FUN part of a struct node */
static INLINE bool
is_DATA(NODEPTR n)
{
  return (n.node_bits & PTR_TAG_MASK) == PTR_TAG_DATA;
}

/* For a FUN part of a struct node */
/* Returns the data tag or D_NONE */
static INLINE tag_t
get_DATA_TAG(NODEPTR n)
{
  if (is_DATA(n))
    return n.node_bits >> PTR_TAG_BITS;
  else
    return D_NONE;
}

static INLINE NODEPTR
mk_TAG(enum node_tag t)
{
  NODEPTR n;
  n.node_bits = MK_TAG(t);
  return n;
}

static INLINE NODEPTR
mk_DATA_TAG(enum node_tag t)
{
  NODEPTR n;
  n.node_bits = (t << PTR_TAG_BITS) | PTR_TAG_DATA;
  return n;
}

static INLINE NODEPTR
mk_CASE(enum node_tag t, int num_constr)
{
  NODEPTR n;
  n.node_bits = MK_CASE(t, num_constr);
  return n;
}

/* t is V_CONSTR or V_ARM, k is the constructor number, n the number of arguments */
static INLINE NODEPTR
mk_CON(enum node_tag t, int constr_no, int num_arg)
{
  NODEPTR n;
  n.node_bits = MK_CON(t, constr_no, num_arg);
  return n;
}

/* Is the NODEPTR a virtual pointer, i.e., an encoded tag? */
static INLINE bool
is_VIRT(NODEPTR n)
{
  return (n.node_bits & PTR_TAG_MASK) == PTR_TAG_TAG;
}

/* The tag of a virtual NODEPTR (without the extra info) */
static INLINE enum node_tag
get_VIRT_TAG(NODEPTR n)
{
  return (enum node_tag)((n.node_bits >> PTR_TAG_BITS) & PTR_TAG_TAG_MASK);
}

static INLINE bool
eq_NODEPTR(NODEPTR p, NODEPTR q)
{
  return p.node_ptr == q.node_ptr;
}

/***************************************/


struct ioarray;
struct bytestring;
struct forptr;
struct mthread;
struct mvar;
struct weak_ptr;

typedef struct PACKED node {
  NODEPTR            ufun;
  union {
    NODEPTR          uuarg;
    NODEPTR          uuind;
    value_t          uuvalue;
#if WANT_FLOAT32
    flt32_t          uuflt32value;
    uint32_t         uuint32value;
#endif  /* WANT_FLOAT32 */
#if WANT_FLOAT64
    flt64_t          uuflt64value;
#endif  /* WANT_FLOAT32 */
#if WANT_INT64
    int64_t          uuint64value;
#endif  /* WANT_INT64 */
    const char      *uucstring;
    void            *uuptr;
    HsFunPtr         uufunptr;
    struct ioarray  *uuarray;
    struct forptr   *uuforptr;      /* foreign pointers and byte arrays */
    struct mthread  *uuthread;
    struct mvar     *uumvar;
    struct weak_ptr *uuweak;
  } uarg;
#if WANT_PROFILE
  /* See prof_charge().  The counts and the label are only used in loaded nodes. */
  struct node       *prof_src;    /* the loaded node this node comes from, itself for a loaded node */
  counter_t          prof_red;    /* reductions charged to this node */
  counter_t          prof_unwind; /* unwinding steps charged to this node */
  counter_t          prof_alloc;  /* cells allocated by reductions charged to this node */
  uint32_t           prof_label;  /* the label of the definition + 1, 0 for none, PROF_MAIN for the main expression */
  uint32_t           prof_ctx;    /* the call context the node was created in, see prof_push */
  /* Counts for this node itself, while it lives, see prof_tick(). */
  counter_t          prof_ncalls; /* unwinding steps through this node */
  counter_t          prof_nred;   /* reductions charged to this node */
  counter_t          prof_nupd;   /* times a reduction rewrote this node to an application */
#endif  /* WANT_PROFILE */
} node;

#if 0
/* If the NODEPTR points to an application, return the FUN */
static INLINE NODEPTR
get_FUN(NODEPTR n)
{
  struct node *p = get_node_ptr(n);
  if (p) {
    NODEPTR q = p->ufun;
    if ((q.node_bits & PTR_AP_MASK) == PTR_AP_AP)
      return q;
  }
  return NIL_NODEPTR;
}
#endif

/* If the NODEPTR points to an indirection, return it */
static INLINE NODEPTR
get_IND(NODEPTR n)
{
  struct node *p = get_node_ptr(n);
  if (p) {
    NODEPTR q = p->ufun;
    if (get_DATA_TAG(q) == D_IND) {
      return p->uarg.uuind;
    }
  }
  return NIL_NODEPTR;
}

static INLINE void
set_IND(NODEPTR n, NODEPTR t)
{
  struct node *p = get_node_ptr(n);
  if (!p)
    ERR("set_IND");
  p->ufun = mk_DATA_TAG(D_IND);
  p->uarg.uuind = t;
}

/* Set the tag of a data node, n is a real pointer */
static INLINE void
set_DATA_TAG(NODEPTR n, enum node_tag t)
{
  XASSERT(get_node_ptr(n) != NULL);
  n.node_ptr->ufun = mk_DATA_TAG(t);
}

/*
 * The tag of any NODEPTR (this is what GETTAG used to be):
 *  - virtual pointer: the encoded tag (T_xxx or V_xxx)
 *  - real pointer to an application: X_AP
 *  - real pointer to a data node: the D_xxx tag (including D_IND)
 */
static INLINE enum node_tag
get_XTAG(NODEPTR n)
{
  if (is_VIRT(n))
    return get_VIRT_TAG(n);
  NODEPTR f = n.node_ptr->ufun;
  if (is_AP(f))
    return X_AP;
  return (enum node_tag)get_DATA_TAG(f);
}

/* Number of arms of a virtual V_CASES/V_LOOKD/V_LOOKS */
static INLINE int
get_CASE_size(NODEPTR n)
{
  XASSERT(is_VIRT(n));
  tag_t t = n.node_bits >> PTR_TAG_BITS;
  return (t >> PTR_TAG_TAG_BITS) & PTR_TAG_NCON_MASK;
}

/*
 * Case arities.
 * A dense case, D_a0_..._a{m-1}, has m arms, and the arm for constructor k
 * takes ak arguments, the fields of the constructor.  The interpreter only
 * uses m, but the arities are needed to turn the graph back into code,
 * since they cannot be read back from the arms.
 * The arities are kept in a table, and a V_CASED holds an index into it.
 * Equal entries are shared, and the entries are never freed.
 */
struct case_arity {
  uint16_t m;                   /* number of arms */
  uint8_t *arities;             /* arities[k] is the number of fields of constructor k */
};
static struct case_arity *case_arities;  /* the table */
static uint32_t num_case_arities;        /* number of entries in use */
static uint32_t max_case_arities;        /* number of entries allocated */

#define PTR_TAG_CASED_BITS (PTR_TAG_NCON_BITS + PTR_TAG_NARG_BITS)  /* bits for the table index */
#define MAX_CASE_ARITIES ((uint32_t)1 << PTR_TAG_CASED_BITS)

/* The case arities of a virtual V_CASED */
static INLINE struct case_arity *
get_CASED_arity(NODEPTR n)
{
  XASSERT(is_VIRT(n));
  tag_t t = n.node_bits >> PTR_TAG_BITS;
  return &case_arities[(t >> PTR_TAG_TAG_BITS) & (MAX_CASE_ARITIES - 1)];
}

/* Find the arities in the table, or add them, and return the index.
 * Combinator code is rarely loaded, and the table is small,
 * so a linear search will do. */
static uint32_t
find_case_arity(uint32_t m, const uint8_t *arities)
{
  for (uint32_t i = 0; i < num_case_arities; i++) {
    struct case_arity *c = &case_arities[i];
    if (c->m == m && memcmp(c->arities, arities, m) == 0)
      return i;
  }
  if (num_case_arities >= max_case_arities) {
    if (num_case_arities >= MAX_CASE_ARITIES)
      ERR("too many case arities");
    max_case_arities = max_case_arities ? 2 * max_case_arities : 64;
    case_arities = mrealloc(case_arities, max_case_arities * sizeof(struct case_arity));
  }
  struct case_arity *c = &case_arities[num_case_arities];
  c->m = m;
  c->arities = mmalloc(m);
  memcpy(c->arities, arities, m);
  return num_case_arities++;
}

/* Constructor number and number of arguments of a virtual V_CONSTR/V_ARM */
static INLINE void
get_CON_info(NODEPTR n, int *kp, int *np)
{
  XASSERT(is_VIRT(n));
  tag_t t = n.node_bits >> PTR_TAG_BITS;
  *kp = (t >> PTR_TAG_TAG_BITS) & PTR_TAG_NCON_MASK;
  *np = (t >> (PTR_TAG_TAG_BITS + PTR_TAG_NCON_BITS)) & PTR_TAG_NARG_MASK;
}

static INLINE void
mk_AP(NODEPTR n, NODEPTR fun, NODEPTR arg)
{
  n.node_ptr->ufun = fun;
  n.node_ptr->uarg.uuarg = arg;
}

#define HEAPREF(i) &cells[(i)]
#define NPTR(n) ((n).node_ptr)
#define GETVALUE(p) NPTR(p)->uarg.uuvalue
#define GETINT64VALUE(p) NPTR(p)->uarg.uuint64value
#define GETINT32VALUE(p) NPTR(p)->uarg.uuint32value
#define GETFLTVALUE(p) NPTR(p)->uarg.uuflt32value
#define GETDBLVALUE(p) NPTR(p)->uarg.uuflt64value
#define SETVALUE(p,v) NPTR(p)->uarg.uuvalue = v
#define SETINT64VALUE(p,v) NPTR(p)->uarg.uuint64value = v
#define SETINT32VALUE(p,v) NPTR(p)->uarg.uuint32value = v
#define SETFLTVALUE(p,v) NPTR(p)->uarg.uuflt32value = v
#define SETDBLVALUE(p,v) NPTR(p)->uarg.uuflt64value = v
#define FUN(p) NPTR(p)->ufun
#define ARG(p) NPTR(p)->uarg.uuarg
#define CSTR(p) NPTR(p)->uarg.uucstring
#define PTR(p) NPTR(p)->uarg.uuptr
#define FUNPTR(p) NPTR(p)->uarg.uufunptr
#define FORPTR(p) NPTR(p)->uarg.uuforptr
#define BSTR(p) NPTR(p)->uarg.uuforptr->payload
#define ARR(p) NPTR(p)->uarg.uuarray
#define THR(p) NPTR(p)->uarg.uuthread
#define MVAR(p) NPTR(p)->uarg.uumvar
#define WEAK(p) NPTR(p)->uarg.uuweak

#define NODE_SIZE sizeof(node)
#define LABEL(n) ((heapoffs_t)((n).node_ptr - cells))
node *cells;                 /* All cells */

/* Prefetch a node we know we'll need soon but haven't dereferenced yet. */
#if defined(__GNUC__) || defined(__clang__)
#define PREFETCH_READ(addr) __builtin_prefetch((addr), 0, 2)
#else
#define PREFETCH_READ(addr) do { } while(0)
#endif

/*
 * Byte arrays.
 * This is used for both immutable and mutable arrays.
 * This struct is often passed by value.
 */
struct bytestring {
  size_t   bs_size;                  /* current size of string */
  size_t   bs_capacity;              /* size allocated for string, 0 if immutable */
  void    *bs_array;                 /* bytes */
};

/* Create a new immutable bytestring, if buf is NULL also allocates the array */
struct bytestring
mk_ro_bytestring(size_t size, void *buf)
{
  struct bytestring bs;
  bs.bs_capacity = 0;
  bs.bs_size = size;
  bs.bs_array = buf ? buf : mmalloc(size);
  return bs;
}

/*
 * Arrays are allocated with malloc()/free().
 * During GC they are marked, and all elements in the array are
 * recursively marked.
 * At the end of the the mark phase there is a scan of all
 * arrays, and the unmarked ones are freed.
 */
struct ioarray {
  struct ioarray *next;         /* all ioarrays are linked together */
  bool permanent;               /* this array should never be GC-ed */
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

//REGISTER(counter_t num_reductions,r19);
counter_t num_reductions = 0;
counter_t num_alloc = 0;
counter_t num_gc = 0;
counter_t num_yield = 0;
counter_t num_resched = 0;
counter_t num_thread_reap = 0;
counter_t num_mvar_alloc = 0;
counter_t num_mvar_free = 0;
counter_t num_stable_alloc = 0;
counter_t num_stable_free = 0;
counter_t num_new_weak = 0;
counter_t num_gc_weak = 0;
uintptr_t gc_mark_time = 0;
uintptr_t gc_scan_time = 0;
uintptr_t run_time = 0;
uintptr_t boot_time = 0;

#define MAIN_THREAD 1
uvalue_t num_thread_create = MAIN_THREAD;

#define MAXSTACKDEPTH 0
#if MAXSTACKDEPTH
stackptr_t max_stack_depth = 0;
counter_t max_c_stack = 0;
counter_t cur_c_stack = 0;
#define MAXSTACK if (stack_ptr > max_stack_depth) max_stack_depth = stack_ptr
#else
#define MAXSTACK
#endif

NODEPTR atptr;

REGISTER(NODEPTR *stack,r20);
REGISTER(stackptr_t stack_ptr,r21);
#if STACKOVL
#define PUSH(x) do { if (stack_ptr >= stack_size-2) stackerr(); stack[++stack_ptr] = (x); MAXSTACK; } while(0)
#else  /* STACKOVL */
#define PUSH(x) do {                                            stack[++stack_ptr] = (x); MAXSTACK; } while(0)
#endif  /* STACKOVL */
#define TOP(n) stack[stack_ptr - (n)]
#define POP(n) stack_ptr -= (n)
#define POPTOP() stack[stack_ptr--]
#define GCCHECK(n) gc_check((n))
#define CLEARSTK() do { stack_ptr = -1; } while(0)
#define GCCHECKSAVE(p, n) do { PUSH(p); GCCHECK(n); (p) = TOP(0); POP(1); } while(0)

heapoffs_t heap_size;       /* number of heap cells */
heapoffs_t heap_start;      /* first location in heap that needs GC */
REGISTER(stackptr_t stack_size,r22);      /* number of stack slots */

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

NORETURN void
memerr(void)
{
  ERR("Out of memory");
}

#if WANT_PROFILE
void prof_stack_dump(void);
#endif

NORETURN
void
stackerr(void)
{
#if WANT_PROFILE
  prof_stack_dump();
#endif
  ERR("stack overflow");
}

/***************************************/

#include "bfile.c"

/***************************************/

struct ioarray*
arr_alloc(size_t sz, NODEPTR e)
{
  struct ioarray *arr = mmalloc(sizeof(struct ioarray) + (sz-1) * sizeof(NODEPTR));
  size_t i;

  arr->next = array_root;
  array_root = arr;
  arr->marked = 0;
  arr->permanent = false;
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
  struct ioarray *arr = mmalloc(sizeof(struct ioarray) + (sz-1) * sizeof(NODEPTR));

  arr->next = array_root;
  array_root = arr;
  arr->marked = 0;
  arr->permanent = false;
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
    tick_table = mmalloc(tick_table_size * sizeof(struct tick_entry));
    tick_index = 0;
  }
  if (tick_index >= tick_table_size) {
    tick_table_size *= 2;
    tick_table = mrealloc(tick_table, tick_table_size * sizeof(struct tick_entry));
  }
  tick_table[tick_index].tick_name = name;
  tick_table[tick_index].tick_count = 0;
  return tick_index++;
}

/* Called with the tick index. */
static INLINE void
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
      fprintf(f, "%-60s %10"PRIcounter"\n", (char *)tick_table[i].tick_name.bs_array, n);
  }
}
#endif

enum th_sched { mt_main, mt_resched, mt_raise };
/* The two enums below are known by the Haskell code.  Do not change order */
enum th_state {
  ts_runnable,
  ts_wait_mvar,
  ts_wait_time,
  ts_finished,
  ts_died,
  ts_wait_io,   /* not visible to Haskell; must stay after ts_died */
};
enum mask_state { mask_unmasked, mask_interruptible, mask_uninterruptible };

/***************** HANDLER *****************/

struct handler {
  jmp_buf         hdl_buf;      /* env storage */
  struct handler *hdl_old;      /* old handler */
  NODEPTR         hdl_exn;      /* used temporarily to pass the exception value */
} *cur_handler = 0;

/***************** THREAD ******************/

struct mthread {
  enum th_state   mt_state;      /* thread state */
  enum mask_state mt_mask;       /* making state. */
  struct mthread *mt_next;       /* all threads linked together */
  struct mthread *mt_queue;      /* runq/waitq link */
  counter_t       mt_slice;      /* reduction steps until yielding */
  counter_t       mt_num_slices; /* number of slices so far */
  NODEPTR         mt_root;       /* root of the graph to reduce */
  struct mvar    *mt_exn;        /* possible thrown exception */
  struct mthread *mt_throwto;    /* When set, we are in throwTo to this thread and have
                                  * already handed over the exception; we are waiting for
                                  * the target to take it.  See throwto(). */
  bool            mt_throwdone;  /* The target has taken the exception (or is gone). */
  NODEPTR         mt_mval;       /* Filled when put_mvar wakes a thread waiting in read_mvar.
                                  * The value cannot be taken from the mvar by the read, because
                                  * we need to guarantee that all reads get the same value. */
  bool            mt_mark;       /* marked as accessible */
  uvalue_t        mt_id;         /* thread number, thread 1 is the main thread */
#if WANT_IO_POLL
  int             mt_fd;         /* The file descriptor that we are waiting on,
                                  * IO_POLL_WAITING_FOR_NONE, or IO_POLL_EVENT_HAS_HAPPENED */
#define             IO_POLL_WAITING_FOR_NONE (-1)
#define             IO_POLL_EVENT_HAS_HAPPENED (-2)
  int             mt_events;     /* POLLIN or POLLOUT */
#endif /* WANT_IO_POLL */
#if defined(CLOCK_INIT)
  CLOCK_T         mt_at;         /* time to wake up when in threadDelay */
#endif
};
struct mthread  *all_threads = 0;   /* all threads */

struct mqueue {
  struct mthread *mq_head;
  struct mthread *mq_tail;
};
struct mqueue runq  = { 0, 0 }; /* runnable threads */
struct mqueue timeq = { 0, 0 }; /* waiting for a timer to expire, sorted in time order */
struct mqueue pollq = { 0, 0 }; /* waiting for I/O on a file descriptor */

struct mvar {
  struct mvar    *mv_next;      /* all mvars linked together */
  NODEPTR         mv_data;      /* contents of the mvar, or NIL when empty */
  struct mqueue   mv_takeput;   /* queue of threads waiting for take or put, single wakeup */
  struct mqueue   mv_read;      /* queue of threads waiting for read, multiple wakeup */
  bool            mv_mark;      /* marked as accessible */
};
struct mvar      *all_mvars = 0;   /* all mvars */

jmp_buf          sched;             /* jump here to yield */
counter_t        slice = SLICE;     /* normal time slice;
                                     * on an M4 Mac this is about 0.3ms */
//REGISTER(counter_t glob_slice,r23);
REGISTER(int glob_slice,r23);

NODEPTR          the_exn;       /* Used to propagate the exception for longjmp(sched, mt_raise) */

/****** StablePtr ******/

size_t sp_capacity = 4;         /* initial size of stable pointer table */
NODEPTR *sp_table;              /* stable pointer table */

static void
init_stableptr(void)
{
  sp_table = mmalloc(sp_capacity * sizeof(NODEPTR)); /* stable pointer table, all free */
  for (size_t i = 0; i < sp_capacity; i++)
    sp_table[i] = NIL_NODEPTR;
}

static uvalue_t
new_stableptr(NODEPTR n)
{
  size_t i;

  COUNT(num_stable_alloc);
  /* Linear search for an empty slot. */
  /* Not ideal, but fine for a small number of StablePtr. */
  for(i = 1; i < sp_capacity; i++) { /* index 0 reserved according to the spec */
    if (is_NIL(sp_table[i]))
      break;
  }
  if (i == sp_capacity) {
    /* table is full, so double its size */
    sp_capacity *= 2;
    sp_table = mrealloc(sp_table, sp_capacity * sizeof(NODEPTR));
    for(size_t j = i; j < sp_capacity; j++)
      sp_table[j] = NIL_NODEPTR;
  }
  sp_table[i] = n;
  return (uvalue_t)i;
}

static NODEPTR
deref_stableptr(uvalue_t sp)
{
  if (sp >= sp_capacity || is_NIL(sp_table[sp]))
    ERR("deref_stableptr");
  return sp_table[sp];
}

static void
free_stableptr(uvalue_t sp)
{
  if (sp >= sp_capacity || is_NIL(sp_table[sp]))
    ERR("free_stableptr");
  COUNT(num_stable_free);
  sp_table[sp] = NIL_NODEPTR;
}

/* The order of these must be kept in sync with Control.Exception.Internal.rtsExn */
enum rts_exn { exn_stackoverflow, exn_heapoverflow, exn_threadkilled, exn_userinterrupt,
               exn_dividebyzero, exn_blockedmvar, exn_blockedstm, exn_overflow,
               exn_serialize, exn_deserialize, exn_last };

NORETURN void raise_exn(NODEPTR exn);
struct mvar* new_mvar(void);
NODEPTR take_mvar(bool try, struct mvar *mv);
NORETURN void die_exn(NODEPTR exn);
void thread_intr(struct mthread *mt);
int put_mvar(bool try, struct mvar *mv, NODEPTR v);
NODEPTR mkInt(value_t i);
NODEPTR mkInt64(int64_t i);
NODEPTR mkFlt32(flt32_t d);
NODEPTR mkFlt64(flt64_t d);
NODEPTR mkPtr(void* p);
struct mthread* new_thread(NODEPTR root);
void gc(void);
void async_throwto(struct mthread*, NODEPTR);

#if WANT_STDIO
void pp(FILE*, NODEPTR);
#endif

/* Needed during reduction */
NODEPTR intTable[HIGH_INT - LOW_INT];

#define combK               mk_TAG(T_K)
#define combA               mk_TAG(T_A)
#define combI               mk_TAG(T_I)
#define combB               mk_TAG(T_B)
#define combC               mk_TAG(T_C)
#define combZ               mk_TAG(T_Z)
#define combU               mk_TAG(T_U)
#define combK2              mk_TAG(T_K2)
#define combK3              mk_TAG(T_K3)
#define combBB              mk_TAG(T_BB)
#define combCC              mk_TAG(T_CC)
#define combP               mk_TAG(T_P)
#define combIOBIND          mk_TAG(T_IO_BIND)
#define combIORETURN        mk_TAG(T_IO_RETURN)
#define combIOTHEN          mk_TAG(T_IO_THEN)
#define combSETMASKINGSTATE mk_TAG(T_IO_SETMASKINGSTATE)
#define combPERFORMIO       mk_TAG(T_IO_PERFORMIO)
#define combBININT1         mk_TAG(T_BININT1)
#define combBININT2         mk_TAG(T_BININT2)
#define combUNINT1          mk_TAG(T_UNINT1)
#define combBININT64_1      mk_TAG(T_BININT64_1)
#define combBININT64_2      mk_TAG(T_BININT64_2)
#define combUNINT64_1       mk_TAG(T_UNINT64_1)
#define combBINFLT1         mk_TAG(T_BINFLT1)
#define combBINFLT2         mk_TAG(T_BINFLT2)
#define combUNFLT1          mk_TAG(T_UNFLT1)
#define combBINDBL1         mk_TAG(T_BINDBL1)
#define combBINDBL2         mk_TAG(T_BINDBL2)
#define combUNDBL1          mk_TAG(T_UNDBL1)
#define combBINBS1          mk_TAG(T_BINBS1)
#define combBINBS2          mk_TAG(T_BINBS2)
#define combTHROWTO         mk_TAG(T_IO_THROWTO)
#define combCATCHR          mk_TAG(T_CATCHR)
#define combFP2P            mk_TAG(T_FP2P)

#define combFalse           mk_CON(V_CONSTR, 0, 0)
#define combTrue            mk_CON(V_CONSTR, 1, 0)
#define combNothing         mk_CON(V_CONSTR, 0, 0)
#define combJust            mk_CON(V_CONSTR, 1, 1)
#define combUnit            mk_CON(V_CONSTR, 0, 0)
#define combNil             mk_CON(V_CONSTR, 0, 0)
#define combCons            mk_CON(V_CONSTR, 1, 2)
#define combPair            mk_CON(V_CONSTR, 0, 2)
#define combLT              mk_CON(V_CONSTR, 0, 0)
#define combEQ              mk_CON(V_CONSTR, 1, 0)
#define combGT              mk_CON(V_CONSTR, 2, 0)

NODEPTR spare_node;             /* an unused node in the heap, used in printrec */
NODEPTR combShowExn;
NODEPTR comb_stdin, comb_stdout, comb_stderr;

NODEPTR combPFst, combPSnd;
NODEPTR combPUnit;
NODEPTR combWorld;

/*******************************/

#if WANT_ARGS
/* This single element array hold a list of the program arguments. */
struct ioarray *argarray;
#endif  /* WANT_ARGS */

int verbose = 0;
int gcbell = 0;


#if WANT_SIGINT
volatile bool has_sigint = false;
void
handle_sigint(int s)
{
  has_sigint = true;
}
#endif

/* Check that there are k nodes available, if not then GC. */
INLINE void
gc_check(size_t k)
{
  if (k < num_free)
    return;
  gc();
}

/* Add the thread to the tail of runq */
void
add_q_tail(struct mqueue *q, struct mthread *mt)
{
  if (!q->mq_head) {
    /* q is empty, so mt goes first */
    q->mq_head = mt;
  } else {
    /* link mt to the end of the runq */
    q->mq_tail->mt_queue = mt;
  }
  q->mq_tail = mt;               /* mt is now last */
  mt->mt_queue = 0;           /* mt is last, so no next */
}

void
add_runq_tail(struct mthread *mt)
{
  mt->mt_state = ts_runnable;
  add_q_tail(&runq, mt);
}

/* Add the thread to the head of runq */
void
add_runq_head(struct mthread *mt)
{
  mt->mt_state = ts_runnable;
  mt->mt_queue = runq.mq_head;
  runq.mq_head = mt;
  if (!runq.mq_tail)
    runq.mq_tail = mt;           /* runq was empty */
}

/* Add the thread k to runq right after mt, which is in runq */
void
add_runq_after(struct mthread *mt, struct mthread *k)
{
  k->mt_state = ts_runnable;
  k->mt_queue = mt->mt_queue;
  mt->mt_queue = k;
  if (runq.mq_tail == mt)
    runq.mq_tail = k;            /* mt was last */
}

struct mthread*
remove_q_head(struct mqueue *q)
{
  struct mthread *mt = q->mq_head; /* front thread */
  if (!mt)
    return 0;
  q->mq_head = mt->mt_queue;       /* skip to next thread */
  if (!q->mq_head)
    q->mq_tail = 0;                /* q is now empty */
  return mt;
}

int
find_and_unlink(struct mqueue *mq, struct mthread *mt)
{
  struct mthread **mtp;

  for(mtp = &mq->mq_head; *mtp && *mtp != mt; mtp = &(*mtp)->mt_queue)
    ;
  if (!*mtp)
    return 0;                   /* not found */
  *mtp = mt->mt_queue;          /* unlink */
  if (*mtp)
    return 1;                   /* the unlinked thread was not the tail */
  if (mq->mq_head) {
    for (mt = mq->mq_head; mt->mt_queue; mt = mt->mt_queue)
      ;                         /* find the last element */
    mq->mq_tail = mt;
  } else {
    /* q is empty */
    mq->mq_tail = 0;
  }
  return 1;
}

/* This is a yucky hack */
bool doing_rnf = false;              /* REMOVE */
#if THREAD_DEBUG
const bool thread_trace = false;
#endif  /* THREAD_DEBUG */

/* clean up temporary globals to prepare for rescheduling */
void
cleanup(struct mthread *mt, enum th_state ts)
{
  /* We are going to reschedule, so clean up thread state:
   *  stack pointer
   *  error handlers
   */
#if THREAD_DEBUG
  if (thread_trace)
    printf("cleanup: %d state=%d\n", (int)mt->mt_id, ts);
#endif  /* THREAD_DEBUG */
  mt->mt_slice = stack_ptr;   /* we need stack_ptr reductions to just reach where we left off */
  mt->mt_state = ts;
  CLEARSTK();                 /* reset stack */
  doing_rnf = false;
  /* free all error handlers */
  for (struct handler *h = cur_handler; h; ) {
    struct handler *n = h;
    h = h->hdl_old;
    FREE(n);
  }
  cur_handler = 0;
}

/* reschedule, does not return */
NORETURN void
resched(struct mthread *mt, enum th_state ts)
{
  cleanup(mt, ts);
  longjmp(sched, mt_resched);
}

#if THREAD_DEBUG
void
dump_q(const char *s, struct mqueue q)
{
  printf("   %s=[", s);
  for(struct mthread *mt = q.mq_head; mt; mt = mt->mt_queue) {
    printf("%d ", (int)mt->mt_id);
  }
  printf("]\n");
}
#endif  /* THREAD_DEBUG */

/* Check if its time to wake up some threads waiting for a time. */
void
check_timeq(void)
{
#if defined(CLOCK_INIT)
  CLOCK_T now = CLOCK_GET();
  while (timeq.mq_head && timeq.mq_head->mt_at <= now) {
    struct mthread *mt = remove_q_head(&timeq);
    add_runq_tail(mt);
    mt->mt_at = -1;             /* indicate that the delay has expired */
#if THREAD_DEBUG
    if (thread_trace)
      printf("check_timeq: %d done\n", (int)mt->mt_id);
#endif  /* THREAD_DEBUG */
  }
#if THREAD_DEBUG
  if (thread_trace) {
    printf("check_timeq: exit\n");
    dump_q("runq", runq);
  }
#endif  /* THREAD_DEBUG */
#endif
}

void
check_pollq(int timeout)
{
#if WANT_IO_POLL
#define MAX_POLL_FDS 100
  struct pollfd fds[MAX_POLL_FDS];
  int nfds = 0;
  for(struct mthread *mt = pollq.mq_head; mt; mt = mt->mt_queue) {
    if (nfds >= MAX_POLL_FDS)
      ERR("check_pollq: too many FDs");
    fds[nfds].fd = mt->mt_fd;
    fds[nfds].events = mt->mt_events;
    nfds++;
  }
#if THREAD_DEBUG
  if (thread_trace)
    printf("check_pollq: enter poll(_, %d, %d)\n", nfds, timeout);
#endif  /* THREAD_DEBUG */
  int r = poll(fds, nfds, timeout);
  if (r < 0)
    return;                     /* silently ignore errors */
  nfds = 0;
  struct mthread *next;
  for(struct mthread *mt = pollq.mq_head; mt; mt = next) {
    next = mt->mt_queue;
    if (fds[nfds].revents & (mt->mt_events | POLLHUP)) {
      /* Some event has happened, move the thread back to the runq. */
      find_and_unlink(&pollq, mt); /* remove from I/O queue */
      add_runq_tail(mt);
#if THREAD_DEBUG
    if (thread_trace)
      printf("check_pollq: FD=%d thread=%d done\n", mt->mt_fd, (int)mt->mt_id);
#endif  /* THREAD_DEBUG */
      mt->mt_fd = IO_POLL_EVENT_HAS_HAPPENED;
    }
    nfds++;
  }
#if THREAD_DEBUG
  if (thread_trace) {
    printf("check_pollq: exit\n");
    dump_q("runq", runq);
  }
#endif  /* THREAD_DEBUG */
#endif  /* WANT_IO_POLL */
}

/* Wake every thread that is blocked in throwTo waiting for mt to take its
 * exception.  Called when mt takes one, and when mt finishes or dies. */
void
wake_throwto(struct mthread *mt)
{
  for (struct mthread *k; (k = remove_q_head(&mt->mt_exn->mv_read)); ) {
#if THREAD_DEBUG
    if (thread_trace)
      printf("wake_throwto: wake %d, which threw to %d\n", (int)k->mt_id, (int)mt->mt_id);
#endif  /* THREAD_DEBUG */
    /* The exception has been delivered, so the thrower continues next: at
     * the tail of the run queue it would first wait for a slice of every
     * other runnable thread, which made a loop of killThread quadratic. */
    k->mt_throwdone = true;
    if (runq.mq_head == mt)
      add_runq_after(mt, k);    /* mt is the running thread (check_thrown): k goes right after it */
    else
      add_runq_head(k);
  }
}

/* mt has finished or died, so it takes no more exceptions.  Release everyone
 * blocked in throwTo to it: the thrower whose exception is in mt's exception
 * MVar waits on its read queue, and further throwers wait in its put queue
 * because the MVar is full.  Each of them runs throwTo again, finds mt gone,
 * and returns. */
void
release_throwto(struct mthread *mt)
{
  wake_throwto(mt);
  mt->mt_exn->mv_data = NIL_NODEPTR; /* an exception that was never taken */
  for (struct mthread *k; (k = remove_q_head(&mt->mt_exn->mv_takeput)); ) {
#if THREAD_DEBUG
    if (thread_trace)
      printf("release_throwto: wake %d, which threw to %d\n", (int)k->mt_id, (int)mt->mt_id);
#endif  /* THREAD_DEBUG */
    add_runq_tail(k);
  }
}

void
throwto(struct mthread *mt, NODEPTR exn)
{
#if THREAD_DEBUG
  if (thread_trace) {
    printf("throwto: id=%d\n", (int)mt->mt_id);
  }
#endif  /* THREAD_DEBUG */
  struct mthread *me = runq.mq_head;

  if (mt == me) {
    /* Throwing to ourselves: just hand the exception over.  It is raised at the
     * next interruption point, so there is nothing to wait for. */
    (void)put_mvar(false, mt->mt_exn, exn); /* never returns if it blocks */
    return;
  }
  if (me->mt_throwto == mt) {
    /* We handed the exception over in an earlier execution of this throwTo and
     * blocked; being back here means the target has taken it, or has died. */
#if THREAD_DEBUG
    if (thread_trace)
      printf("throwto: id=%d delivered\n", (int)mt->mt_id);
#endif  /* THREAD_DEBUG */
    me->mt_throwto = 0;
    return;
  }
  thread_intr(mt);
  if (mt->mt_state == ts_died || mt->mt_state == ts_finished)
    return;                     /* there is nothing left to interrupt */
#if THREAD_DEBUG
  if (thread_trace) {
    printf("throwto: id=%d put_mvar exn\n", (int)mt->mt_id);
  }
#endif  /* THREAD_DEBUG */
  (void)put_mvar(false, mt->mt_exn, exn); /* never returns if it blocks */
  /* GHC's throwTo does not return until the exception has been raised in the
   * target thread, so wait for the target to take it.  Without this the target
   * stays runnable with an exception pending, and can still run a stretch of
   * code -- and consume shared state -- before it is interrupted.
   * We wait on the read queue of the target's exception MVar: check_thrown and
   * a finishing thread wake it, and thread_intr searches it, so the wait is
   * itself interruptible. */
  me = remove_q_head(&runq);
  me->mt_throwto = mt;
  me->mt_throwdone = false;
  add_q_tail(&mt->mt_exn->mv_read, me);
  /* Let the target run next.  If it is somewhere down the run queue (it was
   * runnable, or thread_intr has just put it at the tail) it would otherwise
   * take the exception only after every thread ahead of it has had a slice,
   * so killing n runnable threads cost n passes over the run queue. */
  if (mt->mt_state == ts_runnable && find_and_unlink(&runq, mt))
    add_runq_head(mt);
  resched(me, ts_wait_mvar);     /* never returns */
}

/* mt is interrupted while it waits in throwTo.  As in GHC, an interrupted throwTo
 * has no effect: if the target has not taken mt's exception yet, take it back.
 * (It is the one in the target's exception MVar, since mt waits on its read queue
 * until the target takes it.)  The next thrower waiting to put one may then go on. */
void
revoke_throwto(struct mthread *mt)
{
  struct mthread *target = mt->mt_throwto;
  if (target && !mt->mt_throwdone) {
#if THREAD_DEBUG
    if (thread_trace)
      printf("revoke_throwto: %d revokes its exception to %d\n", (int)mt->mt_id, (int)target->mt_id);
#endif  /* THREAD_DEBUG */
    /* Take our exception back; a thread waiting to throw to target may then put its own.
     * The MVar holds our exception, so this does not block. */
    (void)take_mvar(true, target->mt_exn);
  }
  mt->mt_throwto = 0;
  mt->mt_throwdone = false;
}

void
check_thrown(bool intr)
{
  if (runq.mq_head->mt_mask == mask_uninterruptible ||
      (!intr && runq.mq_head->mt_mask == mask_interruptible)) {
    /* interrupts are masked, so don't throw */
    return;
  }
  NODEPTR exn = take_mvar(true, runq.mq_head->mt_exn); /* get the exception */
  if (is_NIL(exn))
    return;            /* no thrown exception */
  /* the current thread has an async exception */
  wake_throwto(runq.mq_head);    /* the throwTo that sent it may now return */
  revoke_throwto(runq.mq_head);  /* we are unwinding; drop any throwTo of our own */
#if THREAD_DEBUG
  if (thread_trace)
    printf("check_thrown: exn for %d\n", (int)runq.mq_head->mt_id);
#endif  /* THREAD_DEBUG */

  raise_exn(exn);
}

void
check_sigint(void)
{
#if WANT_SIGINT
  if (has_sigint) {
    /* We have a signal, so send an async exception to the main thread */
    has_sigint = false;
    for(struct mthread *mt = all_threads; mt; mt = mt->mt_next) {
      if (mt->mt_id == MAIN_THREAD) {
#if THREAD_DEBUG
        if (thread_trace)
          printf("sending signal to main\n");
#endif  /* THREAD_DEBUG */
        async_throwto(mt, mkInt(exn_userinterrupt));
        break;
      }
    }
  }
#endif
}

/* Used to detect calls to error while we are already in a call to error. */
bool in_raise = false;

/* Inlining makes very little difference */
/*static INLINE*/ void
yield(void)
{
  if (in_raise)                 /* don't context switch when we are dying */
    return;
  COUNT(num_yield);
  runq.mq_head->mt_num_slices++;
  // XXX should check mt_thrown here

  YIELD_EXTRA;                  /* platform specific extra stuff */

  if (timeq.mq_head)
    check_timeq();
  check_thrown(false);
  check_sigint();

  if (pollq.mq_head) {
    /* Check if any threads blocked on IO can be scheduled. Since we pass in a delay of 0, checking
     * for the events will not block. */
    check_pollq(0);
  }

// printf("yield %p %d\n", runq, (int)stack_ptr);
  /* if there is nothing after in the runq then there is no need to reschedule */
  if (!runq.mq_head->mt_queue) {
#if THREAD_DEBUG
    if (thread_trace) {
      printf("yield: %d no other threads\n", (int)runq.mq_head->mt_id);
      dump_q("runq", runq);
    }
#endif  /* THREAD_DEBUG */
    glob_slice = slice;
    num_reductions += glob_slice-1;
    return;
  }

  /* Unlink from runq */
  struct mthread *mt = remove_q_head(&runq);
  /* link into back of runq */
  add_runq_tail(mt);
#if THREAD_DEBUG
  if (thread_trace) {
    printf("yield: resched %d\n", (int)mt->mt_id);
    dump_q("runq", runq);
  }
#endif  /* THREAD_DEBUG */
  resched(mt, ts_runnable);
}

struct mthread*
new_thread(NODEPTR root)
{
  struct mthread *mt = mmalloc(sizeof(struct mthread));

#if THREAD_DEBUG
  if (thread_trace) {
    printf("new_thread: mt=%p root=%p\n", mt, root.node_ptr);
  }
#endif  /* THREAD_DEBUG */
  mt->mt_mask = mask_unmasked;
  mt->mt_root = root;
  mt->mt_exn = new_mvar();
  mt->mt_throwto = 0;
  mt->mt_throwdone = false;
  mt->mt_mval = NIL_NODEPTR;
  mt->mt_slice = 0;
  mt->mt_mark = false;
  mt->mt_num_slices = 0;
  mt->mt_id = num_thread_create++;
#if WANT_IO_POLL
  mt->mt_fd = IO_POLL_WAITING_FOR_NONE;
  mt->mt_events = 0;
#endif
#if defined(CLOCK_INIT)
  mt->mt_at = 0;                /* delay has not expired */
#endif

  /* add to all_threads */
  mt->mt_next = all_threads;
  all_threads = mt;

  /* add to tail of runq */
  add_runq_tail(mt);            /* sets runnable */
#if THREAD_DEBUG
  if (thread_trace) {
    printf("new_thread: add %d to runq tail\n", (int)mt->mt_id);
    dump_q("runq", runq);
  }
#endif  /* THREAD_DEBUG */
  return mt;
}

struct mvar*
new_mvar(void)
{
  COUNT(num_mvar_alloc);
  struct mvar *mv = mmalloc(sizeof(struct mvar));

  mv->mv_data = NIL_NODEPTR;
  mv->mv_takeput.mq_head = 0;
  mv->mv_takeput.mq_tail = 0;
  mv->mv_read.mq_head = 0;
  mv->mv_read.mq_tail = 0;

  /* add to all_mvars */
  mv->mv_next = all_mvars;
  mv->mv_mark = false;
  all_mvars = mv;

#if THREAD_DEBUG
  if (thread_trace)
    printf("new_mvar: mvar=%p\n", mv);
#endif  /* THREAD_DEBUG */
  return mv;
}

NODEPTR
take_mvar(bool try, struct mvar *mv)
{
#if THREAD_DEBUG
  if (thread_trace) {
    printf("take_mvar: start mvar=%p\n", mv);
    dump_q("takeput", mv->mv_takeput);
  }
#endif  /* THREAD_DEBUG */
  NODEPTR n;
  if (!is_NIL(n = mv->mv_data)) {
    /* The mvar is full. */
#if THREAD_DEBUG
    if (thread_trace)
      printf("take_mvar: mvar=%p full\n", mv);
#endif  /* THREAD_DEBUG */
    mv->mv_data = NIL_NODEPTR;           /* now empty */

    /* move one thread waiting to put to the runq */
    struct mthread *mt;
    if((mt = remove_q_head(&mv->mv_takeput))) {
#if THREAD_DEBUG
      if (thread_trace) {
        printf("take_mvar: mvar=%p wake %d\n", mv, (int)mt->mt_id);
      }
#endif  /* THREAD_DEBUG */
      add_runq_tail(mt);
#if THREAD_DEBUG
      if (thread_trace) {
        dump_q("runq", runq);
      }
#endif  /* THREAD_DEBUG */
    }
#if THREAD_DEBUG
    if (thread_trace) {
      printf("take_mvar: end mvar=%p return %p\n", mv, n.node_ptr);
    }
#endif  /* THREAD_DEBUG */
    return n;                   /* return the data */
  } else {
    /* The mvar is empty. */
#if THREAD_DEBUG
    if (thread_trace)
      printf("take_mvar: mvar=%p empty\n", mv);
#endif  /* THREAD_DEBUG */
    /* mvar is empty */
    if (try)
      return NIL_NODEPTR;
    struct mthread *mt = remove_q_head(&runq);
    add_q_tail(&mv->mv_takeput, mt);
#if THREAD_DEBUG
    if (thread_trace) {
      printf("take_mvar: end mvar=%p suspend %d\n", mv, (int)mt->mt_id);
      dump_q("runq", runq);
      dump_q("takeput", mv->mv_takeput);
    }
#endif  /* THREAD_DEBUG */
    /* Unlink from runq */
    resched(mt, ts_wait_mvar);    /* never returns */
    NOTREACHED;
  }
}

NODEPTR
read_mvar(bool try, struct mvar *mv)
{
  NODEPTR n;
  if (!is_NIL(n = runq.mq_head->mt_mval)) {
    /* The putMVar delivers the data in mt_mval and wakes the thread.
     * So first first check if we already have data in mt_mval. */
    runq.mq_head->mt_mval = NIL_NODEPTR; /* empty again */
    return n;                    /* returned the stashed data */
  }
  if (!is_NIL(n = mv->mv_data)) {
    /* mvar is full */
    return n;                   /* return the data */
  } else {
    /* mvar is empty */
    if (try)
      return NIL_NODEPTR;
#if THREAD_DEBUG
    if (thread_trace) {
      printf("read_mvar: suspend %d\n", (int)runq.mq_head->mt_id);
      dump_q("runq", runq);
    }
#endif  /* THREAD_DEBUG */
    struct mthread *mt = remove_q_head(&runq);
    add_q_tail(&mv->mv_read, mt);
    resched(mt, ts_wait_mvar);                /* never returns */
    NOTREACHED;
  }
}

int
put_mvar(bool try, struct mvar *mv, NODEPTR v)
{
#if THREAD_DEBUG
  if (thread_trace) {
    printf("put_mvar: mvar=%p\n", mv);
    dump_q("takeput", mv->mv_takeput);
    dump_q("read", mv->mv_read);
  }
#endif  /* THREAD_DEBUG */
  if (!is_NIL(mv->mv_data)) {
    /* The mvar is full. */
#if THREAD_DEBUG
    if (thread_trace)
      printf("put_mvar: mvar=%p full\n", mv);
#endif  /* THREAD_DEBUG */
    if (try)
      return 0;
    struct mthread *mt = remove_q_head(&runq);
    add_q_tail(&mv->mv_takeput, mt); /* put on mvar queue */
#if THREAD_DEBUG
    if (thread_trace) {
      printf("put_mvar: suspend %d\n", (int)mt->mt_id);
      dump_q("runq", runq);
      dump_q("takeput", mv->mv_takeput);
    }
#endif  /* THREAD_DEBUG */
    resched(mt, ts_wait_mvar);                  /* never returns */
  } else {
    /* The mvar is empty. */
#if THREAD_DEBUG
    if (thread_trace)
      printf("put_mvar: mvar=%p empty\n", mv);
#endif  /* THREAD_DEBUG */
    /* mvar is empty */
    mv->mv_data = v;              /* put the data in the mvar */
    if (mv->mv_takeput.mq_head || mv->mv_read.mq_head) {
      /* one or more threads are waiting */
      struct mthread *mt;
      if ((mt = remove_q_head(&mv->mv_takeput))) {
        /* wake up one 'take' */
#if THREAD_DEBUG
        if (thread_trace)
          printf("put_mvar: wake-1 %d\n", (int)mt->mt_id);
#endif  /* THREAD_DEBUG */
        add_runq_tail(mt);             /* and schedule for execution later */
      }
      for(;;) {
        /* wake up all 'read' */
        mt = remove_q_head(&mv->mv_read);
        if (!mt)
          break;
#if THREAD_DEBUG
        if (thread_trace)
          printf("put_mvar: wake-N %d\n", (int)mt->mt_id);
#endif  /* THREAD_DEBUG */
        if (!is_NIL(mt->mt_mval))
          ERR("put_mvar: mt_mval != NIL");
        mt->mt_mval = v;               /* value for restarted read */
        add_runq_tail(mt);             /* and schedule for execution later */
      }
#if THREAD_DEBUG
      if (thread_trace) {
        printf("put_mvar: end\n");
        dump_q("runq", runq);
      }
#endif  /* THREAD_DEBUG */
      /* return to caller */
    } else {
#if THREAD_DEBUG
      if (thread_trace) {
        printf("put_mvar: mvar=%p no waiters\n", mv);
      }
#endif  /* THREAD_DEBUG */
      /* no threads waiting, so store the value */
      mv->mv_data = v;
      /* return to caller */
    }
  }
  return 1;
}

NORETURN void
thread_delay(uvalue_t usecs)
{
#if !defined(CLOCK_INIT)
  ERR("thread_delay: no clock");
#else
  /* XXX should check if there is already a throw exn */
  struct mthread *mt = remove_q_head(&runq);
  mt->mt_at = CLOCK_GET() + usecs; /* wakeup time */
#if THREAD_DEBUG
  if (thread_trace)
    printf("thread_delay: id=%d usecs=%ld\n", (int)mt->mt_id, (long)usecs);
#endif  /* THREAD_DEBUG */
  /* insert in delayq which is kept sorted in time order */
  struct mthread **tq;
  for (tq = &timeq.mq_head; *tq; tq = &(*tq)->mt_queue) {
    if (mt->mt_at <= (*tq)->mt_at)
      break;
  }
  mt->mt_queue = *tq;           /* forward link */
  *tq = mt;                     /* and put mt in place */
  if (!mt->mt_queue)            /* no forward link */
    timeq.mq_tail = mt;
  resched(mt, ts_wait_time);
#endif
}

/* Pause execution if something might still happen */
void
pause_exec(void)
{
/*
 * We end up here if the run queue is empty. If there is no thread waiting for
 * a delay to expire, we will never resume operation and we are deadlocked. However, if
 * we compile with WANT_IO_POLL there might be threads waiting for IO events, so in
 * that case we check for them as well. If there is no thread waiting for a delay or an
 * IO event, we are deadlocked.
 */
#if WANT_IO_POLL
  /* Check for deadlock situation */
  if (!pollq.mq_head
#if defined(CLOCK_INIT)
     && !timeq.mq_head
#endif
    ) ERR("deadlock");

  /* Loop until at least one thread is runnable.*/
  while (!runq.mq_head) {
    int timeout_ms = -1; /* block indefinitely if only io_waiters */
#if defined(CLOCK_INIT)
    /* If there are threads blocked on delays, compute the timeout_ms to account for that. */
    if (timeq.mq_head) {
      CLOCK_T dly = timeq.mq_head->mt_at - CLOCK_GET();
      if (dly > 0) {
        /* poll() can be unreliable, so sleep shorter than the delay */
        dly /= 1100;            /* 1.1=sleep shorter, 1000=convert us to ms */
        timeout_ms = dly == 0 ? 1 : dly; /* sleep at least 1ms to avoid busy wait */
      } else {
        timeout_ms = 0;         /* delay has already expired */
      }
    }
    check_timeq();
#endif  /* defined(CLOCK_INIT) */
    check_pollq(timeout_ms);
    check_sigint();		/* if there is a SIGINT, this will put a thread on the runq */
  }

#else /* !WANT_IO_POLL */

#if defined(CLOCK_INIT)
  if (timeq.mq_head) {
    struct mthread *mt;
    while (!runq.mq_head && (mt = timeq.mq_head)) {
      /* We are waiting for a delay to expire, so sleep a while */
      CLOCK_T dly = mt->mt_at - CLOCK_GET();
      if (dly > 0) {
        /* usleep() can be unreliable, so sleep shorter than the delay */
        dly /= 4;
        if (dly < 50) dly = 50;
        CLOCK_SLEEP((useconds_t)dly);
      }
      check_timeq();
    }
  } else {
#if THREAD_DEBUG
    if (0) {
      dump_q("runq", runq);
      dump_q("timeq", timeq);
      if (0) {
        for(struct mvar *mv = all_mvars; mv; mv = mv->mv_next) {
          printf("mvar %p, data=%p\n", mv, mv->mv_data.node_ptr);
          dump_q("takeput", mv->mv_takeput);
        }
      }
      for(struct mthread *mt = all_threads; mt; mt = mt->mt_next) {
        if (!is_NIL(mt->mt_exn->mv_data)) {
          printf("### bad thread ThreadId#%d mask=%d state=%d\n", (int)mt->mt_id, mt->mt_mask, mt->mt_state);
        }
      }
    }
#endif               /* THREAD_DEBUG */
    ERR("deadlock");            /* XXX throw async to main thread */
  }
#else  /* CLOCK_INIT */
  ERR("no clock");
#endif  /* CLOCK_INIT */
#endif /* !WANT_IO_POLL */
}

/* Interrupt a sleeping thread in a throwTo/threadDelay */
void
thread_intr(struct mthread *mt)
{
#if THREAD_DEBUG
  if (thread_trace)
    printf("thread_intr: id=%d state=%d\n", (int)mt->mt_id, mt->mt_state);
#endif  /* THREAD_DEBUG */
  switch(mt->mt_state) {
  case ts_runnable:
    break;                      /* already on runq */
  case ts_wait_mvar:
    if (mt->mt_mask == mask_uninterruptible) /* uninterruptible */
      break;
    /* we don't know which mvar we are waiting on, so look at all of them */
    /* XXX should add a pointer in mthread to the mvar */
    for (struct mvar *mv = all_mvars; mv; mv = mv->mv_next) {
      if (find_and_unlink(&mv->mv_takeput, mt))
          goto found;
      if (find_and_unlink(&mv->mv_read, mt))
          goto found;
    }
    ERR("thread_intr: mvar");
  found:
#if defined(CLOCK_INIT)
    mt->mt_at = -1;             /* don't wait again */
#endif
    mt->mt_mval = NIL_NODEPTR;          /* no longer waiting on the mvar */
    add_runq_tail(mt);
    break;
  case ts_wait_time:
#if THREAD_DEBUG
    if (thread_trace) {
      printf("thread_intr: ts_wait_time mask=%d\n", (int)mt->mt_mask);
    }
#endif  /* THREAD_DEBUG */
    if (mt->mt_mask == mask_uninterruptible) /* uninterruptible */
      break;
    /* find thread in timeq */
    if (!find_and_unlink(&timeq, mt))
      ERR("thread_intr: timeq");
    /* XXX should adjust mq_tail */
    add_runq_tail(mt);
    break;
  case ts_wait_io:
#if THREAD_DEBUG
    if (thread_trace) {
      printf("thread_intr: ts_wait_io mask=%d\n", (int)mt->mt_mask);
    }
#endif  /* THREAD_DEBUG */
    if (mt->mt_mask == mask_uninterruptible) /* uninterruptible */
      break;
    /* find thread in timeq */
    if (!find_and_unlink(&pollq, mt))
      ERR("thread_intr: pollq");
    /* XXX should adjust mq_tail */
    add_runq_tail(mt);
    break;
  case ts_finished:
  case ts_died:
#if THREAD_DEBUG
    if (thread_trace) {
      printf("thread_intr: finished/died\n");
    }
#endif  /* THREAD_DEBUG */
    break;
  default:
    ERR("thread_intr");
  }
#if THREAD_DEBUG
  if (thread_trace) {
    printf("thread_intr: done\n");
    dump_q("runq", runq);
  }
#endif  /* THREAD_DEBUG */
}

NORETURN void
raise_exn(NODEPTR exn)
{
#if THREAD_DEBUG
  if (thread_trace) {
    printf("raise_exn: %p\n", exn.node_ptr);
    dump_q("runq", runq);
  }
#endif  /* THREAD_DEBUG */

  if (cur_handler) {
    /* Pass the exception to the handler */
    cur_handler->hdl_exn = exn;
    longjmp(cur_handler->hdl_buf, 1);
  } else {
    /* No exception handler, jump to the scheduler */
    the_exn = exn;
    longjmp(sched, mt_raise);
  }
}

/* Raise a RTS exception identified by a number rather than an exception value */
NORETURN void
raise_rts(enum rts_exn exn) {
  raise_exn(mkInt(exn));
}

/***************** GC ******************/

/* Set FREE bit to 0 */
static INLINE void mark_used(NODEPTR n)
{
  struct node *p = get_node_ptr(n);
  if (!p)
    return;
  heapoffs_t i = p - cells;
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
  struct node *p = get_node_ptr(n);
  if (!p)
    return;
  heapoffs_t i = p - cells;
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
  struct node *p = get_node_ptr(n);
  if (!p)
    return 1;
  heapoffs_t i = p - cells;
  if (i < heap_start)
    return 1;
#if SANITY
  if (i >= free_map_nwords * BITS_PER_WORD)
    ERR("is_marked_used");
#endif
  return (free_map[i / BITS_PER_WORD] & (1ULL << (i % BITS_PER_WORD))) == 0;
}

#if WANT_SAMPLE
/*
 * Sampling profiler, to find hot function nodes (see jit-plan.md).
 * Every sample_interval reductions (+RTS -S[N] -RTS) look at the spine of
 * the node that is being evaluated.  Evaluation continues after a
 * reduction with the spine partly unwound, so the outer part of the spine
 * may already be on the stack.  The application nodes strictly inside the
 * outermost one are the candidates: partial applications, i.e., function
 * values, and their parts.  The outermost node is not a candidate, since
 * it is the redex (or an application of it), which is updated in place,
 * e.g., by every iteration of a loop.
 * Each candidate is counted in a hash table by its address.  A node that
 * is built for one call never recurs, so only shared nodes get counts.
 * The table is not a GC root; entries for freed nodes are dropped after
 * every GC.
 */
#if !defined(SAMPLE_TABLE_BITS)
#define SAMPLE_TABLE_BITS 18          /* 16K entries miss about 10% of the recurring nodes in the compiler */
#endif
#define SAMPLE_TABLE_SIZE (1 << SAMPLE_TABLE_BITS)
#define SAMPLE_MAX_DEPTH  32    /* how far down a spine to look */
#define SAMPLE_REPORT     30    /* number of entries to report */
#define SAMPLE_DUMP       10    /* number of entries to write to files */

struct sample_entry {
  NODEPTR   node;               /* NIL when empty */
  counter_t count;              /* +1 on a hit, -1 on a miss; replaced at 0 */
  counter_t hits;               /* number of hits since the node got the entry */
  counter_t outer;              /* number of hits where it was the outermost hit in its spine */
};
struct sample_entry *sample_table;
counter_t sample_interval = 0;           /* 0 means no sampling */
counter_t sample_left = ~(counter_t)0;   /* reductions until the next sample */
counter_t num_samples;                   /* number of samples */
counter_t num_samples_hit;               /* samples where some candidate recurred */
counter_t num_samples_none;              /* samples without any candidate */
counter_t sample_miss_heads[T_LAST_TAG+1]; /* the heads of the samples where no candidate recurred */

/* Extracting the subgraphs of hot nodes, see jx_extract. */
extern int jx_limit;
extern int jx_threshold;
static void jx_hot(struct sample_entry *e);
static void jx_gc(void);
static void jx_report(void);

static INLINE uint32_t
sample_hash(NODEPTR n)
{
  return (uint32_t)((n.node_bits >> 3) * 2654435761u) >> (32 - SAMPLE_TABLE_BITS);
}

/* Skip indirections. */
static NODEPTR
sample_skip_ind(NODEPTR p)
{
  while (!is_VIRT(p) && !is_AP(FUN(p)) && get_DATA_TAG(FUN(p)) == D_IND)
    p = ARG(p);
  return p;
}

/* Count one candidate, return its entry if it was already in the table. */
static struct sample_entry *
sample_count(NODEPTR p)
{
  struct sample_entry *e = &sample_table[sample_hash(p)];
  if (e->node.node_bits == p.node_bits) {
    e->count++;
    e->hits++;
    return e;
  }
  /* A different node.  Decrement, and take over the entry when the count
   * reaches 0, so the entry goes to the node that is hit most often. */
  if (e->count > 0)
    e->count--;
  if (e->count == 0) {
    e->node = p;
    e->count = 1;
    e->hits = 1;
    e->outer = 0;
  }
  return 0;
}

void
sample_node(NODEPTR n)
{
  if (!sample_interval) {
    sample_left = ~(counter_t)0;
    return;
  }
  sample_left = sample_interval;
  num_samples++;

  /* The part of the spine that is on the stack: the stack entries that
   * are applications of the node below them. */
  stackptr_t k = 0;
  NODEPTR below = sample_skip_ind(n);
  while (k <= stack_ptr) {
    NODEPTR s = TOP(k);
    if (is_VIRT(s) || !is_AP(FUN(s)) || sample_skip_ind(FUN(s)).node_bits != below.node_bits)
      break;
    below = s;
    k++;
  }

  /* The whole spine, outermost node first. */
  NODEPTR spine[SAMPLE_MAX_DEPTH];
  int ns = 0;
  for (stackptr_t j = k; j > 0 && ns < SAMPLE_MAX_DEPTH; j--)
    spine[ns++] = TOP(j - 1);
  NODEPTR p = n;
  while (ns < SAMPLE_MAX_DEPTH && !is_VIRT(p)) {
    NODEPTR f = FUN(p);
    if (!is_AP(f)) {
      if (get_DATA_TAG(f) == D_IND) {
        p = ARG(p);
        continue;
      }
      break;                    /* a value */
    }
    spine[ns++] = p;
    p = f;
  }

  /* The outermost candidate that recurs is presumably the function. */
  bool hit = false;
  for (int j = 1; j < ns; j++) {
    struct sample_entry *e = sample_count(spine[j]);
    if (e && !hit) {
      e->outer++;
      hit = true;
      if (jx_limit && e->outer == (counter_t)jx_threshold)
        jx_hot(e);              /* it just got hot */
    }
  }
  if (ns <= 1) {
    num_samples_none++;
  } else if (hit) {
    num_samples_hit++;
  } else {
    /* Record the head, p, to see what these samples are. */
    enum node_tag t = is_VIRT(p) ? get_VIRT_TAG(p) : (enum node_tag)get_DATA_TAG(FUN(p));
    if (t <= T_LAST_TAG)
      sample_miss_heads[t]++;
  }
}

/* Drop the entries for nodes that were not marked by the GC. */
void
sample_gc(void)
{
  if (!sample_table)
    return;
  for (size_t i = 0; i < SAMPLE_TABLE_SIZE; i++) {
    struct sample_entry *e = &sample_table[i];
    if (!is_NIL(e->node) && !is_marked_used(e->node)) {
      e->node = NIL_NODEPTR;
      e->count = 0;
      e->hits = 0;
      e->outer = 0;
    }
  }
  jx_gc();
}
#endif  /* WANT_SAMPLE */

/*
 * Bump allocation inside a run of free cells (BUMP_ALLOC, default on).
 *
 * The free map is scanned first-fit in address order, so consecutive
 * allocations almost always come from consecutive cells anyway.  Finding the
 * cell one at a time costs a word load, FFS, a bit clear and a store to
 * next_scan_index per allocation, and the store-forwarding chain through
 * free_map[i] and next_scan_index serialises back-to-back allocations.
 * Instead bump_refill() claims a whole run of free cells at once (clearing
 * their map bits eagerly) and alloc_node just bumps a pointer through it.
 * Runs average a few dozen cells on the self-compile, so the refill is cold.
 *
 * Invariants: num_free counts the cells not yet claimed, and num_alloc the
 * cells claimed, so neither changes per allocation.  The claimed-but-unused
 * tail of the run (at most a map word) is counted as allocated: gc_check may
 * collect that much early, and NUM_ALLOC subtracts the tail, which a GC
 * abandons (mark_all_free).  The map overstates "used" by that tail, which is
 * harmless because the map is only read by the GC, and gc() rebuilds it from
 * scratch.
 * bump_ptr/bump_end are globals, never cached in evali locals: longjmp
 * (T_CATCHR, resched) and nested GCs would otherwise see stale values.
 */
#if !defined(BUMP_ALLOC)
#define BUMP_ALLOC 1
#endif

#if BUMP_ALLOC
static struct node *bump_ptr, *bump_end; /* current run: [bump_ptr, bump_end) */
#define NUM_ALLOC (num_alloc - (counter_t)(bump_end - bump_ptr)) /* cells allocated */
#else
#define NUM_ALLOC num_alloc
#endif

static INLINE void mark_all_free(void)
{
  memset(free_map, ~0, free_map_nwords * sizeof(bits_t));
  next_scan_index = heap_start;
#if BUMP_ALLOC
  /* The map is about to be rebuilt by the marker (or is fresh at init), so a
   * run claimed from the old map is stale: its cells may now be live.
   * The unused tail of the run was never allocated. */
  num_alloc -= (counter_t)(bump_end - bump_ptr);
  bump_ptr = bump_end = 0;
#endif
}

#if BUMP_ALLOC
/* Claim the next run of free cells.  Out of line: it runs once per run, and
 * keeping it out of alloc_node keeps the hot path to a compare and a bump. */
static NOINLINE void
bump_refill(void)
{
  heapoffs_t i = next_scan_index / BITS_PER_WORD;
  heapoffs_t pos;
  bits_t word, inv, mask;
  int k, run;

  /* This can happen if we run out of memory when parsing.
   * It also guarantees that the map has a free cell. */
  if (num_free == 0)
    ERR("alloc_node");
  for(;;) {
    /* Not just under SANITY: an inconsistent num_free must not turn into a
     * read past the map. */
    if (i >= free_map_nwords)
      ERR("bump_refill: free_map");
    word = free_map[i];
    if (word)
      break;
    i++;
  }
  k = FFS(word) - 1;            /* bit index of the first free cell */
  /* Run length = number of consecutive 1 bits starting at k.  Bit 0 of
   * word >> k is set, so inv is 0 only for k == 0 with every bit set; that
   * case is handled explicitly because CTZ(0) is not portable. */
  inv = ~(word >> k);
  run = inv ? (int)CTZ(inv) : (int)(BITS_PER_WORD - k);
  pos = i * BITS_PER_WORD + k;
  /* The last map word has bits past heap_size when heap_size is not a
   * multiple of the word size; those bits are set but the cells do not exist. */
  if (pos + run > heap_size)
    run = (int)(heap_size - pos);
  mask = (run == (int)BITS_PER_WORD ? ~(bits_t)0 : (((bits_t)1 << run) - 1)) << k;
  free_map[i] = word & ~mask;   /* claim the whole run now */
  num_free -= run;
  num_alloc += run;
  next_scan_index = pos + run;
  bump_ptr = HEAPREF(pos);
  bump_end = bump_ptr + run;
}
#endif  /* BUMP_ALLOC */

#if WANT_PROFILE
/*
 * Dynamic profiling (WANT_PROFILE).
 * Every node knows the loaded node it comes from, prof_src.  A node of the
 * program, as read from the combinator file, is its own source; a node created
 * by a reduction gets the source the reduction was charged to (prof_cur).
 * A reduction is charged to the source of the innermost application on the
 * spine, i.e., to the code that runs (see prof_charge), and an unwinding step
 * to the source of the application node, i.e., the code that built it.
 * So a closure is charged to the code that created it, wherever it runs.
 * A loaded node also knows its definition (prof_label), and at the end the
 * counts are summed per definition (prof_report).  The cells of the loaded
 * program are never reused (prof_pin), so prof_src stays valid.
 */
static struct node prof_runtime;               /* pseudo source for what the runtime does on its own */
static struct node *prof_cur = &prof_runtime;  /* the source of new nodes */
static int prof_loading = 0;                   /* reading the program */
static struct node *prof_lo = 0, *prof_hi = 0; /* the cells of the loaded program */
static struct node *prof_def_start = 0;        /* the first cell of the definition being read */
static heapoffs_t prof_nlabels = 0;            /* number of labels in the program */
static char **prof_names = 0;                  /* the name of each label, see prof_end_load */
#define PROF_MAIN 0xffffffff                   /* prof_label of the main expression */
#define PROF_LOADED(p) ((p) >= prof_lo && (p) < prof_hi)

/*
 * Call contexts, to find the callers of a definition.  A context is a chain of
 * definitions, like a call stack, in which a definition occurs at most once.
 * The context of a reduction is the caller's context, i.e., that of the
 * innermost application on the spine that was built at run time, extended
 * with the definition the reduction is charged to (prof_push).  If that
 * definition is already in the chain, the chain is cut back to it, so
 * recursion does not make chains grow.  A new node gets the context of the
 * reduction that creates it, and so does the node a reduction overwrites (see
 * ap and ap2 in evali), so a tail call is made from the context of the code
 * that makes it.  Context 0 is the top level; a CAF is called from there, by
 * the definition it is in (see prof_charge).
 */
struct prof_context {
  uint32_t  parent;             /* the caller's context */
  uint32_t  label;              /* the definition, as prof_label */
  counter_t red;                /* reductions in this context */
  counter_t alloc;              /* cells allocated in this context */
};
#define PROF_TOP_LABEL 0xfffffffe                       /* label of context 0 */
static struct prof_context prof_ctx0 = { 0, PROF_TOP_LABEL, 0, 0 };
static struct prof_context *prof_ctxs = &prof_ctx0;    /* all contexts */
static uint32_t prof_nctxs = 1, prof_maxctxs = 1;
static uint32_t prof_ctx_cur = 0;                       /* the context of new nodes */
/* A hash table of the results of prof_push, the key is context<<32 | label */
#define PROF_NO_KEY UINT64_MAX
static uint64_t *prof_memo_keys = 0;
static uint32_t *prof_memo_vals = 0;
static uint64_t prof_memo_size = 0, prof_memo_used = 0;

static INLINE uint64_t
prof_hash(uint64_t key, uint64_t size)
{
  return (key * 0x9E3779B97F4A7C15ULL) >> 20 & (size - 1);
}

static void
prof_memo_insert(uint64_t key, uint32_t val)
{
  uint64_t h = prof_hash(key, prof_memo_size);
  while (prof_memo_keys[h] != PROF_NO_KEY)
    h = (h + 1) & (prof_memo_size - 1);
  prof_memo_keys[h] = key;
  prof_memo_vals[h] = val;
  prof_memo_used++;
}

static void
prof_memo_grow(void)
{
  uint64_t *okeys = prof_memo_keys, osize = prof_memo_size;
  uint32_t *ovals = prof_memo_vals;
  prof_memo_size = osize ? 2 * osize : 1 << 16;
  prof_memo_keys = mmalloc(prof_memo_size * sizeof(uint64_t));
  prof_memo_vals = mmalloc(prof_memo_size * sizeof(uint32_t));
  for (uint64_t h = 0; h < prof_memo_size; h++)
    prof_memo_keys[h] = PROF_NO_KEY;
  prof_memo_used = 0;
  for (uint64_t h = 0; h < osize; h++)
    if (okeys[h] != PROF_NO_KEY)
      prof_memo_insert(okeys[h], ovals[h]);
  if (okeys) {
    FREE(okeys);
    FREE(ovals);
  }
}

/* The context for code of definition lab called from context ctx. */
static uint32_t
prof_push(uint32_t ctx, uint32_t lab)
{
  uint64_t key = (uint64_t)ctx << 32 | lab;
  uint64_t h;
  uint32_t c;

  if (2 * (prof_memo_used + 1) > prof_memo_size)
    prof_memo_grow();
  for (h = prof_hash(key, prof_memo_size); prof_memo_keys[h] != PROF_NO_KEY; h = (h + 1) & (prof_memo_size - 1))
    if (prof_memo_keys[h] == key)
      return prof_memo_vals[h];
  /* Not seen before: cut back to lab if it is in the chain, or else extend the chain. */
  for (c = ctx; c != 0 && prof_ctxs[c].label != lab; c = prof_ctxs[c].parent)
    ;
  if (c == 0) {
    if (prof_nctxs == prof_maxctxs) {
      struct prof_context *o = prof_ctxs;
      if (prof_nctxs >= 0xfffffff0)
        ERR("profiling: too many call contexts");
      prof_maxctxs = prof_maxctxs < 0x10000 ? 0x10000 : prof_maxctxs < 0x7ffffff8 ? 2 * prof_maxctxs : 0xfffffff0;
      prof_ctxs = mmalloc(prof_maxctxs * sizeof(struct prof_context));
      memcpy(prof_ctxs, o, prof_nctxs * sizeof(struct prof_context));
      if (o != &prof_ctx0)
        FREE(o);
    }
    c = prof_nctxs++;
    prof_ctxs[c].parent = ctx;
    prof_ctxs[c].label = lab;
    prof_ctxs[c].red = 0;
    prof_ctxs[c].alloc = 0;
  }
  prof_memo_insert(key, c);
  return c;
}

/*
 * Hot nodes: counts per node, by address, like the sampler on the jit-sampler
 * branch, but exact.  Every node counts the unwinding steps through it
 * (prof_ncalls) and the reductions charged to it (prof_nred, the innermost
 * application on the spine, see prof_charge), and how often a reduction
 * rewrites it (prof_nupd).  A shared function value, e.g., a partial
 * application like insert dOrdInt, is unwound once per call.  The redex of a
 * loop is unwound once per iteration too, but it is also rewritten by every
 * iteration.
 * The counts are cleared when a cell is allocated, which has the same effect
 * as clearing the counts of the unused cells after a GC.  A node that reaches
 * PROF_HOT_MIN counts goes on the hot list.  At every GC the dead nodes on
 * the hot list, and the loaded ones that are unreachable, are taken off it
 * (prof_hot_gc); the PROF_DEAD_KEEP ones with the most counts are kept, with
 * a description of the node, since the cell will be reused.  Nothing that a
 * dead node points to has been reused before the GC that finds it dead, so
 * it can still be described then.  The live ones are described at the end.
 * Most loaded nodes are code, which the counts per definition cover, so only
 * the loaded nodes that apply another definition or have been rewritten are
 * reported (PROF_INTERESTING).
 */
#if !defined(PROF_HOT_MIN)
#define PROF_HOT_MIN 10000
#endif
#define PROF_DEAD_KEEP 1000
static struct node **prof_hot = 0;      /* the live (as of the last GC) nodes with at least PROF_HOT_MIN counts */
static size_t prof_nhot = 0, prof_maxhot = 0;
struct prof_dead {
  counter_t ncalls, nred, nupd;
  counter_t gc;                 /* the GC that found it dead, 0 if it is live */
  struct node *src;             /* its prof_src, a loaded node (or itself) or &prof_runtime */
  int loaded;                   /* a node of the loaded program, dead when it became unreachable */
  int calls_def;                /* its head is another definition, see prof_describe */
  char *desc;                   /* the description of the node */
};
/* The dead nodes with the most counts, made at run time or loaded.  The
 * loaded ones are only kept if they are interesting, see prof_retire. */
struct prof_keep {
  struct prof_dead *es;
  size_t n, min;                /* min is the entry with the fewest counts when full */
};
static struct prof_keep prof_dead_rt, prof_dead_ld;
static struct node **prof_roots = 0;    /* the root node of each definition, if it is a cell */
static struct node **prof_first = 0;    /* the first cell of each definition, index prof_nlabels for main */

static void
prof_hot_add(struct node *p)
{
  if (prof_nhot == prof_maxhot) {
    prof_maxhot = prof_maxhot ? 2 * prof_maxhot : 1024;
    prof_hot = mrealloc(prof_hot, prof_maxhot * sizeof(struct node *));
  }
  prof_hot[prof_nhot++] = p;
}

/* Count one event in the counter c of node p.  The total grows by one at a
 * time, so a node gets on the hot list once. */
static INLINE void
prof_tick(struct node *p, counter_t *c)
{
  (*c)++;
  if (p->prof_ncalls + p->prof_nred == PROF_HOT_MIN)
    prof_hot_add(p);
}

static INLINE void
prof_new(struct node *p)
{
  p->prof_ncalls = 0;
  p->prof_nred = 0;
  p->prof_nupd = 0;
  if (prof_loading) {
    /* Allocation is consecutive in the fresh heap, and prof_set_label relies on it. */
    if (!prof_lo)
      prof_lo = prof_def_start = p;
    else if (p != prof_hi)
      ERR("profiling: the program is not loaded into consecutive cells");
    prof_hi = p + 1;
    p->prof_src = p;
    p->prof_red = 0;
    p->prof_unwind = 0;
    p->prof_alloc = 0;
    p->prof_label = 0;
    p->prof_ctx = 0;
  } else {
    p->prof_src = prof_cur;
    prof_cur->prof_alloc++;
    p->prof_ctx = prof_ctx_cur;
    prof_ctxs[prof_ctx_cur].alloc++;
  }
}

/* The cells read since the last label belong to the definition with label lab. */
static void
prof_set_label(uint32_t lab)
{
  if (!prof_lo)
    return;
  for (struct node *p = prof_def_start; p < prof_hi; p++)
    p->prof_label = lab;
  if (prof_first) {
    heapoffs_t i = lab == PROF_MAIN ? prof_nlabels : lab - 1;
    if (i <= prof_nlabels)
      prof_first[i] = prof_def_start;
  }
  prof_def_start = prof_hi;
}
#endif  /* WANT_PROFILE */

/* Allocate a new node */
static INLINE struct node*
alloc_node_ptr(void)
{
  struct node *n;

#if BUMP_ALLOC
  if (bump_ptr == bump_end)
    bump_refill();
  n = bump_ptr++;
#else  /* BUMP_ALLOC */
  /* This can happen if we run out of memory when parsing. */
  if (num_free <= 0)
    ERR("alloc_node");

  heapoffs_t i = next_scan_index / BITS_PER_WORD;
  int k;                        /* will contain bit pos + 1 */
  heapoffs_t pos;
  heapoffs_t word;

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
  COUNT(num_alloc);
  num_free--;
#endif  /* BUMP_ALLOC */

#if WANT_PROFILE
  prof_new(n);
#endif
  return n;
}

/* Create a NODEPTR from a tag.  Either a real node or an inline tag */
static INLINE NODEPTR
alloc_node(enum node_tag t)
{
  XASSERT(t < V_CONSTR || t > T_FIRST_REAL_TAG); /* V_ tags need extra info, use mk_CON/mk_CASE */
  if (t > T_FIRST_REAL_TAG)
    return mk_TAG(t);
  else {
    struct node *p = alloc_node_ptr();
    NODEPTR n;
    n.node_ptr = p;
    FUN(n) = mk_DATA_TAG(t);
    /* ARG slot uninitialized */
    return n;
  }
}

/* Create a new application node */
static INLINE NODEPTR
new_ap(NODEPTR f, NODEPTR a)
{
  struct node *p = alloc_node_ptr();
  NODEPTR n;
  n.node_ptr = p;
  FUN(n) = f;
  ARG(n) = a;
  return n;
}

NODEPTR evali(NODEPTR n);

/* If this is non-0 it means that the threading system is active. */
struct mthread *main_thread = 0;

void
start_exec(NODEPTR root)
{
  struct mthread *mt;

  mt = new_thread(new_ap(root, combWorld)); /* main thread */
  mt->mt_id = MAIN_THREAD;                  /* make it the main thread in case this is foreign export calling */
  main_thread = mt;

  switch(setjmp(sched)) {
  case mt_main:
    break;
  case mt_resched:
    COUNT(num_resched);
    break;
  case mt_raise:
    /* We have an uncaught exception.
     * If it's the main thread, this kills the program.
     * Otherwise, it just kills the thread.
     */
    if (in_raise) {
      ERR("FATAL: exception while trying to die");
      EXIT(1);
    }
    mt = remove_q_head(&runq);
    if (mt->mt_id == MAIN_THREAD) {
      die_exn(the_exn);
    } else {
#if THREAD_DEBUG
      if (thread_trace) {
        printf("start_exec: mt=%p id=%d died from exn\n", mt, (int)mt->mt_id);
      }
#endif  /* THREAD_DEBUG */
      mt->mt_state = ts_died;
      mt->mt_root = NIL_NODEPTR;
      release_throwto(mt);      /* release anyone blocked in throwTo to it */
    }
  }
#if THREAD_DEBUG
  if (thread_trace) {
    printf("start_exec:\n");
    dump_q("runq", runq);
  }
#endif  /* THREAD_DEBUG */
  for(;;) {
    if (!runq.mq_head)
      pause_exec();
    mt = runq.mq_head;          /* front thread */
    if (!mt)                    /* this should never happen */
      ERR("no threads");

    glob_slice = mt->mt_slice + slice;
#if THREAD_DEBUG
    if (thread_trace)
      printf("start_exec: start %d, slice=%d\n", (int)mt->mt_id, (int)glob_slice);
#endif  /* THREAD_DEBUG */
    num_reductions += glob_slice-1;
    (void)evali(mt->mt_root);         /* run it */
    num_reductions -= glob_slice;
    /* when evali() returns the thread is done */
    (void)remove_q_head(&runq);                      /* remove front thread */

#if THREAD_DEBUG
    if (thread_trace) {
      printf("start_exec: mt=%p id=%d finished\n", mt, (int)mt->mt_id);
    }
#endif  /* THREAD_DEBUG */
    mt->mt_state = ts_finished;
    mt->mt_root = NIL_NODEPTR;
    release_throwto(mt);        /* release anyone blocked in throwTo to it */
    /* XXX mt_mval, mt_thrown */

    if (mt->mt_id == MAIN_THREAD) {
      main_thread = 0;
#if THREAD_DEBUG
      if (thread_trace) {
        printf("start_exec: main thread done\n");
      }
#endif  /* THREAD_DEBUG */
      return;                   /* when the main thread dies it's all over */
    }
  }
}

/* One node of each kind for primitives, these are never GCd. */
/* We use linear search in this, because almost all lookups
 * are among the combinators.
 */
static const
struct {
  const char *name;
  const enum node_tag tag;
  const uint8_t arity;                /* the number of arguments it needs to reduce, see arity_virt */
  const enum node_tag flipped;        /* What should (C op) reduce to? defaults to T_FREE */
  //  NODEPTR node;
} primops[] = {
  /* combinators */
  { "B", T_B, 3 },
  { "O", T_O, 4 },
  { "K", T_K, 2, T_A },
  { "C'", T_CC, 4 },
  { "C", T_C, 3 },
  { "A", T_A, 2, T_K },
  { "S'", T_SS, 4 },
  { "P", T_P, 3 },
  { "R", T_R, 2 },
  { "I", T_I, 1 },
  { "S", T_S, 3 },
  { "U", T_U, 2 },
  { "Y", T_Y, 1 },
  { "B'", T_BB, 2 },
  { "Z", T_Z, 2 },
  { "K2", T_K2, 2 },
  { "K3", T_K3, 2 },
  { "K4", T_K4, 2 },
  { "C'B", T_CCB, 3 },
/* primops */
  { "+", T_ADD, 2, T_ADD },
  { "-", T_SUB, 2, T_SUBR },
  { "*", T_MUL, 2, T_MUL },
  { "quot", T_QUOT, 2 },
  { "rem", T_REM, 2 },
  { "u+", T_UADD, 2, T_UADD },
  { "u-", T_USUB, 2, T_USUBR },
  { "u*", T_UMUL, 2, T_UMUL },
  { "uquot", T_UQUOT, 2 },
  { "urem", T_UREM, 2 },
  { "subtract", T_SUBR, 2, T_SUB },
  { "usubtract", T_USUBR, 2, T_USUB },
  { "neg", T_NEG, 1 },
  { "uneg", T_UNEG, 1 },
  { "and", T_AND, 2, T_AND },
  { "or", T_OR, 2, T_OR },
  { "xor", T_XOR, 2, T_XOR },
  { "inv", T_INV, 1 },
  { "shl", T_SHL, 2 },
  { "shr", T_SHR, 2 },
  { "ashr", T_ASHR, 2 },
  { "popcount", T_POPCOUNT, 1 },
  { "clz", T_CLZ, 1 },
  { "ctz", T_CTZ, 1 },
#if WANT_FLOAT64
  { "d+", T_DADD, 2, T_DADD },
  { "d-", T_DSUB, 2 },
  { "d*", T_DMUL, 2, T_DMUL },
  { "d/", T_DDIV, 2 },
  { "dneg", T_DNEG, 1 },
  { "itod", T_ITOD, 1 },
  { "Itod", T_I64TOD, 1 },
  { "utod", T_UTOD, 1 },
  { "dtoi", T_DTOI, 1 },
  { "d==", T_DEQ, 2, T_DEQ },
  { "d/=", T_DNE, 2, T_DNE },
  { "d<", T_DLT, 2, T_DGT },
  { "d<=", T_DLE, 2, T_DGE },
  { "d>", T_DGT, 2, T_DLT },
  { "d>=", T_DGE, 2, T_DLE },
#endif  /* WANT_FLOAT64 */
#if WANT_FLOAT64 && WANT_FLOAT32
  { "dtof", T_DTOF, 1 },
  { "ftod", T_FTOD, 1 },
#endif  /* WANT_FLOAT64 && WANT_FLOAT32 */
#if WANT_FLOAT32
  { "f+", T_FADD, 2, T_FADD },
  { "f-", T_FSUB, 2 },
  { "f*", T_FMUL, 2, T_FMUL },
  { "f/", T_FDIV, 2 },
  { "fneg", T_FNEG, 1 },
  { "Itof", T_I64TOF, 1 },
  { "itof", T_ITOF, 1 },
  { "utof", T_UTOF, 1 },
  { "ftoi", T_FTOI, 1 },
  { "f==", T_FEQ, 2, T_FEQ },
  { "f/=", T_FNE, 2, T_FNE },
  { "f<", T_FLT, 2, T_FGT },
  { "f<=", T_FLE, 2, T_FGE },
  { "f>", T_FGT, 2, T_FLT },
  { "f>=", T_FGE, 2, T_FLE },
#endif  /* WANT_FLOAT32 */

  { "bs++", T_BSAPPEND, 2 },
  { "bs++.", T_BSAPPENDDOT, 2 },
  { "bs==", T_BSEQ, 2, T_BSEQ },
  { "bs/=", T_BSNE, 2, T_BSNE },
  { "bs<", T_BSLT, 2, T_BSGT },
  { "bs<=", T_BSLE, 2, T_BSGE },
  { "bs>", T_BSGT, 2, T_BSLT },
  { "bs>=", T_BSGE, 2, T_BSLE },
  { "bscmp", T_BSCMP, 2 },
  { "bsunpack", T_BSUNPACK, 1 },
  { "bsreplicate", T_BSREPLICATE, 2 },
  { "bslength", T_BSLENGTH, 1 },
  { "bssubstr", T_BSSUBSTR, 3 },
  { "bsindex", T_BSINDEX, 2 },
  { "bsnew", T_BSNEW, 3 },
  { "bsread", T_BSREAD, 3 },
  { "bswrite", T_BSWRITE, 4 },
  { "bsfreeze", T_BSFREEZE, 2 },
  { "bsappbyte", T_BSAPPBYTE, 3 },
  { "bsappchar", T_BSAPPCHAR, 3 },

  { "ord", T_I, 1 },
  { "chr", T_I, 1 },
  { "==", T_EQ, 2, T_EQ },
  { "/=", T_NE, 2, T_NE },
  { "<", T_LT, 2, T_GT },
  { "u<", T_ULT, 2, T_UGT },
  { "u<=", T_ULE, 2, T_UGE },
  { "u>", T_UGT, 2, T_ULT },
  { "u>=", T_UGE, 2, T_ULE },
  { "<=", T_LE, 2, T_GE },
  { ">", T_GT, 2, T_LT },
  { ">=", T_GE, 2, T_LE },
  { "fp+", T_FPADD, 2 },
  { "fp2p", T_FP2P, 1 },
  { "fpnew", T_FPNEW, 2 },
  { "fpfin", T_FPFIN, 3 },
  //  { "fpstr", T_FPSTR },
  { "fp2bs", T_FP2BS, 2 },
  { "bs2fp", T_BS2FP, 1 },
  { "seq", T_SEQ, 2 },
  { "icmp", T_ICMP, 2 },
  { "ucmp", T_UCMP, 2 },
  { "rnf", T_RNF, 2 },
  { "balanceL", T_BALANCEL, 4 },
  { "balanceR", T_BALANCER, 4 },
  { "identEq", T_IDENTEQ, 2 },
  { "fromUTF8", T_BSFROMUTF8, 1 },
  { "headUTF8", T_BSHEADUTF8, 1 },
  { "tailUTF8", T_BSTAILUTF8, 1 },
  /* Data.Text helpers: lengths and offsets are in code points and bytes */
  { "utf8length", T_UTF8LENGTH, 1 },
  { "utf8offset", T_UTF8OFFSET, 2 },
  { "utf8offsetend", T_UTF8OFFSETEND, 2 },
  { "utf8offsetfrom", T_UTF8OFFSETFROM, 3 },
  { "utf8offsetendbefore", T_UTF8OFFSETENDBEFORE, 3 },
  { "utf8decode", T_UTF8DECODE, 2 },
  { "utf8decodeend", T_UTF8DECODEEND, 2 },
  { "utf8reverse", T_UTF8REVERSE, 1 },
  { "utf8skipspace", T_UTF8SKIPSPACE, 2 },
  { "utf8skipnonspace", T_UTF8SKIPNONSPACE, 2 },
  { "utf8trimspaceend", T_UTF8TRIMSPACEEND, 1 },
  { "bsfind", T_BSFIND, 3 },
  { "bsrfind", T_BSRFIND, 2 },
  { "bsasciicase", T_BSASCIICASE, 2 },
  { "bsisascii", T_BSISASCII, 1 },
  { "bsconcat", T_BSCONCAT, 1 },
  /* IO primops */
  { "IO.>>=", T_IO_BIND, 3 },
  { "IO.>>", T_IO_THEN, 2 },
  { "IO.return", T_IO_RETURN, 3 },
  { "IO.serialize", T_IO_SERIALIZE, 3 },
  { "IO.print", T_IO_PRINT, 3 },
  { "IO.deserialize", T_IO_DESERIALIZE, 2 },
  { "IO.stdin", T_IO_STDIN, 0 },
  { "IO.stdout", T_IO_STDOUT, 0 },
  { "IO.stderr", T_IO_STDERR, 0 },
  { "IO.getArgRef", T_IO_GETARGREF, 1 },
  { "IO.performIO", T_IO_PERFORMIO, 1 },
  { "IO.atomic", T_IO_ATOMIC, 2 },
  { "IO.gc", T_IO_GC, 2 },
  { "IO.stats", T_IO_STATS, 1 },
  { "IO.pp", T_IO_PP, 2 },
  { "IO.lazyBind", T_IO_LAZYBIND, 3 },
  { "IO.lazyBindST", T_IO_LAZYBINDST, 3 },
  { "IO.strict", T_IO_STRICT, 2 },
  { "raise", T_RAISE, 1 },
  { "catch", T_CATCH, 3 },
  { "catchr", T_CATCHR, 3 },
  { "A.alloc", T_ARR_ALLOC, 3 },
  { "A.copy", T_ARR_COPY, 2 },
  { "A.size", T_ARR_SIZE, 2 },
  { "A.read", T_ARR_READ, 3 },
  { "A.write", T_ARR_WRITE, 4 },
  { "A.trunc", T_ARR_TRUNC, 3 },
  { "A.==", T_ARR_EQ, 2 },
  { "IO.fork", T_IO_FORK, 2 },
  { "IO.thid", T_IO_THID, 1 },
  { "thnum", T_THNUM, 1 },
  { "IO.throwto", T_IO_THROWTO, 3 },
  { "IO.yield", T_IO_YIELD, 1 },
  { "IO.newmvar", T_IO_NEWMVAR, 1 },
  { "IO.takemvar", T_IO_TAKEMVAR, 2 },
  { "IO.putmvar", T_IO_PUTMVAR, 3 },
  { "IO.readmvar", T_IO_READMVAR, 2 },
  { "IO.trytakemvar", T_IO_TRYTAKEMVAR, 2 },
  { "IO.tryputmvar", T_IO_TRYPUTMVAR, 3 },
  { "IO.tryreadmvar", T_IO_TRYREADMVAR, 2 },
  { "IO.threaddelay", T_IO_THREADDELAY, 2 },
  { "IO.threadstatus", T_IO_THREADSTATUS, 2 },
  { "IO.getmaskingstate", T_IO_GETMASKINGSTATE, 1 },
  { "IO.setmaskingstate", T_IO_SETMASKINGSTATE, 2 },
  { "packCString", T_PACKCSTRING, 2 },
  { "packCStringLen", T_PACKCSTRINGLEN, 3 },
  { "bsgrab", T_BSGRAB, 2 },
  { "bsgrablen", T_BSGRABLEN, 3 },
  { "toPtr", T_TOPTR, 1 },
  { "toInt", T_TOINT, 1 },
  { "toDbl", T_TODBL, 1 },
  { "toFlt", T_TOFLT, 1 },
  { "fromDbl", T_FROMDBL, 1 },
  { "fromFlt", T_FROMFLT, 1 },
  { "toFunPtr", T_TOFUNPTR, 1 },
  { "IO.ccall", D_IO_CCALL, 0 },
  { "isint", T_ISINT, 1 },
  { "SPnew", T_SPNEW, 2 },
  { "SPderef", T_SPDEREF, 2 },
  { "SPfree", T_SPFREE, 2 },
  { "Wknew", T_WKNEW, 3 },
  { "Wknewfin", T_WKNEWFIN, 4 },
  { "Wkderef", T_WKDEREF, 2 },
  { "Wkfinal", T_WKFINAL, 2 },
  { "binint2", T_BININT2, 0 },
  { "binint1", T_BININT1, 0 },
  { "bindbl2", T_BINDBL2, 0 },
  { "bindbl1", T_BINDBL1, 0 },
  { "binbs2", T_BINBS2, 0 },
  { "binbs1", T_BINBS1, 0 },
  { "unint1", T_UNINT1, 0 },
  { "undbl1", T_UNDBL1, 0 },
  { "IO.waitrdfd", T_IO_WAITRDFD, 2 },
  { "IO.waitwrfd", T_IO_WAITWRFD, 2 },
#if WANT_INT64
  { "I+", T_ADD64, 2, T_ADD64 },
  { "I-", T_SUB64, 2, T_SUBR64 },
  { "I*", T_MUL64, 2, T_MUL64 },
  { "Iquot", T_QUOT64, 2 },
  { "Irem", T_REM64, 2 },
  { "Iu+", T_UADD64, 2, T_UADD64 },
  { "Iu-", T_USUB64, 2, T_USUBR64 },
  { "Iu*", T_UMUL64, 2, T_UMUL64 },
  { "Iuquot", T_UQUOT64, 2 },
  { "Iurem", T_UREM64, 2 },
  { "Isubtract", T_SUBR64, 2, T_SUB64 },
  { "Iusubtract", T_USUBR64, 2, T_USUB64 },
  { "Ineg", T_NEG64, 1 },
  { "Iuneg", T_UNEG64, 1 },
  { "Iand", T_AND64, 2, T_AND64 },
  { "Ior", T_OR64, 2, T_OR64 },
  { "Ixor", T_XOR64, 2, T_XOR64 },
  { "Iinv", T_INV64, 1 },
  { "Ishl", T_SHL64, 2 },
  { "Ishr", T_SHR64, 2 },
  { "Iashr", T_ASHR64, 2 },
  { "Ipopcount", T_POPCOUNT64, 1 },
  { "Iclz", T_CLZ64, 1 },
  { "Ictz", T_CTZ64, 1 },
  { "I==", T_EQ64, 2, T_EQ64 },
  { "I/=", T_NE64, 2, T_NE64 },
  { "I<", T_LT64, 2, T_GT64 },
  { "Iu<", T_ULT64, 2, T_UGT64 },
  { "Iu<=", T_ULE64, 2, T_UGE64 },
  { "Iu>", T_UGT64, 2, T_ULT64 },
  { "Iu>=", T_UGE64, 2, T_ULE64 },
  { "I<=", T_LE64, 2, T_GE64 },
  { "I>", T_GT64, 2, T_LT64 },
  { "I>=", T_GE64, 2, T_LE64 },
  { "Iicmp", T_ICMP64, 2 },
  { "Iucmp", T_UCMP64, 2 },
  { "itoI", T_ITOI64, 1 },
  { "Itoi", T_I64TOI, 1 },
  { "utoU", T_UTOU64, 1 },
  { "Utou", T_U64TOU, 1 },
#endif  /* WANT_INT64 */
  { "tick", D_TICK, 1 },           /* fake op */
  { "D_", V_CASED, 0 },
  { "L_", V_LOOKD, 0 },
  { "E_", V_CASES, 0 },
  { "M_", V_LOOKS, 0 },
  { "C_", V_CONSTR, 0 },
  { "A_", V_ARM, 0 },
};

#if GCRED
enum node_tag flip_ops[T_LAST_TAG+1];
#endif

/*
 * Use a hash table with open linear probing for the lookup of primpo names.
 * Make the hash table twice the size of the number of primops so we get a load factor of 0.5.
 */
#define PRIMOP_HASH_SIZE (2 * sizeof primops / sizeof primops[0] + 5)
/*
 * Without the + 5 above the time on my MacBook more that doubles!
 * It's a mystery why.
 */
static struct {
  const char   *name;           /* NULL marks an empty slot */
  enum node_tag tag;
} primop_hash[PRIMOP_HASH_SIZE];

/* FNV-1a hashing. It's simple and fast. */
static uint32_t
primop_hash_str(const char *s)
{
  uint32_t h = 2166136261;
  while (*s) {
    h ^= (uint8_t)*s++;
    h *= 16777619;
  }
  return h % PRIMOP_HASH_SIZE;
}

/* Build the initial table that maps a hash to a pair of a name and tag */
static void
init_primop_hash(void)
{
  size_t i;

  for (i = 0; i < sizeof primops / sizeof primops[0]; i++) {
    uint32_t k = primop_hash_str(primops[i].name);

    /* This slot is already occupied, so we progress to the next slot. */
    /* This must terminate, size the hash table is twice the size of the primops. */
    while (primop_hash[k].name) {
      k = (k + 1) % PRIMOP_HASH_SIZE;
    }
    primop_hash[k].name = primops[i].name;
    primop_hash[k].tag  = primops[i].tag;
  }
}

/* Return the tag for a primop name, or -1 if there is no such primop. */
static int
lookup_primop(const char *name)
{
  uint32_t k = primop_hash_str(name);

  while (primop_hash[k].name) {
    if (strcmp(primop_hash[k].name, name) == 0)
      return (int)primop_hash[k].tag;
    k = (k + 1) % PRIMOP_HASH_SIZE;
  }
  return -1;
}

#if WANT_STDIO
/* Create a dummy foreign pointer for the standard stdio handles. */
/* These handles are never gc():d. */
void
mk_std(NODEPTR n, FILE *f)
{
  struct final *fin = mcalloc(1, sizeof(struct final));
  struct forptr *fp = mcalloc(1, sizeof(struct forptr));
  BFILE *bf = add_utf8(add_FILE(f));
  FORPTR(n) = fp;
  fin->arg = bf;
  fin->back = fp;
  fp->payload.bs_array = bf;
  fp->finalizer = fin;
}
#endif

void
init_nodes(void)
{
  size_t j;
  NODEPTR x;

  cells = mmalloc(heap_size * sizeof(node));
  free_map_nwords = (heap_size + BITS_PER_WORD - 1) / BITS_PER_WORD; /* bytes needed for free map */
  free_map = mmalloc(free_map_nwords * sizeof(bits_t));

  /* The permanent nodes are allocated from the start of the heap.
   * Afterwards heap_start is moved past them, so they are never GCed. */
  heap_start = 0;
  mark_all_free();
  num_free = heap_size;

  /* Initialize node name array.
   * Go backwards, so that the first name of a tag wins, e.g., I and not its aliases ord and chr. */
  for (j = sizeof primops / sizeof primops[0]; j-- > 0; ) {
    tag_names[primops[j].tag] = primops[j].name;
    tag_arity[primops[j].tag] = primops[j].arity;
#if GCRED
    flip_ops[primops[j].tag] = primops[j].flipped;
#endif
  }
  init_primop_hash();

  /* Set up permanent nodes */
#if INTTABLE
  /* Allocate permanent Int nodes */
  for (int i = LOW_INT; i < HIGH_INT; i++) {
    NODEPTR n = alloc_node(D_INT);
    intTable[i - LOW_INT] = n;
    SETVALUE(n, i);
  }
#endif

#if WANT_STDIO
  comb_stdin  = alloc_node(D_FORPTR); mk_std(comb_stdin,  stdin);
  comb_stdout = alloc_node(D_FORPTR); mk_std(comb_stdout, stdout);
  comb_stderr = alloc_node(D_FORPTR); mk_std(comb_stderr, stderr);
#endif

  /* The displaySomeException compiles to (D_2 (U (K2 A))) */
  x = new_ap(combK2, combA);                 /* (K2 A) */
  x = new_ap(combU, x);                      /* (U (K2 A)) */
  {
    uint8_t arities[1] = { 2 };              /* SomeException has a dictionary and a value */
    combShowExn = new_ap(mk_CASE(V_CASED, find_case_arity(1, arities)), x); /* (D_2 (U (K2 A))) */
  }

  /* The IO monad uses pairs implemented with Scott encoding, i.e., with P */
  combPUnit = new_ap(combP, combUnit);
  combPFst  = new_ap(combU, combK); /* fst for pairs constructed with P */
  combPSnd  = new_ap(combU, combA); /* snd for pairs constructed with P */
  combWorld = mkInt(99999);

  spare_node = alloc_node(D_INT);

  /* The permanent nodes occupy [0 .. next_scan_index].
   * Round up heap_start to the next bitword boundary to avoid the permanent nodes. */
  heap_start = (next_scan_index + 1 + BITS_PER_WORD - 1) / BITS_PER_WORD * BITS_PER_WORD;
  mark_all_free();              /* also sets next_scan_index = heap_start */

  num_free = heap_size - heap_start;
}

#if GCRED
counter_t red_a, red_k, red_i, red_int, red_flip, red_bi, red_bxi, red_ccbi, red_cc, red_cci, red_ccbbcp;
#endif
counter_t red_bb, red_k4, red_k3, red_k2, red_ccb, red_z, red_r;

//counter_t mark_depth;
//counter_t max_mark_depth = 0;

void mark(NODEPTR *np);
void mark_mvar(struct mvar *mv);
void mark_thread(struct mthread *mt);

/* Follow indirections */
static INLINE NODEPTR
indir(NODEPTR *np)
{
  NODEPTR n = *np;
  for(;;) {
    NODEPTR p = get_IND(n);
    if (is_NIL(p))
      break;
    n = p;
  }
  *np = n;
  return n;
}

/***** weak pointers *****/

struct weak_ptr {
  struct weak_ptr *next;        /* list of all weak pointers. */
  int marked;                   /* seen by GC */
  NODEPTR key;                  /* key, this is the weak pointer */
  NODEPTR value;                /* associated value */
  NODEPTR finalize;             /* maybe finalizer */
};
struct weak_ptr *allweaks;      /* head of all weak pointers */

/* After GC mark phase, deal with weak pointers */
void
sweep_weaks(void)
{
 restart:
  /* all weak pointer records are alive, marked or not */
  for (struct weak_ptr *wp = allweaks; wp; wp = wp->next) {
    if (is_NIL(wp->value))
      continue;                 /* the weak pointer is already dead */
    (void)indir(&wp->key);
    if (is_marked_used(wp->key)) {
      /* The key is used, so mark the other parts */
      if (!is_marked_used(wp->value) ||
          (!is_NIL(wp->finalize) && !is_marked_used(wp->finalize))) {
        /* Not already marked */
        mark(&wp->value);
        if (!is_NIL(wp->finalize))
          mark(&wp->finalize);
        /* This marking might have marked other keys, so restart the scan */
        goto restart;
      }
    } else {
      /* The key is not marked, so the weak reference is dead */
      wp->value = NIL_NODEPTR;
    }
  }

  /* Create finalizers for all weak pointers that just died */
  for (struct weak_ptr *wp = allweaks; wp; wp = wp->next) {
    if (is_NIL(wp->value) && !is_NIL(wp->finalize)) {
      struct mthread *mt = new_thread(wp->finalize);
      mark_thread(mt);        /* mark it, since overall thread marking has already run */
      wp->finalize = NIL_NODEPTR;
      wp->key = NIL_NODEPTR;            /* not needed, but for sanity */
      /* Marking the finalizer does not resurrect keys */
    }
  }

  /* If a weak pointer object is unreferenced and it has been finalized,
   * then it can be garbage collected. */
  for (struct weak_ptr **wpp = &allweaks; *wpp; ) {
    struct weak_ptr *wp = *wpp;
    if (!wp->marked && is_NIL(wp->value)) {
      /* not marked, so unlink and free */
      *wpp = wp->next;
      COUNT(num_gc_weak);
      FREE(wp);
    } else {
      /* point to the next weak_ptr */
      wpp = &wp->next;
    }
  }
}

NODEPTR
new_weak_ptr(NODEPTR key, NODEPTR value, NODEPTR finalize)
{
  struct weak_ptr *wp = mmalloc(sizeof(struct weak_ptr));
  wp->next = allweaks;
  allweaks = wp;
  wp->marked = 0;
  wp->key = key;
  wp->value = value;
  if (!is_NIL(finalize)) {
    wp->finalize = new_ap(finalize, combWorld);
  } else {
    wp->finalize = NIL_NODEPTR;
  }

  COUNT(num_new_weak);
  NODEPTR n = alloc_node(D_WEAK);
  WEAK(n) = wp;
  return n;
}

NODEPTR
deref_weak_ptr(struct weak_ptr *wp)
{
  if (is_NIL(wp->value))
    return combNothing;
  return new_ap(combJust, wp->value);
}

void
finalize_weak_ptr(struct weak_ptr *wp)
{
  NODEPTR final = wp->finalize;
  if (is_NIL(final))
    return;
  wp->finalize = NIL_NODEPTR;
  (void)evali(final);
}

/**********************************************************/

/* Throwing, e.g., a UserInterrupt exception, to the main thread
 * can happen from any thread (the one that happens to poll).
 * Throwing an exception can block, so we can't throw it from
 * the current thread.  Instead, we spawn a new thread, whose
 * only job it is to throw the exception.
 */
void
async_throwto(struct mthread *mt, NODEPTR exn)
{
  GCCHECK(4);
  NODEPTR thid = alloc_node(D_THID);
  THR(thid) = mt;		/* thread ID for mt */
  NODEPTR root = new_ap(new_ap(new_ap(combTHROWTO, thid), exn), combWorld); /* root = throwTo thid exn */
  (void)new_thread(root);       /* spawn and put on runq, i.e., forkIO root */
}

void
mark_thread(struct mthread *mt)
{
  if (mt->mt_mark)
    return;                     /* already marked */
  mt->mt_mark = true;
  if (!is_NIL(mt->mt_root))
    mark(&mt->mt_root);
  mark_mvar(mt->mt_exn);
  if (mt->mt_throwto)
    mark_thread(mt->mt_throwto);
  if (!is_NIL(mt->mt_mval))
    mark(&mt->mt_mval);
}

void
mark_mvar(struct mvar *mv)
{
  if (mv->mv_mark)
    return;
  mv->mv_mark = true;
  if (!is_NIL(mv->mv_data))
    mark(&mv->mv_data);
  for (struct mthread *mt = mv->mv_takeput.mq_head; mt; mt = mt->mt_queue)
    mark_thread(mt);
  for (struct mthread *mt = mv->mv_read.mq_head; mt; mt = mt->mt_queue)
    mark_thread(mt);
}

#if GCRED
/* The evaluation stack pointer when the GC started.
 * The mark phase pushes on the same stack, so stack_ptr cannot be used by gc_red_ok(). */
stackptr_t gc_eval_stack_ptr;

/*
 * Only allow GC reductions when the node is not near the top of the stack.
 * The reason is that when GC is triggered we are just starting a reduction
 * and the combinator at the left-bottom of the spine is being reduced.
 * If a GC reduction removes this combinator, then bad things happen.
 */
static int
gc_red_ok(NODEPTR n)
{
  for (stackptr_t s = gc_eval_stack_ptr; s >= 0 && s >= gc_eval_stack_ptr - 5; s--)
    if (n.node_ptr == stack[s].node_ptr)
      return 0;
  return 1;
}

/*
 * Try to do a reduction of the application node n during GC.
 * If one applies, n is turned into an indirection to the result, and the result is returned.
 * Otherwise NIL is returned.
 * All reductions just select an existing node or a combinator, so nothing is allocated.
 */
static NODEPTR
gc_reduce(NODEPTR n)
{
  NODEPTR fun = indir(&FUN(n));
  NODEPTR arg = indir(&ARG(n));
  enum node_tag funt = get_XTAG(fun);
  enum node_tag argt = get_XTAG(arg);
  enum node_tag funfunt = funt == X_AP ? get_XTAG(indir(&FUN(fun))) : D_NONE;
  enum node_tag funargt = argt == X_AP ? get_XTAG(indir(&FUN(arg))) : D_NONE;
  NODEPTR r;

  switch (funfunt) {
  case T_A:
    /* A x y --> y.  This is really only fruitful just after parsing. */
    r = arg;
    COUNT(red_a);
    goto red;
  case T_K:
    /* K x y --> x */
    r = indir(&ARG(fun));
    COUNT(red_k);
    goto red;
  case T_B:
    if (argt == T_I) {
      /* B x I --> x */
      r = indir(&ARG(fun));
      COUNT(red_bxi);
      goto red;
    }
    break;
  case T_CCB:
    if (argt == T_I) {
      /* C'B x I --> x */
      r = indir(&ARG(fun));
      COUNT(red_ccbi);
      goto red;
    }
    break;
  default:
    break;
  }

  switch (funt) {
  case T_I:
    /* I x --> x */
    r = arg;
    COUNT(red_i);
    goto red;
  case T_CC:
    if (argt == T_I) {
      /* C' I --> C */
      r = combC;
      COUNT(red_cci);
      goto red;
    }
    break;
  case T_CCB:
    if (argt == X_AP && get_XTAG(indir(&ARG(arg))) == T_P) {
      NODEPTR funarg = indir(&FUN(arg));
      if (get_XTAG(funarg) == X_AP &&
          get_XTAG(indir(&FUN(funarg))) == T_B &&
          get_XTAG(indir(&ARG(funarg))) == T_C) {
        /* C'B ((B C) P) --> C */
        r = combC;
        COUNT(red_ccbbcp);
        goto red;
      }
    }
    break;
  case T_B:
    if (argt == T_I) {
      /* B I --> I */
      r = combI;
      COUNT(red_bi);
      goto red;
    }
    break;
  case T_C:
    if (funargt == T_C) {
      /* C (C x) --> x */
      r = indir(&ARG(arg));
      COUNT(red_cc);
      goto red;
    }
    if (flip_ops[argt] != D_NONE) {
      /* C op --> flipped op */
      r = mk_TAG(flip_ops[argt]);
      COUNT(red_flip);
      goto red;
    }
    break;
  default:
    break;
  }
  return NIL_NODEPTR;

 red:
  /* Don't create an indirection loop, e.g., for x = I x */
  if (r.node_bits == n.node_bits || !gc_red_ok(n))
    return NIL_NODEPTR;
  set_IND(n, r);
  return r;
}
#endif  /* GCRED */

/*
 * Marking is a DRAM-latency bound pointer chase: a cell is visited once per
 * GC and almost never in cache, so each visit costs a full miss.
 * The work stack lets us know the next MARK_PFQ cells in advance, so we
 * prefetch them and keep that many misses in flight (a FIFO prefetch queue
 * as in Cher et al., "Software prefetching for mark-sweep GC").
 * On an Apple M1 this cut mark time by about 3x; 16 entries was the knee.
 * Set -DMARK_PFQ=0 to get the plain stack-driven loop (e.g. embedded targets).
 */
#if !defined(MARK_PFQ)
#define MARK_PFQ 16
#endif
#if defined(__has_builtin)
#if __has_builtin(__builtin_prefetch)
#define MARK_PREFETCH(p) __builtin_prefetch(p)
#endif
#elif defined(__GNUC__)
#define MARK_PREFETCH(p) __builtin_prefetch(p)
#endif
#if !defined(MARK_PREFETCH)
/* No prefetch available, so the ring would be pure overhead. */
#undef MARK_PFQ
#define MARK_PFQ 0
#endif
#if MARK_PFQ > 0 && !defined(MARK_PFQ_MAXDEPTH)
/*
 * The ring reorders the depth-first walk: it always refills from the newest
 * work-stack entries and consumes one entry per visit, so on a long spine
 * whose visits push more entries than they consume (a list of records,
 * a string read from a handle, ...) the older entries are stranded on the
 * stack and it grows linearly with the length of the list instead of staying
 * at the depth of the structure.  Above this much growth within one mark()
 * call the plain depth-first step is used until the stack has drained again,
 * which bounds the extra stack to this limit plus the structure's depth.
 * The self-compile peaks around 20K slots, well below the default.
 */
#define MARK_PFQ_MAXDEPTH ((stackptr_t)(stack_size / 8))
#endif

/* Push a pointer to a NODEPTR slot on the stack, for the mark phase. */
#define PUSH_SLOT(s) do { NODEPTR _q; _q.node_pptr = (s); PUSH(_q); } while(0)

/* Mark all used nodes reachable from *np, updating *np. */
void
mark(NODEPTR *np)
{
  stackptr_t stk = stack_ptr;
  NODEPTR n;
  struct node *p;
#if MARK_PFQ > 0
  /* Ring of pending slots whose cells have been prefetched.
   * Every (possibly nested) mark() call owns its own ring and drains it
   * before returning, so recursion for integer leaves and the
   * thread/mvar side structures is unaffected. */
  NODEPTR *pfq[MARK_PFQ];
  unsigned pfq_head = 0, pfq_tail = 0, pfq_n = 0;
#endif

  //  mark_depth++;
  //  if (mark_depth % 10000 == 0)
  //    PRINT("mark depth %"PRIcounter"\n", mark_depth);
 top:
  n = indir(np);
  p = get_node_ptr(n);
  if (!p)
    goto fin;                   /* not a real pointer, but there may be more pending work */

  /* n/p points to a real node */
  if (p < cells || p > cells + heap_size)
    ERR("bad n");
  if (is_marked_used(n)) {
    goto fin;
  }
  num_marked++;
  mark_used(n);
  if (is_AP(FUN(n))) {
#if GCRED
    if (want_gc_red) {
      NODEPTR r = gc_reduce(n);
      if (!is_NIL(r)) {
        /* n is now an indirection to r.  Redirect this reference and mark r;
         * other references to n will be redirected when they are marked. */
        *np = r;
        goto top;
      }
    }
#endif  /* GCRED */
#if MARK_PFQ > 0
    if (stack_ptr - stk <= MARK_PFQ_MAXDEPTH) {
      /* The probe keeps already-marked (shared) nodes, and virtual pointers,
       * off the work stack; it only touches the bitmap, not the cell.
       * The cell itself is not read until the ring has prefetched it, which is
       * why the mark-integer-leaves shortcut of the plain step below (it needs
       * the cell's tag) is not used. */
      if (!is_marked_used(ARG(n)))
        PUSH_SLOT(&ARG(n));
      /* FUN becomes a work item too, so the ring can prefetch it.
       * It is pushed last so that the spine is still walked first. */
      if (!is_marked_used(FUN(n)))
        PUSH_SLOT(&FUN(n));
      goto fin;
    }
    /* Stack has grown too much (see MARK_PFQ_MAXDEPTH): plain step. */
#endif  /* MARK_PFQ */
    /* Mark integer leaves immediately instead of deferring them while walking
     * the other branch.  In particular, a string can contain a distinct integer
     * node (behind an indirection) for every character read from a handle.
     */
    if (get_XTAG(indir(&ARG(n))) == D_INT) {
      mark(&ARG(n));
    } else if (!is_marked_used(ARG(n))) {
      /* The probe keeps already-marked (shared) nodes off the work stack. */
      PREFETCH_READ(ARG(n).node_ptr);   /* won't be popped and dereferenced until later */
      PUSH_SLOT(&ARG(n));
    }
    /* Iterate straight down the spine; keeps the work stack shallow. */
    np = &FUN(n);
    goto top;
  }

  switch (get_DATA_TAG(FUN(n))) {
#if GCRED && INTTABLE
  case D_INT:
    {
      /* Replace a small INT by the shared permanent node */
      value_t val = GETVALUE(n);
      if (LOW_INT <= val && val < HIGH_INT) {
        NODEPTR i = intTable[val - LOW_INT];
        set_IND(n, i);          /* other references will be redirected later */
        *np = i;                /* redirect this reference */
        COUNT(red_int);
      }
      goto fin;
    }
#endif  /* GCRED && INTTABLE */
  case D_ARR:
    {
      struct ioarray *arr = ARR(n);

      // arr->marked records marking progress through arr.
      if (arr->marked >= arr->size) {
        // An empty array has nothing to scan, but the sweep must keep it.
        if (arr->size == 0)
          arr->marked = 1;
        goto fin;
      }
      // We unmark the array as a whole and push it as long
      // as there's more entries to scan.
      mark_unused(n);
      num_marked--;
      PUSH_SLOT(np);
      if (!is_marked_used(arr->array[arr->marked]))
        PUSH_SLOT(&arr->array[arr->marked]);
      arr->marked++;
      goto fin;
    }

  case D_FORPTR:
    FORPTR(n)->finalizer->marked = 1;
    goto fin;

  case D_THID:
    mark_thread(THR(n));
    goto fin;

  case D_MVAR:
    mark_mvar(MVAR(n));
    goto fin;

  case D_WEAK:
    WEAK(n)->marked = 1;
    goto fin;

  default:
    goto fin;
  }

 fin:
#if MARK_PFQ > 0
  if (stack_ptr - stk <= MARK_PFQ_MAXDEPTH) {
    /* Top up the ring from the work stack, issuing a prefetch for each
     * pending cell, then process the oldest entry: by now its cell has had
     * MARK_PFQ-1 other visits worth of time to arrive in cache. */
    while (pfq_n < MARK_PFQ && stack_ptr > stk) {
      NODEPTR *slot = POPTOP().node_pptr;
      MARK_PREFETCH(slot->node_ptr);
      pfq[pfq_tail] = slot;
      pfq_tail = (pfq_tail + 1) % MARK_PFQ;
      pfq_n++;
    }
    if (pfq_n > 0) {
      np = pfq[pfq_head];
      pfq_head = (pfq_head + 1) % MARK_PFQ;
      pfq_n--;
      goto top;
    }
    /* Ring empty here means the loop above found the stack empty. */
    return;
  }
  /* Too deep (see MARK_PFQ_MAXDEPTH): plain depth-first pops until the
   * stack has drained; the ring's pending entries wait, they are picked up
   * again when the ring is resumed. */
#endif  /* MARK_PFQ */
  if (stack_ptr > stk) {
    np = POPTOP().node_pptr;
    goto top;
  }
}

#if WANT_PROFILE
/* Keep the cells of the loaded program, so that prof_src stays valid.
 * This runs after marking: an unreachable loaded cell is kept, but what
 * it points to is not, so only its profiling fields may be used. */
static void
prof_pin(void)
{
  for (struct node *p = prof_lo; p < prof_hi; p++) {
    NODEPTR n;
    n.node_ptr = p;
    if (!is_marked_used(n)) {
      mark_used(n);
      num_marked++;
    }
  }
}
static void prof_hot_gc(void);
#endif  /* WANT_PROFILE */

/* Perform a garbage collection:
   - Mark nodes from the stack
   - Mark permanent arrays
   - Mark threads that have a root
   - Scan and free arrays
   - Scan and free foreign pointers and run finalizers
   - Scan and free threads
   - Scan and free mvars
*/
void
gc(void)
{
  stackptr_t i;
  //printf("****** GC ********\n");

  // gc_tot += stack_ptr+1;

  num_gc++;
  num_marked = 0;
#if WANT_STDIO
  if (verbose > 1)
    PRINT("gc mark\n");
#endif
  gc_mark_time -= GETTIMEMILLI();
  mark_all_free();
#if GCRED
  gc_eval_stack_ptr = stack_ptr;
#endif
  /* Mark everything reachable from the stack */
  for (i = 0; i <= stack_ptr; i++)
    mark(&stack[i]);

  /* Mark everything reachable from permanent array nodes */
  for (struct ioarray *arr = array_root; arr; arr = arr->next) {
    if (arr->permanent) {
      for (i = 0; i < arr->size; i++)
        mark(&arr->array[i]);
    }
  }

  /* Mark all FFI exports */
  if (xffe_table) {
    for(struct ffe_entry *f = xffe_table; f->ffe_name; f++) {
      mark((NODEPTR*)&f->ffe_value);
    }
  }

  /* Mark used stable pointers */
  for (size_t i = 0; i < sp_capacity; i++) {
    if (!is_NIL(sp_table[i]))
      mark(&sp_table[i]);
  }

  /* Mark everything reachable from the threads.
   * Note, zombie threads have no root so they are not marked.
   */
  for (struct mthread *mt = all_threads; mt; mt = mt->mt_next) {
    if (!is_NIL(mt->mt_root))
      mark_thread(mt);
  }

  /* check for unmarked weak pointers */
  sweep_weaks();
#if WANT_PROFILE
  prof_hot_gc();                /* before prof_pin, to see which loaded nodes are unreachable */
  prof_pin();
#endif

#if WANT_SAMPLE
  sample_gc();
#endif

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

  /* Remove unreferenced zombie threads */
  for (struct mthread **mtp = &all_threads; *mtp; ) {
    struct mthread *mt = *mtp;
    if ((mt->mt_state == ts_died || mt->mt_state == ts_finished) && !mt->mt_mark) {
      COUNT(num_thread_reap);
      *mtp = mt->mt_next;
      FREE(mt);
    } else {
      mt->mt_mark = false;
      mtp = &mt->mt_next;
    }
  }

  /* Remove unreferences mvars */
  for (struct mvar **mvp = &all_mvars; *mvp; ) {
    struct mvar *mv = *mvp;
    if (!mv->mv_mark) {
      COUNT(num_mvar_free);
      *mvp = mv->mv_next;
      FREE(mv);
    } else {
      mv->mv_mark = false;
      mvp = &mv->mv_next;
    }
  }

  gc_scan_time += GETTIMEMILLI();

#if WANT_STDIO
  if (verbose > 1) {
    PRINT("gc done, %"PRIcounter" free\n", num_free);
    /*PRINT(" GC reductions A=%"PRIcounter", K=%"PRIcounter", I=%"PRIcounter", int=%"PRIcounter" flip=%"PRIcounter"\n",
      red_a, red_k, red_i, red_int, red_flip);*/
  }
  if (gcbell) {
    fputc('\007', stderr);      /* ring the bell */
    fflush(stderr);
  }
#endif  /* !WANT_STDIO */

#if 0
  /* For debugging only: mark all free cells */
  for(int n = 0; n < heap_size; n++) {
    NODEPTR p;
    p.node_ptr = HEAPREF(n);
    if (!is_marked_used(p)) {
      set_DATA_TAG(p, D_NONE);
    }
  }
#endif
#if 0
  {
    BFILE *err = add_fd(2);
    putsb("GC ", err); putdecb(num_free, err); putsb(" free\r\n", err);
    closeb(err);
  }
#endif
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

#if WANT_INT64
static INLINE
uint64_t
peek_uint64(uint64_t *p)
{
  return *p;
}

static INLINE
void
poke_uint64(uint64_t *p, uint64_t w)
{
  *p = w;
}
#endif  /* WANT_INT64 */

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

#if WANT_INT64
static INLINE
int64_t
peek_int64(int64_t *p)
{
  return *p;
}

static INLINE
void
poke_int64(int64_t *p, int64_t w)
{
  *p = w;
}
#endif  /* WANT_INT64 */

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
peek_char(char *p)
{
  return *p;
}

static INLINE
void
poke_char(char *p, value_t w)
{
  *p = (char)w;
}

static INLINE
value_t
peek_schar(signed char *p)
{
  return *p;
}

static INLINE
void
poke_schar(signed char *p, value_t w)
{
  *p = (signed char)w;
}

static INLINE
value_t
peek_uchar(unsigned char *p)
{
  return *p;
}

static INLINE
void
poke_uchar(unsigned char *p, value_t w)
{
  *p = (unsigned char)w;
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

/* A long long can be bigger than a value_t, so it is passed as itself. */
static INLINE
long long
peek_llong(long long *p)
{
  return *p;
}

static INLINE
void
poke_llong(long long *p, long long w)
{
  *p = w;
}

static INLINE
unsigned long long
peek_ullong(unsigned long long *p)
{
  return *p;
}

static INLINE
void
poke_ullong(unsigned long long *p, unsigned long long w)
{
  *p = w;
}

static INLINE
value_t
peek_size_t(size_t *p)
{
  return *p;
}

static INLINE
void
poke_size_t(size_t *p, value_t w)
{
  *p = (size_t)w;
}

#if WANT_FLOAT32
static INLINE
flt32_t
peek_flt32(flt32_t *p)
{
  return *p;
}

static INLINE
void
poke_flt32(flt32_t *p, flt32_t w)
{
  *p = w;
}
#endif  /* WANT_FLOAT32 */

#if WANT_FLOAT64
static INLINE
flt64_t
peek_flt64(flt64_t *p)
{
  return *p;
}

static INLINE
void
poke_flt64(flt64_t *p, flt64_t w)
{
  *p = w;
}
#endif  /* WANT_FLOAT64 */

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
    r = alloc_node(D_BADDYN);
    fun = mmalloc(strlen(buf) + 1);
    strcpy(fun, buf);
    CSTR(r) = fun;
  } else {
    r = alloc_node(D_IO_CCALL);
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
  uvalue_t neg = 1;
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
  return (value_t)(neg * i);
}

#if WANT_INT64
int64_t
parse_int64(BFILE *f)
{
  // Parse using uint64_t, which wraps on overflow.
  uint64_t i = 0;
  uint64_t neg = 1;
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
  return (int64_t)(neg * i);
}
#endif  /* WANT_INT64 */

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

/* Create a ByteString node.  It's a foreign pointer tagged with FP_BSTR. */
NODEPTR
mkStrNode(struct bytestring str)
{
  NODEPTR n = alloc_node(D_FORPTR);
  struct forptr *fp = mkForPtrFree(str);
  FORPTR(n) = fp;
  fp->finalizer->fptype = FP_BSTR;
  //printf("mkForPtr n=%p fp=%p %d %s payload.string=%p\n", n, fp, (int)FORPTR(n)->payload.size, (char*)FORPTR(n)->payload.string, FORPTR(n)->payload.string);
  return n;
}

/* Table of labelled nodes for sharing during parsing. */
struct shared_entry {
  heapoffs_t label;
  NODEPTR node;                 /* NIL indicates unused */
};

/* The memory allocated here is never freed.
 * This could be fixed by using a forptr and a
 * finalizer for read UTF-8 strings.
 * Fix this if there is a lot of deserialization.
 */
struct bytestring
parse_string(BFILE *f)
{
  size_t sz = 20;
  uint8_t *buffer = mmalloc(sz);
  size_t i;
  int c;

  for(i = 0;;) {
    c = getb(f);
    if (c < 0)
      ERR("parse string EOF");
    if (c == '"')
      break;
    if (i >= sz - 1) {
      sz *= 2;
      buffer = mrealloc(buffer, sz);
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
  buffer[i] = 0;    /* add a trailing 0 in case we need a C string, not counted in size */
  buffer = mrealloc(buffer, i + 1);
  //printf("parse_string %d %s\n", (int)bs.size, (char*)bs.string);
  return mk_ro_bytestring(i, buffer);
}

struct forptr *new_mpz(void);

struct parse_ctx {
  struct shared_entry *shared_table;
  heapoffs_t shared_table_size;
  jmp_buf deser_err;
  const char *message;
};

/* Look for the label in the table.
 * If it's found, return the node.
 * If not found, return the first empty entry.
*/
NODEPTR *
find_label(struct parse_ctx *pc, heapoffs_t label)
{
  int i;

  for(i = (int)label; ; i++) {
    i %= pc->shared_table_size;
    if (is_NIL(pc->shared_table[i].node)) {
      /* The slot is empty, so claim and return it */
      pc->shared_table[i].label = label;
      return &pc->shared_table[i].node;
    } else if (pc->shared_table[i].label == label) {
      /* Found the label, so return it. */
      return &pc->shared_table[i].node;
    }
    /* Not empty and not found, try next. */
  }
}

#define DESER_ERR(s) do { pc->message = (s); longjmp(pc->deser_err, 1); } while(0);

NODEPTR
parse(struct parse_ctx *pc, BFILE *f)
{
  stackptr_t stk = stack_ptr;
  NODEPTR r, x, y;
  NODEPTR *nodep;
  heapoffs_t l;
  int c;
  size_t j;
  char buf[80];                 /* store names of primitives. */

  for(;;) {
    c = getb(f);
    if (c < 0) DESER_ERR("parse EOF");
    switch (c) {
    case ' ':
    case '\n':
      continue;
    }
    if (num_free < 3)
      DESER_ERR("out of heap reading code");
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
        DESER_ERR("parse: stack");
      return x;
#if WANT_GMP || WANT_IMATH
    case '%':
      {
        struct bytestring bs = parse_string(f); /* get all the digits, terminated by " */
        struct forptr *fp = new_mpz();          /* a new mpz */
        mpz_ptr op = fp->payload.bs_array;      /* get actual pointer */
        mpz_set_str(op, bs.bs_array, 10);       /* convert to an mpz */
        FREE(bs.bs_array);
        r = alloc_node(D_FORPTR);
        FORPTR(r) = fp;
        PUSH(r);
        break;
      }
#endif
    case '&':
      {
        int is32 = gobble(f, '&');
        for (j = 0; (buf[j] = getNT(f)); j++)
          ;
        if (is32) {
#if WANT_FLOAT32
          r = mkFlt32(strtof(buf, NULL));
#else
          r = alloc_node(D_FLT32);
          SETVALUE(r, 0);
#endif
        } else {
#if WANT_FLOAT64
          r = mkFlt64(strtod(buf, NULL));
#else
          r = alloc_node(D_DBL);
          SETVALUE(r, 0);
#endif
        }
        PUSH(r);
        break;
      }
    case '#':
      if (gobble(f, '#')) {
#if WANT_INT64
        r = mkInt64(parse_int64(f));
#else
        DESER_ERR("no Int64");
#endif /* WANT_INT64 */
      } else {
        r = mkInt(parse_int(f));
      }
      PUSH(r);
      break;
    case '[':
      {
        size_t sz;
        struct ioarray *arr;
        size_t i;
        sz = (size_t)parse_int(f);
        if (!gobble(f, ']'))
          DESER_ERR("parse arr 1");
        arr = arr_alloc(sz, NIL_NODEPTR);
        for (i = 0; i < sz; i++) {
          arr->array[i] = TOP(sz - i - 1);
        }
        r = alloc_node(D_ARR);
        ARR(r) = arr;
        POP(sz);
        PUSH(r);
        break;
      }
    case '_':
      /* Reference to a shared value: _label */
      l = parse_int(f);  /* The label */
      nodep = find_label(pc, l);
      if (is_NIL(*nodep)) {
        /* Not yet defined, so make it an indirection */
        *nodep = alloc_node(D_NONE);
        set_IND(*nodep, NIL_NODEPTR);
      }
      PUSH(*nodep);
      break;
    case ':':
      /* Define a shared expression: :label e */
      l = parse_int(f);  /* The label */
#if WANT_PROFILE
      if (prof_loading)
        prof_set_label((uint32_t)l + 1); /* the cells read since the last label are the definition */
#endif
      if (!gobble(f, ' '))
        DESER_ERR("parse ' '");
      nodep = find_label(pc, l);
      x = TOP(0);
#if WANT_PROFILE
      if (prof_loading && prof_roots && l < prof_nlabels && !is_VIRT(x))
        prof_roots[l] = NPTR(x);
#endif
      if (is_NIL(*nodep)) {
        /* not referenced yet, so add a direct reference */
        *nodep = x;
      } else {
#if 0
        /* Sanity check */
        NODEPTR q = get_IND(*nodep);
        if (!q || is_NIL(q))
          ERR("shared != NIL");
#endif
        set_IND(*nodep, x);
      }
      break;
    case '"':
      /* Everything up to the next " is a string.
       * Special characters are encoded as \NNN&,
       * where NNN is the decimal value of the character */
      PUSH(mkStrNode(parse_string(f)));
      break;
    case '$':
      {
        struct bytestring bs = mk_ro_bytestring(parse_int(f), NULL);
        if (!gobble(f, ' '))
          DESER_ERR("bytestring parse error");
        for(size_t i = 0; i < bs.bs_size; i++) {
          ((uint8_t*)bs.bs_array)[i] = getb(f);
        }
        PUSH(mkStrNode(bs));
        break;
      }

#if WANT_TICK
    case '!':
      if (!gobble(f, '"'))
        DESER_ERR("parse !");
      r = alloc_node(D_TICK);
      SETVALUE(r, (value_t)add_tick_table(parse_string(f)));
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
        DESER_ERR("unknown funptr");
      }
      break;
    case 'C':
      if (gobble(f, '_')) {
        int k = parse_int(f);
        if (!gobble(f, '_'))
          ERR("parse C");
        int n = parse_int(f);
        if (k < 0 || k > PTR_TAG_NCON_MASK || n < 0 || n > PTR_TAG_NARG_MASK)
          ERR("parse C");
        r = mk_CON(V_CONSTR, k, n);
        PUSH(r);
        break;
      } else {
        goto def;
      }
    case 'A':
      if (gobble(f, '_')) {
        int k = parse_int(f);
        if (!gobble(f, '_'))
          ERR("parse A");
        int n = parse_int(f);
        if (k < 0 || k > PTR_TAG_NCON_MASK || n < 0 || n > PTR_TAG_NARG_MASK)
          ERR("parse A");
        r = mk_CON(V_ARM, k, n);
        PUSH(r);
        break;
      } else {
        goto def;
      }
    case 'D':
      if (gobble(f, '_')) {
        /* D_a0_..._a{m-1}, m is at most 2047 since the LOOKD marker holds m in 11 bits */
        uint8_t arities[PTR_TAG_NCON_MASK];
        uint32_t m = 0;
        do {
          value_t a = parse_int(f);
          if (m >= PTR_TAG_NCON_MASK || a < 0 || a > PTR_TAG_NARG_MASK)
            ERR("parse D");
          arities[m++] = (uint8_t)a;
        } while (gobble(f, '_'));
        r = mk_CASE(V_CASED, find_case_arity(m, arities));
        PUSH(r);
        break;
      } else {
        goto def;
      }      
    case 'L':
      if (gobble(f, '_')) {
        int m = parse_int(f);
        r = mk_CASE(V_LOOKD, m);
        PUSH(r);
        break;
      } else {
        goto def;
      }      
    case 'E':
      if (gobble(f, '_')) {
        int m = parse_int(f);
        if (m < 0 || m > PTR_TAG_NCON_MASK)
          ERR("parse E");
        r = mk_CASE(V_CASES, m);
        PUSH(r);
        break;
      } else {
        goto def;
      }      
    case 'M':
      if (gobble(f, '_')) {
        int m = parse_int(f);
        r = mk_CASE(V_LOOKS, m);
        PUSH(r);
        break;
      } else {
        goto def;
      }      
    def:
    default:
      buf[0] = c;
      /* A primitive, keep getting char's until end */
      for (j = 1; (buf[j] = getNT(f)); j++) {
        if (j >= sizeof buf) {
          DESER_ERR("bad input");
        }
      }

      /* Look up the primop (O(1) hash lookup) and make a virtual pointer for it. */
      {
        int t = lookup_primop(buf);
        if (t < 0)
          DESER_ERR("unknown primop");
        switch (t) {
#if WANT_STDIO
        /* The standard handles are real (permanent) FORPTR nodes */
        case T_IO_STDIN:  r = comb_stdin;  break;
        case T_IO_STDOUT: r = comb_stdout; break;
        case T_IO_STDERR: r = comb_stderr; break;
#endif  /* WANT_STDIO */
        default:          r = alloc_node(t); break;
        }
      }
      PUSH(r);
      break;
    }
  }
}

void
checkversion(struct parse_ctx *pc, BFILE *f)
{
  char *p = VERSION;
  int c;

  while ((c = *p++)) {
    if (c != getb(f))
      DESER_ERR("version mismatch");
  }
  (void)gobble(f, '\r');                 /* allow extra CR */
}

/* Parse a file */
/* XXX We should not be using the ordinary stack here. */
NODEPTR
parse_top(BFILE *f, struct ffe_entry *ffe)
{
  heapoffs_t numLabels, i;
  NODEPTR n;
  struct parse_ctx spc, *pc = &spc;

  pc->shared_table = 0;
  if (setjmp(pc->deser_err)) {
    /* exception */
    if (pc->shared_table)
      FREE(pc->shared_table);
    return NIL_NODEPTR;
  } else {
    checkversion(pc, f);
    numLabels = parse_int(f);
    if (!gobble(f, '\n'))
      DESER_ERR("size parse");
#if WANT_PROFILE
    if (prof_loading) {
      prof_nlabels = numLabels;
      prof_roots = mcalloc(numLabels + 1, sizeof(struct node *));
      prof_first = mcalloc(numLabels + 1, sizeof(struct node *));
    }
#endif
    gobble(f, '\r');                 /* allow extra CR */
    pc->shared_table_size = 3 * numLabels; /* sparsely populated hashtable */
    pc->shared_table = mmalloc(pc->shared_table_size * sizeof(struct shared_entry));
    for(i = 0; i < pc->shared_table_size; i++)
      pc->shared_table[i].node = NIL_NODEPTR;
    n = parse(pc, f);
    if (ffe) {
      for(struct ffe_entry *f = ffe; f->ffe_name; f++) {
        heapoffs_t l = atoi(f->ffe_name+1); /* the name must be numerical */
        f->ffe_value = find_label(pc, l)->node_ptr;
      }
    }
    FREE(pc->shared_table);
    return n;
  }
}

/* Two bits per node: marked, shared
 * 0, 0   -- not visited
 * 1, 0   -- visited once
 * 1, 1   -- visited more than once
 * 0, 1   -- printed
 */
struct print_bits {
  bits_t *marked_bits;
  bits_t *shared_bits;
  jmp_buf ser_err;
  const char *message;
};
/* Virtual NODEPTRs are never shared, so they have no bits. */
static INLINE void set_bit(bits_t *bits, NODEPTR n)
{
  if (is_VIRT(n))
    return;
  heapoffs_t i = LABEL(n);
  bits[i / BITS_PER_WORD] |= (1ULL << (i % BITS_PER_WORD));
}
#if WANT_STDIO
static INLINE void clear_bit(bits_t *bits, NODEPTR n)
{
  if (is_VIRT(n))
    return;
  heapoffs_t i = LABEL(n);
  bits[i / BITS_PER_WORD] &= ~(1ULL << (i % BITS_PER_WORD));
}
#endif
static INLINE int test_bit(bits_t *bits, NODEPTR n)
{
  if (is_VIRT(n))
    return 0;
  heapoffs_t i = LABEL(n);
  return (bits[i / BITS_PER_WORD] & (1ULL << (i % BITS_PER_WORD))) != 0;
}

size_t strNodes(size_t len);
NODEPTR mkStringC(char *str);

#if WANT_STDIO
void
convdbl(char *str, char *fmt, flt64_t x)
{
  /* Using 16 decimals will lose some precision.
   * 17 would keep the precision, but it frequently looks very ugly.
   */
  (void)snprintf(str, 25, fmt, x);
  if (strcmp(str, "nan") != 0 && strcmp(str, "-nan") != 0 &&
      strcmp(str, "inf") != 0 && strcmp(str, "-inf") != 0 &&
      !strchr(str, '.') && !strchr(str, 'e') && !strchr(str, 'E')) {
    /* There is no decimal point and no exponent, so add a decimal point */
    strcat(str, ".0");
  }
}

void
putdblb(flt64_t x, BFILE *p)
{
  char str[30];
  convdbl(str, "%.16g", x);
  putsb(str, p);
}

void printrec(BFILE *f, struct print_bits *pb, NODEPTR n, bool prefix);

/* Mark all reachable nodes, when a marked node is reached, mark it as shared. */
void
find_sharing(counter_t *num_sharedp, struct print_bits *pb, NODEPTR n)
{
 top:
  { NODEPTR np = n;
    n = indir(&np);
  }
  struct node *p = get_node_ptr(n);
  if (!p)
    return;
  if (p < cells || p >= cells + heap_size) abort();

  //PRINT("find_sharing %p %llu ", n, LABEL(n));
  tag_t tag = get_XTAG(n);
  if (tag == X_AP || tag == D_ARR || tag == D_FORPTR) {
    if (test_bit(pb->shared_bits, n)) {
      /* Alread marked as shared */
      //PRINT("shared\n");
      ;
    } else if (test_bit(pb->marked_bits, n)) {
      /* Already marked, so now mark as shared */
      //PRINT("marked\n");
      set_bit(pb->shared_bits, n);
      (*num_sharedp)++;
    } else {
      /* Mark as visited, and recurse */
      //PRINT("unmarked\n");
      set_bit(pb->marked_bits, n);
      switch(tag) {
      case X_AP:
        find_sharing(num_sharedp, pb, FUN(n));
        n = ARG(n);
        goto top;
      case D_ARR:
        for(size_t i = 0; i < ARR(n)->size; i++) {
          find_sharing(num_sharedp, pb, ARR(n)->array[i]);
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
  uint8_t *str = bs.bs_array;
  putb('"', f);
  for (size_t i = 0; i < bs.bs_size; i++) {
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
    } else {                    /* must be< c == 0xff */
      putb('\\', f); putb('_', f);
    }
#endif
  }
  putb('"', f);
}

#define SER_ERR(s) do { pb->message = (s); longjmp(pb->ser_err, 1); } while(0)
/*
 * Recursively print an expression.
 * This assumes that the shared nodes has been marked as such.
 * The prefix flag is used to get a readable dump.
 */
void
printrec(BFILE *f, struct print_bits *pb, NODEPTR n, bool prefix)
{
  NODEPTR n1;
  int share = 0;
  enum node_tag tag;
  char prbuf[30];

  for (;;) {
    n1 = get_IND(n);
    if (is_NIL(n1))
      break;
    /*putb('*', f);*/
    n = n1;
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
  tag = get_XTAG(n);
  switch (tag) {
  case X_AP:
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
  case D_INT: putb('#', f); putdecb(GETVALUE(n), f); break;
#if WANT_INT64
  case D_INT64: putb('#', f); putb('#', f); putdecb64(GETINT64VALUE(n), f); break;
#endif  /* WANT_INT64 */
#if WANT_FLOAT64
  case D_DBL: putb('&', f); putdblb(GETDBLVALUE(n), f); break;
#endif
#if WANT_FLOAT32
  case D_FLT32: putb('&', f); putb('&', f); putdblb((double)GETFLTVALUE(n), f); break;
#endif
  case D_WEAK: SER_ERR("serialize WEAK unimplemented");
  case D_ARR:
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
  case D_MVAR:
    SER_ERR("cannot serialize MVAR");
    break;
  case D_PTR:
    if(PTR(n) == NULL) {
      if (prefix) {
        putsb("(toPtr #0)", f);
      } else {
        putsb("toPtr #0 @", f);
      }
    } else
#if WANT_STDIO
    /* The pointer can be a forptr comb_std* that has been dereferenced */
    if (PTR(n) == FORPTR(comb_stdin)->payload.bs_array) {
      mk_AP(spare_node, combFP2P, comb_stdin);
      printrec(f, pb, spare_node, prefix);
    } else if (PTR(n) == FORPTR(comb_stdout)->payload.bs_array) {
      mk_AP(spare_node, combFP2P, comb_stdout);
      printrec(f, pb, spare_node, prefix);
    } else if (PTR(n) == FORPTR(comb_stderr)->payload.bs_array) {
      mk_AP(spare_node, combFP2P, comb_stderr);
      printrec(f, pb, spare_node, prefix);
    } else
#endif  /* WANT_STDIO */
    if (prefix) {
      snprintf(prbuf, sizeof prbuf, "PTR<%p>",PTR(n));
      putsb(prbuf, f);
    } else {
      SER_ERR("Cannot serialize pointers");
    }
    break;
  case D_FUNPTR:
    /* There are a few function pointers that happen without user FFI.
     * We need to be able to serialize these.
     * XXX Make a table if we need more.
     */
    if (FUNPTR(n) == 0) {
      putsb(";0 ", f);
    } else if (FUNPTR(n) == (HsFunPtr)closeb) {
      putsb(";closeb ", f);
    } else if (prefix) {
      snprintf(prbuf, sizeof prbuf, "FUNPTR<%p>", FUNPTR(n));
      putsb(prbuf, f);
    } else {
      SER_ERR("Cannot serialize function pointers");
    }
    break;
  case D_THID:
    if (prefix) {
      snprintf(prbuf, sizeof prbuf, "FUNPTR<%d>",(int)THR(n)->mt_id);
    } else {
      SER_ERR("cannot serialize ThreadId yet");
    }
    break;
  case D_FORPTR:
#if WANT_STDIO
    if (eq_NODEPTR(n, comb_stdin))
      putsb("IO.stdin", f);
    else if (eq_NODEPTR(n, comb_stdout))
      putsb("IO.stdout", f);
    else if (eq_NODEPTR(n, comb_stderr))
      putsb("IO.stderr", f);
    else
#endif  /* WANT_STDIO */
#if WANT_GMP || WANT_IMATH
    if (FORPTR(n)->finalizer->fptype == FP_MPZ) {
      /* Serialize as %99999" */
      mpz_ptr op = FORPTR(n)->payload.bs_array; /* get the mpz */
      int sz = mpz_sizeinbase(op, 10);        /* maximum length */
      char *s = mmalloc(sz + 2);
      (void)mpz_get_str(s, 10, op);           /* convert to a string */
      putsb("%", f);
      putsb(s, f);
      putsb("\"", f);                         /* so we can use parse_string */
      FREE(s);
    } else
#endif  /* WANT_GMP || WANT_IMATH */
    if (FORPTR(n)->finalizer->fptype == FP_BSTR) {
      struct bytestring bs = FORPTR(n)->payload;
      if (bs.bs_size > 100) {
        /* Encode large bytestrings with $len data */
        putb('$', f);
        putdecb(bs.bs_size, f);
        putb(' ', f);
        writeb(bs.bs_array, bs.bs_size, f);
      } else {
        print_string(f, bs);
      }
    } else if (prefix) {
      snprintf(prbuf, sizeof prbuf, "FORPTR<%p>",FORPTR(n));
      putsb(prbuf, f);
    } else {
      SER_ERR("Cannot serialize foreign pointers");
    }
    break;
  case D_IO_CCALL: putb('^', f); putsb(FFI_IX(GETVALUE(n)).ffi_name, f); break;
  case D_BADDYN: putb('^', f); putsb(CSTR(n), f); break;
#if WANT_TICK
  case D_TICK:
    putb('!', f);
    print_string(f, tick_table[GETVALUE(n)].tick_name);
    break;
#endif
  case V_CASED:
    {
      struct case_arity *c = get_CASED_arity(n);
      putb('D', f);
      for (uint32_t k = 0; k < c->m; k++) {
        putb('_', f);
        putdecb((value_t)c->arities[k], f);
      }
      break;
    }
  case V_LOOKD:
  case V_CASES:
  case V_LOOKS:
    {
      switch(tag) {
      case V_LOOKD: putb('L', f); break;
      case V_CASES: putb('E', f); break;
      case V_LOOKS: putb('M', f); break;
      default: ERR("V_");
      }
      putb('_', f);
      putdecb((value_t)get_CASE_size(n), f);
      break;
    }
  case V_CONSTR:
    {
      putb('C', f);
      putb('_', f);
      int k, nn;
      get_CON_info(n, &k, &nn);
      putdecb((value_t)k, f);
      putb('_', f);
      putdecb((value_t)nn, f);
      break;
    }
  case V_ARM:
    {
      putb('A', f);
      putb('_', f);
      int k, nn;
      get_CON_info(n, &k, &nn);
      putdecb((value_t)k, f);
      putb('_', f);
      putdecb((value_t)nn, f);
      break;
    }
  default:
    if (0 <= tag && tag <= T_LAST_TAG) {
      if (tag_names[tag]) {
        putsb(tag_names[tag], f);
      } else {
        SER_ERR("TAG1");
      }
    } else {
      SER_ERR("TAG2");
    }
    break;
  }
  if (!prefix) {
    if (tag != X_AP)
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
printb(BFILE *f, NODEPTR n, bool header)
{
  struct print_bits pb;
  counter_t num_shared = 0;

  pb.marked_bits = mcalloc(free_map_nwords, sizeof(bits_t));
  pb.shared_bits = mcalloc(free_map_nwords, sizeof(bits_t));
  find_sharing(&num_shared, &pb, n);
  if (header) {
    putsb(VERSION, f);
    putdecb(num_shared, f);
    putb('\n', f);
  }
  if (setjmp(pb.ser_err)) {
    /* exception */
    FREE(pb.marked_bits);
    FREE(pb.shared_bits);
    raise_rts(exn_serialize);
  } else {
    printrec(f, &pb, n, !header);
    if (header) {
      putb('}', f);
    }
    FREE(pb.marked_bits);
    FREE(pb.shared_bits);
  }
}

/* Show a graph. */
void
pps(NODEPTR n)
{
  pp(stdout, n);
}

#if WANT_SAMPLE
static int
sample_cmp(const void *a, const void *b)
{
  counter_t x = ((const struct sample_entry *)a)->hits;
  counter_t y = ((const struct sample_entry *)b)->hits;
  return x < y ? 1 : x > y ? -1 : 0;
}

/* Serialize the graph of a node to a file, as printb does, but without
 * raising an exception if it cannot be serialized. */
static void
sample_dump(const char *name, NODEPTR n)
{
  FILE *f = fopen(name, "w");
  if (!f)
    return;
  BFILE *bf = add_FILE(f);
  struct print_bits pb;
  counter_t num_shared = 0;
  pb.marked_bits = mcalloc(free_map_nwords, sizeof(bits_t));
  pb.shared_bits = mcalloc(free_map_nwords, sizeof(bits_t));
  find_sharing(&num_shared, &pb, n);
  putsb(VERSION, bf);
  putdecb(num_shared, bf);
  putb('\n', bf);
  if (setjmp(pb.ser_err)) {
    putsb("\ncannot serialize: ", bf);
    putsb(pb.message, bf);
    putb('\n', bf);
  } else {
    printrec(bf, &pb, n, false);
    putb('}', bf);
  }
  FREE(pb.marked_bits);
  FREE(pb.shared_bits);
  closeb(bf);
}

/* Report the hottest nodes, and write the hottest ones to files. */
void
sample_report(void)
{
  if (!sample_interval)
    return;
  struct sample_entry *es = mmalloc(SAMPLE_TABLE_SIZE * sizeof(struct sample_entry));
  size_t ne = 0;
  for (size_t i = 0; i < SAMPLE_TABLE_SIZE; i++)
    if (!is_NIL(sample_table[i].node))
      es[ne++] = sample_table[i];
  qsort(es, ne, sizeof(struct sample_entry), sample_cmp);
  fprintf(stderr, "sampling: %"PRIcounter" samples, one per %"PRIcounter" reductions\n",
          num_samples, sample_interval);
  fprintf(stderr, "  %5.1f%% with a recurring candidate, %5.1f%% without, %5.1f%% with no candidates\n",
          100.0 * num_samples_hit / num_samples,
          100.0 * (num_samples - num_samples_hit - num_samples_none) / num_samples,
          100.0 * num_samples_none / num_samples);
  fprintf(stderr, "  the heads of the samples without a recurring candidate:");
  for (int r = 0; r < 12; r++) {
    int best = -1;
    for (int t = 0; t <= T_LAST_TAG; t++)
      if (sample_miss_heads[t] && (best < 0 || sample_miss_heads[t] > sample_miss_heads[best]))
        best = t;
    if (best < 0)
      break;
    fprintf(stderr, " %s %.1f%%", TAGNAME(best) ? TAGNAME(best) : "?", 100.0 * sample_miss_heads[best] / num_samples);
    sample_miss_heads[best] = 0;
  }
  fprintf(stderr, "\n");
  fprintf(stderr, "  rank       hits   share      outer  node                head\n");
  for (size_t k = 0; k < ne && k < SAMPLE_REPORT; k++) {
    /* Find the head of the node's spine, and the number of arguments it has. */
    NODEPTR h = es[k].node;
    int nargs = 0;
    while (!is_VIRT(h)) {
      NODEPTR f = FUN(h);
      if (is_AP(f)) {
        h = f;
        nargs++;
      } else if (get_DATA_TAG(f) == D_IND) {
        h = ARG(h);
      } else {
        break;
      }
    }
    const char *head = is_VIRT(h) && TAGNAME(get_VIRT_TAG(h)) ? TAGNAME(get_VIRT_TAG(h)) : "?";
    char name[32];
    name[0] = 0;
    if (k < SAMPLE_DUMP) {
      snprintf(name, sizeof name, "mhs-sample-%02d.comb", (int)k + 1);
      sample_dump(name, es[k].node);
    }
    fprintf(stderr, "  %4d %10"PRIcounter" %6.2f%% %10"PRIcounter"  %-18p  %s with %d args  %s\n",
            (int)k + 1, es[k].hits, 100.0 * es[k].hits / num_samples, es[k].outer,
            (void *)get_node_ptr(es[k].node), head, nargs, name);
  }
  FREE(es);
  jx_report();
}
#endif  /* WANT_SAMPLE */

void
pp(FILE *f, NODEPTR n)
{
  BFILE *bf = add_FILE(f);
  printb(bf, n, false);
  putb('\n', bf);
  freeb_file(bf);
}

#if 0
NODEPTR *topnode;

void
ppmsg(const char *msg, NODEPTR n)
{
  printf("%s", msg);
  pp(stdout, n);
  printf("\n");
}

void
dump(const char *msg, NODEPTR at)
{
  atptr = at;
  printf("dump: %s\n", msg);
  pp(stdout, *topnode);
}
#endif

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
  n = alloc_node(D_INT);
  SETVALUE(n, i);
  return n;
}

#if WANT_INT64
NODEPTR
mkInt64(int64_t i)
{
  NODEPTR n;
  n = alloc_node(D_INT64);
  SETINT64VALUE(n, i);
  return n;
}
#endif  /* WANT_INT64 */

#if WANT_FLOAT32
NODEPTR
mkFlt32(flt32_t d)
{
  NODEPTR n;
  n = alloc_node(D_FLT32);
  SETFLTVALUE(n, d);
  return n;
}
#endif  /* WANT_FLOAT32 */

#if WANT_FLOAT64
NODEPTR
mkFlt64(flt64_t d)
{
  NODEPTR n;
  n = alloc_node(D_DBL);
  SETDBLVALUE(n, d);
  return n;
}
#endif  /* WANT_FLOAT64 */

NODEPTR
mkPtr(void* p)
{
  NODEPTR n;
  n = alloc_node(D_PTR);
  PTR(n) = p;
  return n;
}

NODEPTR
mkFunPtr(void (*p)(void))
{
  NODEPTR n;
  n = alloc_node(D_FUNPTR);
  FUNPTR(n) = p;
  return n;
}

struct forptr*
mkForPtr(struct bytestring bs)
{
  struct final *fin = mcalloc(1, sizeof(struct final));
  struct forptr *fp = mcalloc(1, sizeof(struct forptr));
  if (bs.bs_size == NOSIZE) {
    num_fin_alloc++;
  } else {
    num_bs_alloc++;
    num_bs_inuse += bs.bs_size;
    num_bs_bytes += bs.bs_size;
    if (num_bs_inuse > num_bs_inuse_max)
      num_bs_inuse_max = num_bs_inuse;
  }
  //printf("mkForPtr p=%p fin=%p fp=%p\n", p, fin, fp);
  fin->next = final_root;
  final_root = fin;
  fin->final = 0;
  fin->arg = bs.bs_array;
  fin->size = bs.bs_size;          /* The size is not really needed */
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
  struct bytestring bs;
  bs.bs_size = NOSIZE;
  bs.bs_capacity = 0;
  bs.bs_array = p;
  return mkForPtr(bs);
}

struct forptr*
addForPtr(struct forptr *ofp, int s)
{
  struct forptr *fp = mcalloc(1, sizeof(struct forptr));
  struct final *fin = ofp->finalizer;

  /* Only allowed for immutable bytestrings */
  if (ofp->payload.bs_capacity != 0) {
    ERR("addForPtr");
  }

  fp->next = ofp;
  fin->back = fp;
  if (ofp->payload.bs_size != NOSIZE)
    fp->payload.bs_size = ofp->payload.bs_size - s;
  fp->payload.bs_array = (uint8_t*)ofp->payload.bs_array + s;
  fp->finalizer = fin;
  return fp;
}

struct forptr*
bssubstr(struct forptr *fp, value_t offs, value_t len)
{
  struct forptr *res = addForPtr(fp, offs);
  res->payload.bs_size = len;
  return res;
}

/* The array might have moved, so update base pointer in the finalizer. */
void
adjforptr(struct forptr *fp)
{
  struct final *fin = fp->finalizer;
  if (fp->next || fin->back != fp) {
    /* We can only do this if fp is the only pointer to the allocated array.
       Sharing can only happen by adding to a forptr, and we don't do that
       to mutable BSTR pointers. */
    ERR("adjforptr");
  }
  fin->arg = fp->payload.bs_array;
}
    
void
bsappbyte(struct forptr *fp, uint8_t b)
{
  struct bytestring *bsp = &fp->payload;
  if (bsp->bs_size >= bsp->bs_capacity) {
    bsp->bs_capacity += bsp->bs_capacity / 2 + 2;
    bsp->bs_array = mrealloc(bsp->bs_array, bsp->bs_capacity);
    adjforptr(fp);
  }
  ((uint8_t*)bsp->bs_array)[bsp->bs_size++] = b;
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
  const unsigned char *str = bs.bs_array; /* no sign bits, please */

  n = mkNil();
  for(i = bs.bs_size; i > 0; i--) {
    nc = mkInt(str[i-1]);
    n = mkCons(nc, n);
  }
  return n;
}

NODEPTR
mkStringC(char *str)
{
  return mkString(mk_ro_bytestring(strlen(str), str));
}

NODEPTR
mkStringU(struct bytestring bs)
{
  BFILE *ubuf = add_utf8(openb_rd_mem(bs.bs_array, bs.bs_size));
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
  for(i = 0; i < bs.bs_size; i++) {
    nc = mkInt(((uint8_t *)bs.bs_array)[i]);
    *np = mkCons(nc, *np);
    np = &ARG(*np);
  }
  return n;
}

/* XXX This should somehow be merged with other utf8 decoders */
/* Decode first character of a string and optionally return the rest of the string. */
/* Handles regular and modified UTF-8. */
value_t
headutf8(struct bytestring bs, void **ret)
{
  uint8_t *p = bs.bs_array;
  if (bs.bs_size == 0)
    ERR("headUTF8 0");
#if !WANT_UTF8
  if (ret)
    *ret = p + 1;
  return *p;
#else
  int c1 = *p++;
  if ((c1 & 0x80) == 0) {
    if (ret)
      *ret = p;
    return c1;
  }
  if ((c1 & 0xe0) == 0xc0 && bs.bs_size >= 2) {
    int c2 = p[0];
    if ((c2 & 0xc0) == 0x80) {
      int c = ((c1 & 0x1f) << 6) | (c2 & 0x3f);
      if (c == 0 || c >= 0x80) {
        if (ret)
          *ret = p + 1;
        return c;
      }
    }
  } else if ((c1 & 0xf0) == 0xe0 && bs.bs_size >= 3) {
    int c2 = p[0], c3 = p[1];
    if ((c2 & 0xc0) == 0x80 && (c3 & 0xc0) == 0x80) {
      int c = ((c1 & 0x0f) << 12) | ((c2 & 0x3f) << 6) | (c3 & 0x3f);
      if (c >= 0x800 && (c < 0xd800 || c > 0xdfff)) {
        if (ret)
          *ret = p + 2;
        return c;
      }
    }
  } else if ((c1 & 0xf8) == 0xf0 && bs.bs_size >= 4) {
    int c2 = p[0], c3 = p[1], c4 = p[2];
    if ((c2 & 0xc0) == 0x80 && (c3 & 0xc0) == 0x80 && (c4 & 0xc0) == 0x80) {
      int c = ((c1 & 0x07) << 18) | ((c2 & 0x3f) << 12) | ((c3 & 0x3f) << 6) | (c4 & 0x3f);
      if (c >= 0x10000 && c < 0x110000) {
        if (ret)
          *ret = p + 3;
        return c;
      }
    }
  }
  if (ret)
    *ret = p;
  return 0xdc00 | c1;
#endif  /* WANT_UTF8 */
}

/*
 * Helpers for Data.Text, whose Text is a (modified) UTF-8 ByteString.
 * Positions are byte offsets; counts are in characters.  A character
 * starts at every byte that is not a continuation byte (10xxxxxx).
 * Without WANT_UTF8 every byte is a character.
 */
#if WANT_UTF8
#define UTF8CONT(b) (((b) & 0xc0) == 0x80)
#else
#define UTF8CONT(b) ((void)(b), 0)
#endif

/* Number of characters. */
static value_t
utf8length(struct bytestring bs)
{
  const uint8_t *p = bs.bs_array;
  value_t n = 0;
  for (size_t i = 0; i < bs.bs_size; i++)
    n += !UTF8CONT(p[i]);
  return n;
}

/* Byte offset after the first k characters from byte offset i (clamped
 * to the string). */
static value_t
utf8offsetfrom(struct bytestring bs, value_t i0, value_t k)
{
  const uint8_t *p = bs.bs_array;
  size_t n = bs.bs_size;
  size_t i = i0 < 0 ? 0 : (size_t)i0 > n ? n : (size_t)i0;
  for (; k > 0 && i < n; k--) {
    i++;
    while (i < n && UTF8CONT(p[i]))
      i++;
  }
  return i;
}

/* Byte offset where the last k characters before byte offset i start
 * (clamped to the string). */
static value_t
utf8offsetendbefore(struct bytestring bs, value_t i0, value_t k)
{
  const uint8_t *p = bs.bs_array;
  size_t n = bs.bs_size;
  size_t i = i0 < 0 ? 0 : (size_t)i0 > n ? n : (size_t)i0;
  for (; k > 0 && i > 0; k--) {
    i--;
    while (i > 0 && UTF8CONT(p[i]))
      i--;
  }
  return i;
}

/* Byte offset after the first k characters (clamped to the string). */
static value_t
utf8offset(struct bytestring bs, value_t k)
{
  return utf8offsetfrom(bs, 0, k);
}

/* Byte offset where the last k characters start (clamped to the string). */
static value_t
utf8offsetend(struct bytestring bs, value_t k)
{
  return utf8offsetendbefore(bs, bs.bs_size, k);
}

/* Decode the character starting at byte offset i (which must be < size).
 * Returns (code point << 3) | (bytes used). */
static value_t
utf8decode(struct bytestring bs, value_t i)
{
  struct bytestring rest;
  void *out;
  value_t c;
  if (i < 0 || (size_t)i >= bs.bs_size)
    ERR("utf8decode");
  rest.bs_array = (uint8_t *)bs.bs_array + i;
  rest.bs_size = bs.bs_size - i;
  rest.bs_capacity = 0;
  c = headutf8(rest, &out);
  return (c << 3) | ((uint8_t *)out - (uint8_t *)rest.bs_array);
}

/* Decode the character that ends at byte offset e (which must be > 0).
 * Returns (code point << 3) | (bytes used). */
static value_t
utf8decodeend(struct bytestring bs, value_t e)
{
  const uint8_t *p = bs.bs_array;
  value_t s;
  if (e <= 0 || (size_t)e > bs.bs_size)
    ERR("utf8decodeend");
  for (s = e - 1; s > 0 && UTF8CONT(p[s]); s--)
    ;
  return (utf8decode(bs, s) & ~(value_t)7) | (e - s);
}

/* Data.Char.isSpace: the ASCII spaces and the Unicode Space_Separator (Zs)
 * characters. */
static int
utf8isspace(value_t c)
{
  return c == ' ' || (c >= '\t' && c <= '\r') ||
         c == 0xa0 || c == 0x1680 || (c >= 0x2000 && c <= 0x200a) ||
         c == 0x202f || c == 0x205f || c == 0x3000;
}

/* First byte offset >= i whose character is (if want is 0: is not) a space. */
static value_t
utf8skip(struct bytestring bs, value_t i, int want)
{
  const uint8_t *p = bs.bs_array;
  value_t n = bs.bs_size;
  if (i < 0)
    i = 0;
  while (i < n) {
    if (p[i] < 0x80) {          /* ASCII fast path */
      if (utf8isspace(p[i]) == want)
        return i;
      i++;
    } else {
      value_t r = utf8decode(bs, i);
      if (utf8isspace(r >> 3) == want)
        return i;
      i += r & 7;
    }
  }
  return n;
}

/* Byte offset after the string with trailing spaces removed. */
static value_t
utf8trimspaceend(struct bytestring bs)
{
  const uint8_t *p = bs.bs_array;
  value_t e = bs.bs_size;
  while (e > 0) {
    if (p[e-1] < 0x80) {
      if (!utf8isspace(p[e-1]))
        break;
      e--;
    } else {
      value_t r = utf8decodeend(bs, e);
      if (!utf8isspace(r >> 3))
        break;
      e -= r & 7;
    }
  }
  return e;
}

/* The characters in reverse order. */
static struct bytestring
utf8reverse(struct bytestring bs)
{
  struct bytestring r = mk_ro_bytestring(bs.bs_size, NULL);
  const uint8_t *p = bs.bs_array;
  uint8_t *q = (uint8_t *)r.bs_array + bs.bs_size;
  size_t i = 0, n = bs.bs_size;
  while (i < n) {
    size_t s = i++;
    while (i < n && UTF8CONT(p[i]))
      i++;
    q -= i - s;
    memcpy(q, p + s, i - s);
  }
  return r;
}

/* Byte offset of the first occurrence of pat in str at or after i, or -1.
 * Since UTF-8 is self-synchronizing, a match of a valid encoding always
 * starts at a character boundary. */
static value_t
bsfind(struct bytestring pat, struct bytestring str, value_t i)
{
  const uint8_t *p = pat.bs_array, *s = str.bs_array;
  size_t m = pat.bs_size, n = str.bs_size;
  if (i < 0)
    i = 0;
  if ((size_t)i > n || m > n - i)
    return -1;
  if (m == 0)
    return i;
  for (const uint8_t *q = s + i, *end = s + (n - m); q <= end; q++) {
    q = memchr(q, p[0], end - q + 1);
    if (!q)
      return -1;
    if (memcmp(q + 1, p + 1, m - 1) == 0)
      return q - s;
  }
  return -1;
}

/* Byte offset of the last occurrence of pat in str, or -1.
 * An empty pattern occurs at the end. */
static value_t
bsrfind(struct bytestring pat, struct bytestring str)
{
  const uint8_t *p = pat.bs_array, *s = str.bs_array;
  size_t m = pat.bs_size, n = str.bs_size;
  if (m > n)
    return -1;
  if (m == 0)
    return n;
  for (size_t i = n - m + 1; i-- > 0; )
    if (s[i] == p[0] && memcmp(s + i + 1, p + 1, m - 1) == 0)
      return i;
  return -1;
}

/* 1 if every byte is ASCII, else 0. */
static value_t
bsisascii(struct bytestring bs)
{
  const uint8_t *p = bs.bs_array;
  for (size_t i = 0; i < bs.bs_size; i++)
    if (p[i] >= 0x80)
      return 0;
  return 1;
}

/* ASCII lower case (upper == 0) or upper case (upper != 0). */
static struct bytestring
bsasciicase(struct bytestring bs, value_t upper)
{
  struct bytestring r = mk_ro_bytestring(bs.bs_size, NULL);
  const uint8_t *p = bs.bs_array;
  uint8_t *q = r.bs_array;
  for (size_t i = 0; i < bs.bs_size; i++) {
    uint8_t c = p[i];
    if (upper ? (c >= 'a' && c <= 'z') : (c >= 'A' && c <= 'Z'))
      c ^= 0x20;
    q[i] = c;
  }
  return r;
}

/* Evaluate to a Bool */
static INLINE value_t
evalbool(NODEPTR n)
{
  return eq_NODEPTR(evali(n), combTrue);
}

/* Evaluate to an INT */
static INLINE value_t
evalint(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (get_XTAG(n) != D_INT) {
    ERR1("evalint, bad tag %s", TAGNAME(get_XTAG(n)));
  }
#endif
  return GETVALUE(n);
}

/*
 * Experiment: balanceL and balanceR from MicroHs.IdentMap written by hand,
 * to see what compiled code for them would gain.  The type is
 *   data Map a = Nil | One Ident a | Node (Map a) Int Ident a (Map a)
 * The sizes are computed strictly, and the nodes are built directly.
 * They must agree with the Haskell definitions, including when a
 * node becomes a One.
 * The compiler uses them when useBalancePrimitives in MicroHs.IdentMap
 * is True.  See the JIT plan for the results.
 */
#define MAP_NIL  mk_CON(V_CONSTR, 0, 0)
#define MAP_ONE  mk_CON(V_CONSTR, 1, 2)
#define MAP_NODE mk_CON(V_CONSTR, 2, 5)
#define MAP_OMEGA 2
#define MAP_ALPHA 2
#define MAP_NODE_CELLS 6        /* five applications and the size */

/*
 * Evaluating can GC, and the GC short-circuits indirections and frees them
 * (and the first GCs after loading also rewrite the graph).  So no NODEPTR
 * is kept across an evaluation.  A map is named by a slot that points to it,
 * starting from the arguments on the stack, and the pointers are read from
 * the slots after the last GCCHECK.  A slot is a field of an application
 * that is reachable, and cells do not move, so the slot stays valid.
 * NULL names Nil.
 */

/* The field slots of the map that np points to, which is in WHNF.
 * f[0] is the last field.  Returns the number of fields:
 * 0 for Nil, 2 for One, 5 for Node. */
static int
map_fields(NODEPTR *np, NODEPTR **f)
{
  int i = 0;
  if (!np)
    return 0;
  for (NODEPTR p = indir(np); !is_VIRT(p); p = indir(&FUN(p))) {
    if (i >= 5)
      ERR("map_fields");
    f[i++] = &ARG(p);
  }
  if (i != 0 && i != 2 && i != 5)
    ERR("map_fields");
  return i;
}

#define MAP_LEFT  4             /* field numbers of a Node, counted from the end */
#define MAP_SIZE  3
#define MAP_RIGHT 0

/* The slot of the left or right subtree of a map in WHNF.  A One has Nil subtrees. */
static NODEPTR *
map_sub(NODEPTR *np, int which)
{
  NODEPTR *f[5];
  return map_fields(np, f) == 5 ? f[which] : NULL;
}

/* Evaluate a map, and the size field of a Node, and return the size. */
static value_t
map_eval(NODEPTR *np)
{
  NODEPTR *f[5];
  if (!np)
    return 0;
  (void)evali(*np);
  switch (map_fields(np, f)) {
  case 0:  return 0;
  case 2:  return 1;
  default: return evalint(*f[MAP_SIZE]);
  }
}

/* The map in a slot, in WHNF. */
static NODEPTR
map_ptr(NODEPTR *np)
{
  return np ? indir(np) : MAP_NIL;
}

/* The key and the value of a One or a Node. */
static void
map_kv(NODEPTR *np, NODEPTR *kp, NODEPTR *vp)
{
  NODEPTR *f[5];
  if (map_fields(np, f) == 2) {
    *kp = *f[1]; *vp = *f[0];
  } else {
    *kp = *f[2]; *vp = *f[1];
  }
}

/* The node l k v r as an application f a, where sl and sr are the sizes of l and r.
 * This is IdentMap.node, which is the same as node' when l or r is not Nil.
 * Uses MAP_NODE_CELLS-1 cells, and does not GC. */
static void
map_mk(NODEPTR *fp, NODEPTR *ap, NODEPTR l, value_t sl, NODEPTR k, NODEPTR v, NODEPTR r, value_t sr)
{
  if (sl + sr == 0) {
    *fp = new_ap(MAP_ONE, k);
    *ap = v;
  } else {
    *fp = new_ap(new_ap(new_ap(new_ap(MAP_NODE, l), mkInt(sl + sr + 1)), k), v);
    *ap = r;
  }
}

/* As map_mk, but returns a new node.  Uses MAP_NODE_CELLS cells. */
static NODEPTR
map_new(NODEPTR l, value_t sl, NODEPTR k, NODEPTR v, NODEPTR r, value_t sr)
{
  NODEPTR f, a;
  map_mk(&f, &a, l, sl, k, v, r, sr);
  return new_ap(f, a);
}

/* balanceL (tag is T_BALANCEL) or balanceR applied to the four arguments
 * on the top of the stack.  The arguments are popped, and the redex is
 * overwritten with the result, an application, which is returned.
 * It is not inlined, and has a single result, so that evali is not
 * compiled worse (passing &x and &y from evali cost 4% instructions). */
static NOINLINE NODEPTR
map_balance(enum node_tag tag)
{
  NODEPTR f, a, res;
  NODEPTR *fp = &f, *ap = &a;
  NODEPTR *lp = &ARG(TOP(0)), *rp = &ARG(TOP(3));
  NODEPTR k, v, k1, v1, k2, v2;
  NODEPTR *ap1, *bp, *cp, *dp;
  value_t sl, sr, sa, sb, sc, sd;

  sl = map_eval(lp);
  sr = map_eval(rp);
  if (sl + sr <= 1 ||
      (tag == T_BALANCEL ? sl <= MAP_OMEGA * sr : sr <= MAP_OMEGA * sl)) {
    GCCHECK(MAP_NODE_CELLS);
    map_mk(fp, ap, map_ptr(lp), sl, ARG(TOP(1)), ARG(TOP(2)), map_ptr(rp), sr);
  } else if (tag == T_BALANCEL) {
    /* rotateR, l is Node ll _ lk lv lr */
    sa = map_eval(map_sub(lp, MAP_LEFT));       /* ll */
    sb = map_eval(map_sub(lp, MAP_RIGHT));      /* lr */
    if (sb < MAP_ALPHA * sa) {
      /* singleR (Node ll _ lk lv lr) k v r = node' ll lk lv (node lr k v r) */
      GCCHECK(2 * MAP_NODE_CELLS);
      ap1 = map_sub(lp, MAP_LEFT); bp = map_sub(lp, MAP_RIGHT);
      map_kv(lp, &k1, &v1);
      k = ARG(TOP(1)); v = ARG(TOP(2));
      map_mk(fp, ap, map_ptr(ap1), sa, k1, v1,
             map_new(map_ptr(bp), sb, k, v, map_ptr(rp), sr), sb + sr + 1);
    } else {
      /* doubleR (Node ll _ lk lv (Node lrl _ lrk lrv lrr)) k v r =
       *   node' (node ll lk lv lrl) lrk lrv (node lrr k v r)
       * A One for lr is Node Nil _ lrk lrv Nil. */
      if (sb == 0)
        ERR("balanceL");
      sc = map_eval(map_sub(map_sub(lp, MAP_RIGHT), MAP_LEFT));   /* lrl */
      sd = map_eval(map_sub(map_sub(lp, MAP_RIGHT), MAP_RIGHT));  /* lrr */
      GCCHECK(3 * MAP_NODE_CELLS);
      ap1 = map_sub(lp, MAP_LEFT); bp = map_sub(lp, MAP_RIGHT);
      cp = map_sub(bp, MAP_LEFT); dp = map_sub(bp, MAP_RIGHT);
      map_kv(lp, &k1, &v1);
      map_kv(bp, &k2, &v2);
      k = ARG(TOP(1)); v = ARG(TOP(2));
      map_mk(fp, ap, map_new(map_ptr(ap1), sa, k1, v1, map_ptr(cp), sc), sa + sc + 1, k2, v2,
                     map_new(map_ptr(dp), sd, k, v, map_ptr(rp), sr), sd + sr + 1);
    }
  } else {
    /* rotateL, r is Node rl _ rk rv rr */
    sa = map_eval(map_sub(rp, MAP_LEFT));       /* rl */
    sb = map_eval(map_sub(rp, MAP_RIGHT));      /* rr */
    if (sa < MAP_ALPHA * sb) {
      /* singleL l k v (Node rl _ rk rv rr) = node' (node l k v rl) rk rv rr */
      GCCHECK(2 * MAP_NODE_CELLS);
      ap1 = map_sub(rp, MAP_LEFT); bp = map_sub(rp, MAP_RIGHT);
      map_kv(rp, &k1, &v1);
      k = ARG(TOP(1)); v = ARG(TOP(2));
      map_mk(fp, ap, map_new(map_ptr(lp), sl, k, v, map_ptr(ap1), sa), sl + sa + 1, k1, v1,
             map_ptr(bp), sb);
    } else {
      /* doubleL l k v (Node (Node rll _ rlk rlv rlr) _ rk rv rr) =
       *   node' (node l k v rll) rlk rlv (node rlr rk rv rr)
       * A One for rl is Node Nil _ rlk rlv Nil. */
      if (sa == 0)
        ERR("balanceR");
      sc = map_eval(map_sub(map_sub(rp, MAP_LEFT), MAP_LEFT));    /* rll */
      sd = map_eval(map_sub(map_sub(rp, MAP_LEFT), MAP_RIGHT));   /* rlr */
      GCCHECK(3 * MAP_NODE_CELLS);
      ap1 = map_sub(rp, MAP_LEFT); bp = map_sub(rp, MAP_RIGHT);
      cp = map_sub(ap1, MAP_LEFT); dp = map_sub(ap1, MAP_RIGHT);
      map_kv(rp, &k1, &v1);
      map_kv(ap1, &k2, &v2);
      k = ARG(TOP(1)); v = ARG(TOP(2));
      map_mk(fp, ap, map_new(map_ptr(lp), sl, k, v, map_ptr(cp), sc), sl + sc + 1, k2, v2,
                     map_new(map_ptr(dp), sd, k1, v1, map_ptr(bp), sb), sd + sb + 1);
    }
  }
  POP(4);
  res = TOP(-1);
  FUN(res) = f;
  ARG(res) = a;
  return res;
}

/*
 * Experiment: (==) on Ident as a primitive, to see what compiled code for
 * it would gain.  The compiler uses it when useIdentEqPrimitive in
 * MicroHs.Ident is True.
 * An Ident is  Ident SLoc Text,  i.e., C_0_2 applied to the location and
 * the name, and Text is a newtype of ByteString.  So the name is the last
 * field, and it evaluates to a bytestring.
 */
#define IDENT_CON mk_CON(V_CONSTR, 0, 2)

/* The slot of the name of an Ident in WHNF. */
static NODEPTR *
ident_name(NODEPTR *np)
{
  NODEPTR p = indir(np);
#if SANITY
  if (is_VIRT(p) || is_VIRT(indir(&FUN(p))) || indir(&FUN(indir(&FUN(p)))).node_bits != IDENT_CON.node_bits)
    ERR("ident_name");
#endif  /* SANITY */
  return &ARG(p);
}

/* (==) on the two Idents on the top of the stack.  Evaluating can GC, so,
 * as in map_balance, the pointers are read from the slots after the last
 * evaluation. */
static NOINLINE int
ident_eq(void)
{
  NODEPTR *xp = &ARG(TOP(0)), *yp = &ARG(TOP(1));
  NODEPTR *xn, *yn;
  NODEPTR x, y;
  struct bytestring xbs, ybs;

  (void)evali(*xp);
  xn = ident_name(xp);
  (void)evali(*xn);
  (void)evali(*yp);
  yn = ident_name(yp);
  (void)evali(*yn);
  x = indir(xn);
  y = indir(yn);
#if SANITY
  if (get_XTAG(x) != D_FORPTR || FORPTR(x)->finalizer->fptype != FP_BSTR ||
      get_XTAG(y) != D_FORPTR || FORPTR(y)->finalizer->fptype != FP_BSTR)
    ERR("ident_eq");
#endif  /* SANITY */
  if (x.node_bits == y.node_bits)
    return 1;
  xbs = BSTR(x);
  ybs = BSTR(y);
  return xbs.bs_size == ybs.bs_size && memcmp(xbs.bs_array, ybs.bs_array, xbs.bs_size) == 0;
}

#if WANT_INT64
/* Evaluate to an INT */
static INLINE uint64_t
evalint64(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (get_XTAG(n) != D_INT64) {
    ERR1("evalint64, bad tag %s", TAGNAME(get_XTAG(n)));
  }
#endif
  return GETINT64VALUE(n);
}
#endif  /* WANT_INT64 */

#if WANT_FLOAT64
/* Evaluate to a flt64_t */
static INLINE flt64_t
evaldbl(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (get_XTAG(n) != D_DBL) {
    ERR1("evaldbl, bad tag %s", TAGNAME(get_XTAG(n)));
  }
#endif
  return GETDBLVALUE(n);
}
#endif  /* WANT_FLOAT64 */

#if WANT_FLOAT32
/* Evaluate to a flt32_t */
static INLINE flt32_t
evalflt(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (get_XTAG(n) != D_FLT32) {
    ERR1("evaldbl, bad tag %s", TAGNAME(get_XTAG(n)));
  }
#endif
  return GETFLTVALUE(n);
}
#endif  /* WANT_FLOAT32 */

/* Evaluate to a T_PTR */
void *
evalptr(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (get_XTAG(n) != D_PTR) {
    ERR1("evalptr, bad tag %s", TAGNAME(get_XTAG(n)));
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
  if (get_XTAG(n) != D_FUNPTR) {
    ERR1("evalfunptr, bad tag %s", TAGNAME(get_XTAG(n)));
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
  if (get_XTAG(n) != D_FORPTR) {
    ERR1("evalforptr, bad tag %s", TAGNAME(get_XTAG(n)));
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
  if (get_XTAG(n) != D_FORPTR || FORPTR(n)->finalizer->fptype != FP_BSTR) {
    ERR1("evalbstr, bad tag %s", TAGNAME(get_XTAG(n)));
  }
#endif
  return FORPTR(n);
}

/* Evaluate to a T_THID */
struct mthread *
evalthid(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (get_XTAG(n) != D_THID) {
    ERR1("evalthid, bad tag %s", TAGNAME(get_XTAG(n)));
  }
#endif
  return THR(n);
}

/* Evaluate to a T_MVAR */
struct mvar *
evalmvar(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (get_XTAG(n) != D_MVAR) {
    ERR1("evalmvar, bad tag %s", TAGNAME(get_XTAG(n)));
  }
#endif
  return MVAR(n);
}

/* Evaluate to a T_WEAK */
void *
evalweak(NODEPTR n)
{
  n = evali(n);
#if SANITY
  if (get_XTAG(n) != D_WEAK) {
    ERR1("evalweak, bad tag %s", TAGNAME(get_XTAG(n)));
  }
#endif
  return WEAK(n);
}

/* Evaluate a string, returns a newly allocated buffer.
 * XXX this is cheating, should use continuations.
 * XXX the malloc()ed string is leaked if we yield in here.
 * Caller is responsible to free().
 * Does modified UTF-8 encoding.
 * Only used to display an uncaught exception.
 * XXX Should remove from DYNSYM
 */
struct bytestring
evalstring(NODEPTR n)
{
  size_t sz = 100;
  char *buf = mmalloc(sz);
  size_t offs;
  uvalue_t c;
  NODEPTR x;

  for (offs = 0;;) {
    if (offs >= sz - 4) {
      sz *= 2;
      buf = mrealloc(buf, sz);
    }
    PUSH(n);                    /* protect the list from GC */
    n = evali(n);
    POP(1);
    if (eq_NODEPTR(n, combNil))
      break;
    else if (get_XTAG(n) == X_AP && get_XTAG(x = indir(&FUN(n))) == X_AP && eq_NODEPTR(indir(&FUN(x)), combCons)) { /* Cons */
      PUSH(n);                  /* protect from GC */
      c = evalint(ARG(x));
      n = POPTOP();
      if ((c & 0x1ff800) == 0xd800) {
        // c is a surrogate
        c = 0xfffd; // replacement character
      }
      if (0 < c && c < 0x80) {   /* exclude 0, since this is modified UTF-8 */
        buf[offs++] = (char)c;
      } else if (c < 0x800) {
        /* 0 encodes here, with an over-long representation */
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
  return mk_ro_bytestring(offs, buf);
}

/*
 * Concatenate a list of ByteStrings with a single copy.
 * The spine and the elements must already be evaluated (concatBS in
 * Data.ByteString.Internal does that): evaluating here could yield to
 * another thread, which restarts the primitive from the beginning.
 * Without evaluation there is no allocation of nodes, so no GC either.
 */
static NODEPTR
bsconcat_follow(NODEPTR n)
{
  return indir(&n);
}

struct bytestring
bsconcat(NODEPTR list)
{
  struct bytestring r;
  size_t size = 0;
  uint8_t *q = 0;
  int pass;

  for (pass = 0; pass < 2; pass++) {
    NODEPTR n = list, x, e;
    if (pass == 1) {
      r = mk_ro_bytestring(size, NULL);
      q = r.bs_array;
    }
    for (;;) {
      n = bsconcat_follow(n);
      if (eq_NODEPTR(n, combNil))
        break;
      if (!(get_XTAG(n) == X_AP && get_XTAG(x = indir(&FUN(n))) == X_AP && eq_NODEPTR(indir(&FUN(x)), combCons)))
        ERR("bsconcat: list not evaluated");
      e = bsconcat_follow(ARG(x));
      if (get_XTAG(e) != D_FORPTR)
        ERR("bsconcat: element not evaluated");
      struct bytestring bs = FORPTR(e)->payload;
      if (pass == 0) {
        size += bs.bs_size;
      } else {
        memcpy(q, bs.bs_array, bs.bs_size);
        q += bs.bs_size;
      }
      n = ARG(n);
    }
  }
  return r;
}

struct bytestring
bsreplicate(size_t size, uint8_t value)
{
  struct bytestring bs = mk_ro_bytestring(size, NULL);
  memset(bs.bs_array, value, size);
  return bs;
}

struct bytestring
bsappend(struct bytestring p, struct bytestring q)
{
  struct bytestring r = mk_ro_bytestring(p.bs_size + q.bs_size, NULL);
  memcpy(r.bs_array, p.bs_array, p.bs_size);
  memcpy((uint8_t *)r.bs_array + p.bs_size, q.bs_array, q.bs_size);
  return r;
}

struct bytestring
bsappenddot(struct bytestring p, struct bytestring q)
{
  struct bytestring r = mk_ro_bytestring(r.bs_size = p.bs_size + q.bs_size + 1, NULL);
  memcpy(r.bs_array, p.bs_array, p.bs_size);
  memcpy((uint8_t *)r.bs_array + p.bs_size, ".", 1);
  memcpy((uint8_t *)r.bs_array + p.bs_size + 1, q.bs_array, q.bs_size);
  return r;
}

/*
 * Compare bytestrings.
 */
int
bscompare(struct bytestring bsp, struct bytestring bsq)
{
  size_t len = bsp.bs_size < bsq.bs_size ? bsp.bs_size : bsq.bs_size;
  if (len) {
    int r = memcmp(bsp.bs_array, bsq.bs_array, len);
    if (r < 0)
      return -1;
    if (r > 0)
      return 1;
  }
  /* Got to the end of the shorter string. */
  /* The shorter string is considered smaller. */
  if (bsp.bs_size < bsq.bs_size)
    return -1;
  if (bsp.bs_size > bsq.bs_size)
    return 1;
  return 0;
}

void
rnf_rec(bits_t *done, NODEPTR n)
{
 top:
  if (test_bit(done, n))
    return;
  set_bit(done, n);
  n = evali(n);
  if (get_XTAG(n) == X_AP) {
    PUSH(ARG(n));               /* protect from GC */
    rnf_rec(done, FUN(n));
    n = POPTOP();
    goto top;
  }
}

void
rnf(value_t noerr, NODEPTR n)
{
  /* Mark visited nodes to avoid getting stuck in loops. */
  bits_t *done = mcalloc(free_map_nwords, sizeof(bits_t));
  if (doing_rnf)
    ERR("recursive rnf()");
  doing_rnf = (bool)noerr;
  rnf_rec(done, n);
  doing_rnf = false;
  FREE(done);
}

#if 0
counter_t num_arm, num_bad_arm;
#endif

#if WANT_PROFILE
/* Reductions per primitive: every combinator, primop, case and constructor
 * tag, and every foreign function (a D_IO_CCALL node, by its number in
 * FFI_IX).  A reduction where the primitive has too few arguments is a
 * partial application being evaluated; it is counted as partial too (see
 * CHECK in evali), so the uses of a primitive are its reductions minus its
 * partial ones.  See prof_report_prims. */
static counter_t prof_prim_red[T_LAST_TAG+1];
static counter_t prof_prim_partial[T_LAST_TAG+1];
static counter_t *prof_ffi_red = 0, *prof_ffi_partial = 0;
static size_t prof_nffi = 0;

/* The number of foreign functions: the runtime's and the program's. */
static void
prof_ffi_init(void)
{
  size_t k = num_ffi;
  if (xffi_table)
    while (xffi_table[k - num_ffi].ffi_name)
      k++;
  prof_nffi = k;
  prof_ffi_red = calloc(k, sizeof(counter_t));
  prof_ffi_partial = calloc(k, sizeof(counter_t));
  if (!prof_ffi_red || !prof_ffi_partial)
    memerr();
}

static INLINE void
prof_prim(counter_t *tags, counter_t **ffis, enum node_tag tag, NODEPTR n)
{
  tags[tag]++;
  if (tag == D_IO_CCALL) {
    size_t a = (size_t)GETVALUE(n);
    if (!prof_ffi_red)
      prof_ffi_init();
    if (a < prof_nffi)
      (*ffis)[a]++;
  }
}
#define PROF_PARTIAL() prof_prim(prof_prim_partial, &prof_ffi_partial, tag, n)

/* Charge the reduction about to happen at node n (with tag tag) to the code
 * that runs: the source of the innermost application on the spine of this
 * evali (stack entries above stk).  Virtual stack entries (case markers, the
 * BININT markers) are skipped.  A constructor selecting a case arm is charged
 * to the case, which is below the constructor's fields and the marker.
 * Every pass through the top of evali is charged somewhere, so the counts add
 * up to num_reductions.  The source is also what new nodes get.
 * The reduction is also charged to a call context, see prof_push. */
/* Unwinding steps by the kind of head they lead to (see prof_head_kind), and
 * by the node unwound: a cell of the loaded program (code) or one built at
 * run time.  The steps since the last dispatch are credited to the head that
 * the next dispatch finds. */
enum prof_head { PH_COMB, PH_PRIM, PH_CASE, PH_CONSTR, PH_VALUE, PH_FFI, PH_N };
static const char *prof_head_names[PH_N] = { "combinator", "primop", "case", "constructor", "value", "foreign call" };
static counter_t prof_unw_since[2];             /* [loaded] since the last dispatch */
static counter_t prof_unw_head[PH_N][2];

static INLINE enum prof_head
prof_head_kind(enum node_tag tag)
{
  if (tag >= T_S && tag <= T_CCB) return PH_COMB;
  if (tag > T_FIRST_REAL_TAG)     return PH_PRIM;
  if (tag == V_CONSTR)            return PH_CONSTR;
  if (tag >= V_CONSTR)            return PH_CASE;
  if (tag == D_IO_CCALL)          return PH_FFI;
  return PH_VALUE;
}

static INLINE void
prof_charge(NODEPTR n, enum node_tag tag, stackptr_t stk)
{
  {
    enum prof_head h = prof_head_kind(tag);
    prof_unw_head[h][0] += prof_unw_since[0];
    prof_unw_head[h][1] += prof_unw_since[1];
    prof_unw_since[0] = prof_unw_since[1] = 0;
  }
  struct node *src = 0, *p, *caf = 0, *cell = 0;
  uint32_t caller = 0, ctx;
  int found = 0;
  stackptr_t i = stack_ptr;
  if (tag == V_CONSTR) {
    int k, nn;
    get_CON_info(n, &k, &nn);
    if (stack_ptr - stk > nn + 1 && is_VIRT(TOP(nn)))
      i = stack_ptr - nn - 1;   /* TOP(nn+1), the case */
  }
  for (; i > stk; i--) {
    if (!is_VIRT(stack[i])) {
      cell = NPTR(stack[i]);
      src = cell->prof_src;
      break;
    }
  }
  /* The caller: the innermost application built at run time, in the
   * expression being evaluated, which ends at a marker (a strict argument or a
   * case scrutinee) or the bottom of this evali.  If there is none, the
   * expression is part of the loaded program, i.e., a CAF, and its caller is
   * the definition it is in, called from the top level. */
  for (; i > stk && !is_VIRT(stack[i]); i--) {
    p = NPTR(stack[i]);
    if (!PROF_LOADED(p)) {
      caller = p->prof_ctx;
      found = 1;
      break;
    }
    caf = p;
  }
  if (!src) {
    if (is_VIRT(n)) {
      src = &prof_runtime;
    } else {
      p = NPTR(n);
      cell = p;
      src = p->prof_src;
      if (!PROF_LOADED(p)) {
        caller = p->prof_ctx;
        found = 1;
      } else {
        caf = p;
      }
    }
  }
  if (!found && caf)
    caller = prof_push(0, caf->prof_label);
  prof_prim(prof_prim_red, &prof_ffi_red, tag, n);
  src->prof_red++;
  if (cell)
    prof_tick(cell, &cell->prof_nred);
  prof_cur = src;
  ctx = prof_ctxs[caller].label == src->prof_label ? caller : prof_push(caller, src->prof_label);
  prof_ctxs[ctx].red++;
  prof_ctx_cur = ctx;
}
#define PROF_UNWIND(n) do { struct node *_p = NPTR(n); _p->prof_src->prof_unwind++; prof_tick(_p, &_p->prof_ncalls); \
                             prof_unw_since[PROF_LOADED(_p)]++; } while(0)
#define PROF_OVERWRITE(n) do { NPTR(n)->prof_ctx = prof_ctx_cur; NPTR(n)->prof_nupd++; } while(0)
#else
#define PROF_UNWIND(n) do { } while(0)
#define PROF_OVERWRITE(n) do { } while(0)
#endif  /* WANT_PROFILE */

#if !defined(EXTRASANITY)
#define EXTRASANITY 0           /* sanity checks that cost time, unlike SANITY */
#endif

#if EXTRASANITY || WANT_SAMPLE  /* for the sanity checks, and the extractor */
/* The number of arguments the virtual node h (the head of a spine: a
 * combinator, primitive, case, or constructor) needs before it reduces, or -1
 * if it never reduces (a constructor).  For the combinators that also reduce
 * with fewer arguments (e.g., B' x y = B (x y)) it is the smaller number. */
static INLINE int
arity_virt(NODEPTR h)
{
  enum node_tag t = get_VIRT_TAG(h);
  switch (t) {
  case V_CONSTR:
  case V_ARM:
  case V_LOOKD:
  case V_LOOKS:
    return -1;
  case V_CASED:
    return get_CASED_arity(h)->m + 1;
  case V_CASES:
    return 2 * get_CASE_size(h) + 2;
  default:
    return tag_arity[t];
  }
}
#endif  /* EXTRASANITY || WANT_SAMPLE */

#if EXTRASANITY
/* Is n in weak head normal form, i.e., would evaluating it do no reduction?
 * It is if it is a value (an Int, a ByteString, ...), a constructor applied to
 * at most its fields, or a partial application.  So count the arguments down
 * the spine, and compare with the arity of the head. */
static INLINE bool
is_whnf(NODEPTR n)
{
  int d = 0;                    /* arguments so far */
  for (;;) {
    if (is_VIRT(n)) {
      int a = arity_virt(n);
      return a < 0 || d < a;
    }
    NODEPTR f = FUN(n);
    if (is_AP(f)) {             /* an application */
      d++;
      n = f;
      continue;
    }
    switch (get_DATA_TAG(f)) {
    case D_IND:      n = ARG(n); continue;
    case D_IO_CCALL: return d < FFI_IX(GETVALUE(n)).ffi_arity;
    case D_TICK:     return d < 1;
    case D_BADDYN:   return false; /* evaluating it is an error */
    default:         return true;  /* a value */
    }
  }
}

/* A case checks for as many arguments as the arity in primops[]; the
 * combinators that also reduce with fewer arguments check for more in their
 * other branch.  The arity of the other tags is in the node, see arity_virt. */
static void
check_arity(enum node_tag tag, int k)
{
  if (tag <= T_FIRST_REAL_TAG || tag >= T_LAST_TAG || k == tag_arity[tag])
    return;
  switch (tag) {
  case T_BB: case T_Z: case T_R: case T_K2: case T_K3: case T_K4: case T_CCB:
    if (k > tag_arity[tag])
      return;
    break;
  default:
    break;
  }
#if WANT_STDIO
  fprintf(stderr, "arity of %s: CHECK(%d), but primops[] says %d\n", TAGNAME(tag), k, (int)tag_arity[tag]);
#endif
  ERR("check_arity");
}
#endif  /* EXTRASANITY */

#if WANT_SAMPLE
/*
 * Extracting the subgraph of a hot function node, as the input of a JIT
 * (see jit-plan.md).  For now the subgraphs of the hottest nodes are only
 * written to a file (+RTS -S -J[N] -RTS).
 *
 * The graph is traversed depth first from the root, and the application
 * nodes that are code, i.e., partial applications, are copied into an array.
 * The traversal stops at
 *  - data: a value, or a constructor with its fields;
 *  - a redex, i.e., an application with as many arguments as its head needs;
 *    it can be shared, so the copy refers to it instead;
 *  - a node that starts a cycle, other than the root.  It can be compiled
 *    on its own.  That a node starts a cycle is only known when the traversal
 *    comes back to it, so it is copied first; when the traversal is done with
 *    it, its copy and everything after it in the array is dropped, and it
 *    becomes a reference;
 *  - a node that would make the copy larger than JX_EXPLORE times the size
 *    limit.
 * The array is also the table of visited nodes, so sharing is kept.  A cycle
 * back to the root is a reference to the root's copy, entry 0.
 *
 * Finding the cycles needs a depth first traversal, but cutting the copy at
 * the size limit in depth first order would spend it on whatever is first,
 * e.g., the rotations of a balance function, and cut the tests near the root.
 * So the traversal explores more, and then jx_select keeps the copied nodes
 * that are nearest the root, breadth first, up to the size limit.
 */
#define JX_EXPLORE 16
enum jx_kind { JX_COPY, JX_DATA, JX_REDEX, JX_CYCLE, JX_CUT };
/* Why the copy stopped at a reference: a value or constructor (whnf), a
 * redex, the start of a cycle, or the size limit (depth, since the nodes
 * furthest from the root are cut). */
static const char *jx_why[] = { "copy", "whnf", "redex", "cycle", "depth" };

/* An operand of a copied application: an index in the array, or a leaf. */
struct jx_opnd {
  int     index;                /* -1 for a leaf */
  NODEPTR leaf;                 /* a combinator, primitive, constructor, FFI function, ... */
};

struct jx_entry {
  NODEPTR        orig;          /* the node it is a copy of, or refers to */
  enum jx_kind   kind;
  bool           active;        /* being copied, i.e., on the traversal stack */
  bool           starts_cycle;  /* the traversal came back to it while it was active */
  struct jx_opnd fun, arg;      /* for JX_COPY */
  int            uses;          /* references from other entries, for printing */
};

#define JX_LIMIT 500                    /* the default size limit */
int jx_limit = 0;                       /* the size limit (+RTS -J); 0 means no extraction */
int jx_threshold = 100;                 /* samples as the outermost node before a node is hot */
static struct jx_entry *jx;             /* the extracted subgraph; jx[0] is the root */
static int jx_n;                        /* the number of entries */
static int jx_copies;                   /* the number of JX_COPY entries */

/* What the application node p is: code to copy, data, or a redex. */
static enum jx_kind
jx_classify(NODEPTR p)
{
  int d = 0;                    /* arguments so far */
  NODEPTR n = p;
  for (;;) {
    if (is_VIRT(n)) {
      int a = arity_virt(n);
      return a < 0 ? JX_DATA : d < a ? JX_COPY : JX_REDEX;
    }
    NODEPTR f = FUN(n);
    if (is_AP(f)) {
      d++;
      n = f;
      continue;
    }
    switch (get_DATA_TAG(f)) {
    case D_IND:      n = ARG(n); continue;
    case D_IO_CCALL: return d < FFI_IX(GETVALUE(n)).ffi_arity ? JX_COPY : JX_REDEX;
    case D_TICK:     return d < 1 ? JX_COPY : JX_REDEX;
    default:         return d == 0 ? JX_DATA : JX_REDEX; /* a value, or an error */
    }
  }
}

static struct jx_opnd
jx_visit(NODEPTR p)
{
  struct jx_opnd o = { -1, NIL_NODEPTR };
  p = sample_skip_ind(p);
  if (is_VIRT(p)) {
    o.leaf = p;
    return o;
  }
  for (int i = 0; i < jx_n; i++) {
    if (jx[i].orig.node_bits == p.node_bits) {
      if (jx[i].active && i != 0)
        jx[i].starts_cycle = true;
      o.index = i;
      return o;
    }
  }
  enum jx_kind k;
  NODEPTR f = FUN(p);
  if (is_AP(f)) {
    k = jx_classify(p);
  } else {
    switch (get_DATA_TAG(f)) {
    case D_IO_CCALL:
    case D_TICK:
      o.leaf = p;               /* a function, which never changes */
      return o;
    case D_BADDYN:
      k = JX_REDEX;             /* evaluating it is an error */
      break;
    default:
      k = JX_DATA;
      break;
    }
  }
  if (k == JX_COPY && jx_copies >= JX_EXPLORE * jx_limit)
    k = JX_CUT;
  int i = jx_n++;
  jx[i] = (struct jx_entry){ .orig = p, .kind = k };
  o.index = i;
  if (k != JX_COPY)
    return o;
  jx_copies++;
  jx[i].active = true;
  struct jx_opnd fo = jx_visit(FUN(p));
  struct jx_opnd ao = jx_visit(ARG(p));
  jx[i].fun = fo;
  jx[i].arg = ao;
  jx[i].active = false;
  if (jx[i].starts_cycle) {
    for (int j = i; j < jx_n; j++)
      if (jx[j].kind == JX_COPY)
        jx_copies--;
    jx_n = i + 1;
    jx[i] = (struct jx_entry){ .orig = p, .kind = JX_CYCLE };
  }
  return o;
}

/* Keep the jx_limit copied nodes that are nearest the root, breadth first.
 * A copied node that is used by a kept one, but is not kept itself, becomes
 * a cut reference, and what is only reachable through it is dropped.  The
 * kept entries stay in the same order, so the root stays entry 0. */
static void
jx_select(void)
{
  int *queue = mmalloc(jx_n * sizeof(int));
  int *newi = mmalloc(jx_n * sizeof(int));   /* new index, -1 if dropped */
  bool *keep = mcalloc(jx_n, sizeof(bool));  /* a copied node that is kept */
  for (int i = 0; i < jx_n; i++)
    newi[i] = -1;
  int qh = 0, qt = 0, kept = 1;
  queue[qt++] = 0;
  keep[0] = true;
  newi[0] = 0;                  /* used */
  while (qh < qt) {
    int i = queue[qh++];
    struct jx_opnd os[2] = { jx[i].fun, jx[i].arg };
    for (int k = 0; k < 2; k++) {
      int j = os[k].index;
      if (j < 0 || newi[j] >= 0)
        continue;
      newi[j] = 0;              /* used */
      if (jx[j].kind == JX_COPY && kept < jx_limit) {
        keep[j] = true;
        kept++;
        queue[qt++] = j;
      }
    }
  }
  /* Compact: the new index is never larger than the old one. */
  int n = 0;
  for (int i = 0; i < jx_n; i++)
    if (newi[i] >= 0)
      newi[i] = n++;
  for (int i = 0; i < jx_n; i++) {
    if (newi[i] < 0)
      continue;
    struct jx_entry e = jx[i];
    if (e.kind == JX_COPY) {
      if (keep[i]) {
        if (e.fun.index >= 0) e.fun.index = newi[e.fun.index];
        if (e.arg.index >= 0) e.arg.index = newi[e.arg.index];
      } else {
        e.kind = JX_CUT;
      }
    }
    jx[newi[i]] = e;
  }
  jx_n = n;
  jx_copies = kept;
  FREE(queue);
  FREE(newi);
  FREE(keep);
}

/* Print a node that is not an application. */
static void
jx_print_leaf(FILE *f, NODEPTR n)
{
  if (is_VIRT(n)) {
    enum node_tag t = get_VIRT_TAG(n);
    int k, nn;
    switch (t) {
    case V_CASED: {
      struct case_arity *c = get_CASED_arity(n);
      fputc('D', f);
      for (uint32_t j = 0; j < c->m; j++)
        fprintf(f, "_%d", (int)c->arities[j]);
      return;
    }
    case V_CASES: fprintf(f, "E_%d", (int)get_CASE_size(n)); return;
    case V_LOOKD: fprintf(f, "L_%d", (int)get_CASE_size(n)); return;
    case V_LOOKS: fprintf(f, "M_%d", (int)get_CASE_size(n)); return;
    case V_CONSTR:
    case V_ARM:
      get_CON_info(n, &k, &nn);
      fprintf(f, "%s_%d_%d", t == V_CONSTR ? "C" : "A", k, nn);
      return;
    default:
      fprintf(f, "%s", t <= T_LAST_TAG && TAGNAME(t) ? TAGNAME(t) : "?");
      return;
    }
  }
  switch (get_DATA_TAG(FUN(n))) {
  case D_INT:      fprintf(f, "#%"PRIvalue, GETVALUE(n)); return;
#if WANT_INT64
  case D_INT64:    fprintf(f, "##%lld", (long long)GETINT64VALUE(n)); return;
#endif
#if WANT_FLOAT64
  case D_DBL:      fprintf(f, "&%g", (double)GETDBLVALUE(n)); return;
#endif
#if WANT_FLOAT32
  case D_FLT32:    fprintf(f, "&&%g", (double)GETFLTVALUE(n)); return;
#endif
  case D_IO_CCALL: fprintf(f, "^%s", FFI_IX(GETVALUE(n)).ffi_name); return;
  case D_BADDYN:   fprintf(f, "^%s", CSTR(n)); return;
  case D_TICK:     fprintf(f, "!tick%"PRIvalue, GETVALUE(n)); return;
  case D_FORPTR:
#if WANT_GMP || WANT_IMATH
    if (FORPTR(n)->finalizer->fptype == FP_MPZ) {
      mpz_ptr op = FORPTR(n)->payload.bs_array;
      char *str = mmalloc(mpz_sizeinbase(op, 10) + 2);
      (void)mpz_get_str(str, 10, op);
      fprintf(f, "%%%s", str);
      FREE(str);
      return;
    }
#endif  /* WANT_GMP || WANT_IMATH */
    if (FORPTR(n)->finalizer->fptype == FP_BSTR) {
      struct bytestring bs = FORPTR(n)->payload;
      int len = bs.bs_size > 40 ? 40 : (int)bs.bs_size;
      fputc('"', f);
      for (int j = 0; j < len; j++) {
        int c = ((unsigned char *)bs.bs_array)[j];
        if (c == '"' || c == '\\')
          fprintf(f, "\\%c", c);
        else if (c < ' ' || c > '~')
          fprintf(f, "\\%d", c);
        else
          fputc(c, f);
      }
      fputs(bs.bs_size > 40 ? "\"..." : "\"", f);
      return;
    }
    fprintf(f, "<FORPTR %p>", (void *)get_node_ptr(n));
    return;
  default:
    fprintf(f, "<D%d %p>", (int)get_DATA_TAG(FUN(n)), (void *)get_node_ptr(n));
    return;
  }
}

/* Print a node of the original graph, to the given depth. */
static void
jx_print_orig(FILE *f, NODEPTR n, int depth, bool paren)
{
  n = sample_skip_ind(n);
  if (is_VIRT(n) || !is_AP(FUN(n))) {
    jx_print_leaf(f, n);
    return;
  }
  if (depth == 0) {
    fputs("...", f);
    return;
  }
  NODEPTR args[16];
  int na = 0;
  while (!is_VIRT(n) && is_AP(FUN(n)) && na < 16) {
    args[na++] = ARG(n);
    n = sample_skip_ind(FUN(n));
  }
  if (paren)
    fputc('(', f);
  jx_print_orig(f, n, depth - 1, true);
  for (int j = na - 1; j >= 0; j--) {
    fputc(' ', f);
    jx_print_orig(f, args[j], depth - 1, true);
  }
  if (paren)
    fputc(')', f);
}

/* Is entry i a value that is printed where it is used, e.g., #42? */
static bool
jx_inline_data(int i)
{
  return jx[i].kind == JX_DATA && !is_AP(FUN(jx[i].orig));
}

/*
 * Decompiling an extracted subgraph to a lambda expression, by reducing the
 * combinators symbolically.  The rules are those of Mhs.Reflect: a
 * combinator with too few arguments is eta expanded, Y becomes a recursive
 * let, and the case combinators become case expressions.  The terms are
 * never updated, and each term is decompiled once, so sharing is kept; a
 * term that is used more than once is printed as s1@(...) the first time,
 * and as s1 after that.
 */
enum dc_kind { DC_APP, DC_LAM, DC_VAR, DC_LEAF, DC_REF, DC_CASE, DC_LET };

struct dc_node {
  enum dc_kind kind;
  int a, b, c, d;               /* APP: fun, arg.  LAM: var, body.  VAR: var.  REF: jx index.
                                 * LET: var, rhs, body.  CASE: scrutinee, first arm, arms, default or -1 */
  NODEPTR leaf;                 /* LEAF */
  int dec;                      /* the decompiled term, or -1 */
  int simp;                     /* the simplified term, or -1 */
  int sgen, sres;               /* memo of dc_subst: the result, if sgen is dc_sgen */
  int inl;                      /* printed on one line: 1 yes, -1 no, 0 not known */
  int uses;                     /* uses in the decompiled term */
  int label;                    /* the name of a shared term, sN */
};
struct dc_arm { int k, n, var, body; }; /* C_k_n x_var ... x_{var+n-1} -> body */

#define DC_MAX_NODES (1 << 20)
#define DC_MAX_ARMS  (1 << 14)
#define DC_MAX_ARGS  64
#define DC_FUEL      100000     /* combinator reductions */
#define DC_MAX_PRINT 20000      /* nodes printed */

static struct dc_node *dc;
static int dc_n;
static struct dc_arm *dc_arms;
static int dc_narms;
static int dc_nvars;
static long dc_fuel;
static long dc_printed;
static jmp_buf dc_fail;

static int
dc_new(enum dc_kind k, int a, int b, int c, int d, NODEPTR leaf)
{
  if (dc_n >= DC_MAX_NODES)
    longjmp(dc_fail, 1);
  dc[dc_n] = (struct dc_node){ k, a, b, c, d, leaf, -1, -1, 0, 0, 0, 0, 0 };
  return dc_n++;
}
static int dc_app(int f, int a) { return dc_new(DC_APP, f, a, 0, 0, NIL_NODEPTR); }
static int dc_var(int v)        { return dc_new(DC_VAR, v, 0, 0, 0, NIL_NODEPTR); }

/* The term for an operand of the extracted graph. */
static int
dc_from_opnd(struct jx_opnd o, int *memo)
{
  if (o.index < 0)
    return dc_new(DC_LEAF, 0, 0, 0, 0, o.leaf);
  int i = o.index;
  if (memo[i] < 0) {
    if (i == 0)
      memo[i] = dc_new(DC_REF, 0, 0, 0, 0, NIL_NODEPTR); /* a recursive call */
    else if (jx[i].kind == JX_COPY)
      memo[i] = dc_app(dc_from_opnd(jx[i].fun, memo), dc_from_opnd(jx[i].arg, memo));
    else if (jx_inline_data(i))
      memo[i] = dc_new(DC_LEAF, 0, 0, 0, 0, jx[i].orig);
    else
      memo[i] = dc_new(DC_REF, i, 0, 0, 0, NIL_NODEPTR);
  }
  return memo[i];
}

/* The number of arguments of a combinator, 0 if it is not one. */
static int
dc_comb_arity(enum node_tag t)
{
  switch (t) {
  case T_I:                                             return 1;
  case T_K: case T_A: case T_U:                         return 2;
  case T_S: case T_B: case T_C: case T_Z: case T_K2:
  case T_P: case T_R:                                   return 3;
  case T_SS: case T_BB: case T_CC: case T_CCB: case T_K3:
  case T_O:                                             return 4;
  case T_K4:                                            return 5;
  default:                                              return 0;
  }
}

static int
dc_rule(enum node_tag t, int *x)
{
#define AP dc_app
  switch (t) {
  case T_S:   return AP(AP(x[0], x[2]), AP(x[1], x[2]));    /* S x y z = x z (y z) */
  case T_K:   return x[0];                                  /* K x y = x */
  case T_I:   return x[0];                                  /* I x = x */
  case T_B:   return AP(x[0], AP(x[1], x[2]));              /* B x y z = x (y z) */
  case T_C:   return AP(AP(x[0], x[2]), x[1]);              /* C x y z = x z y */
  case T_SS:  return AP(AP(x[0], AP(x[1], x[3])), AP(x[2], x[3])); /* S' x y z w = x (y w) (z w) */
  case T_BB:  return AP(AP(x[0], x[1]), AP(x[2], x[3]));    /* B' x y z w = x y (z w) */
  case T_CC:  return AP(AP(x[0], AP(x[1], x[3])), x[2]);    /* C' x y z w = x (y w) z */
  case T_CCB: return AP(AP(x[0], x[2]), AP(x[1], x[3]));    /* C'B x y z w = x z (y w) */
  case T_A:   return x[1];                                  /* A x y = y */
  case T_U:   return AP(x[1], x[0]);                        /* U x y = y x */
  case T_Z:   return AP(x[0], x[1]);                        /* Z x y z = x y */
  case T_K2:                                                /* K2 x y z = x */
  case T_K3:                                                /* K3 x y z w = x */
  case T_K4:  return x[0];                                  /* K4 x y z w v = x */
  case T_P:   return AP(AP(x[2], x[0]), x[1]);              /* P x y z = z x y */
  case T_R:   return AP(AP(x[1], x[2]), x[0]);              /* R x y z = y z x */
  case T_O:   return AP(AP(x[3], x[0]), x[1]);              /* O x y z w = w x y */
  default:    ERR("dc_rule");
  }
#undef AP
}

/* The head of the spine of e, with the arguments, first one first. */
static int
dc_spine(int e, int *args, int *na)
{
  int tmp[DC_MAX_ARGS];
  int n = 0;
  while (dc[e].kind == DC_APP) {
    if (n >= DC_MAX_ARGS)
      longjmp(dc_fail, 2);
    tmp[n++] = dc[e].b;
    e = dc[e].a;
  }
  for (int i = 0; i < n; i++)
    args[i] = tmp[n - 1 - i];
  *na = n;
  return e;
}

static int
dc_apps(int f, int *args, int n)
{
  for (int i = 0; i < n; i++)
    f = dc_app(f, args[i]);
  return f;
}

static int dc_go(int e);

/* Apply f to the decompiled arguments. */
static int
dc_args(int f, int *args, int n)
{
  for (int i = 0; i < n; i++)
    f = dc_app(f, dc_go(args[i]));
  return f;
}

/* Too few arguments, so eta expand with k new variables. */
static int
dc_eta(int e, int k)
{
  int v0 = dc_nvars;
  dc_nvars += k;
  for (int i = 0; i < k; i++)
    e = dc_app(e, dc_var(v0 + i));
  int b = dc_go(e);
  for (int i = k - 1; i >= 0; i--)
    b = dc_new(DC_LAM, v0 + i, b, 0, 0, NIL_NODEPTR);
  return b;
}

/* An arm for constructor k with n fields. */
static void
dc_arm(int ai, int k, int n, int f)
{
  int v0 = dc_nvars;
  dc_nvars += n;
  for (int j = 0; j < n; j++)
    f = dc_app(f, dc_var(v0 + j));
  int b = dc_go(f);
  dc_arms[ai] = (struct dc_arm){ k, n, v0, b };
}

static int
dc_new_arms(int m)
{
  if (dc_narms + m > DC_MAX_ARMS)
    longjmp(dc_fail, 3);
  int a0 = dc_narms;
  dc_narms += m;
  return a0;
}

/* D_a0_..._a{m-1} f0 ... f{m-1} e */
static int
dc_dense(NODEPTR lf, int *args)
{
  struct case_arity *ca = get_CASED_arity(lf);
  int m = (int)ca->m;
  int a0 = dc_new_arms(m);
  for (int k = 0; k < m; k++)
    dc_arm(a0 + k, k, (int)ca->arities[k], args[k]);
  return dc_new(DC_CASE, dc_go(args[m]), a0, m, -1, NIL_NODEPTR);
}

/* E_m d A_k1_n1 f1 ... A_km_nm fm e
 * The tags can be expressions, as in LOOKS.  If a tag is not known, e.g., when
 * it comes from an argument after eta expansion, return -1. */
static int
dc_sparse(NODEPTR lf, int *args, int n)
{
  int m = (int)get_CASE_size(lf);
  int tags[m];
  for (int j = 0; j < m; j++) {
    int t = tags[j] = dc_go(args[1 + 2 * j]);
    if (dc[t].kind != DC_LEAF || !is_VIRT(dc[t].leaf) || get_VIRT_TAG(dc[t].leaf) != V_ARM)
      return -1;
  }
  int a0 = dc_new_arms(m);
  for (int j = 0; j < m; j++) {
    int k, nf;
    get_CON_info(dc[tags[j]].leaf, &k, &nf);
    dc_arm(a0 + j, k, nf, args[2 + 2 * j]);
  }
  int d = dc_go(args[0]);
  return dc_new(DC_CASE, dc_go(args[n - 1]), a0, m, d, NIL_NODEPTR);
}

static int dc_go1(int e);

/* Decompile e, once. */
static int
dc_go(int e)
{
  if (dc[e].dec < 0)
    dc[e].dec = dc_go1(e);
  return dc[e].dec;
}

static int
dc_go1(int e)
{
  for (;;) {
    int args[DC_MAX_ARGS], na;
    int h = dc_spine(e, args, &na);
    if (dc[h].kind == DC_LEAF && is_VIRT(dc[h].leaf)) {
      NODEPTR lf = dc[h].leaf;
      enum node_tag t = get_VIRT_TAG(lf);
      int n = dc_comb_arity(t);
      if (n > 0) {
        if (--dc_fuel <= 0)
          longjmp(dc_fail, 5);
        if (na < n)
          return dc_eta(e, n - na);
        e = dc_apps(dc_rule(t, args), args + n, na - n);
        continue;
      }
      if (t == T_Y && na >= 1) {
        /* Y f = let r = f r in r */
        int v = dc_nvars++;
        int b = dc_go(dc_app(args[0], dc_var(v)));
        return dc_args(dc_new(DC_LET, v, b, dc_var(v), 0, NIL_NODEPTR), args + 1, na - 1);
      }
      if (t == V_CASED || t == V_CASES) {
        n = t == V_CASED ? (int)get_CASED_arity(lf)->m + 1 : 2 * (int)get_CASE_size(lf) + 2;
        if (na < n)
          return dc_eta(e, n - na);
        int c = t == V_CASED ? dc_dense(lf, args) : dc_sparse(lf, args, n);
        if (c < 0)
          return dc_args(h, args, na);      /* unknown tags, so an application of E_m */
        return dc_args(c, args + n, na - n);
      }
    } else if (dc[h].kind == DC_LEAF && get_DATA_TAG(FUN(dc[h].leaf)) == D_TICK && na >= 1) {
      e = dc_apps(args[0], args + 1, na - 1);
      continue;
    }
    return dc_args(h, args, na);
  }
}

/*
 * Simplifying a decompiled term.  This is the place for the transformations
 * of the decompiled code.  So far:
 *  - a case on a known constructor becomes its arm, with the fields
 *    substituted for the arm's variables, or the default.
 * Still to come is cheap strictness, i.e., strictness that can be found
 * locally, so that arithmetic can be done strictly.
 */

/* Rebuild the case e with fn applied to its parts; e itself if none changed. */
static int
dc_map_case(int e, int (*fn)(int))
{
  int m = dc[e].c, a0 = dc[e].b;
  int s = fn(dc[e].a);
  int d = dc[e].d >= 0 ? fn(dc[e].d) : -1;
  int bodies[m > 0 ? m : 1];
  bool changed = s != dc[e].a || d != dc[e].d;
  for (int j = 0; j < m; j++) {
    bodies[j] = fn(dc_arms[a0 + j].body);
    changed = changed || bodies[j] != dc_arms[a0 + j].body;
  }
  if (!changed)
    return e;
  int na0 = dc_new_arms(m);
  for (int j = 0; j < m; j++) {
    dc_arms[na0 + j] = dc_arms[a0 + j];
    dc_arms[na0 + j].body = bodies[j];
  }
  return dc_new(DC_CASE, s, na0, m, d, NIL_NODEPTR);
}

/* Substitute dc_sterms[i] for the variable dc_svar+i, i < dc_sn.  The
 * variables are all different, so nothing can be captured. */
static int dc_sgen;
static int dc_svar, dc_sn;
static int *dc_sterms;

static int
dc_subst(int e)
{
  if (dc[e].sgen == dc_sgen)
    return dc[e].sres;
  int r = e;
  switch (dc[e].kind) {
  case DC_VAR:
    if (dc[e].a >= dc_svar && dc[e].a < dc_svar + dc_sn)
      r = dc_sterms[dc[e].a - dc_svar];
    break;
  case DC_APP: {
    int a = dc_subst(dc[e].a), b = dc_subst(dc[e].b);
    if (a != dc[e].a || b != dc[e].b)
      r = dc_app(a, b);
    break;
  }
  case DC_LAM: {
    int b = dc_subst(dc[e].b);
    if (b != dc[e].b)
      r = dc_new(DC_LAM, dc[e].a, b, 0, 0, NIL_NODEPTR);
    break;
  }
  case DC_LET: {
    int b = dc_subst(dc[e].b), c = dc_subst(dc[e].c);
    if (b != dc[e].b || c != dc[e].c)
      r = dc_new(DC_LET, dc[e].a, b, c, 0, NIL_NODEPTR);
    break;
  }
  case DC_CASE:
    r = dc_map_case(e, dc_subst);
    break;
  default:
    break;
  }
  dc[e].sgen = dc_sgen;
  dc[e].sres = r;
  return r;
}

static int
dc_subst_vars(int e, int var, int n, int *terms)
{
  dc_sgen++;
  dc_svar = var;
  dc_sn = n;
  dc_sterms = terms;
  return dc_subst(e);
}

static int
dc_simplify(int e)
{
  if (dc[e].simp >= 0)
    return dc[e].simp;
  int r = e;
  switch (dc[e].kind) {
  case DC_APP: {
    int a = dc_simplify(dc[e].a), b = dc_simplify(dc[e].b);
    if (a != dc[e].a || b != dc[e].b)
      r = dc_app(a, b);
    break;
  }
  case DC_LAM: {
    int b = dc_simplify(dc[e].b);
    if (b != dc[e].b)
      r = dc_new(DC_LAM, dc[e].a, b, 0, 0, NIL_NODEPTR);
    break;
  }
  case DC_LET: {
    int b = dc_simplify(dc[e].b), c = dc_simplify(dc[e].c);
    if (b != dc[e].b || c != dc[e].c)
      r = dc_new(DC_LET, dc[e].a, b, c, 0, NIL_NODEPTR);
    break;
  }
  case DC_CASE: {
    /* A case on a known constructor? */
    int args[DC_MAX_ARGS], na;
    int h = dc_spine(dc_simplify(dc[e].a), args, &na);
    if (dc[h].kind == DC_LEAF && is_VIRT(dc[h].leaf) && get_VIRT_TAG(dc[h].leaf) == V_CONSTR) {
      int k, n;
      get_CON_info(dc[h].leaf, &k, &n);
      if (na == n) {
        int j;
        for (j = 0; j < dc[e].c; j++) {
          struct dc_arm *a = &dc_arms[dc[e].b + j];
          if (a->k == k) {
            r = dc_simplify(dc_subst_vars(a->body, a->var, a->n, args));
            break;
          }
        }
        if (j < dc[e].c)
          break;
        if (dc[e].d >= 0) {
          r = dc_simplify(dc[e].d);
          break;
        }
      }
    }
    r = dc_map_case(e, dc_simplify);
    break;
  }
  default:
    break;
  }
  dc[e].simp = r;
  return r;
}

static bool
dc_occurs(int v, int e)
{
  switch (dc[e].kind) {
  case DC_VAR: return dc[e].a == v;
  case DC_APP: return dc_occurs(v, dc[e].a) || dc_occurs(v, dc[e].b);
  case DC_LAM: return dc_occurs(v, dc[e].b);
  case DC_LET: return dc_occurs(v, dc[e].b) || dc_occurs(v, dc[e].c);
  case DC_CASE:
    if (dc_occurs(v, dc[e].a) || (dc[e].d >= 0 && dc_occurs(v, dc[e].d)))
      return true;
    for (int j = 0; j < dc[e].c; j++)
      if (dc_occurs(v, dc_arms[dc[e].b + j].body))
        return true;
    return false;
  default: return false;
  }
}

/* Print a leaf; an operator in parentheses, a negative number in parentheses
 * if it is an argument. */
static void
dc_print_leaf(FILE *f, NODEPTR n, int p)
{
  if (is_VIRT(n)) {
    enum node_tag t = get_VIRT_TAG(n);
    if (t <= T_LAST_TAG && TAGNAME(t) && t != V_CONSTR && t != V_ARM && t != V_TAG &&
        t != V_CASED && t != V_CASES && t != V_LOOKD && t != V_LOOKS) {
      const char *s = TAGNAME(t);
      bool op = false;
      for (const char *q = s; *q; q++)
        if (!(isalnum((unsigned char)*q) || *q == '_' || *q == '\'' || *q == '.'))
          op = true;
      fprintf(f, op ? "(%s)" : "%s", s);
      return;
    }
    if (t == V_CONSTR) {
      int k, nf;
      get_CON_info(n, &k, &nf);
      fprintf(f, "C_%d", k);    /* the number of fields is clear from the use */
      return;
    }
  } else if (get_DATA_TAG(FUN(n)) == D_INT) {
    value_t i = GETVALUE(n);
    fprintf(f, i < 0 && p > 1 ? "(%"PRIvalue")" : "%"PRIvalue, i);
    return;
  }
  jx_print_leaf(f, n);
}

/* Count the uses of the terms in a decompiled term. */
static void
dc_count(int e)
{
  if (dc[e].uses++)
    return;                     /* the subterms are counted already */
  switch (dc[e].kind) {
  case DC_APP: dc_count(dc[e].a); dc_count(dc[e].b); break;
  case DC_LAM: dc_count(dc[e].b); break;
  case DC_LET: dc_count(dc[e].b); dc_count(dc[e].c); break;
  case DC_CASE:
    dc_count(dc[e].a);
    if (dc[e].d >= 0)
      dc_count(dc[e].d);
    for (int j = 0; j < dc[e].c; j++)
      dc_count(dc_arms[dc[e].b + j].body);
    break;
  default: break;
  }
}

/*
 * A shared term, i.e., one with several uses, is bound by a let, and its uses
 * are its name, s1, s2, ...  The let goes in the innermost scope that has all
 * the uses.  A scope is the body of the definition (0), of a lambda or a let
 * (1+e for the node e), or of a case arm (dc_arm_scope).  Every term that is
 * not shared is printed once, so the scope of a node is well defined.  The
 * definition of a shared term is printed where it is bound, so the uses in it
 * are found after that, i.e., the shared terms are handled from the outside in.
 */
static bool
dc_shared(int e)
{
  return dc[e].uses > 1 && dc[e].kind != DC_VAR && dc[e].kind != DC_LEAF && dc[e].kind != DC_REF;
}

static int *dc_site;            /* the scope of the let of a shared term, or -1 */
static int *dc_sparent;         /* the enclosing scope of a scope */
static int *dc_sdepth;          /* the depth of a scope */
static bool *dc_slets;          /* does the scope have lets */
static int *dc_order;           /* the shared terms, the ones they use first */
static int dc_norder;
static char *dc_seen;
static int dc_nodes0;           /* dc_n when the scopes were found */
static int dc_nlabels;

static int
dc_arm_scope(int arm)
{
  return 1 + dc_nodes0 + arm;
}

/* Find the shared terms in e, the ones a shared term uses before it. */
static void dc_order_term(int e);

static void
dc_order_parts(int e)
{
  switch (dc[e].kind) {
  case DC_APP: dc_order_term(dc[e].a); dc_order_term(dc[e].b); break;
  case DC_LAM: dc_order_term(dc[e].b); break;
  case DC_LET: dc_order_term(dc[e].b); dc_order_term(dc[e].c); break;
  case DC_CASE:
    dc_order_term(dc[e].a);
    if (dc[e].d >= 0)
      dc_order_term(dc[e].d);
    for (int j = 0; j < dc[e].c; j++)
      dc_order_term(dc_arms[dc[e].b + j].body);
    break;
  default: break;
  }
}

static void
dc_order_term(int e)
{
  if (!dc_shared(e)) {
    dc_order_parts(e);
  } else if (!dc_seen[e]) {
    dc_seen[e] = 1;
    dc_order_parts(e);
    dc_order[dc_norder++] = e;
  }
}

/* The innermost scope that encloses both scopes. */
static int
dc_lca(int a, int b)
{
  while (dc_sdepth[a] > dc_sdepth[b]) a = dc_sparent[a];
  while (dc_sdepth[b] > dc_sdepth[a]) b = dc_sparent[b];
  while (a != b) {
    a = dc_sparent[a];
    b = dc_sparent[b];
  }
  return a;
}

static void
dc_new_scope(int sc, int parent)
{
  dc_sparent[sc] = parent;
  dc_sdepth[sc] = dc_sdepth[parent] + 1;
}

/* Record the uses of shared terms in the parts of e, which is in scope. */
static void dc_scope_term(int e, int scope);

static void
dc_scope_parts(int e, int scope)
{
  switch (dc[e].kind) {
  case DC_APP:
    dc_scope_term(dc[e].a, scope);
    dc_scope_term(dc[e].b, scope);
    break;
  case DC_LAM:
    dc_new_scope(1 + e, scope);
    dc_scope_term(dc[e].b, 1 + e);
    break;
  case DC_LET:
    dc_new_scope(1 + e, scope);
    dc_scope_term(dc[e].b, 1 + e);
    dc_scope_term(dc[e].c, 1 + e);
    break;
  case DC_CASE:
    dc_scope_term(dc[e].a, scope);
    if (dc[e].d >= 0)
      dc_scope_term(dc[e].d, scope);
    for (int j = 0; j < dc[e].c; j++) {
      int sc = dc_arm_scope(dc[e].b + j);
      dc_new_scope(sc, scope);
      dc_scope_term(dc_arms[dc[e].b + j].body, sc);
    }
    break;
  default:
    break;
  }
}

static void
dc_scope_term(int e, int scope)
{
  if (dc_shared(e))
    dc_site[e] = dc_site[e] < 0 ? scope : dc_lca(dc_site[e], scope);
  else
    dc_scope_parts(e, scope);
}

/* Find the scope of the let of every shared term in the root e. */
static void
dc_scopes(int e)
{
  dc_nodes0 = dc_n;
  int ns = 1 + dc_n + dc_narms;
  FREE(dc_site); FREE(dc_sparent); FREE(dc_sdepth); FREE(dc_slets); FREE(dc_order); FREE(dc_seen);
  dc_site = mmalloc(dc_n * sizeof(int));
  dc_sparent = mmalloc(ns * sizeof(int));
  dc_sdepth = mmalloc(ns * sizeof(int));
  dc_slets = mcalloc(ns, sizeof(bool));
  dc_order = mmalloc(dc_n * sizeof(int));
  dc_seen = mcalloc(dc_n, 1);
  for (int i = 0; i < dc_n; i++)
    dc_site[i] = -1;
  dc_sparent[0] = 0;
  dc_sdepth[0] = 0;
  dc_norder = 0;
  dc_order_term(e);
  dc_scope_term(e, 0);
  for (int i = dc_norder - 1; i >= 0; i--) {   /* the outer ones first */
    int t = dc_order[i];
    dc_scope_parts(t, dc_site[t]);
  }
  for (int i = 0; i < dc_norder; i++)
    dc_slets[dc_site[dc_order[i]]] = true;
}

/* Name the shared terms that are bound in the scope. */
static void
dc_label_scope(int scope)
{
  for (int i = 0; i < dc_norder; i++)
    if (dc_site[dc_order[i]] == scope)
      dc[dc_order[i]].label = ++dc_nlabels;
}

/* The column of the output, from the position in the file. */
static long dc_line_start;

static int
dc_column(FILE *f)
{
  return (int)(ftell(f) - dc_line_start);
}

/* A new line, indented to column col. */
static void
dc_nl(FILE *f, int col)
{
  fputc('\n', f);
  dc_line_start = ftell(f);
  fprintf(f, "%*s", col, "");
}

#define DC_WIDTH 120            /* the width of a line */

/* Can the term be printed on one line?  A let cannot, and neither can a
 * scope with lets.  A shared term that is used is just its name. */
static bool dc_inline(int e);

static bool
dc_inline_use(int e)
{
  return dc_shared(e) || dc_inline(e);
}

static bool
dc_inline(int e)
{
  if (dc[e].inl)
    return dc[e].inl > 0;
  bool r;
  switch (dc[e].kind) {
  case DC_APP:
    r = dc_inline_use(dc[e].a) && dc_inline_use(dc[e].b);
    break;
  case DC_LAM:
    r = !dc_slets[1 + e] && dc_inline_use(dc[e].b);
    break;
  case DC_LET:
    r = false;
    break;
  case DC_CASE:
    r = dc_inline_use(dc[e].a) && (dc[e].d < 0 || dc_inline_use(dc[e].d));
    for (int j = 0; j < dc[e].c && r; j++)
      r = !dc_slets[dc_arm_scope(dc[e].b + j)] && dc_inline_use(dc_arms[dc[e].b + j].body);
    break;
  default:
    r = true;
    break;
  }
  dc[e].inl = r ? 1 : -1;
  return r;
}

static int
dc_digits(long n)
{
  int d = n < 0 ? 2 : 1;
  for (n = n < 0 ? -n : n; n >= 10; n /= 10)
    d++;
  return d;
}

/* The width of a leaf, as dc_print_leaf prints it (roughly, for the rare ones). */
static int
dc_leaf_width(NODEPTR n)
{
  if (is_VIRT(n)) {
    enum node_tag t = get_VIRT_TAG(n);
    if (t == V_CONSTR) {
      int k, nf;
      get_CON_info(n, &k, &nf);
      return 2 + dc_digits(k);
    }
    if (t <= T_LAST_TAG && TAGNAME(t))
      return (int)strlen(TAGNAME(t)) + 2;
    return 8;
  }
  if (get_DATA_TAG(FUN(n)) == D_INT)
    return dc_digits(GETVALUE(n)) + (GETVALUE(n) < 0 ? 2 : 0);
  return 12;
}

/* The width of a term printed on one line, but at most lim. */
static int dc_width(int e, int lim);

static int
dc_width_use(int e, int lim)
{
  return dc_shared(e) ? 1 + dc_digits(dc_norder) : dc_width(e, lim);
}

static int
dc_width(int e, int lim)
{
  int w = 0;
#define W(x) do { if (w < lim) w += (x); } while (0)
  switch (dc[e].kind) {
  case DC_VAR:  W(1 + dc_digits(dc[e].a)); break;
  case DC_LEAF: W(dc_leaf_width(dc[e].leaf)); break;
  case DC_REF:  W(dc[e].a == 0 ? 4 : 20); break;
  case DC_APP: {
    /* An argument that is not atomic is in parentheses. */
    int b = dc[e].b;
    bool paren = !dc_shared(b) && dc[b].kind != DC_VAR && dc[b].kind != DC_LEAF && dc[b].kind != DC_REF;
    W(dc_width_use(dc[e].a, lim - w) + 1 + (paren ? 2 : 0));
    W(dc_width_use(b, lim - w));
    break;
  }
  case DC_LAM:  W(6 + 4); W(dc_width_use(dc[e].b, lim - w)); break;
  case DC_LET:  W(lim); break;
  case DC_CASE:
    W(15 + dc_width_use(dc[e].a, lim - w));
    for (int j = 0; j < dc[e].c && w < lim; j++) {
      struct dc_arm *a = &dc_arms[dc[e].b + j];
      W(9 + 4 * a->n + dc_width_use(a->body, lim - w));
    }
    if (dc[e].d >= 0)
      W(7 + dc_width_use(dc[e].d, lim - w));
    break;
  }
#undef W
  return w < lim ? w : lim;
}

/* Printing on one line, since the term being printed fits. */
static bool dc_flat;

/* Does the term fit on one line, starting at column col? */
static bool
dc_fits_term(int e, int col)
{
  if (dc_flat)
    return true;
  int room = DC_WIDTH - col;
  return dc_inline(e) && room > 0 && dc_width(e, room + 1) <= room;
}

/* Does a use of the term fit?  A use of a shared term is just its name. */
static bool
dc_fits(int e, int col)
{
  return dc_shared(e) || dc_fits_term(e, col);
}

static void dc_print(FILE *f, int e, int p);
static void dc_print1(FILE *f, int e, int p);

/* Print a term that fits on one line. */
static void
dc_print_flat(FILE *f, int e, int p)
{
  bool old = dc_flat;
  dc_flat = true;
  dc_print(f, e, p);
  dc_flat = old;
}

/* Print the definitions of the shared terms bound in the scope, one per line
 * at column c, each as sN = term. */
static void
dc_print_defs(FILE *f, int scope, int c, bool first)
{
  for (int i = 0; i < dc_norder; i++) {
    int t = dc_order[i];
    if (dc_site[t] != scope)
      continue;
    if (!first)
      dc_nl(f, c);
    first = false;
    fprintf(f, "s%d =", dc[t].label);
    if (dc_fits_term(t, dc_column(f) + 1)) {
      fputc(' ', f);
      bool old = dc_flat;
      dc_flat = true;
      dc_print1(f, t, 0);
      dc_flat = old;
    } else {
      dc_nl(f, c + 2);
      dc_print1(f, t, 0);
    }
  }
}

/* Print the body e of the scope: the lets of the shared terms bound there,
 * and then e. */
static void
dc_print_scope(FILE *f, int e, int scope)
{
  if (dc_slets[scope]) {
    int c = dc_column(f);
    dc_label_scope(scope);
    fputs("let ", f);
    dc_print_defs(f, scope, c + 4, true);
    dc_nl(f, c);
    fputs("in  ", f);
  }
  dc_print(f, e, 0);
}

/* Print the body of a lambda or an arm of the scope, after "->" at column c. */
static void
dc_print_body(FILE *f, int e, int c, int scope)
{
  if (!dc_slets[scope] && dc_fits(e, dc_column(f) + 1)) {
    fputc(' ', f);
    dc_print_flat(f, e, 0);
  } else {
    dc_nl(f, c + 2);
    dc_print_scope(f, e, scope);
  }
}

/* Print a use of a term: the name of a shared term, otherwise the term. */
static void
dc_print(FILE *f, int e, int p)
{
  if (++dc_printed > DC_MAX_PRINT) {
    if (dc_printed == DC_MAX_PRINT + 1)
      fputs("...", f);
    return;
  }
  if (dc_shared(e)) {
    if (dc[e].label)
      fprintf(f, "s%d", dc[e].label);
    else
      fputs("s?", f);
    return;
  }
  dc_print1(f, e, p);
}

/* Print a term.  p is 0 anywhere, 1 for the function of an application,
 * and 2 for an argument.  A term that does not fit on one line is laid out:
 * a case has its arms on separate lines, indented from the case, a let has
 * the in below the let, and an application has its arguments on separate
 * lines, indented from the function. */
static void
dc_print1(FILE *f, int e, int p)
{
  switch (dc[e].kind) {
  case DC_VAR:
    fprintf(f, "x%d", dc[e].a);
    break;
  case DC_LEAF:
    dc_print_leaf(f, dc[e].leaf, p);
    break;
  case DC_REF:
    if (dc[e].a == 0)
      fputs("self", f);
    else
      fprintf(f, "%s@%p", jx_why[jx[dc[e].a].kind], (void *)get_node_ptr(jx[dc[e].a].orig));  /* a node in the heap */
    break;
  case DC_LAM: {
    if (p > 0) fputc('(', f);
    int c = dc_column(f);
    fputc('\\', f);
    int b = e, last = e;
    for (bool first = true; dc[b].kind == DC_LAM && (first || !dc_shared(b)); first = false, b = dc[b].b) {
      fprintf(f, first ? "x%d" : " x%d", dc[b].a);
      last = b;
    }
    fputs(" ->", f);
    dc_print_body(f, b, c, 1 + last);
    if (p > 0) fputc(')', f);
    break;
  }
  case DC_APP:
    if (p > 1) fputc('(', f);
    if (dc_fits_term(e, dc_column(f))) {
      bool old = dc_flat;
      dc_flat = true;
      dc_print(f, dc[e].a, 1);
      fputc(' ', f);
      dc_print(f, dc[e].b, 2);
      dc_flat = old;
    } else {
      /* The function, and the arguments that fit on the same line; the rest
       * on separate lines. */
      int c = dc_column(f);
      int args[DC_MAX_ARGS], na = 0;
      int h = e;
      do {
        args[na++] = dc[h].b;
        h = dc[h].a;
      } while (dc[h].kind == DC_APP && !dc_shared(h) && na < DC_MAX_ARGS);
      dc_print(f, h, 1);
      bool broken = false;
      for (int i = na - 1; i >= 0; i--) {
        if (!broken && dc_fits(args[i], dc_column(f) + 1)) {
          fputc(' ', f);
          dc_print_flat(f, args[i], 2);
        } else {
          broken = true;
          dc_nl(f, c + 2);
          dc_print(f, args[i], 2);
        }
      }
    }
    if (p > 1) fputc(')', f);
    break;
  case DC_LET: {
    /* The recursive let from Y, with the shared terms bound in its scope. */
    if (p > 0) fputc('(', f);
    int c = dc_column(f);
    int sc = 1 + e;
    dc_label_scope(sc);
    fprintf(f, "let x%d =", dc[e].a);
    if (dc_fits(dc[e].b, dc_column(f) + 1)) {
      fputc(' ', f);
      dc_print_flat(f, dc[e].b, 0);
    } else {
      dc_nl(f, c + 6);
      dc_print(f, dc[e].b, 0);
    }
    dc_print_defs(f, sc, c + 4, false);
    dc_nl(f, c);
    fputs("in  ", f);
    dc_print(f, dc[e].c, 0);
    if (p > 0) fputc(')', f);
    break;
  }
  case DC_CASE: {
    if (p > 0) fputc('(', f);
    if (dc_fits_term(e, dc_column(f))) {
      bool old = dc_flat;
      dc_flat = true;
      fputs("case ", f);
      dc_print(f, dc[e].a, 0);
      fputs(" of { ", f);
      for (int j = 0; j < dc[e].c; j++) {
        struct dc_arm *a = &dc_arms[dc[e].b + j];
        fprintf(f, "%sC_%d", j ? "; " : "", a->k);
        for (int v = a->var; v < a->var + a->n; v++)
          fprintf(f, dc_occurs(v, a->body) ? " x%d" : " _", v);
        fputs(" -> ", f);
        dc_print(f, a->body, 0);
      }
      if (dc[e].d >= 0) {
        fputs(dc[e].c ? "; _ -> " : "_ -> ", f);
        dc_print(f, dc[e].d, 0);
      }
      fputs(" }", f);
      dc_flat = old;
      if (p > 0) fputc(')', f);
      break;
    }
    int c = dc_column(f);
    fputs("case ", f);
    bool one = dc_fits(dc[e].a, dc_column(f));
    dc_print(f, dc[e].a, 0);
    if (one) {
      fputs(" of", f);
    } else {
      dc_nl(f, c);
      fputs("of", f);
    }
    for (int j = 0; j < dc[e].c; j++) {
      struct dc_arm *a = &dc_arms[dc[e].b + j];
      dc_nl(f, c + 2);
      fprintf(f, "C_%d", a->k);
      for (int v = a->var; v < a->var + a->n; v++)
        fprintf(f, dc_occurs(v, a->body) ? " x%d" : " _", v);
      fputs(" ->", f);
      dc_print_body(f, a->body, c + 2, dc_arm_scope(dc[e].b + j));
    }
    if (dc[e].d >= 0) {
      dc_nl(f, c + 2);
      fputs("_ ->", f);
      dc_print_body(f, dc[e].d, c + 2, -1);
    }
    if (p > 0) fputc(')', f);
    break;
  }
  }
}

/* Decompile the extracted subgraph and print it. */
static void
dc_decompile(FILE *f)
{
  static int *memo;
  if (!dc) {
    dc = mmalloc(DC_MAX_NODES * sizeof(struct dc_node));
    dc_arms = mmalloc(DC_MAX_ARMS * sizeof(struct dc_arm));
  }
  FREE(memo);
  memo = mmalloc(jx_n * sizeof(int));
  for (int i = 0; i < jx_n; i++)
    memo[i] = -1;
  dc_n = 0;
  dc_narms = 0;
  dc_nvars = 1;
  dc_fuel = DC_FUEL;
  dc_printed = 0;
  dc_nlabels = 0;
  int r = setjmp(dc_fail);
  if (r) {
    static const char *why[] = { "", "too many nodes", "too many arguments", "too many arms",
                                 "", "too many reductions" };
    fprintf(f, "cannot decompile: %s\n", why[r]);
    return;
  }
  int root = dc_app(dc_from_opnd(jx[0].fun, memo), dc_from_opnd(jx[0].arg, memo));
  int e = dc_simplify(dc_go(root));
  dc_count(e);
  dc[e].uses = 1;               /* the root is not shared */
  dc_scopes(e);
  /* Print it as a definition, self x1 ... xn = body */
  dc_line_start = ftell(f);
  fputs("self", f);
  int b = e, sc = 0;
  for (; dc[b].kind == DC_LAM && (b == e || !dc_shared(b)); b = dc[b].b) {
    fprintf(f, " x%d", dc[b].a);
    sc = 1 + b;
  }
  fputs(" =", f);
  dc_print_body(f, b, 0, sc);
  fputc('\n', f);
}

/* The nodes copied by the earlier extractions, to skip them as roots. */
static NODEPTR *jx_done;
static size_t jx_ndone, jx_done_size;

/* Extract the subgraph of a hot node, and print it.  Return false if the node
 * was skipped. */
static bool
jx_extract(FILE *f, int rank, struct sample_entry *e)
{
  NODEPTR r = sample_skip_ind(e->node);
  for (size_t j = 0; j < jx_ndone; j++)
    if (jx_done[j].node_bits == r.node_bits)
      return false;
  fprintf(f, "== %d: node %p, hot after %"PRIcounter" samples, the outermost in %"PRIcounter" of them\n",
          rank, (void *)get_node_ptr(r), num_samples, e->outer);
  if (is_VIRT(r) || !is_AP(FUN(r)) || jx_classify(r) != JX_COPY) {
    fprintf(f, "not a partial application: ");
    jx_print_orig(f, r, 3, false);
    fprintf(f, "\n\n");
    return true;
  }
  if (!jx)
    jx = mmalloc((3 * (size_t)JX_EXPLORE * jx_limit + 2) * sizeof(struct jx_entry));
  jx_n = 0;
  jx_copies = 0;
  (void)jx_visit(r);
  jx_select();
  for (int i = 0; i < jx_n; i++) {
    if (jx[i].kind != JX_COPY)
      continue;
    if (jx_ndone == jx_done_size) {
      jx_done_size = jx_done_size ? 2 * jx_done_size : 1024;
      jx_done = realloc(jx_done, jx_done_size * sizeof(NODEPTR));
      if (!jx_done)
        memerr();
    }
    jx_done[jx_ndone++] = jx[i].orig;
  }

  int count[JX_CUT + 1] = { 0 };
  for (int i = 0; i < jx_n; i++) {
    count[jx[i].kind]++;
    if (jx[i].kind == JX_COPY) {
      if (jx[i].fun.index >= 0) jx[jx[i].fun.index].uses++;
      if (jx[i].arg.index >= 0) jx[jx[i].arg.index].uses++;
    }
  }
  fprintf(f, "%d nodes copied, %d references back to the root, references that stopped the copy: whnf %d, redex %d, cycle %d, depth %d\n",
          count[JX_COPY], jx[0].uses, count[JX_DATA], count[JX_REDEX], count[JX_CYCLE], count[JX_CUT]);
  jx[0].uses++;                 /* so it is never printed inline */
  dc_decompile(f);
  fputc('\n', f);
  return true;
}

static FILE *jx_file;
static int jx_nhot;

/* The node of the entry got hot: extract its subgraph now, as a JIT would. */
static void
jx_hot(struct sample_entry *e)
{
  if (!jx_file) {
    jx_file = fopen("mhs-extract.txt", "w");
    if (!jx_file)
      return;
  }
  if (jx_extract(jx_file, jx_nhot + 1, e)) {
    jx_nhot++;
    fflush(jx_file);
  }
}

/* Forget the copied nodes that were freed, since the addresses are reused. */
static void
jx_gc(void)
{
  size_t k = 0;
  for (size_t j = 0; j < jx_ndone; j++)
    if (is_marked_used(jx_done[j]))
      jx_done[k++] = jx_done[j];
  jx_ndone = k;
}

static void
jx_report(void)
{
  if (!jx_limit)
    return;
  if (jx_file)
    fclose(jx_file);
  jx_file = 0;
  fprintf(stderr, "  the subgraphs of %d nodes that got hot (the outermost in %d samples), at most %d nodes copied, are in mhs-extract.txt\n",
          jx_nhot, jx_threshold, jx_limit);
}
#endif  /* WANT_SAMPLE */

/*
 * The interpreter is split in three functions:
 *  evali       the core: unwinding, combinators, case and constructors, the IO
 *              monad combinators, seq, and Int arithmetic.  It handles most
 *              reductions.
 *  evalprimop  every other primitive; evali calls it for a tag it does not handle.
 *  evalprimret the continuations of the strict primitives that are not on Int,
 *              i.e., what to do when an argument has been evaluated and there is
 *              a BININT64_1 (etc) marker on the stack.
 * evalprimop and evalprimret return the node to continue with, and whether
 * evali should evaluate it (PRIM_TOP, i.e., goto top) or return it as a value
 * (PRIM_RET, i.e., goto ret).
 */
enum prim_next { PRIM_TOP, PRIM_RET };
struct prim_res { NODEPTR n; enum prim_next next; };

/* Macros shared by evali, evalprimop, and evalprimret.  RET, GOTOP, CHECK,
 * and the GOxxx macros are defined separately for evali and for the other two. */
#define HASNARGS(k) (stack_ptr - stk >= (k))
/* Check that there are at least k arguments, return if not. */
#if EXTRASANITY
#define CHECK_ARITY(k) check_arity(tag, (k))
#else
#define CHECK_ARITY(k) do { } while(0)
#endif
#if WANT_PROFILE
#define CHECK(k) do { CHECK_ARITY(k); if (!HASNARGS(k)) { PROF_PARTIAL(); RET; } } while(0)
#else
#define CHECK(k) do { CHECK_ARITY(k); if (!HASNARGS(k)) RET; } while(0)
#endif
#define SETIND(n, x) set_IND(n, x)
#define GOBOOL(b) do { GOIND(b ? combTrue : combFalse); } while(0)
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
#define CHKARG5 do { CHECK(5); POP(5); n = TOP(-1); /*v = ARG(n);*/ w = ARG(TOP(-2)); z = ARG(TOP(-3)); y = ARG(TOP(-4)); x = ARG(TOP(-5)); } while(0)
/* Non-popping versions */
#define CHKARG1NP do { CHECK(1); n = TOP(0);                                               x = ARG(n);      } while(0)
#define CHKARG2NP do { CHECK(2); n = TOP(1);                              y = ARG(n);      x = ARG(TOP(0)); } while(0)
#define CHKARG3NP do { CHECK(3); n = TOP(2);             z = ARG(n);      y = ARG(TOP(1)); x = ARG(TOP(0)); } while(0)
#define CHKARG4NP do { CHECK(4); n = TOP(3); w = ARG(n); z = ARG(TOP(2)); y = ARG(TOP(1)); x = ARG(TOP(0)); } while(0)

/* Alloc a possible GC action, e, between setting x and popping */
#define CHKARGEV1(e)   do { CHECK(1); x = ARG(TOP(0)); e; POP(1); n = TOP(-1); } while(0)

/* Overwrite the (real) node n with a data node */
#define SETINT(n,r)    do { set_DATA_TAG((n), D_INT); SETVALUE((n), (r)); } while(0)
#define SETINT64(n,r)  do { set_DATA_TAG((n), D_INT64); SETINT64VALUE((n), (r)); } while(0)
#define SETDBL(n,d)    do { set_DATA_TAG((n), D_DBL); SETDBLVALUE((n), (d)); } while(0)
#define SETFLT(n,d)    do { set_DATA_TAG((n), D_FLT32); SETFLTVALUE((n), (d)); } while(0)
#define SETPTR(n,r)    do { set_DATA_TAG((n), D_PTR); PTR(n) = (r); } while(0)
#define SETFUNPTR(n,r) do { set_DATA_TAG((n), D_FUNPTR); FUNPTR(n) = (r); } while(0)
#define SETFORPTR(n,r) do { set_DATA_TAG((n), D_FORPTR); FORPTR(n) = (r); } while(0)
#define SETBSTR(n,r)   do { set_DATA_TAG((n), D_FORPTR); FORPTR(n) = (r); FORPTR(n)->finalizer->fptype = FP_BSTR; } while(0)
#define OPINT1(e)      do { CHECK(1); xi = evalint(ARG(TOP(0)));                            e; POP(1); n = TOP(-1); } while(0);
#define OPPTR2(e)      do { CHECK(2); xp = evalptr(ARG(TOP(0))); yp = evalptr(ARG(TOP(1))); e; POP(2); n = TOP(-1); } while(0);
#define CMPP(op)       do { OPPTR2(r = xp op yp); GOIND(r ? combTrue : combFalse); } while(0)


/* Control macros for evalprimop and evalprimret */
#define RET do { return (struct prim_res){ n, PRIM_RET }; } while(0)
#define GOTOP(x) do { return (struct prim_res){ (x), PRIM_TOP }; } while(0)
#define GOIND(x) do { NODEPTR _x = (x); SETIND(n, _x); GOTOP(_x); } while(0)
/* These continue at evali's top rather than at its ap and ap2, so they do the
 * profiler's PROF_OVERWRITE themselves (see prof_push). */
#define GOAP(f,a) do { FUN(n) = (f); ARG(n) = (a); PROF_OVERWRITE(n); GOTOP(n); } while(0)
#define GOAP2(f,a,b) do { FUN(n) = new_ap((f), (a)); ARG(n) = (b); PROF_OVERWRITE(n); PROF_OVERWRITE(FUN(n)); GOTOP(n); } while(0)
#define GOPAIR(a) do { FUN(n) = new_ap(combP, (a)); PROF_OVERWRITE(n); GOTOP(n); } while(0)
#define GOPAIRUNIT do { FUN(n) = combPUnit; PROF_OVERWRITE(n); GOTOP(n); } while(0)

/* Reduce the primitive tag, which is at the head of the spine (n).
 * stk is the stack base of the calling evali. */
static NOINLINE struct prim_res
evalprimop(NODEPTR n, enum node_tag tag, stackptr_t stk)
{
  NODEPTR x, y, z, w;
  value_t xi, yi;
  struct forptr *xfp;
  struct ioarray *arr;
#if WANT_STDIO
  void *bfile;
  int hdr;
#endif  /* WANT_STDIO */

  switch (tag) {

#if WANT_FLOAT32
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
    PUSH(combBINFLT2);
    GOTOP(n);
  case T_FNEG:
    CHECK(1);
    n = ARG(TOP(0));
    PUSH(combUNFLT1);
    GOTOP(n);

  case T_I64TOF:
    {
#if WANT_INT64
    CHECK(1);
    flt32_t rf = (flt32_t)evalint64(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETFLT(n, rf);
    RET;
#else
    ERR("No Int64");
#endif
    }

  case T_ITOF:
    {
    CHECK(1);
    flt32_t rf = (flt32_t)evalint(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETFLT(n, rf);
    RET;
    }

  case T_UTOF:
    {
    CHECK(1);
    flt32_t rf = (flt32_t)(uvalue_t)evalint(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETFLT(n, rf);
    RET;
    }

  case T_FTOI:
    {
    CHECK(1);
    value_t i = (value_t)evalflt(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETINT(n, i);
    RET;
    }

#endif  /* WANT_FLOAT32 */

#if WANT_FLOAT64 && WANT_FLOAT32
  case T_DTOF:
    {
    CHECK(1);
    float xf = (float)evaldbl(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETFLT(n, xf);
    RET;
    }
  case T_FTOD:
    {
    CHECK(1);
    double xd = (double)evalflt(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETDBL(n, xd);
    RET;
    }
#endif  /* WANT_FLOAT64 && WANT_FLOAT32 */

#if WANT_FLOAT64
  case T_DADD:
  case T_DSUB:
  case T_DMUL:
  case T_DDIV:
  case T_DEQ:
  case T_DNE:
  case T_DLT:
  case T_DLE:
  case T_DGT:
  case T_DGE:
    CHECK(2);
    n = ARG(TOP(1));
    PUSH(combBINDBL2);
    GOTOP(n);
  case T_DNEG:
    CHECK(1);
    n = ARG(TOP(0));
    PUSH(combUNDBL1);
    GOTOP(n);

#if WANT_INT64
  case T_I64TOD:
    {
    CHECK(1);
    flt64_t rd = (flt64_t)evalint64(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETDBL(n, rd);
    RET;
    }
#endif

  case T_ITOD:
    {
    CHECK(1);
    flt64_t rd = (flt64_t)evalint(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETDBL(n, rd);
    RET;
    }

  case T_UTOD:
    {
    CHECK(1);
    flt64_t rd = (flt64_t)(uvalue_t)evalint(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETDBL(n, rd);
    RET;
    }

  case T_DTOI:
    {
    CHECK(1);
    value_t i = (value_t)evaldbl(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETINT(n, i);
    RET;
    }

#endif  /* WANT_FLOAT64 */

  case T_ISINT:
    CHECK(1);
    x = evali(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETINT(n, get_XTAG(x) == D_INT ? GETVALUE(x) : -1);
    RET;

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
    GOTOP(n);

#if WANT_INT64
  case T_ADD64:
  case T_SUB64:
  case T_MUL64:
  case T_QUOT64:
  case T_REM64:
  case T_SUBR64:
  case T_UADD64:
  case T_USUB64:
  case T_UMUL64:
  case T_UQUOT64:
  case T_UREM64:
  case T_USUBR64:
  case T_AND64:
  case T_OR64:
  case T_XOR64:
  case T_SHL64:
  case T_SHR64:
  case T_ASHR64:
  case T_EQ64:
  case T_NE64:
  case T_LT64:
  case T_LE64:
  case T_GT64:
  case T_GE64:
  case T_ICMP64:
  case T_ULT64:
  case T_ULE64:
  case T_UGT64:
  case T_UGE64:
  case T_UCMP64:
    CHECK(2);
    /*
    fprintf(stderr, "bin64 op=%s\n", TAGNAME(tag)); fflush(stderr);
    { NODEPTR x = evali(ARG(TOP(1)));
      fprintf(stderr, "x.tag=%s x.val=%lld\n", TAGNAME(get_XTAG(x)), GETINT64VALUE(x)); fflush(stderr);
      NODEPTR y = evali(ARG(TOP(0)));
      fprintf(stderr, "y.tag=%s y.val=%ld\n", TAGNAME(get_XTAG(y)), GETVALUE(y)); fflush(stderr);
    }
    */
    n = ARG(TOP(1));
    if (get_XTAG(n) == D_INT64) {
      //fprintf(stderr, "push combBININT64_1\n"); fflush(stderr);
      n = ARG(TOP(0));
      PUSH(combBININT64_1);
      if (get_XTAG(n) == D_INT64) {
        /* Both arguments are evaluated: the BININT64_1 continuation in
         * evalprimret does the operation. */
        RET;
      }
    } else {
      //fprintf(stderr, "push combBININT64_2\n"); fflush(stderr);
      PUSH(combBININT64_2);
    }
    GOTOP(n);
  case T_NEG64:
  case T_UNEG64:
  case T_INV64:
  case T_POPCOUNT64:
  case T_CLZ64:
  case T_CTZ64:
    CHECK(1);
    n = ARG(TOP(0));
    PUSH(combUNINT64_1);
    GOTOP(n);
#endif  /* WANT_INT64 */


    /* Convert between different types. */
#define CONV(t, set, get) do { CHECK(1); x = evali(ARG(TOP(0))); n = POPTOP(); set_DATA_TAG(n, t); set(n, get(x)); RET; } while(0)
#if WANT_INT64
  case T_TODBL:    CONV(D_DBL,    SETINT64VALUE, GETINT64VALUE); /* raw int64_t -> double */
  case T_FROMDBL:  CONV(D_INT64,  SETINT64VALUE, GETINT64VALUE); /* raw double -> int64_t */
  case T_ITOI64:   CONV(D_INT64,  SETINT64VALUE, GETVALUE);
  case T_UTOU64:   CONV(D_INT64,  SETINT64VALUE, (uint64_t)GETVALUE);
  case T_I64TOI:   CONV(D_INT,    SETVALUE,      GETINT64VALUE);
  case T_U64TOU:   CONV(D_INT,    SETVALUE,      GETINT64VALUE);
#endif  /* WANT_INT64 */
#if WANT_FLOAT32
  case T_TOFLT:    CONV(D_FLT32,  SETINT32VALUE, GETINT32VALUE);
  case T_FROMFLT:  CONV(D_INT,    SETVALUE,      GETINT32VALUE);
#endif  /* WANT_FLOAT32 */
  case T_TOINT:    CONV(D_INT,    SETVALUE,      GETVALUE);
  case T_TOPTR:    CONV(D_PTR,    SETVALUE,      GETVALUE);
  case T_TOFUNPTR: CONV(D_FUNPTR, SETVALUE,      GETVALUE);
#undef CONV

  case T_FPADD: CHECK(2); xfp = evalforptr(ARG(TOP(0))); yi = evalint(ARG(TOP(1))); POP(2); n = TOP(-1); SETFORPTR(n, addForPtr(xfp, yi)); RET;
  case T_FP2P:
    CHECK(1);
    xfp = evalforptr(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETPTR(n, xfp->payload.bs_array);
    RET;

  case T_FP2BS:
    CHECK(2);
    xfp = evalforptr(ARG(TOP(0)));
    xi = evalint(ARG(TOP(1)));
    POP(2);
    n = TOP(-1);
    xfp->payload.bs_size = xi;
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
      GOBOOL(arr == ARR(y));
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
      (void)headutf8(xfp->payload, &out);             /* skip one UTF8 character */
      xi = (char*)out - (char*)xfp->payload.bs_array; /* offset */
      yi = xfp->payload.bs_size - xi;                 /* remaining length */
      SETBSTR(n, bssubstr(xfp, xi, yi));              /* make a substring */
    }
    RET;

    /* Data.Text helpers on one ByteString */
  case T_UTF8LENGTH:
  case T_UTF8TRIMSPACEEND:
  case T_UTF8REVERSE:
  case T_BSISASCII:
    CHECK(1);
    xfp = evalbstr(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    switch (tag) {
    case T_UTF8LENGTH:       SETINT(n, utf8length(xfp->payload)); break;
    case T_UTF8TRIMSPACEEND: SETINT(n, utf8trimspaceend(xfp->payload)); break;
    case T_BSISASCII:        SETINT(n, bsisascii(xfp->payload)); break;
    default:                 SETBSTR(n, mkForPtrFree(utf8reverse(xfp->payload))); break;
    }
    RET;

    /* Data.Text helpers on a ByteString and an Int */
  case T_UTF8OFFSET:
  case T_UTF8OFFSETEND:
  case T_UTF8DECODE:
  case T_UTF8DECODEEND:
  case T_UTF8SKIPSPACE:
  case T_UTF8SKIPNONSPACE:
  case T_BSASCIICASE:
    CHECK(2);
    xfp = evalbstr(ARG(TOP(0)));
    xi = evalint(ARG(TOP(1)));
    POP(2);
    n = TOP(-1);
    switch (tag) {
    case T_UTF8OFFSET:       SETINT(n, utf8offset(xfp->payload, xi)); break;
    case T_UTF8OFFSETEND:    SETINT(n, utf8offsetend(xfp->payload, xi)); break;
    case T_UTF8DECODE:       SETINT(n, utf8decode(xfp->payload, xi)); break;
    case T_UTF8DECODEEND:    SETINT(n, utf8decodeend(xfp->payload, xi)); break;
    case T_UTF8SKIPSPACE:    SETINT(n, utf8skip(xfp->payload, xi, 0)); break;
    case T_UTF8SKIPNONSPACE: SETINT(n, utf8skip(xfp->payload, xi, 1)); break;
    default:                 SETBSTR(n, mkForPtrFree(bsasciicase(xfp->payload, xi))); break;
    }
    RET;

  case T_UTF8OFFSETFROM:
  case T_UTF8OFFSETENDBEFORE:
    CHECK(3);
    {
      value_t xk;
      xfp = evalbstr(ARG(TOP(0)));
      xi = evalint(ARG(TOP(1)));
      xk = evalint(ARG(TOP(2)));
      POP(3);
      n = TOP(-1);
      if (tag == T_UTF8OFFSETFROM)
        SETINT(n, utf8offsetfrom(xfp->payload, xi, xk));
      else
        SETINT(n, utf8offsetendbefore(xfp->payload, xi, xk));
    }
    RET;

  case T_BSFIND:
    CHECK(3);
    {
      struct forptr *pfp = evalbstr(ARG(TOP(0)));
      struct forptr *sfp = evalbstr(ARG(TOP(1)));
      xi = evalint(ARG(TOP(2)));
      POP(3);
      n = TOP(-1);
      SETINT(n, bsfind(pfp->payload, sfp->payload, xi));
    }
    RET;

  case T_BSCONCAT:
    CHECK(1);
    {
      struct bytestring rb = bsconcat(ARG(TOP(0)));   /* evaluates nothing */
      POP(1);
      n = TOP(-1);
      SETBSTR(n, mkForPtrFree(rb));
    }
    RET;

  case T_BSRFIND:
    CHECK(2);
    {
      struct forptr *pfp = evalbstr(ARG(TOP(0)));
      struct forptr *sfp = evalbstr(ARG(TOP(1)));
      POP(2);
      n = TOP(-1);
      SETINT(n, bsrfind(pfp->payload, sfp->payload));
    }
    RET;

  case T_BSFROMUTF8:
    if (doing_rnf) RET;
    CHECK(1);

    xfp = evalbstr(ARG(TOP(0)));
    GCCHECK(strNodes(xfp->payload.bs_size));
    POP(1);
    n = TOP(-1);
    //printf("T_FROMUTF8 x = %p fp=%p payload.string=%p\n", x, x->uarg.uuforptr, x->uarg.uuforptr->payload.string);
    GOIND(mkStringU(xfp->payload));

  case T_BSUNPACK:
    if (doing_rnf) RET;
    CHECK(1);
    struct forptr *xfp = evalbstr(ARG(TOP(0)));
    GCCHECK(strNodes(xfp->payload.bs_size));
    POP(1);
    n = TOP(-1);
    GOIND(bsunpack(xfp->payload));

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
    SETINT(n, xfp->payload.bs_size);
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
    SETINT(n, ((uint8_t *)xfp->payload.bs_array)[xi]);
    RET;

  case T_BSNEW:
    {
    CHKARG3NP;
    struct bytestring bs;
    bs.bs_size = evalint(x);
    bs.bs_capacity = evalint(y);
    GCCHECK(2);                 /* PAIR + StrNode */
    bs.bs_array = mmalloc(bs.bs_capacity);
    memset(bs.bs_array, 0, bs.bs_size);
    POP(3);
    GOPAIR(mkStrNode(bs));
    }

  case T_BSFREEZE:
    {
    CHKARG2NP;
    xfp = evalbstr(x);
    GCCHECK(1);                 /* PAIR */
    struct bytestring *bsp = &xfp->payload;
    if (bsp->bs_size != bsp->bs_capacity) {
      bsp->bs_array = mrealloc(bsp->bs_array, bsp->bs_size);
      adjforptr(xfp);           /* in case bs_array moved */
    }
    bsp->bs_capacity = 0;        /* mark a immutable */
    POP(2);
    GOPAIR(x);
    }

  case T_BSAPPBYTE:
    CHKARG3NP;
    xfp = evalbstr(x);
    xi = evalint(y);
    bsappbyte(xfp, xi);
    POP(3);
    GOPAIRUNIT;
    
  case T_BSAPPCHAR:
    CHKARG3NP;
    xfp = evalbstr(x);
    xi = evalint(y);
    if (0xdc80 <= xi && xi <= 0xdcff) {
      /* Using dc80-dcff to roundtrip bad bytes */
      bsappbyte(xfp, xi & 0xff);
    } else {
    if ((xi & 0x1ff800) == 0xd800) {
      /* xi is a surrogate */
      xi = 0xfffd; /* replacement character */
    }
    if (0 < xi && xi < 0x80) {   /* exclude 0, since this is modified UTF-8 */
      bsappbyte(xfp, xi);
    } else if (xi < 0x800) {
      /* 0 encodes here, with an over-long representation */
      bsappbyte(xfp, ((xi >> 6 )       ) | 0xc0);
      bsappbyte(xfp, ((xi      ) & 0x3f) | 0x80);
    } else if (xi < 0x10000) {
      bsappbyte(xfp, ((xi >> 12)       ) | 0xe0);
      bsappbyte(xfp, ((xi >> 6 ) & 0x3f) | 0x80);
      bsappbyte(xfp, ((xi      ) & 0x3f) | 0x80);
    } else if (xi < 0x110000) {
      bsappbyte(xfp, ((xi >> 18)       ) | 0xf0);
      bsappbyte(xfp, ((xi >> 12) & 0x3f) | 0x80);
      bsappbyte(xfp, ((xi >> 6 ) & 0x3f) | 0x80);
      bsappbyte(xfp, ((xi      ) & 0x3f) | 0x80);
    } else {
      ERR("invalid char");
    }
    }
    POP(3);
    GOPAIRUNIT;

  case T_BSREAD:
    CHKARG3NP;
    xfp = evalbstr(x);
    xi = evalint(y);
    GCCHECK(2);                 /* PAIR + Int */
    POP(3);
    yi = ((uint8_t *)xfp->payload.bs_array)[xi];
    GOPAIR(mkInt(yi));

  case T_BSWRITE:
    CHKARG4NP;
    xfp = evalbstr(x);
    xi = evalint(y);
    yi = evalint(z);
    POP(4);
    ((uint8_t *)xfp->payload.bs_array)[xi] = (uint8_t)yi;
    GOPAIRUNIT;

  case T_RAISE:
    if (doing_rnf) RET;
    CHKARG1;
    raise_exn(x);               /* never returns */

  case T_SPNEW:
    GCCHECK(2);                 /* PAIR + Int */
    CHKARG2;
    xi = new_stableptr(x);
    GOPAIR(mkInt(xi));
  case T_SPDEREF:
    CHKARG2NP;
    xi = evalint(x);
    GCCHECK(1);                 /* PAIR */
    POP(2);
    GOPAIR(deref_stableptr(xi));
  case T_SPFREE:
    CHKARG2NP;
    xi = evalint(x);
    free_stableptr(xi);
    POP(2);
    GOPAIRUNIT;

  case T_WKNEW:
    GCCHECK(2);                 /* PAIR + weak */
    CHKARG3;
    GOPAIR(new_weak_ptr(x, y, NIL_NODEPTR));
  case T_WKNEWFIN:
    GCCHECK(3);                 /* PAIR + weak + finalizer */
    CHKARG4;
    GOPAIR(new_weak_ptr(x, y, z));
  case T_WKDEREF:
    CHKARG2NP;
    x = deref_weak_ptr(evalweak(x));
    GCCHECK(1);                 /* PAIR */
    POP(2);
    GOPAIR(x);
  case T_WKFINAL:
    CHKARG2NP;
    finalize_weak_ptr(evalweak(x));
    POP(2);
    GOPAIRUNIT;

  case T_BALANCEL:
  case T_BALANCER:
    CHECK(4);
    GOTOP(map_balance(tag));    /* the redex, overwritten with the result */
  case T_IDENTEQ:
    CHECK(2);
    {
      int eq = ident_eq();
      POP(2);
      n = TOP(-1);
      GOBOOL(eq);
    }

  case T_RNF:
    if (doing_rnf) RET;
    CHECK(2);
    xi = evalint(ARG(TOP(0)));
    rnf(xi, ARG(TOP(1))); POP(2); n = TOP(-1); GOIND(combUnit);

  case T_IO_PERFORMIO:
    GCCHECK(2);
    if (doing_rnf) RET;
    CHKARG1;
    /* Conjure up a new world and evaluate the io with that world, finally selecting the result */
    /* PERFORMIO io  -->  io World K */
#if 1
    GOAP2(x, combWorld, combK);
#else
    {
      /* Don't count performio reductions. */
      /* Useful when Debug.Trace.trace should have zero cost */
      NODEPTR p1 = new_ap(x, combWorld);
      NODEPTR p2 = new_ap(p1, combK);
      counter_t s = glob_slice;
      glob_slice = 1000000000;
      NODEPTR p3 = evali(p2);
      glob_slice = s;
      GOIND(p3);
    }
#endif

  case T_IO_ATOMIC:
    /* Perform an IO action "atomically".
     * It's not really atomic, but 10 uninterrupted slices.
     * This is good enough for atomicModifyIORef. */
    {
      GCCHECK(2);
      CHKARG2NP;                     /* set x=action, y=world, n */
      NODEPTR p1 = new_ap(x, y);
      NODEPTR p2 = new_ap(p1, combK);
      glob_slice += slice * 10;      /* give the current thread 10 extra slices */
      NODEPTR p3 = evali(p2);        /* this evaluation is guarenteed at least 10 uninterrupted slices */
      glob_slice -= slice * 10;      /* and take them away again */
      GCCHECKSAVE(p3, 2);
      POP(2);
      GOPAIR(p3);
    }
    break;
  case T_IO_LAZYBINDST:
    /* Lazy bind, used for the lazy ST monad.
     * DO NOT USE FOR IO, because effects are not guaranteed to happen.
     *   (x `lazyBind` y) z = let w = x z in y (fst w) (snd w)
     */
    GCCHECK(4);
    CHKARG3;
    w = new_ap(x, z);
    GOAP2(y, new_ap(combPFst, w), new_ap(combPSnd, w));
  case T_IO_LAZYBIND:
    /* Lazy bind, used for unsafeInterleaveIO.
     *   (x `lazyBind` y) z = let w = x z in y (fst w) (snd w)
     * since the actual state token doesn't matter we use the more efficient
     *   (x `lazyBind` y) z = let w = x z in y (fst w) z
     */
    GCCHECK(4);
    CHKARG3;
    w = new_ap(x, z);
    GOAP2(y, new_ap(combPFst, w), z);
  case T_IO_STRICT:
    CHKARG2;
    /* Force the world argument before executing the IO.
     * IO.strict io World = seq World (io World)
     */
    (void)evali(y);             /* evaluate the world */
    GOAP(x, y);                 /* and run IO computation */
#if WANT_STDIO
  case T_IO_PP:
    CHKARG2;
    pp(stderr, x);
    GOPAIRUNIT;
  case T_IO_PRINT:
    hdr = false;
    goto ser;
  case T_IO_SERIALIZE:
    hdr = true;
  ser:
#if 0
    gc();                     /* DUBIOUS: do a GC to get possible GC reductions */
#endif
    CHKARG3NP;
    bfile = (struct BFILE*)evalptr(x);
    printb(bfile, evali(y), hdr);
    putb('\n', bfile);
    POP(3);
    GOPAIRUNIT;
  case T_IO_DESERIALIZE:
    CHKARG2NP;
    bfile = (struct BFILE*)evalptr(x);
    gc();                     /* make sure we have room.  GC during parse is dodgy. */
    x = parse_top(bfile, 0);
    if (is_NIL(x))
      raise_rts(exn_deserialize);
    POP(2);
    GOPAIR(x);                /* allocates a cell, but we did a GC above */
#endif
#if WANT_ARGS
  case T_IO_GETARGREF:
    GCCHECK(2);
    CHKARG1;
    x = alloc_node(D_ARR);
    ARR(x) = argarray;
    GOPAIR(x);
#endif
  case D_IO_CCALL:
    {
      GCCHECK(1);                 /* room for placeholder */
      int a = (int)GETVALUE(n);   /* function number */
      //printf("  %s\n", FFI_IX(a).ffi_name);
      int arity = FFI_IX(a).ffi_arity;
      CHECK(arity);
      funptr_t f = FFI_IX(a).ffi_fun;
      PUSH(mkPtr(0));             /* placeholder for result, protected from GC */
      int k = f(stk);             /* call FFI function, return number of arguments */
      if (k != arity) {
#if WANT_STDIO
        fprintf(stderr, "ccall arity %s %d!=%d\n", FFI_IX(a).ffi_name, arity, k);
#endif
        ERR("ccall arity");     /* temporary sanity check */
      }
      GCCHECK(1);                 /* room for pair */
      x = POPTOP();               /* pop actual result */
      POP(arity);                 /* pop the pushed arguments */
      if (stack_ptr < 0)
        ERR("CCALL POP");
      n = POPTOP();               /* node to update */
      GOPAIR(x);                  /* and this is the result */
    }

  case T_PACKCSTRING:
    {
      CHKARG2NP;                  /* sets x, y, n */
      {
      char *cstr = evalptr(x);
      struct bytestring bs = mk_ro_bytestring(strlen(cstr), NULL);
      memcpy(bs.bs_array, cstr, bs.bs_size);
      NODEPTR res = mkStrNode(bs);
      GCCHECKSAVE(res, 2);
      POP(2);
      GOPAIR(res);
      }
    }
  case T_PACKCSTRINGLEN:
    {
      CHKARG3NP;                /* sets x,y,z,n */
      {
      char *cstr = evalptr(x);
      struct bytestring bs = mk_ro_bytestring(evalint(y), NULL);
      memcpy(bs.bs_array, cstr, bs.bs_size);
      NODEPTR res = mkStrNode(bs);
      POP(3);
      GCCHECKSAVE(res, 2);
      GOPAIR(res);
      }
    }
  case T_BSGRAB:
    {
      CHKARG2NP;                  /* sets x, y, n */
      {
      char *p = evalptr(x);
      struct bytestring bs = mk_ro_bytestring(strlen(p), p);
      NODEPTR res = mkStrNode(bs);
      GCCHECKSAVE(res, 2);
      POP(2);
      GOPAIR(res);
      }
    }
  case T_BSGRABLEN:
    {
      CHKARG3NP;                  /* sets x, y, z, n */
      {
      struct bytestring bs = mk_ro_bytestring(evalint(y), evalptr(x));
      NODEPTR res = mkStrNode(bs);
      GCCHECKSAVE(res, 2);
      POP(3);
      GOPAIR(res);
      }
    }

  case T_ARR_ALLOC:
    {
      CHKARG3NP;                /* sets x,y,z,n */
      {
      size_t size = evalint(x);
      struct ioarray *arr = arr_alloc(size, y);
      GCCHECK(2);
      NODEPTR res = alloc_node(D_ARR);
      ARR(res) = arr;
      POP(3);
      GOPAIR(res);
      }
    }
  case T_ARR_COPY:
    {
      CHKARG2NP;
      {
      NODEPTR a = evali(x);
      if (get_XTAG(a) != D_ARR)
        ERR("T_ARR_COPY tag");
      struct ioarray *arr = arr_copy(ARR(a));
      GCCHECK(2);
      NODEPTR res = alloc_node(D_ARR);
      ARR(res) = arr;
      POP(2);
      GOPAIR(res);
      }
    }
  case T_ARR_SIZE:
    {
      CHKARG2NP;
      NODEPTR a = evali(x);
      if (get_XTAG(a) != D_ARR)
        ERR("bad ARR tag");
      GCCHECK(2);
      NODEPTR res = mkInt(ARR(a)->size);
      POP(2);
      GOPAIR(res);
    }
  case T_ARR_READ:
    {
      CHKARG3NP;                /* sets x,y,n */
      size_t i = evalint(y);
      NODEPTR a = evali(x);
      if (get_XTAG(a) != D_ARR)
        ERR("bad ARR tag");
      if (i >= ARR(a)->size)
        ERR("ARR_READ");
      GCCHECK(1);
      NODEPTR res = ARR(a)->array[i];
      POP(3);
      GOPAIR(res);
    }
  case T_ARR_WRITE:
    {
      CHKARG4NP;                /* sets x,y,z,n */
      size_t i = evalint(y);
      NODEPTR a = evali(x);
      if (get_XTAG(a) != D_ARR)
        ERR("bad ARR tag");
      if (i >= ARR(a)->size) {
        ERR("ARR_WRITE");
      }
      ARR(a)->array[i] = z;
      POP(4);
      GOPAIRUNIT;
      }

  case T_ARR_TRUNC:
    {
      CHKARG3NP;                /* sets x,y,n */
      size_t i = evalint(y);
      NODEPTR a = evali(x);
      if (get_XTAG(a) != D_ARR)
        ERR("bad ARR tag");
      if (i >= ARR(a)->size) {
        ERR("ARR_TRUNC");
      }
      ARR(a)->size = i;
      POP(3);
      GOPAIRUNIT;
      }

  case T_FPNEW:
    {
      CHKARG2NP;
      //printf("T_FPNEW\n");
      void *xp = evalptr(x);
      //printf("T_FPNEW xp=%p\n", xp);
      GCCHECK(2);
      NODEPTR res = alloc_node(D_FORPTR);
      SETFORPTR(res, mkForPtrP(xp));
      POP(2);
      GOPAIR(res);
    }
  case T_FPFIN:
    {
      CHKARG3NP;
      //printf("T_FPFIN\n");
      struct forptr *xfp = evalforptr(y);
      //printf("T_FPFIN xfp=%p\n", xfp);
      HsFunPtr xp = evalfunptr(x);
      //printf("T_FPFIN yp=%p\n", yp);
      xfp->finalizer->final = xp;
      POP(3);
      GOPAIRUNIT;
    }
  case T_IO_GC:
    //printf("gc()\n");
    CHKARG2NP;
    {
      int red = evalint(x);
      int oldred = want_gc_red;
      want_gc_red = red;
      gc();
      want_gc_red = oldred;
    }
    POP(2);
    GOPAIRUNIT;

  case T_IO_STATS:
    {
    GCCHECK(5);
    CHKARG1;
    NODEPTR res = new_ap(new_ap(combPair, mkInt((uvalue_t)NUM_ALLOC)), mkInt((uvalue_t)(num_reductions - glob_slice)));
    GOPAIR(res);
    }

  case T_IO_FORK:
    {
      GCCHECK(3);
      CHKARG2;                /* set x=io, y=ST, n */
      struct mthread *mt = new_thread(new_ap(x, y)); /* copy the world */
      mt->mt_mask = runq.mq_head->mt_mask; /* inherit masking state */
      NODEPTR res = alloc_node(D_THID);
      THR(res) = mt;
      GOPAIR(res);
    }

  case T_IO_THID:
    {
      GCCHECK(2);
      CHKARG1;
      NODEPTR res = alloc_node(D_THID);
      THR(res) = runq.mq_head;            /* head of the run queue is the current thread */
      GOPAIR(res);
    }
  case T_IO_THROWTO:
    {
      CHKARG3NP;                /* x=this, y=exn, z=ST */
      check_thrown(true);       /* check if we have a thrown exception */
      struct mthread *mt = evalthid(x);
      throwto(mt, y);
      POP(3);
      GOPAIRUNIT;
    }
  case T_IO_YIELD:
    CHKARG1;
    yield();
    GOPAIRUNIT;

  case T_IO_NEWMVAR:
    {
      GCCHECK(2);
      CHKARG1;
      struct mvar *mv = new_mvar();
      NODEPTR res = alloc_node(D_MVAR);
      MVAR(res) = mv;
      GOPAIR(res);
    }
  case T_IO_TAKEMVAR:
    {
      CHKARG2NP;             /* set x=mvar, y=ST */
      check_thrown(true);    /* check if we have a thrown exception */
      NODEPTR res = take_mvar(false, evalmvar(x));         /* never returns if it blocks */
      GCCHECKSAVE(res, 1);
      POP(2);
      GOPAIR(res);
    }
  case T_IO_READMVAR:
    {
      CHKARG2NP;
      check_thrown(true);    /* check if we have a thrown exception */
      NODEPTR res = read_mvar(false, evalmvar(x));         /* never returns if it blocks */
      GCCHECKSAVE(res, 1);
      POP(2);
      GOPAIR(res);
    }
  case T_IO_PUTMVAR:
    {
      CHKARG3NP;             /* set x=mvar, y=value, z=ST */
      check_thrown(true);    /* check if we have a thrown exception */
      (void)put_mvar(false, evalmvar(x), y); /* never returns if it blocks */
      POP(3);
      GOPAIRUNIT;
    }
  case T_IO_TRYTAKEMVAR:
    {
      CHKARG2NP;
      NODEPTR res = take_mvar(true, evalmvar(x));
      GCCHECKSAVE(res, 2);
      if (!is_NIL(res))
        res = new_ap(combJust, res);
      else
        res = combNothing;
      POP(2);
      GOPAIR(res);
    }
  case T_IO_TRYREADMVAR:
    {
      CHKARG2NP;
      NODEPTR res = read_mvar(true, evalmvar(x));
      if (!is_NIL(res)) {
        GCCHECKSAVE(res, 2);
        res = new_ap(combJust, res);
      } else {
        res = combNothing;
      }
      POP(2);
      GOPAIR(res);
    }
  case T_IO_TRYPUTMVAR:
    {
      CHKARG3NP;
      NODEPTR res = put_mvar(true, evalmvar(x), y) ? combTrue : combFalse;
      GCCHECKSAVE(res, 1);
      POP(3);
      GOPAIR(res);
    }
  case T_IO_THREADDELAY:
    {
      CHKARG2NP;
#if defined(CLOCK_INIT)
      check_thrown(true);      /* check if we have a thrown exception */
      if (runq.mq_head->mt_at == -1) {
        /* delay has already expired, so just return */
        runq.mq_head->mt_at = 0;
        POP(2);
        GOPAIRUNIT;
      } else {
        thread_delay(evalint(x)); /* never returns */
      }
#else
      ERR("threadDelay: no clock");
#endif
    }
  case T_IO_THREADSTATUS:
    {
      CHKARG2NP;
      struct mthread *mt = evalthid(x);
      GCCHECK(2);
      POP(2);
      GOPAIR(mkInt(mt->mt_state));
    }
  case T_IO_WAITRDFD:
  case T_IO_WAITWRFD: {
#if WANT_IO_POLL
    CHKARG2NP; /* x = filedescriptor, y = RealWorld; no pop yet */
    GCCHECK(2);

    /*
     * When the thread wakes up again it will re-execute that last op.
     * check_pollq() sets mt_fd=IO_POLL_EVENT_HAS_HAPPENED when waking the thread.
     * If we did not do this check we would just sleep again.
     */
    if (runq.mq_head->mt_fd == IO_POLL_EVENT_HAS_HAPPENED) {
      runq.mq_head->mt_fd = IO_POLL_WAITING_FOR_NONE;
      POP(2);
      GOPAIR(mkInt(0));
    }

    check_thrown(true);      /* check if we have a thrown exception */

    int fd = evalint(x);
    int events = tag == T_IO_WAITRDFD ? POLLIN : POLLOUT;

    /* Set up the waiting thread's state, preparing it to leave the run queue
     * until an event is ready for it.
     */
    struct mthread *mt = remove_q_head(&runq);
    mt->mt_fd     = fd;
    mt->mt_events = events;
    add_q_tail(&pollq, mt);     /* put it on the q of I/O waiters */
#if THREAD_DEBUG
      if (thread_trace) {
        printf("T_IO_WAITxxFD: wait for FD=%d, events=%x, thread=%d\n", fd, events, (int)mt->mt_id);
      }
#endif  /* THREAD_DEBUG */

    POP(2);
    resched(mt, ts_wait_io);    /* set the thread state and reschedule */
#else /* WANT_IO_POLL */
    CHKARG2;
#if WANT_ERRNO
    errno = EINVAL;
#endif
    GCCHECK(2);
    GOPAIR(mkInt(-1));          /* cannot poll */
#endif /* WANT_IO_POLL */
  }
  case T_IO_GETMASKINGSTATE:
    GCCHECK(2);
    CHKARG1;                    /* x = ST */
    GOPAIR(mkInt(runq.mq_head->mt_mask));

  case T_IO_SETMASKINGSTATE:
    CHKARG2;                    /* x = level, y = ST */
    runq.mq_head->mt_mask = evalint(x);
    GOPAIRUNIT;

  case T_CATCH:
    /* CATCH x y z --> CATCHR (x z) y z */
    GCCHECK(3);
    CHKARG3;                    /* x=io, y=hdl, z=ST */
    GOAP(new_ap(new_ap(combCATCHR, new_ap(x, z)), y), z);
  case T_CATCHR:
    {
      CHKARG3NP;                /* x = (io st), y = hdl, z = st, n = (CATCHR (io st)) h */
      struct handler *h = mmalloc(sizeof *h);
      h->hdl_old = cur_handler;
      cur_handler = h;
      stackptr_t ostack = stack_ptr;;    /* old stack pointer */
      enum mask_state omask = runq.mq_head->mt_mask;     /* old mask */
      if (setjmp(h->hdl_buf)) {
        /* An exception occurred: */
        stack_ptr = ostack;
        runq.mq_head->mt_mask = mask_interruptible; /* evaluate with mask */
        NODEPTR exn = h->hdl_exn;       /* exception value */
        cur_handler = h->hdl_old;       /* reset handler */
        FREE(h);
        GCCHECK(8);
        POP(3);
        /*
         * Run:
         *  hdl exn `primBind` \ r ->
         *  primSetMaskingState omask `primThen`
         *  primReturn r
         * i.e.,
         *  primBind (hdl exn) (B' primThen (primSetMaskingState omask) primReturn)
         */
        NODEPTR p = new_ap(combIOBIND, new_ap(y, exn));
        NODEPTR q = new_ap(new_ap(new_ap(combBB, combIOTHEN), new_ap(combSETMASKINGSTATE, mkInt(omask))), combIORETURN);
        GOAP2(p, q, z);
      } else {
        /* Normal execution: */
        x = evali(x);             /* execute first argument */
        /* No exception occurred */
        cur_handler = h->hdl_old; /* restore old handler */
        FREE(h);
        POP(3);
        GOIND(x);
      }
    }

  case T_THNUM:
    {
    CHECK(1);
    struct mthread *mt = evalthid(ARG(TOP(0)));
    POP(1);
    n = TOP(-1);
    SETINT(n, (uvalue_t)mt->mt_id);
    RET;
    }

#if WANT_TICK
  case D_TICK:
    xi = GETVALUE(n);
    CHKARG1;
    // fprintf(stderr, "tick=%s\n", (char*)tick_table[xi].tick_name.string); fflush(stderr);
    dotick(xi);
    GOIND(x);
#endif

  default:
    ERR1("eval tag %s", TAGNAME(get_XTAG(n)));
  }
  RET;
}

/* n has been evaluated, and the marker tag is on top of the stack. */
static NOINLINE struct prim_res
evalprimret(NODEPTR n, enum node_tag tag)
{
  uvalue_t yu, ru;
#if WANT_INT64
  uint64_t x64u, y64u, r64u;
#endif  /* WANT_INT64 */
#if WANT_FLOAT32
  flt32_t xf, yf, rf;
#endif  /* WANT_FLOAT32 */
#if WANT_FLOAT64
  flt64_t xd, yd, rd;
#endif  /* WANT_FLOAT64 */
  struct bytestring xbs, ybs, rbs;
  value_t r;
  NODEPTR p, y;

  switch (tag) {

#if WANT_INT64
    case T_BININT64_2:
      n = ARG(TOP(1));
      TOP(0) = combBININT64_1;
      GOTOP(n);

    case T_BININT64_1:
      /* First argument */
#if SANITY
      if (get_XTAG(n) != D_INT64) {
        //fprintf(stderr, "tag=%s\n", TAGNAME(get_XTAG(n))); fflush(stderr);
        ERR("BININT64 0");
      }
#endif  /* SANITY */
      x64u = (uint64_t)GETINT64VALUE(n);
      /* Second argument */
      y = ARG(TOP(2));
      while (get_XTAG(y) == D_IND)
        y = get_IND(y);
      /* The second argument to the shift ops is an int, so use a hack for that */
      if (get_XTAG(y) == D_INT64)
        y64u = (uint64_t)GETINT64VALUE(y);
      else if (get_XTAG(y) == D_INT)
        yu = (uvalue_t)GETVALUE(y);
      else
        ERR("BININT64 1");
      p = FUN(TOP(1));
      POP(3);
      n = TOP(-1);
    binint64:
      switch (get_XTAG(p)) {
      case D_IND:   p = get_IND(p); goto binint64;
      case T_ADD64: ADD_OVERFLOW(int64_t, r64u, x64u, y64u); break;
      case T_SUB64: SUB_OVERFLOW(int64_t, r64u, x64u, y64u); break;
      case T_MUL64: MUL_OVERFLOW(int64_t, r64u, x64u, y64u); break;
      case T_SUBR64:SUB_OVERFLOW(int64_t, r64u, y64u, x64u); break;
      case T_QUOT64:if (y64u == 0)
                      raise_rts(exn_dividebyzero);
                    else if ((int64_t)x64u == INT64_MIN && (int64_t)y64u == -1)
                      raise_rts(exn_overflow);
                    else
                      r64u = (uint64_t)((int64_t)x64u / (int64_t)y64u);
                    break;
      case T_REM64: if (y64u == 0)
                      raise_rts(exn_dividebyzero);
                    else if ((int64_t)y64u == -1) /* MIN % -1 traps on x86 */
                      r64u = 0;
                    else
                      r64u = (uint64_t)((int64_t)x64u % (int64_t)y64u);
                    break;
      case T_UADD64:r64u = x64u + y64u; break;
      case T_USUB64:r64u = x64u - y64u; break;
      case T_UMUL64:r64u = x64u * y64u; break;
      case T_USUBR64:r64u = y64u - x64u; break;
      case T_UQUOT64:if (y64u == 0)
                      raise_rts(exn_dividebyzero);
                    else
                      r64u = x64u / y64u;
                    break;
      case T_UREM64:if (y64u == 0)
                      raise_rts(exn_dividebyzero);
                    else
                      r64u = x64u % y64u;
                    break;
      case T_AND64: r64u = x64u & y64u; break;
      case T_OR64:  r64u = x64u | y64u; break;
      case T_XOR64: r64u = x64u ^ y64u; break;
      case T_SHL64: r64u = x64u << yu; break;
      case T_SHR64: r64u = x64u >> yu; break;
      case T_ASHR64:r64u = (uint64_t)((int64_t)x64u >> yu); break;

      case T_EQ64:  GOBOOL(x64u == y64u);
      case T_NE64:  GOBOOL(x64u != y64u);
      case T_ULT64: GOBOOL(x64u <  y64u);
      case T_ULE64: GOBOOL(x64u <= y64u);
      case T_UGT64: GOBOOL(x64u >  y64u);
      case T_UGE64: GOBOOL(x64u >= y64u);
      case T_UCMP64:GOIND(x64u <  y64u ? combLT   : x64u > y64u ? combGT : combEQ);
      case T_LT64:  GOBOOL((int64_t)x64u <  (int64_t)y64u);
      case T_LE64:  GOBOOL((int64_t)x64u <= (int64_t)y64u);
      case T_GT64:  GOBOOL((int64_t)x64u >  (int64_t)y64u);
      case T_GE64:  GOBOOL((int64_t)x64u >= (int64_t)y64u);
      case T_ICMP64:GOIND((int64_t)x64u <  (int64_t)y64u ? combLT   : (int64_t)x64u > (int64_t)y64u ? combGT : combEQ);

      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("BININT64");
      }
      SETINT64(n, (int64_t)r64u);
      RET;

    case T_UNINT64_1:
      /* The argument */
#if SANITY
      if (get_XTAG(n) != D_INT64)
        ERR("UNINT64 0");
#endif
      x64u = (uint64_t)GETINT64VALUE(n);
      p = FUN(TOP(1));
      POP(2);
      n = TOP(-1);
    unint64:
      switch (get_XTAG(p)) {
      case D_IND:        p = get_IND(p); goto unint64;
      case T_NEG64:      if ((int64_t)x64u == INT64_MIN) raise_rts(exn_overflow); r64u = -x64u; break;
      case T_UNEG64:     r64u = -x64u; break;
      case T_INV64:      r64u = ~x64u; break;
      case T_POPCOUNT64: ru = POPCOUNT64(x64u); SETINT(n, (value_t)ru); RET;
      case T_CLZ64:      ru = CLZ64(x64u); SETINT(n, (value_t)ru); RET;
      case T_CTZ64:      ru = CTZ64(x64u); SETINT(n, (value_t)ru); RET;
      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("UNINT64");
      }
      SETINT64(n, (int64_t)r64u);
      RET;
#endif  /* WANT_INT64 */

#if WANT_FLOAT32
    case T_BINFLT2:
      n = ARG(TOP(1));
      TOP(0) = combBINFLT1;
      GOTOP(n);

    case T_BINFLT1:
      /* First argument */
#if SANITY
      if (get_XTAG(n) != D_FLT32)
        ERR("BINDBL 0");
#endif  /* SANITY */
      xf = GETFLTVALUE(n);
      /* Second argument */
      y = ARG(TOP(2));
      while (get_XTAG(y) == D_IND)
        y = get_IND(y);
#if SANITY
      if (get_XTAG(y) != D_FLT32)
        ERR("BINDBL 1");
#endif  /* SANITY */
      yf = GETFLTVALUE(y);
      p = FUN(TOP(1));
      POP(3);
      n = TOP(-1);
    binflt:
      switch (get_XTAG(p)) {
      case D_IND:   p = get_IND(p); goto binflt;
      case T_FADD:  rf = xf + yf; break;
      case T_FSUB:  rf = xf - yf; break;
      case T_FMUL:  rf = xf * yf; break;
      case T_FDIV:  rf = xf / yf; break;

      case T_FEQ:   GOBOOL(xf == yf);
      case T_FNE:   GOBOOL(xf != yf);
      case T_FLT:   GOBOOL(xf <  yf);
      case T_FLE:   GOBOOL(xf <= yf);
      case T_FGT:   GOBOOL(xf >  yf);
      case T_FGE:   GOBOOL(xf >= yf);

      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("BINFLT");
      }
      SETFLT(n, rf);
      RET;

    case T_UNFLT1:
      /* The argument */
#if SANITY
      if (get_XTAG(n) != D_FLT32)
        ERR("UNFLT 0");
#endif
      xf = GETFLTVALUE(n);
      p = FUN(TOP(1));
      POP(2);
      n = TOP(-1);
    unflt:
      switch (get_XTAG(p)) {
      case D_IND:   p = get_IND(p); goto unflt;
      case T_FNEG:  rf = -xf; break;
      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("UNFLT");
      }
      SETFLT(n, rf);
      RET;
#endif  /* WANT_FLOAT32 */

#if WANT_FLOAT64
    case T_BINDBL2:
      n = ARG(TOP(1));
      TOP(0) = combBINDBL1;
      GOTOP(n);

    case T_BINDBL1:
      /* First argument */
#if SANITY
      if (get_XTAG(n) != D_DBL)
        ERR("BINDBL 0");
#endif  /* SANITY */
      xd = GETDBLVALUE(n);
      /* Second argument */
      y = ARG(TOP(2));
      while (get_XTAG(y) == D_IND)
        y = get_IND(y);
#if SANITY
      if (get_XTAG(y) != D_DBL)
        ERR("BINDBL 1");
#endif  /* SANITY */
      yd = GETDBLVALUE(y);
      p = FUN(TOP(1));
      POP(3);
      n = TOP(-1);
    bindbl:
      switch (get_XTAG(p)) {
      case D_IND:   p = get_IND(p); goto bindbl;
      case T_DADD:  rd = xd + yd; break;
      case T_DSUB:  rd = xd - yd; break;
      case T_DMUL:  rd = xd * yd; break;
      case T_DDIV:  rd = xd / yd; break;

      case T_DEQ:   GOBOOL(xd == yd);
      case T_DNE:   GOBOOL(xd != yd);
      case T_DLT:   GOBOOL(xd <  yd);
      case T_DLE:   GOBOOL(xd <= yd);
      case T_DGT:   GOBOOL(xd >  yd);
      case T_DGE:   GOBOOL(xd >= yd);

      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("BINDBL");
      }
      SETDBL(n, rd);
      RET;

    case T_UNDBL1:
      /* The argument */
#if SANITY
      if (get_XTAG(n) != D_DBL)
        ERR("UNDBL 0");
#endif
      xd = GETDBLVALUE(n);
      p = FUN(TOP(1));
      POP(2);
      n = TOP(-1);
    undbl:
      switch (get_XTAG(p)) {
      case D_IND:   p = get_IND(p); goto undbl;
      case T_DNEG:  rd = -xd; break;
      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("UNDBL");
      }
      SETDBL(n, rd);
      RET;
#endif  /* WANT_FLOAT64 */

    case T_BINBS2:
      n = ARG(TOP(1));
      TOP(0) = combBINBS1;
      GOTOP(n);

    case T_BINBS1:
      /* First argument */
#if SANITY
      if (get_XTAG(n) != D_FORPTR || FORPTR(n)->finalizer->fptype != FP_BSTR)
        ERR("BINBS 0");
#endif  /* SANITY */
      xbs = BSTR(n);
      /* Second argument */
      y = ARG(TOP(2));
      while (get_XTAG(y) == D_IND)
        y = get_IND(y);
#if SANITY
      if (get_XTAG(y) != D_FORPTR || FORPTR(y)->finalizer->fptype != FP_BSTR)
        ERR("BINBS 1");
#endif  /* SANITY */
      ybs = BSTR(y);
      p = FUN(TOP(1));
      POP(3);
      n = TOP(-1);
    binbs:
      switch (get_XTAG(p)) {
      case D_IND:    p = get_IND(p); goto binbs;

      case T_BSAPPEND: rbs = bsappend(xbs, ybs); break;
      case T_BSAPPENDDOT: rbs = bsappenddot(xbs, ybs); break;
      case T_BSEQ:   GOBOOL(bscompare(xbs, ybs) == 0);
      case T_BSNE:   GOBOOL(bscompare(xbs, ybs) != 0);
      case T_BSLT:   GOBOOL(bscompare(xbs, ybs) <  0);
      case T_BSLE:   GOBOOL(bscompare(xbs, ybs) <= 0);
      case T_BSGT:   GOBOOL(bscompare(xbs, ybs) >  0);
      case T_BSGE:   GOBOOL(bscompare(xbs, ybs) >= 0);
      case T_BSCMP:  r = bscompare(xbs, ybs); GOIND(r < 0 ? combLT : r > 0 ? combGT : combEQ);

      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("BINBS");
      }
      SETBSTR(n, mkForPtrFree(rbs));
      RET;

  default:
    ERR1("evalprimret %s", TAGNAME(tag));
  }
}

#undef RET
#undef GOTOP
#undef GOIND
#undef GOAP
#undef GOAP2
#undef GOPAIR
#undef GOPAIRUNIT

/*
 * evali keeps the hot globals in locals of the same name, so the C compiler
 * can keep them in registers: stack_ptr, glob_slice, bump_ptr, and bump_end,
 * and the ones it only reads: stack, stack_size, and (with BUMP_ALLOC, where
 * only refills and GC change it) num_free.  Inside evali the names refer to
 * the locals, so PUSH, TOP, CHKARGn, GCCHECK, etc. use them; the functions
 * below reach the globals.  EV_SAVE writes the locals back, and EV_LOAD reads them again.
 * The globals must be current whenever other code runs, so evali does EV_SAVE
 * before every call that can look at them, allocate, GC, or longjmp (an
 * exception or a thread switch), and EV_LOAD after the calls that return.
 * Allocation (new_ap) and GCCHECK have their own versions in evali.
 */
#if !defined(EV_CACHE)
/* The profiler looks at the globals from inside evali (prof_charge reads the
 * stack, new nodes get their source in alloc_node_ptr), so it goes without the
 * cache; it is slow anyway. */
#define EV_CACHE (!WANT_PROFILE)
#endif

#if EV_CACHE
static INLINE stackptr_t ev_get_stack_ptr(void) { return stack_ptr; }
static INLINE void ev_set_stack_ptr(stackptr_t s) { stack_ptr = s; }
static INLINE NODEPTR *ev_get_stack(void) { return stack; }
static INLINE stackptr_t ev_get_stack_size(void) { return stack_size; }
static INLINE int ev_get_glob_slice(void) { return glob_slice; }
static INLINE void ev_set_glob_slice(int s) { glob_slice = s; }
#if BUMP_ALLOC
static INLINE counter_t ev_get_num_free(void) { return num_free; }
static INLINE struct node *ev_get_bump_ptr(void) { return bump_ptr; }
static INLINE struct node *ev_get_bump_end(void) { return bump_end; }
static INLINE void ev_set_bump(struct node *p, struct node *e) { bump_ptr = p; bump_end = e; }
#define EV_SAVE_BUMP() ev_set_bump(bump_ptr, bump_end)
#define EV_LOAD_BUMP() do { bump_ptr = ev_get_bump_ptr(); bump_end = ev_get_bump_end(); num_free = ev_get_num_free(); } while(0)

/* new_ap allocating from evali's bump_ptr and bump_end, passed by reference,
 * and updating its num_free after a refill.  Always inlined, so they stay in
 * registers. */
static ALWAYS_INLINE NODEPTR
ev_new_ap(NODEPTR f, NODEPTR a, struct node **bpp, struct node **bep, counter_t *nfp)
{
  NODEPTR n;
  if (*bpp == *bep) {
    bump_refill();              /* sets the globals; the old run is used up */
    *bpp = bump_ptr;
    *bep = bump_end;
    *nfp = num_free;
  }
  n.node_ptr = (*bpp)++;
  FUN(n) = f;
  ARG(n) = a;
  return n;
}
#else  /* BUMP_ALLOC */
#define EV_SAVE_BUMP() do { } while(0)
#define EV_LOAD_BUMP() do { } while(0)
#endif  /* BUMP_ALLOC */
#define EV_SAVE() do { ev_set_stack_ptr(stack_ptr); ev_set_glob_slice(glob_slice); EV_SAVE_BUMP(); } while(0)
#define EV_LOAD() do { stack_ptr = ev_get_stack_ptr(); glob_slice = ev_get_glob_slice(); EV_LOAD_BUMP(); } while(0)

/* evali's own allocation and GC check, see above */
#if BUMP_ALLOC
#define new_ap(f, a) ev_new_ap((f), (a), &bump_ptr, &bump_end, &num_free)
#endif  /* BUMP_ALLOC */
#undef GCCHECK
#define GCCHECK(k) do { if (!((k) < num_free)) { EV_SAVE(); gc(); EV_LOAD(); } } while(0)
#define EV_RAISE(e) do { EV_SAVE(); raise_rts(e); } while(0)
#else  /* EV_CACHE */
#define EV_SAVE() do { } while(0)
#define EV_LOAD() do { } while(0)
#define EV_RAISE(e) raise_rts(e)
#endif  /* EV_CACHE */

/* Evaluate a node, returns when the node is in WHNF. */
NODEPTR
evali(NODEPTR an)
{
#if EV_CACHE
  /* The cached globals, see above. */
  stackptr_t stack_ptr = ev_get_stack_ptr();
  int glob_slice = ev_get_glob_slice();
  NODEPTR * const stack = ev_get_stack();
  const stackptr_t stack_size = ev_get_stack_size();
#if BUMP_ALLOC
  struct node *bump_ptr = ev_get_bump_ptr();
  struct node *bump_end = ev_get_bump_end();
  counter_t num_free = ev_get_num_free();
#endif  /* BUMP_ALLOC */
#endif  /* EV_CACHE */
  NODEPTR n = an;
  stackptr_t stk = stack_ptr;
#if WANT_PROFILE
  struct node *prof_saved = prof_cur; /* a strict primitive allocates after evaluating its arguments */
  uint32_t prof_ctx_saved = prof_ctx_cur;
#endif
  NODEPTR x, y, z, w;
  enum node_tag tag;

  //fprintf(stderr, "evali enter stk=%d\n", (int)stk);

#if MAXSTACKDEPTH
  counter_t old_cur_c_stack = cur_c_stack;
  if (++cur_c_stack > max_c_stack)
    max_c_stack = cur_c_stack;
#endif

/* Control macros for evali */
#define RET do { goto ret; } while(0)
#define GOIND(x) do { NODEPTR _x = (x); SETIND(n, _x); n = _x; goto top; } while(0)
#define GOAP(f,a) do { FUN(n) = (f); ARG(n) = (a); goto ap; } while(0)
#define GOAP2(f,a,b) do { FUN(n) = new_ap((f), (a)); ARG(n) = (b); goto ap2; } while(0)

 top:
  /*pp(stdout, an);*/
#if WANT_SAMPLE
  if (--sample_left == 0) {
    EV_SAVE();                  /* sample_node looks at the stack */
    sample_node(n);
  }
#endif
  if (--glob_slice <= 0) {
    EV_SAVE();
    yield();
    EV_LOAD();
  }
  for(;;) {
    if (is_VIRT(n)) {
      /* A virtual pointer, i.e., a combinator, primop, constructor, etc. */
      tag = get_VIRT_TAG(n);
      break;
    }
    NODEPTR f = FUN(n);
    if (is_AP(f)) {
      /* follow AP nodes down the spine */
      PROF_UNWIND(n);
      PUSH(n);
      n = f;
      continue;
    }
    tag = (enum node_tag)get_DATA_TAG(f);
    if (tag == D_IND) {
      /* Follow and short-circuit the chain of indirections. */
      NODEPTR on = n;
      do {
        n = ARG(n);
      } while (!is_VIRT(n) && get_DATA_TAG(FUN(n)) == D_IND);
      ARG(on) = n;              /* short-circuit; on is still an IND */
      continue;                 /* n can be an AP node */
    }
    break;
  }
  /* Invariant: at this point n=current node, tag=get_XTAG(n), and tag is not X_AP or D_IND */
#if WANT_PROFILE
  prof_charge(n, tag, stk);
#endif

  //printf("%s %d\n", tag_names[tag], (int)stack_ptr);
  //if (stack_ptr < -1)
  //  ERR("stack_ptr");
  switch (tag) {
    /* n has been overwritten by GOAP and friends, see prof_push */
  ap2:         PROF_OVERWRITE(n); PROF_UNWIND(n); PUSH(n); n = FUN(n);
  ap:
  case X_AP:   PROF_OVERWRITE(n); PROF_UNWIND(n); PUSH(n);
    n = FUN(n); goto top;

  case D_INT:    RET;
  case D_DBL:    RET;
  case D_INT64:  RET;
  case D_FLT32:  RET;
  case D_PTR:    RET;
  case D_FUNPTR: RET;
  case D_FORPTR: RET;
  case D_ARR:    RET;
  case D_THID:   RET;
  case D_MVAR:   RET;
  case D_WEAK:   RET;
  case D_BADDYN: ERR1("FFI unknown %s", CSTR(n));

  /*
   * Some of these reductions, (e.g., Z x y = K (x y)) are there to avoid
   * that increase in arity that some "optimizations" in Abstract.hs
   * stop reductions from happening.  This can be important for "full laziness".
   * In practice, these reductions almost never happen, but there they are anyway. :)
   */
  case T_S:    GCCHECK(2); CHKARG3; GOAP2(x, z, new_ap(y, z));                            /* S x y z = x z (y z) */
  case T_SS:   GCCHECK(3); CHKARG4; GOAP2(x, new_ap(y, w), new_ap(z, w));                 /* S' x y z w = x (y w) (z w) */
  case T_K:                CHKARG2; GOIND(x);                                             /* K x y = *x */
  case T_A:                CHKARG2; GOIND(y);                                             /* A x y = *y */
  case T_U:                CHKARG2; GOAP(y, x);                                           /* U x y = y x */
  case T_I:                CHKARG1; GOIND(x);                                             /* I x = *x */
  case T_Y:                CHKARG1; GOAP(x, n);                                           /* n@(Y x) = x n */
  case T_B:    GCCHECK(1); CHKARG3; GOAP(x, new_ap(y, z));                                /* B x y z = x (y z) */
  case T_BB:   if (!HASNARGS(4)) {
               GCCHECK(1); CHKARG2; COUNT(red_bb); GOAP(combB, new_ap(x, y)); } else {    /* B' x y = B (x y) */
               GCCHECK(2); CHKARG4; GOAP2(x, y, new_ap(z, w)); }                          /* B' x y z w = x y (z w) */
  case T_Z:    if (!HASNARGS(3)) {
               GCCHECK(1); CHKARG2; COUNT(red_z); GOAP(combK, new_ap(x, y)); } else {     /* Z x y = K (x y) */
                           CHKARG3; GOAP(x, y); }                                         /* Z x y z = x y */
  t_c:
  case T_C:    GCCHECK(1); CHKARG3; GOAP2(x, z, y);                                       /* C x y z = x z y */
  case T_CC:   GCCHECK(2); CHKARG4; GOAP2(x, new_ap(y, w), z);                            /* C' x y z w = x (y w) z */
  t_p:
  case T_P:    GCCHECK(1); CHKARG3; GOAP2(z, x, y);                                       /* P x y z = z x y */
  case T_R:    if(!HASNARGS(3)) {
               GCCHECK(1); CHKARG2; COUNT(red_r); GOAP2(combC, y, x); } else {            /* R x y = C y x */
               GCCHECK(1); CHKARG3; GOAP2(y, z, x); }                                     /* R x y z = y z x */
  case T_O:    GCCHECK(1); CHKARG4; GOAP2(w, x, y);                                       /* O x y z w = w x y */
  case T_K2:   if (!HASNARGS(3)) {
                           CHKARG2; COUNT(red_k2); GOAP(combK, x); } else {               /* K2 x y = K x */
                           CHKARG3; GOIND(x); }                                           /* K2 x y z = *x */
  case T_K3:   if (!HASNARGS(4)) {
                           CHKARG2; COUNT(red_k3); GOAP(combK2, x); } else {              /* K3 x y = K2 x */
                           CHKARG4; GOIND(x); }                                           /* K3 x y z w = *x */
  case T_K4:   if (!HASNARGS(5)) {
                           CHKARG2; COUNT(red_k4); GOAP(combK3, x); } else {              /* K4 x y = K3 x */
                           CHKARG5; GOIND(x); }                                           /* K4 x y z w v = *x */
  case T_CCB:  if (!HASNARGS(4)) {
               GCCHECK(2); CHKARG3; COUNT(red_ccb); GOAP2(combB, new_ap(x, z), y);} else{ /* C'B x y z = B (x z) y */
               GCCHECK(2); CHKARG4; GOAP2(x, z, new_ap(y, w)); }                          /* C'B x y z w = x z (y w) */

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
     * If we switch threads at any time during this the BININT1/2 on top of the stack will be lost,
     * but this doesn't matter; the operation is there and we will find it and redo BININT2 again.
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
  case T_UADD:
  case T_USUB:
  case T_UMUL:
  case T_UQUOT:
  case T_UREM:
  case T_USUBR:
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
    if (get_XTAG(n) == D_INT) {
      n = ARG(TOP(0));
      PUSH(combBININT1);
      if (get_XTAG(n) == D_INT)
        goto binint1;
    } else {
      PUSH(combBININT2);
    }
    goto top;

  case T_NEG:
  case T_UNEG:
  case T_INV:
  case T_POPCOUNT:
  case T_CLZ:
  case T_CTZ:
    CHECK(1);
    n = ARG(TOP(0));
    PUSH(combUNINT1);
    goto top;

  case T_SEQ:  CHECK(2); EV_SAVE(); evali(ARG(TOP(0))); EV_LOAD(); POP(2); n = TOP(-1); y = ARG(n); GOIND(y); /* seq x y = eval(x); y */

  case T_IO_BIND:
    goto t_c;
  case T_IO_RETURN:
    goto t_p;
  case T_IO_THEN:
    GCCHECK(2);
    CHKARG2;
    GOAP2(combIOBIND, x, new_ap(combK, y));

  /* The scrutinee is the last argument:
   *   CASED_m f0 ... f{m-1} e
   *   CASES_m d A_k1_n1 f1 ... A_km_nm fm e
   * Push the virtual LOOKx_m as a continuation marker and evaluate e.
   * The arms stay where they are on the stack.
   * Nothing is allocated, and no heap node is written.
   */
  case V_CASED:
    {
      int m = get_CASED_arity(n)->m;
      CHECK(m + 1);
      n = ARG(TOP(m));
      PUSH(mk_CASE(V_LOOKD, m));
      goto top;
    }
  case V_CASES:
    {
      int m = get_CASE_size(n);
      CHECK(2*m + 2);
      n = ARG(TOP(2*m + 1));
      PUSH(mk_CASE(V_LOOKS, m));
      goto top;
    }
  case V_LOOKD:
  case V_LOOKS:
  case V_ARM:
    RET;
  case V_CONSTR:
    {
      int k, m, nn;
      get_CON_info(n, &k, &nn);
      /* Stack layout when there is a case waiting:
       *   TOP(0) .. TOP(nn-1)   constructor spine, ARG(TOP(i)) is field i
       *   TOP(nn)               LOOKx_m marker
       *   TOP(nn+1) ...         the case spine, the last one is the case redex
       */
      CHECK(nn+1);
      NODEPTR p = TOP(nn);
      if (!is_VIRT(p))
        ERR("CONSTR: no LOOK");
      m = get_CASE_size(p);
      enum node_tag ltag = get_VIRT_TAG(p);
      NODEPTR fk;
      int fidx;                 /* stack index of the selected arm */
      int npop;                 /* number of stack entries to pop, the last one is the case redex */
      if (ltag == V_LOOKD) {
        /* CONSTR_k_n e1 ... en LOOKD_m (CASED_m f0 ... f{m-1} _)  -->  fk e1 ... en */
        fidx = nn + 1 + k;
        npop = nn + 2 + m;
      } else if (ltag == V_LOOKS) {
        /* CONSTR_k_n e1 ... en LOOKS_m (CASES_m d A_k1_n1 f1 ... A_km_nm fm _)  -->  fk e1 ... en
         *                                                                       -->  d, when no matching k
         * The arms are sorted by constructor number, so use binary search.
         */
        int lo = 0, hi = m;     /* search arms [lo, hi) */
        npop = nn + 3 + 2*m;
        for (;;) {
          if (lo >= hi) {
            /* Not found, use the default, which takes no arguments. */
            fk = ARG(TOP(nn + 1));
            POP(npop);
            n = TOP(-1);
            GOIND(fk);
          }
          int mid = (lo + hi) / 2;
          NODEPTR a = ARG(TOP(nn + 2 + 2*mid));
#if 0
          num_arm++;
#endif
          if (!is_VIRT(a) || get_VIRT_TAG(a) != V_ARM) {
#if 0
            num_bad_arm++;
#endif
            EV_SAVE();
            a = evali(a);
            EV_LOAD();
            if (!is_VIRT(a) || get_VIRT_TAG(a) != V_ARM)
              ERR("LOOKS, bad table");
          }
          int ki, ni;
          get_CON_info(a, &ki, &ni);
          if (ki == k) {
            fidx = nn + 3 + 2*mid;
            break;
          } else if (ki < k) {
            lo = mid + 1;
          } else {
            hi = mid;
          }
        }
      } else {
        ERR1("CONSTR: no LOOK, found %s", TAGNAME(ltag));
      }
      if (nn == 0) {
        fk = ARG(TOP(fidx));
        POP(npop);
        n = TOP(-1);
        GOIND(fk);
      }
      /* Build fk e1 ... e{n-1}, and overwrite the case redex with (fk e1 ... e{n-1}) en.
       * This allocates n-1 nodes, and needs no indirection. */
      GCCHECK(nn - 1);
      fk = ARG(TOP(fidx));
      for (int i = 0; i < nn - 1; i++)
        fk = new_ap(fk, ARG(TOP(i)));
      NODEPTR en = ARG(TOP(nn - 1));
      POP(npop);
      n = TOP(-1);
      GOAP(fk, en);
    }

  default:
    {
      /* Every other primitive */
      EV_SAVE();
      struct prim_res pr = evalprimop(n, tag, stk);
      EV_LOAD();
      n = pr.n;
      if (pr.next == PRIM_TOP)
        goto top;
      goto ret;
    }
  }


 ret:
  if (stack_ptr != stk) {
    // In this case, n was an AP that got pushed and potentially
    // updated.
    uvalue_t xu, yu, ru;
    NODEPTR p;

    tag = get_XTAG(TOP(0));
    switch (tag) {
    case T_BININT2:
      n = ARG(TOP(1));
      TOP(0) = combBININT1;
      goto top;

    case T_BININT1:
      /* First argument */
#if SANITY
      if (get_XTAG(n) != D_INT)
        ERR("BININT 0");
#endif  /* SANITY */
    binint1:
      xu = (uvalue_t)GETVALUE(n);
      /* Second argument */
      y = ARG(TOP(2));
      while (get_XTAG(y) == D_IND)
        y = get_IND(y);
#if SANITY
      if (get_XTAG(y) != D_INT)
        ERR("BININT 1");
#endif  /* SANITY */
      yu = (uvalue_t)GETVALUE(y);
      p = FUN(TOP(1));
      POP(3);
      n = TOP(-1);
    binint:
      switch (get_XTAG(p)) {
      case D_IND:   p = get_IND(p); goto binint;
      case T_ADD:   ADD_OVERFLOW(value_t, ru, xu, yu); break;
      case T_SUB:   SUB_OVERFLOW(value_t, ru, xu, yu); break;
      case T_MUL:   MUL_OVERFLOW(value_t, ru, xu, yu); break;
      case T_SUBR:  SUB_OVERFLOW(value_t, ru, yu, xu); break;
      case T_QUOT:  if (yu == 0)
                      EV_RAISE(exn_dividebyzero);
                    else if ((value_t)xu == VALUE_MIN && (value_t)yu == -1)
                      EV_RAISE(exn_overflow);
                    else
                      ru = (uvalue_t)((value_t)xu / (value_t)yu);
                    break;
      case T_REM:   if (yu == 0)
                      EV_RAISE(exn_dividebyzero);
                    else if ((value_t)yu == -1) /* MIN % -1 traps on x86 */
                      ru = 0;
                    else
                      ru = (uvalue_t)((value_t)xu % (value_t)yu);
                    break;
      case T_UADD:  ru = xu + yu; break;
      case T_USUB:  ru = xu - yu; break;
      case T_UMUL:  ru = xu * yu; break;
      case T_USUBR: ru = yu - xu; break;
      case T_UQUOT: if (yu == 0)
                      EV_RAISE(exn_dividebyzero);
                    else
                      ru = xu / yu;
                    break;
      case T_UREM:  if (yu == 0)
                      EV_RAISE(exn_dividebyzero);
                    else
                      ru = xu % yu;
                    break;
      case T_AND:   ru = xu & yu; break;
      case T_OR:    ru = xu | yu; break;
      case T_XOR:   ru = xu ^ yu; break;
      case T_SHL:   ru = xu << yu; break;
      case T_SHR:   ru = xu >> yu; break;
      case T_ASHR:  ru = (uvalue_t)((value_t)xu >> yu); break;

      case T_EQ:    GOBOOL(xu == yu);
      case T_NE:    GOBOOL(xu != yu);
      case T_ULT:   GOBOOL(xu <  yu);
      case T_ULE:   GOBOOL(xu <= yu);
      case T_UGT:   GOBOOL(xu >  yu);
      case T_UGE:   GOBOOL(xu >= yu);
      case T_UCMP:  GOIND(xu <  yu ? combLT   : xu > yu ? combGT : combEQ);
      case T_LT:    GOBOOL((value_t)xu <  (value_t)yu);
      case T_LE:    GOBOOL((value_t)xu <= (value_t)yu);
      case T_GT:    GOBOOL((value_t)xu >  (value_t)yu);
      case T_GE:    GOBOOL((value_t)xu >= (value_t)yu);
      case T_ICMP:  GOIND((value_t)xu <  (value_t)yu ? combLT   : (value_t)xu > (value_t)yu ? combGT : combEQ);

      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("BININT");
      }
      SETINT(n, (value_t)ru);
      goto ret;

    case T_UNINT1:
      /* The argument */
#if SANITY
      if (get_XTAG(n) != D_INT)
        ERR("UNINT 0");
#endif
      xu = (uvalue_t)GETVALUE(n);
      p = FUN(TOP(1));
      POP(2);
      n = TOP(-1);
    unint:
      switch (get_XTAG(p)) {
      case D_IND:      p = get_IND(p); goto unint;
      case T_NEG:
#if WANT_OVERFLOW
                       if ((value_t)xu == VALUE_MIN) EV_RAISE(exn_overflow);
#endif
                       ru = -xu; break;
      case T_UNEG:     ru = -xu; break;
      case T_INV:      ru = ~xu; break;
      case T_POPCOUNT: ru = POPCOUNT(xu); break;
      case T_CLZ:      ru = CLZ(xu); break;
      case T_CTZ:      ru = CTZ(xu); break;
      default:
        //fprintf(stderr, "tag=%d\n", get_XTAG(FUN(TOP(0))));
        ERR("UNINT");
      }
      SETINT(n, (value_t)ru);
      goto ret;

#if WANT_INT64
    case T_BININT64_2:
    case T_BININT64_1:
    case T_UNINT64_1:
#endif  /* WANT_INT64 */
#if WANT_FLOAT32
    case T_BINFLT2:
    case T_BINFLT1:
    case T_UNFLT1:
#endif  /* WANT_FLOAT32 */
#if WANT_FLOAT64
    case T_BINDBL2:
    case T_BINDBL1:
    case T_UNDBL1:
#endif  /* WANT_FLOAT64 */
    case T_BINBS2:
    case T_BINBS1:
      {
        EV_SAVE();
        struct prim_res pr = evalprimret(n, tag);
        EV_LOAD();
        n = pr.n;
        if (pr.next == PRIM_TOP)
          goto top;
        goto ret;
      }

    default:
      stack_ptr = stk;
      n = TOP(-1);
    }
  }
#if MAXSTACKDEPTH
  cur_c_stack = old_cur_c_stack; /* reset rather than counting down, in case of longjump */
#endif
#if WANT_PROFILE
  prof_cur = prof_saved;
  prof_ctx_cur = prof_ctx_saved;
#endif
  //fprintf(stderr, "evali exit stack_ptr=%d\n", (int)stack_ptr);
#if EXTRASANITY
  if (!is_whnf(n))
    ERR("evali: the result is not in WHNF");
#endif
  EV_SAVE();
  return n;
}

/* Back to the global versions */
#if EV_CACHE
#if BUMP_ALLOC
#undef new_ap
#endif  /* BUMP_ALLOC */
#undef GCCHECK
#define GCCHECK(n) gc_check((n))
#endif  /* EV_CACHE */

static char *progname = "?";

NORETURN void
die_exn(NODEPTR exn)
{
  /* No handler:
   * First convert the exception to a string by calling displaySomeException.
   * The display function compiles to combShowExn, so we need to build
   * (combShowExn exn) and evaluate it.
   */
  NODEPTR x;
  const char *msg;

  in_raise = true;

  if (get_XTAG(exn) == D_INT) {
    /* This is the special hack for RTS generated exception, represented by a D_INT */
    enum rts_exn eexn = GETVALUE(exn);
    static const char *msgs[] =
      { "stack overflow",
        "heap overflow",
        "thread killed",
        "user interrupt",
        "DivideByZero",
        "blocked MVar",
        "blocked STM",
        "arithmetic overflow",
        "Serialize",
        "Deserialize",
      };
    if (eexn < 0 || eexn >= exn_last) {
      msg = "unknown";
    } else {
      msg = msgs[eexn];
    }
  } else {
    /* just overwrite the top stack element, we don't need it */
    CLEARSTK();
    GCCHECK(1);
    PUSH(new_ap(combShowExn, exn));  /* TOP(0) = (combShowExn exn) */
    x = evali(TOP(0));               /* evaluate it */
    msg = evalstring(x).bs_array;    /* and convert to a C string */
    POP(1);
  }
#if WANT_STDIO
  /* A horrible hack until we get proper exceptions */
  if (strcmp(msg, "ExitSuccess") == 0) {
    EXIT(0);
  } else if (strncmp(msg, "ExitFailure ", 12) == 0) {
    EXIT(atoi(msg + 12));
  } else {
    fprintf(stderr, "\n%s: uncaught exception: %s\n", progname, msg);
    EXIT(1);
  }
#else  /* WANT_STDIO */
  ERR1("mhs error: %s", msg);
#endif  /* WANT_STDIO */
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

#if WANT_PROFILE
/* The program has been read: the cells after the last label are the main
 * expression.  Then read the names of the labels, which mhs --profile puts
 * after the program:
 *   #names
 *   label name
 *   ...
 * Other runtimes stop reading at the end of the program. */
static void
prof_end_load(BFILE *bf)
{
  int c;
  heapoffs_t l;
  size_t len, cap = 256;
  char *buf;

  prof_set_label(PROF_MAIN);
  prof_loading = 0;
  while ((c = getb(bf)) == '\n' || c == '\r' || c == ' ')
    ;
  if (c != '#' || prof_nlabels == 0)
    return;
  while ((c = getb(bf)) >= 0 && c != '\n')   /* the rest of the #names line */
    ;
  prof_names = mmalloc(prof_nlabels * sizeof(char *));
  for (l = 0; l < prof_nlabels; l++)
    prof_names[l] = 0;
  buf = mmalloc(cap);
  while (c >= 0) {
    c = getb(bf);
    if (c < '0' || c > '9')
      break;
    for (l = 0; c >= '0' && c <= '9'; c = getb(bf))
      l = l * 10 + (c - '0');
    if (c != ' ')
      break;
    len = 0;
    while ((c = getb(bf)) >= 0 && c != '\n') {
      if (len + 1 >= cap)
        buf = mrealloc(buf, cap *= 2);
      buf[len++] = (char)c;
    }
    buf[len] = 0;
    if (l < prof_nlabels && !prof_names[l])
      prof_names[l] = strdup(buf);
  }
  FREE(buf);
}

#if WANT_STDIO
static counter_t *prof_key;     /* for prof_cmp */

static int
prof_cmp(const void *a, const void *b)
{
  counter_t x = prof_key[*(const heapoffs_t *)a], y = prof_key[*(const heapoffs_t *)b];
  return x < y ? 1 : x > y ? -1 : 0;
}

/* The reductions per primitive, see prof_prim.  Entry t <= T_LAST_TAG is the
 * tag t, entry T_LAST_TAG+1+i the foreign function i.  The counts add up to
 * all reductions: a value (an Int, a pointer, ...) also has a reduction when
 * it is returned. */
static void
prof_report_unwind(FILE *f, counter_t tunw)
{
  counter_t t[2] = { 0, 0 }, all = 0;
  for (int h = 0; h < PH_N; h++) {
    t[0] += prof_unw_head[h][0];
    t[1] += prof_unw_head[h][1];
  }
  all = t[0] + t[1];
  if (!all)
    all = 1;
  fprintf(f, "\nUnwinding steps by the head they lead to, and by the node unwound: built at\n");
  fprintf(f, "run time, or a cell of the loaded program (code).  %% of all unwinding steps.\n");
  fprintf(f, "  head                 built at run time       %%    loaded program       %%           total       %%\n");
  for (int h = 0; h < PH_N; h++) {
    counter_t r = prof_unw_head[h][0], l = prof_unw_head[h][1];
    if (!r && !l)
      continue;
    fprintf(f, "  %-14s %18"PRIcounter" %6.2f %17"PRIcounter" %6.2f %15"PRIcounter" %6.2f\n",
            prof_head_names[h], r, 100.0 * r / all, l, 100.0 * l / all, r + l, 100.0 * (r + l) / all);
  }
  fprintf(f, "  %-14s %18"PRIcounter" %6.2f %17"PRIcounter" %6.2f %15"PRIcounter" %6.2f\n",
          "total", t[0], 100.0 * t[0] / all, t[1], 100.0 * t[1] / all, t[0] + t[1], 100.0);
  if (prof_unw_since[0] || prof_unw_since[1] || t[0] + t[1] != tunw)
    fprintf(f, "  (%"PRIcounter" + %"PRIcounter" steps not followed by a dispatch; %"PRIcounter" unwinding steps in all)\n",
            prof_unw_since[0], prof_unw_since[1], tunw);
}

static void
prof_report_prims(FILE *f, counter_t tred)
{
  heapoffs_t np = T_LAST_TAG + 1 + prof_nffi, i;
  counter_t *uses = mmalloc(np * sizeof(counter_t));
  counter_t *part = mmalloc(np * sizeof(counter_t));
  heapoffs_t *order = mmalloc(np * sizeof(heapoffs_t));
  counter_t tuses = 0, tpart = 0, cum = 0;
  char buf[64];

  for (i = 0; i < np; i++) {
    counter_t r, p;
    if (i <= T_LAST_TAG) {
      r = prof_prim_red[i];
      p = prof_prim_partial[i];
      if (i == D_IO_CCALL)
        r = p = 0;              /* listed per foreign function */
    } else {
      r = prof_ffi_red[i - T_LAST_TAG - 1];
      p = prof_ffi_partial[i - T_LAST_TAG - 1];
    }
    uses[i] = r - p;
    part[i] = p;
    tuses += r - p;
    tpart += p;
    order[i] = i;
  }
  prof_key = uses;
  qsort(order, np, sizeof(heapoffs_t), prof_cmp);
  fprintf(f, "\nReductions by primitive.  A use is a reduction with enough arguments; a partial\n");
  fprintf(f, "one is the evaluation of a partial application.  Kinds: comb(inator), prim(op),\n");
  fprintf(f, "ffi (foreign function), case (constructors and case), value (returning a value).\n");
  fprintf(f, "           uses    %%red   cum%%          partial  kind   primitive\n");
  for (heapoffs_t j = 0; j < np; j++) {
    const char *kind, *name;
    i = order[j];
    if (!uses[i] && !part[i])
      continue;
    cum += uses[i];
    if (i > T_LAST_TAG) {
      kind = "ffi";
      snprintf(buf, sizeof buf, "^%s", FFI_IX(i - T_LAST_TAG - 1).ffi_name);
      name = buf;
    } else {
      if (i >= T_S && i <= T_CCB)
        kind = "comb";
      else if (i > T_FIRST_REAL_TAG)
        kind = "prim";
      else if (i >= V_CONSTR)
        kind = "case";
      else
        kind = "value";
      name = TAGNAME(i);
      if (!name) {
        snprintf(buf, sizeof buf, "tag%d", (int)i);
        name = buf;
      }
    }
    fprintf(f, "%15"PRIcounter" %6.2f %6.2f %16"PRIcounter"  %-5s  %s\n",
            uses[i], 100.0 * uses[i] / tred, 100.0 * cum / tred, part[i], kind, name);
  }
  fprintf(f, "%15"PRIcounter" %6.2f %6.2f %16"PRIcounter"  total\n",
          tuses, 100.0 * tuses / tred, 100.0 * cum / tred, tpart);
  FREE(uses); FREE(part); FREE(order);
}

/* Calls per definition: the unwinding steps through the root cell of each
 * definition, i.e., how many times it was applied (or evaluated).  A CAF is
 * overwritten by its value, so it counts about once.  A definition whose code
 * is a bare combinator, constructor, or primitive has no cell of its own, and
 * one that is just another definition shares that one's cell; both are left
 * out.  lab is the label + 1, as in prof_label. */
#define PROF_CALL_DEFS 100
static const char *prof_label_name(uint32_t lab, char *buf, size_t size);
static void
prof_report_calls(FILE *f, counter_t *red, counter_t *alc, heapoffs_t nl)
{
  static const counter_t lims[] = { 100000000, 10000000, 1000000, 100000, 10000, 1000, 100, 10, 1 };
  static const char *names[] = { ">= 100M", "10M-100M", "1M-10M", "100K-1M", "10K-100K", "1K-10K", "100-1K", "10-100", "1-10" };
  enum { NB = sizeof lims / sizeof lims[0] };
  heapoffs_t *order = mmalloc(nl * sizeof(heapoffs_t));
  counter_t *calls = mmalloc(nl * sizeof(counter_t));
  counter_t bcalls[NB];
  heapoffs_t bdefs[NB];
  heapoffs_t lab, n = 0, ncalled = 0, nnocell = 0, nalias = 0;
  counter_t tcalls = 0, cum = 0;
  char buf[64];

  for (int b = 0; b < NB; b++) { bcalls[b] = 0; bdefs[b] = 0; }
  for (lab = 0; lab < nl; lab++)
    calls[lab] = 0;
  for (lab = 1; lab <= prof_nlabels; lab++) {
    struct node *r = prof_roots ? prof_roots[lab - 1] : 0;
    if (!r) {
      nnocell++;
      continue;
    }
    if (r->prof_label != lab) {
      nalias++;
      continue;
    }
    calls[lab] = r->prof_ncalls;
    order[n++] = lab;
    if (!calls[lab])
      continue;
    tcalls += calls[lab];
    ncalled++;
    for (int b = 0; b < NB; b++)
      if (calls[lab] >= lims[b]) {
        bdefs[b]++;
        bcalls[b] += calls[lab];
        break;
      }
  }
  if (!tcalls)
    tcalls = 1;
  prof_key = calls;
  qsort(order, n, sizeof(heapoffs_t), prof_cmp);

  fprintf(f, "\nCalls per definition, i.e., unwinding steps through the root cell of the definition.\n");
  fprintf(f, "%"PRIcounter" calls of %"PRIheap" definitions with a cell of their own, %"PRIheap" of which were called.\n",
          tcalls, n, ncalled);
  fprintf(f, "Left out: %"PRIheap" definitions that are a bare combinator, constructor, or primitive, and %"PRIheap" that are another definition.\n",
          nnocell, nalias);
  fprintf(f, "      calls per definition   definitions            calls  %%calls\n");
  for (int b = 0; b < NB; b++)
    fprintf(f, "%26s %13"PRIheap" %16"PRIcounter" %6.2f\n", names[b], bdefs[b], bcalls[b], 100.0 * bcalls[b] / tcalls);
  fprintf(f, "\nThe %d definitions with the most calls (reductions and cells are self costs, as above)\n", PROF_CALL_DEFS);
  fprintf(f, "          calls  %%calls   cum%%       reductions  red/call  cells/call  definition\n");
  for (heapoffs_t j = 0; j < n && j < PROF_CALL_DEFS; j++) {
    lab = order[j];
    if (!calls[lab])
      break;
    cum += calls[lab];
    fprintf(f, "%15"PRIcounter" %6.2f %6.2f %16"PRIcounter" %9.1f %11.1f  %s\n",
            calls[lab], 100.0 * calls[lab] / tcalls, 100.0 * cum / tcalls, red[lab],
            (double)red[lab] / calls[lab], (double)alc[lab] / calls[lab],
            prof_label_name((uint32_t)lab, buf, sizeof buf));
  }
  FREE(order);
  FREE(calls);
}

static int
prof_cmp_node(const void *a, const void *b)
{
  counter_t x = (*(struct node * const *)a)->prof_red, y = (*(struct node * const *)b)->prof_red;
  return x < y ? 1 : x > y ? -1 : 0;
}

static const char *
prof_label_name(uint32_t lab, char *buf, size_t size)
{
  if (lab == 0)
    return "<no label>";
  if (lab == PROF_MAIN)
    return "<main expression>";
  if (prof_names && lab - 1 < prof_nlabels && prof_names[lab - 1])
    return prof_names[lab - 1];
  snprintf(buf, size, "_%lu", (unsigned long)(lab - 1));
  return buf;
}

/* The description of a hot node, see prof_describe. */
#define PROF_DESC_SIZE 400
#define PROF_DESC_ARGS 24       /* arguments shown in an application */
#define PROF_DESC_DEPTH 3       /* nesting of the applications shown */
struct prof_text {
  char *s;
  size_t len, size;
};

static void
prof_puts(struct prof_text *t, const char *s)
{
  while (*s && t->len + 1 < t->size)
    t->s[t->len++] = *s++;
  t->s[t->len] = 0;
}

static NODEPTR
prof_skip_ind(NODEPTR n)
{
  while (!is_VIRT(n) && get_DATA_TAG(FUN(n)) == D_IND && !is_NIL(ARG(n)))
    n = ARG(n);
  return n;
}

static int
prof_is_root(struct node *p)
{
  uint32_t lab = p->prof_label;
  return prof_roots && lab != 0 && lab != PROF_MAIN && lab <= prof_nlabels && prof_roots[lab - 1] == p;
}

/* A loaded cell: the name of the definition for its root, otherwise
 * {definition+cell}, as in the table of loaded nodes. */
static void
prof_name_cell(struct prof_text *t, struct node *p)
{
  char buf[64];
  uint32_t lab = p->prof_label;
  heapoffs_t i = lab == PROF_MAIN ? prof_nlabels : (heapoffs_t)lab - 1;
  if (p == &prof_runtime) {
    prof_puts(t, "<runtime>");
    return;
  }
  if (prof_is_root(p)) {
    prof_puts(t, prof_label_name(lab, buf, sizeof buf));
    return;
  }
  prof_puts(t, "{");
  prof_puts(t, prof_label_name(lab, buf, sizeof buf));
  if (lab != 0 && prof_first && i <= prof_nlabels && prof_first[i]) {
    snprintf(buf, sizeof buf, "+%ld", (long)(p - prof_first[i]));
    prof_puts(t, buf);
  }
  prof_puts(t, "}");
}

/* A virtual node, or a data node. */
static void
prof_name_leaf(struct prof_text *t, NODEPTR n)
{
  char buf[64];
  enum node_tag tag;
  int k, nn;
  if (is_VIRT(n)) {
    tag = get_VIRT_TAG(n);
    switch (tag) {
    case V_CONSTR:
    case V_ARM:
      get_CON_info(n, &k, &nn);
      snprintf(buf, sizeof buf, "%c_%d_%d", tag == V_CONSTR ? 'C' : 'A', k, nn);
      break;
    case V_CASED:
      {
        struct case_arity *c = get_CASED_arity(n);
        prof_puts(t, "D");
        for (k = 0; k < c->m; k++) {
          snprintf(buf, sizeof buf, "_%d", (int)c->arities[k]);
          prof_puts(t, buf);
        }
        return;
      }
    case V_LOOKD:
    case V_CASES:
    case V_LOOKS:
      snprintf(buf, sizeof buf, "%c_%d", tag == V_LOOKD ? 'L' : tag == V_CASES ? 'E' : 'M', get_CASE_size(n));
      break;
    default:
      snprintf(buf, sizeof buf, "%s", tag <= T_LAST_TAG && TAGNAME(tag) ? TAGNAME(tag) : "?");
      break;
    }
  } else {
    tag = (enum node_tag)get_DATA_TAG(FUN(n));
    if (tag == D_INT) {
      snprintf(buf, sizeof buf, "#%lld", (long long)GETVALUE(n));
    } else if (tag == D_FORPTR && FORPTR(n)->finalizer->fptype == FP_BSTR) {
      struct bytestring bs = FORPTR(n)->payload;
      size_t j, m = 0;
      buf[m++] = '"';
      for (j = 0; j < bs.bs_size && m < 24; j++) {
        char c = ((const char *)bs.bs_array)[j];
        buf[m++] = c >= ' ' && c < 127 && c != '"' ? c : '?';
      }
      if (j < bs.bs_size)
        buf[m++] = '.', buf[m++] = '.';
      buf[m++] = '"';
      buf[m] = 0;
    } else {
      snprintf(buf, sizeof buf, "<%s>", tag <= T_LAST_TAG && TAGNAME(tag) ? TAGNAME(tag) : "?");
    }
  }
  prof_puts(t, buf);
}

/* Describe node n as an application, its head and arguments, with nested
 * applications to depth levels.  The head is a virtual node, a data node, or
 * a loaded cell of a definition, by name.  So for a node made at run time
 * the spine is followed until it reaches the loaded program, and for a
 * loaded node until it leaves its definition.  Only the node itself is
 * followed into the loaded program; a loaded argument is shown by name.
 * Returns 1 if the head is a loaded cell of another definition. */
static int
prof_describe(struct prof_text *t, NODEPTR n, int depth, int top)
{
  NODEPTR args[PROF_DESC_ARGS], q;
  struct node *p, *qp;
  uint32_t home;
  int na = 0, calls_def = 0;

  n = prof_skip_ind(n);
  if (is_VIRT(n) || !is_AP(FUN(n))) {
    prof_name_leaf(t, n);
    return 0;
  }
  p = NPTR(n);
  if (!top && PROF_LOADED(p)) {
    prof_name_cell(t, p);
    return 0;
  }
  if (depth <= 0) {
    prof_puts(t, "..");
    return 0;
  }
  home = PROF_LOADED(p) ? p->prof_label : 0;
  for (q = n; ; q = FUN(q)) {
    q = prof_skip_ind(q);
    if (is_VIRT(q) || !is_AP(FUN(q)))
      break;
    qp = NPTR(q);
    if (qp != p && PROF_LOADED(qp) && (home == 0 || qp->prof_label != home || prof_is_root(qp)))
      break;
    if (na == PROF_DESC_ARGS)
      break;
    args[na++] = ARG(q);
  }
  if (!top)
    prof_puts(t, "(");
  if (is_VIRT(q) || !is_AP(FUN(q))) {
    prof_name_leaf(t, q);
  } else if (na == PROF_DESC_ARGS) {
    prof_puts(t, "...");
  } else {
    prof_name_cell(t, NPTR(q));
    calls_def = 1;
  }
  while (na > 0) {
    prof_puts(t, " ");
    prof_describe(t, args[--na], depth - 1, 0);
  }
  if (!top)
    prof_puts(t, ")");
  return calls_def;
}

/* Fill in the entry for node p. */
static void
prof_describe_node(struct prof_dead *d, struct node *p)
{
  char buf[PROF_DESC_SIZE];
  struct prof_text t;
  NODEPTR n;
  t.s = buf;
  t.len = 0;
  t.size = sizeof buf;
  buf[0] = 0;
  n.node_ptr = p;
  d->ncalls = p->prof_ncalls;
  d->nred = p->prof_nred;
  d->nupd = p->prof_nupd;
  d->gc = 0;
  d->src = p->prof_src;
  d->loaded = PROF_LOADED(p);
  d->calls_def = prof_describe(&t, n, PROF_DESC_DEPTH, 1);
  d->desc = strdup(buf);
}

/* A loaded node is only interesting if it calls another definition, like
 * insert dOrdInt in the code of some definition, or has been rewritten, like
 * a CAF.  Otherwise it is code, and the counts per definition and the table
 * of loaded nodes cover it. */
#define PROF_INTERESTING(d) (!(d)->loaded || (d)->calls_def || (d)->nupd > 0)

/* At a stack overflow, show what the stack holds.  An application is
 * counted by the definition that built it, a virtual entry by its tag. */
static uint32_t
prof_stack_label(NODEPTR n)
{
  return NPTR(n)->prof_src ? NPTR(n)->prof_src->prof_label : 0;
}

void
prof_stack_dump(void)
{
  uint32_t nl = prof_nlabels + 2;
  counter_t *cnt = calloc(nl, sizeof(counter_t));
  counter_t vt[T_LAST_TAG+1];
  char buf[100];
  stackptr_t i;
  memset(vt, 0, sizeof vt);
  for (i = 0; i <= stack_ptr; i++) {
    NODEPTR n = stack[i];
    if (is_VIRT(n)) {
      vt[get_VIRT_TAG(n)]++;
    } else {
      uint32_t lab = prof_stack_label(n);
      uint32_t k = lab == PROF_MAIN ? nl - 1 : lab;
      if (k < nl) cnt[k]++;
    }
  }
  fprintf(stderr, "stack overflow with %ld entries.  Applications by the definition that built them:\n", (long)stack_ptr + 1);
  for (int r = 0; r < 20; r++) {
    uint32_t best = 0;
    for (uint32_t k = 1; k < nl; k++)
      if (cnt[k] > cnt[best]) best = k;
    if (cnt[best] == 0) break;
    fprintf(stderr, "  %10llu  %s\n", (unsigned long long)cnt[best],
            prof_label_name(best == nl - 1 ? PROF_MAIN : best, buf, sizeof buf));
    cnt[best] = 0;
  }
  fprintf(stderr, "Virtual entries:\n");
  for (int t = 0; t <= T_LAST_TAG; t++)
    if (vt[t])
      fprintf(stderr, "  %10llu  %s (%d)\n", (unsigned long long)vt[t], TAGNAME(t) ? TAGNAME(t) : "?", t);
  fprintf(stderr, "The top of the stack, repeats collapsed:\n");
  for (i = stack_ptr; i >= 0 && i > stack_ptr - 400; ) {
    NODEPTR n = stack[i];
    stackptr_t j = i;
    const char *nm;
    if (is_VIRT(n)) {
      int t = get_VIRT_TAG(n);
      while (j >= 0 && is_VIRT(stack[j]) && get_VIRT_TAG(stack[j]) == t) j--;
      nm = TAGNAME(t) ? TAGNAME(t) : "?";
    } else {
      uint32_t lab = prof_stack_label(n);
      while (j >= 0 && !is_VIRT(stack[j]) && prof_stack_label(stack[j]) == lab) j--;
      nm = prof_label_name(lab, buf, sizeof buf);
    }
    fprintf(stderr, "  %6ld x  %s\n", (long)(i - j), nm);
    i = j;
  }
  free(cnt);
}

#define PROF_HOT_NODES 200
#define PROF_CALLER_DEFS 20     /* the definitions whose callers are shown */
#define PROF_CALLERS 10         /* the callers and the call chains shown for each */
#define PROF_CHAIN 6            /* the callers shown in a call chain */
#define PROF_MORE 0xfffffffd    /* the chain continues */

/* Some callers of a context, the most recent first.  Unused entries are
 * PROF_TOP_LABEL, and PROF_MORE ends a chain that was cut off. */
struct prof_chain {
  uint32_t  lab[PROF_CHAIN + 1];
  counter_t red, alloc;
};

static int
prof_cmp_chain_lab(const void *a, const void *b)
{
  const uint32_t *x = ((const struct prof_chain *)a)->lab, *y = ((const struct prof_chain *)b)->lab;
  for (int k = 0; k <= PROF_CHAIN; k++)
    if (x[k] != y[k])
      return x[k] < y[k] ? -1 : 1;
  return 0;
}

static int
prof_cmp_chain_red(const void *a, const void *b)
{
  counter_t x = ((const struct prof_chain *)a)->red, y = ((const struct prof_chain *)b)->red;
  return x < y ? 1 : x > y ? -1 : 0;
}

/* Fill chs with the chains of depth callers for the contexts of label lab,
 * summed over equal chains and sorted by reductions.  Returns their number. */
static uint32_t
prof_chains(struct prof_chain *chs, uint32_t lab, int depth)
{
  uint32_t m = 0, c, a;
  int k;
  for (c = 1; c < prof_nctxs; c++) {
    if (prof_ctxs[c].label != lab)
      continue;
    struct prof_chain *ch = &chs[m++];
    for (k = 0; k <= PROF_CHAIN; k++)
      ch->lab[k] = PROF_TOP_LABEL;
    for (a = prof_ctxs[c].parent, k = 0; k < depth && a != 0; k++, a = prof_ctxs[a].parent)
      ch->lab[k] = prof_ctxs[a].label;
    if (a != 0 && depth > 1)
      ch->lab[k] = PROF_MORE;
    ch->red = prof_ctxs[c].red;
    ch->alloc = prof_ctxs[c].alloc;
  }
  qsort(chs, m, sizeof *chs, prof_cmp_chain_lab);
  uint32_t n = 0;
  for (uint32_t j = 0; j < m; j++) {
    if (n > 0 && prof_cmp_chain_lab(&chs[n-1], &chs[j]) == 0) {
      chs[n-1].red += chs[j].red;
      chs[n-1].alloc += chs[j].alloc;
    } else {
      chs[n++] = chs[j];
    }
  }
  qsort(chs, n, sizeof *chs, prof_cmp_chain_red);
  return n;
}

/* The callers of the definitions with the most reductions, see prof_push. */
static void
prof_report_callers(FILE *f, heapoffs_t *order, heapoffs_t nl)
{
  struct prof_chain *chs;
  uint32_t c, cnt, maxcnt = 0, n, j;
  char buf[64];
  int shown = 0, k;

  /* Count the contexts per label, to size chs. */
  counter_t *nctx = mmalloc(nl * sizeof(counter_t));
  for (heapoffs_t i = 0; i < nl; i++)
    nctx[i] = 0;
  for (c = 1; c < prof_nctxs; c++) {
    uint32_t lab = prof_ctxs[c].label;
    heapoffs_t i = lab == PROF_MAIN ? nl - 1 : lab;
    if (i < nl && ++nctx[i] > maxcnt)
      maxcnt = (uint32_t)nctx[i];
  }
  chs = mmalloc((maxcnt + 1) * sizeof(struct prof_chain));

  fprintf(f, "\n%u call contexts.\n", (unsigned)prof_nctxs);
  fprintf(f, "Callers of the definitions with the most reductions.  The caller is the\n");
  fprintf(f, "definition that built the application; in a call chain the most recent\n");
  fprintf(f, "caller comes first.  Recursion is cut back to the first call.\n");
  for (heapoffs_t jj = 0; jj < nl && shown < PROF_CALLER_DEFS; jj++) {
    heapoffs_t i = order[jj];
    uint32_t lab = i == nl - 1 ? PROF_MAIN : (uint32_t)i;
    counter_t self = 0;
    cnt = 0;
    for (c = 1; c < prof_nctxs; c++)
      if (prof_ctxs[c].label == lab) {
        self += prof_ctxs[c].red;
        cnt++;
      }
    if (!self)
      continue;
    shown++;
    fprintf(f, "\n%s: %"PRIcounter" reductions in %u contexts\n", prof_label_name(lab, buf, sizeof buf), self, (unsigned)cnt);
    for (int depth = 1; depth <= PROF_CHAIN; depth += PROF_CHAIN - 1) {
      n = prof_chains(chs, lab, depth);
      fprintf(f, "     reductions   %%self   cells allocated  %s\n", depth == 1 ? "caller" : "call chain");
      for (j = 0; j < n && j < PROF_CALLERS; j++) {
        fprintf(f, "%15"PRIcounter" %7.2f %17"PRIcounter"  ", chs[j].red, 100.0 * chs[j].red / self, chs[j].alloc);
        for (k = 0; k <= PROF_CHAIN; k++) {
          uint32_t l = chs[j].lab[k];
          if (l == PROF_TOP_LABEL) {
            if (k == 0)
              fprintf(f, "<top level>");
            break;
          }
          if (k > 0)
            fprintf(f, " <- ");
          fprintf(f, "%s", l == PROF_MORE ? "..." : prof_label_name(l, buf, sizeof buf));
        }
        fprintf(f, "\n");
      }
      if (n > PROF_CALLERS) {
        counter_t rest = 0;
        for (; j < n; j++)
          rest += chs[j].red;
        fprintf(f, "%15"PRIcounter" %7.2f %17s  (%u more)\n", rest, 100.0 * rest / self, "", (unsigned)(n - PROF_CALLERS));
      }
    }
  }
  FREE(chs);
  FREE(nctx);
}

#define PROF_HOT_REPORT 100     /* the hot nodes shown in each table */

static int
prof_cmp_dead(const void *a, const void *b)
{
  const struct prof_dead *x = a, *y = b;
  counter_t u = x->ncalls + x->nred, v = y->ncalls + y->nred;
  return u < v ? 1 : u > v ? -1 : 0;
}

/* One table of hot nodes: the kept dead ones in k, and the live ones, with
 * loaded nodes or not. */
static void
prof_report_hot_table(FILE *f, struct prof_keep *k, struct prof_dead *live, size_t nlive, int loaded,
                      counter_t tunw, counter_t tred)
{
  struct prof_dead *es = mmalloc((k->n + nlive + 1) * sizeof(struct prof_dead));
  char made[PROF_DESC_SIZE], status[32];
  struct prof_text t;
  size_t n = 0, j;

  for (j = 0; j < k->n; j++)
    es[n++] = k->es[j];
  for (j = 0; j < nlive; j++)
    if (live[j].loaded == loaded && PROF_INTERESTING(&live[j]))
      es[n++] = live[j];
  qsort(es, n, sizeof(struct prof_dead), prof_cmp_dead);
  fprintf(f, "          calls  %%unwind     reductions    %%red     rewrites  status   node    (%s)\n",
          loaded ? "in" : "made by");
  for (j = 0; j < n && j < PROF_HOT_REPORT; j++) {
    struct prof_dead *e = &es[j];
    t.s = made;
    t.len = 0;
    t.size = sizeof made;
    made[0] = 0;
    prof_name_cell(&t, e->src);
    if (e->gc)
      snprintf(status, sizeof status, "gc %"PRIcounter, e->gc);
    else
      snprintf(status, sizeof status, "live");
    fprintf(f, "%15"PRIcounter" %6.2f %15"PRIcounter" %6.2f %12"PRIcounter"  %-8s %s    (%s)\n",
            e->ncalls, tunw ? 100.0 * e->ncalls / tunw : 0.0,
            e->nred, tred ? 100.0 * e->nred / tred : 0.0,
            e->nupd, status, e->desc ? e->desc : "?", made);
  }
  FREE(es);
}

/* The nodes with the most counts, see prof_hot_add. */
static void
prof_report_hot(FILE *f, counter_t tunw, counter_t tred)
{
  struct prof_dead *live = mmalloc((prof_nhot + 1) * sizeof(struct prof_dead));
  size_t j;

  for (j = 0; j < prof_nhot; j++)
    prof_describe_node(&live[j], prof_hot[j]);
  fprintf(f, "\nHot nodes: the nodes with the most calls (unwinding steps through the node)\n");
  fprintf(f, "plus reductions (charged to the node as the innermost application on the\n");
  fprintf(f, "spine), counted per node while it lives.  Rewrites are reductions that\n");
  fprintf(f, "overwrite the node with an application, e.g., every iteration of a loop.  A\n");
  fprintf(f, "node that is not live was found dead (or, for a loaded node, unreachable) by\n");
  fprintf(f, "GC number N, and is shown as it was then.  In a node, {def+k} is cell k of the\n");
  fprintf(f, "code of definition def, as in the table of loaded nodes below, and .. is an\n");
  fprintf(f, "application that is not shown.  Only the nodes that reach %d counts are\n", (int)PROF_HOT_MIN);
  fprintf(f, "recorded, and only the %d dead ones of each kind with the most counts are\nkept.\n", (int)PROF_DEAD_KEEP);
  fprintf(f, "\nNodes made at run time, by the code in (made by).\n");
  prof_report_hot_table(f, &prof_dead_rt, live, prof_nhot, 0, tunw, tred);
  fprintf(f, "\nLoaded nodes that apply another definition, or have been rewritten (a CAF),\n");
  fprintf(f, "in the code of definition (in).\n");
  prof_report_hot_table(f, &prof_dead_ld, live, prof_nhot, 1, tunw, tred);
  for (j = 0; j < prof_nhot; j++)
    FREE(live[j].desc);
  FREE(live);
}

/* Write the profile to PROGRAM.prof, see prof_charge.  The counts of the loaded
 * nodes are summed per definition; then come the loaded nodes with the most
 * reductions.  Runs when the program exits (atexit). */
static void
prof_report(void)
{
  heapoffs_t nl = prof_nlabels + 2;  /* index 0: no label, l+1: label l, nl-1: main */
  counter_t *red = mmalloc(nl * sizeof(counter_t));
  counter_t *unw = mmalloc(nl * sizeof(counter_t));
  counter_t *alc = mmalloc(nl * sizeof(counter_t));
  struct node **start = mmalloc(nl * sizeof(struct node *));
  heapoffs_t *order = mmalloc(nl * sizeof(heapoffs_t));
  heapoffs_t i, nhot = 0;
  counter_t tred, tunw, talc, cum;
  struct node **hot;
  char fname[1024], buf[64];
  const char *base;
  FILE *f;

  for (i = 0; i < nl; i++) {
    red[i] = unw[i] = alc[i] = 0;
    start[i] = 0;
    order[i] = i;
  }
  for (struct node *p = prof_lo; p < prof_hi; p++) {
    i = p->prof_label == PROF_MAIN ? nl - 1 : p->prof_label;
    red[i] += p->prof_red;
    unw[i] += p->prof_unwind;
    alc[i] += p->prof_alloc;
    if (!start[i])
      start[i] = p;
    if (p->prof_red)
      nhot++;
  }
  tred = prof_runtime.prof_red;
  tunw = prof_runtime.prof_unwind;
  talc = prof_runtime.prof_alloc;
  for (i = 0; i < nl; i++) {
    tred += red[i];
    tunw += unw[i];
    talc += alc[i];
  }
  if (!tred)
    tred = 1;

  base = strrchr(progname, '/');
  base = base ? base + 1 : progname;
  snprintf(fname, sizeof fname, "%s.prof", *base && strcmp(base, "?") ? base : "mhs");
  f = fopen(fname, "w");
  if (!f) {
    fprintf(stderr, "cannot write profile %s\n", fname);
    return;
  }
  fprintf(f, "Profile of %s\n", progname);
  fprintf(f, "%15"PRIcounter" reductions (num_reductions %"PRIcounter")\n", tred, num_reductions);
  fprintf(f, "%15"PRIcounter" unwinding steps\n", tunw);
  fprintf(f, "%15"PRIcounter" cells allocated\n", talc);
  fprintf(f, "%15"PRIcounter" reductions, %"PRIcounter" cells allocated by the runtime itself\n",
          prof_runtime.prof_red, prof_runtime.prof_alloc);
  fprintf(f, "Self costs; a closure is charged to the definition that created it.\n\n");

  prof_key = red;
  qsort(order, nl, sizeof(heapoffs_t), prof_cmp);
  fprintf(f, "     reductions    %%red   cum%%     unwinding steps   cells allocated  definition\n");
  cum = 0;
  for (heapoffs_t j = 0; j < nl; j++) {
    i = order[j];
    if (!red[i] && !unw[i] && !alc[i])
      continue;
    cum += red[i];
    fprintf(f, "%15"PRIcounter" %6.2f %6.2f %19"PRIcounter" %17"PRIcounter"  %s\n",
            red[i], 100.0 * red[i] / tred, 100.0 * cum / tred, unw[i], alc[i],
            prof_label_name(i == nl - 1 ? PROF_MAIN : (uint32_t)i, buf, sizeof buf));
  }

  prof_report_unwind(f, tunw);
  prof_report_prims(f, tred);
  prof_report_calls(f, red, alc, nl);
  prof_report_callers(f, order, nl);
  prof_report_hot(f, tunw, tred);

  /* The loaded nodes with the most reductions */
  hot = mmalloc((nhot + 1) * sizeof(struct node *));
  nhot = 0;
  for (struct node *p = prof_lo; p < prof_hi; p++)
    if (p->prof_red)
      hot[nhot++] = p;
  qsort(hot, nhot, sizeof(struct node *), prof_cmp_node);
  fprintf(f, "\nLoaded nodes with the most reductions (the node is the combinator applied to its first argument)\n");
  fprintf(f, "     reductions    %%red  node   definition+cell\n");
  for (heapoffs_t j = 0; j < nhot && j < PROF_HOT_NODES; j++) {
    struct node *p = hot[j];
    NODEPTR n, fn;
    const char *what;
    n.node_ptr = p;
    fn = FUN(n);
    if (is_VIRT(fn))
      what = TAGNAME(get_VIRT_TAG(fn));
    else if (is_AP(fn))
      what = "@";
    else
      what = TAGNAME(get_DATA_TAG(fn));
    i = p->prof_label == PROF_MAIN ? nl - 1 : p->prof_label;
    fprintf(f, "%15"PRIcounter" %6.2f  %-6s %s+%ld\n",
            p->prof_red, 100.0 * p->prof_red / tred, what ? what : "?",
            prof_label_name(p->prof_label, buf, sizeof buf), (long)(p - start[i]));
  }
  fclose(f);
  if (verbose)
    PRINT("profile written to %s\n", fname);
  FREE(red); FREE(unw); FREE(alc); FREE(start); FREE(order); FREE(hot);
}
#endif  /* WANT_STDIO */

/* A node on the hot list is dead (or, for a loaded node, unreachable): keep
 * it, with a description, if it is interesting and among the PROF_DEAD_KEEP
 * dead nodes of its kind with the most counts.  The description is only
 * needed for the report, which needs stdio. */
static void
prof_retire(struct node *p)
{
#if WANT_STDIO
  struct prof_keep *k = PROF_LOADED(p) ? &prof_dead_ld : &prof_dead_rt;
  struct prof_dead e, *d;
  if (k->n == PROF_DEAD_KEEP && p->prof_ncalls + p->prof_nred <= k->es[k->min].ncalls + k->es[k->min].nred)
    return;
  prof_describe_node(&e, p);
  e.gc = num_gc;
  if (!PROF_INTERESTING(&e)) {
    FREE(e.desc);
    return;
  }
  if (k->n < PROF_DEAD_KEEP) {
    if (!k->es)
      k->es = mmalloc(PROF_DEAD_KEEP * sizeof(struct prof_dead));
    d = &k->es[k->n++];
  } else {
    d = &k->es[k->min];
    FREE(d->desc);
  }
  *d = e;
  if (k->n == PROF_DEAD_KEEP) {
    counter_t m = ~(counter_t)0;
    for (size_t i = 0; i < k->n; i++)
      if (k->es[i].ncalls + k->es[i].nred < m) {
        m = k->es[i].ncalls + k->es[i].nred;
        k->min = i;
      }
  }
#else
  (void)p;
#endif
}

/* Take the dead nodes off the hot list.  This runs after marking, and
 * before prof_pin, so an unmarked loaded node is unreachable. */
static void
prof_hot_gc(void)
{
  size_t i, j = 0;
  for (i = 0; i < prof_nhot; i++) {
    struct node *p = prof_hot[i];
    NODEPTR n;
    n.node_ptr = p;
    if (is_marked_used(n))
      prof_hot[j++] = p;
    else
      prof_retire(p);
  }
  prof_nhot = j;
}
#endif  /* WANT_PROFILE */

extern const uint8_t *combexpr;
extern const int combexprlen;

#if WANT_TICK
int dump_ticks = 0;
#endif

#if WANT_ARGS
  #if WANT_STDIO
  #define MHS_INIT_ARGS(a,b,c,d) mhs_init_args(a,b,c,d)
  #else
  #define MHS_INIT_ARGS(a,b,c,d) mhs_init_args(a,b)
  #endif
#else  /* WANT_ARGS */
  #if WANT_STDIO
  #define MHS_INIT_ARGS(a,b,c,d) mhs_init_args(c,d)
  #else
  #define MHS_INIT_ARGS(a,b,c,d) mhs_init_args()
  #endif
#endif  /* WANT_ARGS */

NODEPTR
MHS_INIT_ARGS(
              int argc, char **argv,
              char **outnamep,
              size_t *file_sizep
)
{
  NODEPTR prog;
#if WANT_ARGS
  char *inname = 0;
  char **av;
  char **gargv;
  int gargc;
  int inrts;
#endif

#if 0
  /* MINGW doesn't do buffering right */
  setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
  setvbuf(stderr, NULL, _IONBF, BUFSIZ);
#endif

#ifdef INITIALIZATION
  main_setup(); /* Do platform specific start-up. */
#endif

#ifdef CLOCK_INIT
  CLOCK_INIT();
#endif

#if WANT_SIGINT
  {
    (void)signal(SIGINT, handle_sigint);
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGINT);
    sigprocmask(SIG_UNBLOCK, &set, NULL);
  }
#endif

  heap_size = HEAP_CELLS;       /* number of heap cells */
  stack_size = STACK_SIZE;      /* number of stack slots */

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
#if WANT_SAMPLE
        else if (strncmp(p, "-S", 2) == 0) {
          sample_interval = p[2] ? (counter_t)memsize(&p[2]) : 1000;
          sample_left = sample_interval;
          sample_table = mcalloc(SAMPLE_TABLE_SIZE, sizeof(struct sample_entry));
        }
        else if (strncmp(p, "-J", 2) == 0)
        {
          /* -J[limit[,threshold]] */
          char *q = &p[2];
          jx_limit = *q && *q != ',' ? (int)strtol(q, &q, 10) : JX_LIMIT;
          if (*q == ',')
            jx_threshold = (int)strtol(q + 1, 0, 10);
        }
#endif
#if WANT_STDIO
        else if (strncmp(p, "-o", 2) == 0)
          *outnamep = &p[2];
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
  stack = mmalloc(sizeof(NODEPTR) * stack_size);
  CLEARSTK();
  init_stableptr();

  num_reductions = 0;

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
    argarray->permanent = true;         /* never GC the arguments, because a T_IO_GETARGREF can reach argarray */
  }
#endif  /* WANT_ARGS */

  {
    /* Read the combinator code. */
    BFILE *bf;
    if (combexpr) {
      /* The code is in memory, create a memore buffer BFILE */
      bf = openb_rd_mem(combexpr, combexprlen);
#if WANT_STDIO
      *file_sizep = combexprlen;
#endif
    } else {
#if WANT_STDIO & WANT_ARGS
      /* Open a regular file */
      FILE *f = fopen(inname, "rb");
      if (!f)
        ERR1("file not found %s", inname);
      fseek(f, 0, SEEK_END);
      *file_sizep = ftell(f);   /* find its size */
      rewind(f);

      bf = add_FILE(f);
#else
      ERR("no stdio");
#endif
    }
    int c = getb(bf);
#if WANT_BASE64
    /* Compressed combinators start with a 'z', otherwise 'v' (for version) */
    if (c != 'z' && c != 'v') {
      /* Neither z nor v, assume base64 encoded */
      ungetb(c, bf);
      bf = add_base64_decoder(bf);
      c = getb(bf);
    }
#endif
    if (c == 'z') {
#if WANT_LZMA
      /* add LZMA decompressor transducer */
      bf = add_lzma_decompressor(bf);
#else
      ERR("LZMA compression not supported");
#endif
    } else {
      /* put it back, we need it */
      ungetb(c, bf);
    }
#if WANT_PROFILE
    prof_loading = 1;
#endif
    prog = parse_top(bf, xffe_table);
    if (is_NIL(prog))
      ERR("deser failed");
#if WANT_PROFILE
    prof_end_load(bf);
#if WANT_STDIO
    atexit(prof_report);        /* also when the program ends with exitWith, or an uncaught exception */
#endif
#endif
    closeb(bf);
  }

  /* GC unused stuff, nice for -o */
  PUSH(prog);
  want_gc_red = 1;
  gc();
  gc();                         /* this finds some more GC reductions */
  want_gc_red = 0;              /* can be enabled, but it is rarely a win */
  prog = POPTOP();
  return prog;
}

void
mhs_init(void)
{
  char *args[2] = { "<mhs_init>", 0 };
  char *outname;
  size_t file_size;
  (void)MHS_INIT_ARGS(1, args, &outname, &file_size);
}

int
mhs_main(int argc, char **argv)
{
  NODEPTR prog;
  char *outname = 0;
  size_t file_size = 0;
#if WANT_KPERF
  counter_t instrs;
#endif  /* WANT_KPERF */
  boot_time = GETTIMEMICRO();

  prog = MHS_INIT_ARGS(argc, argv, &outname, &file_size);

#if WANT_STDIO
  heapoffs_t start_size = num_marked;
  if (outname) {
    /* Save GCed file (smaller), and exit. */
    FILE *out = fopen(outname, "wb");
    if (!out)
      ERR1("cannot open output file %s", outname);
    struct BFILE *bf = add_FILE(out);
    printb(bf, prog, true);
    closeb(bf);
    EXIT(0);
  }
  if (verbose > 2) {
    pp(stdout, prog);
  }
#endif
  run_time -= GETTIMEMILLI();

#if 0
  topnode = &prog;
#endif
#if WANT_KPERF
  if (!start_kperf()) {
    // ERR("kperf init failed");
#if WANT_STDIO
    fprintf(stderr, "start_kperf() failed, ignored\n");
#endif
  }
#endif  /* WANT_KPERF */
  start_exec(prog);
#if WANT_SAMPLE
  sample_report();              /* before the GC below frees everything */
#endif
  /* Flush standard handles in case there is some BFILE buffering */
  flushb((BFILE*)FORPTR(comb_stdout)->payload.bs_array);
  flushb((BFILE*)FORPTR(comb_stderr)->payload.bs_array);
  gc();                      /* Run finalizers */
#if WANT_KPERF
  instrs = end_kperf();
#endif  /* WANT_KPERF */
  run_time += GETTIMEMILLI();

#if WANT_STDIO
  if (verbose) {
    if (verbose > 1) {
      PRINT("node size=%"PRIheap", heap size bytes=%"PRIheap"\n", (heapoffs_t)NODE_SIZE, heap_size * NODE_SIZE);
    }
    setlocale(LC_NUMERIC, "en_US");  /* Make %' work on platforms that support it */
    PRINT("%"PCOMMA"15"PRIheap" combinator file size\n", (heapoffs_t)file_size);
    PRINT("%"PCOMMA"15"PRIheap" cells at start\n", start_size);
    PRINT("%"PCOMMA"15"PRIheap" cells heap size (%"PCOMMA""PRIheap" bytes)\n", heap_size, heap_size * NODE_SIZE);
    PRINT("%"PCOMMA"15"PRIcounter" cells allocated (%"PCOMMA".1f Mbyte/s)\n", NUM_ALLOC, NUM_ALLOC * NODE_SIZE / ((double)run_time / 1000) / 1000000);
    PRINT("%"PCOMMA"15"PRIcounter" GCs\n", num_gc);
    PRINT("%"PCOMMA"15"PRIcounter" max cells used\n", max_num_marked);
    PRINT("%"PCOMMA"15"PRIcounter" reductions (%"PCOMMA".1f Mred/s)\n", num_reductions, num_reductions / ((double)run_time / 1000) / 1000000);
    PRINT("%"PCOMMA"15"PRIcounter" yields (%"PCOMMA""PRIcounter" resched)\n", num_yield, num_resched);
    PRINT("%"PCOMMA"15"PRIcounter" array alloc\n", num_arr_alloc);
    PRINT("%"PCOMMA"15"PRIcounter" array free\n", num_arr_free);
    PRINT("%"PCOMMA"15"PRIcounter" foreign alloc\n", num_fin_alloc);
    PRINT("%"PCOMMA"15"PRIcounter" foreign free\n", num_fin_free);
    PRINT("%"PCOMMA"15"PRIcounter" bytestring alloc (max %"PCOMMA""PRIcounter")\n", num_bs_alloc, num_bs_alloc_max);
    PRINT("%"PCOMMA"15"PRIcounter" bytestring alloc bytes (max %"PCOMMA""PRIcounter")\n", num_bs_bytes, num_bs_inuse_max);
    PRINT("%"PCOMMA"15"PRIcounter" bytestring free\n", num_bs_free);
    PRINT("%"PCOMMA"15"PRIcounter" thread create\n", num_thread_create-1);
    PRINT("%"PCOMMA"15"PRIcounter" thread reap\n", num_thread_reap);
    PRINT("%"PCOMMA"15"PRIcounter" stableptr alloc\n", num_stable_alloc);
    PRINT("%"PCOMMA"15"PRIcounter" stableptr free\n", num_stable_free);
    PRINT("%"PCOMMA"15"PRIcounter" weakptr alloc\n", num_new_weak);
    PRINT("%"PCOMMA"15"PRIcounter" weakptr free\n", num_gc_weak);
    {
      /* XXX temporary */
      size_t bytes = max_case_arities * sizeof(struct case_arity);
      for (uint32_t i = 0; i < num_case_arities; i++)
        bytes += case_arities[i].m;
      PRINT("%"PCOMMA"15d case arity entries (%"PCOMMA"d bytes)\n", (int)num_case_arities, (int)bytes);
    }
#if MAXSTACKDEPTH
    PRINT("%"PCOMMA"15d max stack depth\n", (int)max_stack_depth);
    PRINT("%"PCOMMA"15d max C stack depth\n", (int)max_c_stack);
#endif
#if 0
    PRINT("arms=%"PRIcounter" bad=%"PRIcounter"\n", num_arm, num_bad_arm);
#endif
    // PRINT("%"PCOMMA"15d avg gc stack depth\n", (int)(gc_tot / num_gc));
    // PRINT("%"PCOMMA"15"PRIcounter" max mark depth\n", max_mark_depth);
    PRINT("%15.2fs total expired time\n", (double)run_time / 1000);
    PRINT("%15.2fs gc expired time = %3.1f%% (%.2fs mark + %.2fs scan)\n",
          (double)(gc_mark_time + gc_scan_time) / 1000,
          (double)(gc_mark_time + gc_scan_time) / (double)run_time * 100,
          (double)gc_mark_time / 1000,
          (double)gc_scan_time / 1000);
#if WANT_KPERF
    if (instrs > 0) {
      PRINT("%"PCOMMA"15"PRIcounter" instructions (%.1f instr/red)\n", instrs, (double)instrs / (double)num_reductions);
    }
#endif  /* WANT_KPERF */
#if GCRED
    PRINT(" GC reductions A=%"PRIcounter", K=%"PRIcounter", I=%"PRIcounter", int=%"PRIcounter", flip=%"PRIcounter","
          " BI=%"PRIcounter", BxI=%"PRIcounter", C'BxI=%"PRIcounter", CC=%"PRIcounter", C'I=%"PRIcounter", C'BBCP=%"PRIcounter"\n",
          red_a, red_k, red_i, red_int, red_flip, red_bi, red_bxi, red_ccbi, red_cc, red_cci, red_ccbbcp);
    PRINT(" special reductions B'=%"PRIcounter" K4=%"PRIcounter" K3=%"PRIcounter" K2=%"PRIcounter" C'B=%"PRIcounter", Z=%"PRIcounter", R=%"PRIcounter"\n",
          red_bb, red_k4, red_k3, red_k2, red_ccb, red_z, red_r);
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

/***************************/
/* Foreign export helpers  */

/* The ffe_value in the C code is a struct node*, but it is really a NODEPTR,
 * so it can be a virtual pointer. */
void
ffe_push(struct node *p)
{
  NODEPTR n;
  n.node_ptr = p;
  PUSH(n);
}

void
ffe_pop(void)
{
  POP(1);
}

/* Allocate a new node (will be overwritten) */
stackptr_t
ffe_alloc(void)
{
  PUSH(alloc_node(D_DBL));
  return stack_ptr;
}

void
ffe_apply(void)
{
  NODEPTR arg = POPTOP();
  NODEPTR fun = POPTOP();
  PUSH(new_ap(fun, arg));
}

/* For stand-alone exported functions this is called with the threading inactive.
 * On the other hand, if a 'foreign import' calls back to a 'foreign export' the
 * threading is alread running.
 */
/* XXX This is not quite right.  The surrounding mhs_to_xxx should be in the thread. */
stackptr_t
ffe_eval(void)
{
  if (main_thread) {
    /* threading active, run on current stack */
    (void)evali(TOP(0));
  } else {
    /* Start up the threading to evaluate the node.
     * start_exec() runs an IO action, i.e., it applies its argument to World.
     * The node is a value, not an IO action, so pass (K node), which
     * ignores World.  Applying the node itself to World would give it an
     * extra argument, which a constructor cannot take.
     */
    GCCHECK(2);                 /* K node, and the World application in start_exec */
    start_exec(new_ap(combK, TOP(0)));
  }
  /* The mhs_to_xxx functions bizarrely return the ARG(TOP(n+1)) value.
   * The wrapper will call with n=-1, so we need to put the result at ARG(TOP(0))
   */
  TOP(0) = new_ap(combI, TOP(0));
  return stack_ptr;
}

stackptr_t
ffe_exec(void)
{
  NODEPTR n = POPTOP();
  PUSH(new_ap(combPERFORMIO, n));
  return ffe_eval();
}

/* apply_sp :: StablePtr (Ptr a -> IO (Ptr b)) -> Ptr a -> IO (Ptr b) */
void *
apply_sp(uvalue_t sp, void *arg)
{
  GCCHECK(3);
  NODEPTR f = deref_stableptr(sp);
  NODEPTR a = alloc_node(D_PTR);
  PTR(a) = arg;
  PUSH(new_ap(combPERFORMIO, new_ap(f, a)));
  void *r = evalptr(TOP(0));
  POP(1);
  return r;
}

/*********************/
/* FFI adapters      */

#define MHS_FROM(name, set, type) \
from_t \
name(stackptr_t stk, int n, type x) \
{ \
  NODEPTR r = TOP(0);           /* The pre-allocated cell for the result, */ \
  set(r, x);                    /* Put result in pre-allocated cell. */ \
  return n;                     /* return arity */ \
}
#if WANT_FLOAT64
MHS_FROM(mhs_from_Double, SETDBL, flt64_t);
#endif
#if WANT_FLOAT32
MHS_FROM(mhs_from_Float, SETFLT, flt32_t);
#endif
MHS_FROM(mhs_from_Int, SETINT, value_t);
/* A Bool is the C_0_0 (False) or C_1_0 (True) constructor, not an int node. */
#define SETBOOL(n, x) SETIND((n), (x) ? combTrue : combFalse)
MHS_FROM(mhs_from_Bool, SETBOOL, value_t);
#if WANT_INT64
MHS_FROM(mhs_from_Int64, SETINT64, int64_t);
#endif
MHS_FROM(mhs_from_Word, SETINT, uvalue_t);
MHS_FROM(mhs_from_Word8, SETINT, uvalue_t);
#if WANT_INT64
MHS_FROM(mhs_from_Word64, SETINT64, uint64_t);
#endif
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
#if WANT_INT64
/* CLLong and CULLong are newtypes of Int64 and Word64 */
MHS_FROM(mhs_from_CLLong, SETINT64, long long);
MHS_FROM(mhs_from_CULLong, SETINT64, unsigned long long);
#else
MHS_FROM(mhs_from_CLLong, SETINT, long long);
MHS_FROM(mhs_from_CULLong, SETINT, unsigned long long);
#endif
MHS_FROM(mhs_from_CSize, SETINT, size_t);
#if WANT_TIME
MHS_FROM(mhs_from_CTime, SETINT, time_t);
#endif
// MHS_FROM(mhs_from_CSSize, SETINT, ssize_t);
MHS_FROM(mhs_from_CIntPtr, SETINT, intptr_t);
MHS_FROM(mhs_from_CUIntPtr, SETINT, uintptr_t);
from_t
mhs_from_Unit(stackptr_t stk, int n)
{
  POP(1);                       /* return value cell */
  PUSH(combUnit);               /* push unit instead */
  return n;
}

#define MHS_TO(name, eval, type) \
type name(stackptr_t stk, int n) \
{ \
  return eval(ARG(TOP(n+1)));                /* The stack has a reserved cell on top of the arguments */ \
}
#if WANT_FLOAT32
MHS_TO(mhs_to_Float, evalflt, flt32_t);
#endif
#if WANT_FLOAT64
MHS_TO(mhs_to_Double, evaldbl, flt64_t);
#endif
MHS_TO(mhs_to_Int, evalint, value_t);
MHS_TO(mhs_to_Bool, evalbool, value_t);
#if WANT_INT64
MHS_TO(mhs_to_Int64, evalint64, int64_t);
#endif
MHS_TO(mhs_to_Word, evalint, uvalue_t);
MHS_TO(mhs_to_Word8, evalint, uint8_t);
#if WANT_INT64
MHS_TO(mhs_to_Word64, evalint64, uint64_t);
#endif
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
#if WANT_INT64
/* CLLong and CULLong are newtypes of Int64 and Word64 */
MHS_TO(mhs_to_CLLong, evalint64, long long);
MHS_TO(mhs_to_CULLong, evalint64, unsigned long long);
#else
MHS_TO(mhs_to_CLLong, evalint, long long);
MHS_TO(mhs_to_CULLong, evalint, unsigned long long);
#endif
MHS_TO(mhs_to_CSize, evalint, size_t);
#if WANT_TIME
MHS_TO(mhs_to_CTime, evalint, time_t);
#endif
// MHS_TO(mhs_to_CSSize, evalint, ssize_t);
MHS_TO(mhs_to_CIntPtr, evalint, intptr_t);
MHS_TO(mhs_to_CUIntPtr, evalint, uintptr_t);

/* The rest of this file was generated by the compiler, with some minor edits with #if. */
from_t mhs_GETRAW(int s) { return  mhs_from_Int(s, 0, GETRAW()); }
from_t mhs_GETTIMEMICRO(int s) { return  mhs_from_Int(s, 0, GETTIMEMICRO()); }
from_t mhs_GETBOOTTIMEMICRO(int s) { return  mhs_from_Int(s, 0, boot_time); }
#if WANT_MATH
#if WANT_FLOAT64
from_t mhs_acos(int s) { return mhs_from_Double(s, 1, acos(mhs_to_Double(s, 0))); }
from_t mhs_asin(int s) { return mhs_from_Double(s, 1, asin(mhs_to_Double(s, 0))); }
from_t mhs_atan(int s) { return mhs_from_Double(s, 1, atan(mhs_to_Double(s, 0))); }
from_t mhs_atan2(int s) { return mhs_from_Double(s, 2, atan2(mhs_to_Double(s, 0), mhs_to_Double(s, 1))); }
from_t mhs_cos(int s) { return mhs_from_Double(s, 1, cos(mhs_to_Double(s, 0))); }
from_t mhs_exp(int s) { return mhs_from_Double(s, 1, exp(mhs_to_Double(s, 0))); }
from_t mhs_log(int s) { return mhs_from_Double(s, 1, log(mhs_to_Double(s, 0))); }
from_t mhs_sin(int s) { return mhs_from_Double(s, 1, sin(mhs_to_Double(s, 0))); }
from_t mhs_sqrt(int s) { return mhs_from_Double(s, 1, sqrt(mhs_to_Double(s, 0))); }
from_t mhs_tan(int s) { return mhs_from_Double(s, 1, tan(mhs_to_Double(s, 0))); }
from_t mhs_scalbn(int s) { return mhs_from_Double(s, 2, scalbn(mhs_to_Double(s, 0), mhs_to_Int(s, 1))); }
from_t mhs_pow(int s) { return mhs_from_Double(s, 2, pow(mhs_to_Double(s, 0), mhs_to_Double(s, 1))); }
#endif  /* WANT_FLOAT64 */
#if WANT_FLOAT32
from_t mhs_acosf(int s) { return mhs_from_Float(s, 1, acosf(mhs_to_Float(s, 0))); }
from_t mhs_asinf(int s) { return mhs_from_Float(s, 1, asinf(mhs_to_Float(s, 0))); }
from_t mhs_atanf(int s) { return mhs_from_Float(s, 1, atanf(mhs_to_Float(s, 0))); }
from_t mhs_atan2f(int s) { return mhs_from_Float(s, 2, atan2f(mhs_to_Float(s, 0), mhs_to_Float(s, 1))); }
from_t mhs_cosf(int s) { return mhs_from_Float(s, 1, cosf(mhs_to_Float(s, 0))); }
from_t mhs_expf(int s) { return mhs_from_Float(s, 1, expf(mhs_to_Float(s, 0))); }
from_t mhs_logf(int s) { return mhs_from_Float(s, 1, logf(mhs_to_Float(s, 0))); }
from_t mhs_sinf(int s) { return mhs_from_Float(s, 1, sinf(mhs_to_Float(s, 0))); }
from_t mhs_sqrtf(int s) { return mhs_from_Float(s, 1, sqrtf(mhs_to_Float(s, 0))); }
from_t mhs_tanf(int s) { return mhs_from_Float(s, 1, tanf(mhs_to_Float(s, 0))); }
from_t mhs_scalbnf(int s) { return mhs_from_Float(s, 2, scalbnf(mhs_to_Float(s, 0), mhs_to_Int(s, 1))); }
from_t mhs_powf(int s) { return mhs_from_Float(s, 2, powf(mhs_to_Float(s, 0), mhs_to_Float(s, 1))); }
#endif  /* WANT_FLOAT32 */
#endif  /* WANT_MATH */

#if defined(__EMSCRIPTEN__)
from_t mhs_js_debug(int s) { EM_ASM({ console.log(UTF8ToString($0)) }, mhs_to_Ptr(s, 0)); return mhs_from_Unit(s, 1); }
from_t mhs_js_eval_run(int s) { EM_ASM({ eval(UTF8ToString($0)) }, mhs_to_Ptr(s, 0)); return mhs_from_Unit(s, 1); }
from_t mhs_js_eval_call(int s) { return mhs_from_Ptr(s, 1, EM_ASM_PTR({ return stringToNewUTF8(JSON.stringify(eval(UTF8ToString($0)))) }, mhs_to_Ptr(s, 0))); }
from_t mhs_js_set_haskellCallback(int s) { EM_ASM({ _haskellCallback = $0 }, mhs_to_Int(s, 0)); return mhs_from_Unit(s, 1); }
#endif

#if WANT_STDIO
from_t mhs_add_FILE(int s) { return mhs_from_Ptr(s, 1, add_FILE(mhs_to_Ptr(s, 0))); }
from_t mhs_putchar(int s) { putchar(mhs_to_Int(s, 0)); return mhs_from_Unit(s, 1); } /* for debugging */
from_t mhs_fopen(int s) { return mhs_from_Ptr(s, 2, fopen(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }
from_t mhs_system(int s) { return mhs_from_Int(s, 1, system(mhs_to_Ptr(s, 0))); }
from_t mhs_tmpname(int s) { return mhs_from_Ptr(s, 2, TMPNAME(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }
from_t mhs_remove(int s) { return mhs_from_Int(s, 1, remove(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_STDIO */
#if WANT_FD
from_t mhs_add_fd(int s) { return mhs_from_Ptr(s, 1, add_fd(mhs_to_Int(s, 0))); }
from_t mhs_open(int s) { return mhs_from_Int(s, 3, open(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1), mhs_to_Int(s, 2))); }
#endif  /* WANT_FD */
#if WANT_BUF
from_t mhs_add_buf(int s) { return mhs_from_Ptr(s, 2, add_buf(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
#endif  /* WANT_BUF */
#if WANT_CRLF
from_t mhs_add_crlf(int s) { return mhs_from_Ptr(s, 1, add_crlf(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_CRLF */
#if WANT_UTF8
from_t mhs_add_utf8(int s) { return mhs_from_Ptr(s, 1, add_utf8(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_UTF8 */
#if WANT_BASE64
from_t mhs_add_base64_encoder(int s) { return mhs_from_Ptr(s, 1, add_base64_encoder(mhs_to_Ptr(s, 0))); }
from_t mhs_add_base64_decoder(int s) { return mhs_from_Ptr(s, 1, add_base64_decoder(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_BASE64 */
from_t mhs_closeb(int s) { closeb(mhs_to_Ptr(s, 0)); return mhs_from_Unit(s, 1); }
from_t mhs_addr_closeb(int s) { return mhs_from_FunPtr(s, 0, (HsFunPtr)&closeb); }
from_t mhs_flushb(int s) { flushb(mhs_to_Ptr(s, 0)); return mhs_from_Unit(s, 1); }
from_t mhs_getb(int s) { return mhs_from_Int(s, 1, getb(mhs_to_Ptr(s, 0))); }
from_t mhs_putb(int s) { putb(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_ungetb(int s) { ungetb(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_openwrmem(int s) { return mhs_from_Ptr(s, 0, openb_wr_mem()); }
from_t mhs_openrdmem(int s) { return mhs_from_Ptr(s, 2, openb_rd_mem(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
from_t mhs_getmem(int s) { get_mem(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2));  return mhs_from_Unit(s, 3); }
from_t mhs_readb(int s) { return mhs_from_Int(s, 3, readb(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1), mhs_to_Ptr(s, 2))); }
from_t mhs_writeb(int s) { return mhs_from_Int(s, 3, writeb(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1), mhs_to_Ptr(s, 2))); }
from_t mhs_isattyb(int s) { return mhs_from_Int(s, 1, isattyb(mhs_to_Ptr(s, 0))); }
#if WANT_INT64
from_t mhs_filesizeb(int s) { return mhs_from_Int64(s, 1, filesizeb(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_INT64 */

#if WANT_MD5
from_t mhs_md5Array(int s) { md5Array(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_md5BFILE(int s) { md5BFILE(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_md5String(int s) { md5String(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
#endif  /* WANT_MD5 */

#if WANT_LZ77
from_t mhs_add_lz77_compressor(int s) { return mhs_from_Ptr(s, 1, add_lz77_compressor(mhs_to_Ptr(s, 0))); }
from_t mhs_add_lz77_decompressor(int s) { return mhs_from_Ptr(s, 1, add_lz77_decompressor(mhs_to_Ptr(s, 0))); }
from_t mhs_lz77c(int s) { return mhs_from_CSize(s, 3, lz77c(mhs_to_Ptr(s, 0), mhs_to_CSize(s, 1), mhs_to_Ptr(s, 2))); }
#endif  /* WANT_LZ77 */

#if WANT_LZMA
from_t mhs_add_lzma_compressor(int s) { return mhs_from_Ptr(s, 1, add_lzma_compressor(mhs_to_Ptr(s, 0))); }
from_t mhs_add_lzma_decompressor(int s) { return mhs_from_Ptr(s, 1, add_lzma_decompressor(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_LZ77 */

#if WANT_RLE
from_t mhs_add_rle_compressor(int s) { return mhs_from_Ptr(s, 1, add_rle_compressor(mhs_to_Ptr(s, 0))); }
from_t mhs_add_rle_decompressor(int s) { return mhs_from_Ptr(s, 1, add_rle_decompressor(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_RLE */

#if WANT_BWT
from_t mhs_add_bwt_compressor(int s) { return mhs_from_Ptr(s, 1, add_bwt_compressor(mhs_to_Ptr(s, 0))); }
from_t mhs_add_bwt_decompressor(int s) { return mhs_from_Ptr(s, 1, add_bwt_decompressor(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_BWT */

from_t mhs_calloc(int s) { return mhs_from_Ptr(s, 2, CALLOC(mhs_to_CSize(s, 0), mhs_to_CSize(s, 1))); }
from_t mhs_realloc(int s) { return mhs_from_Ptr(s, 2, REALLOC(mhs_to_Ptr(s, 0), mhs_to_CSize(s, 1))); }
from_t mhs_free(int s) { FREE(mhs_to_Ptr(s, 0)); return mhs_from_Unit(s, 1); }
from_t mhs_addr_free(int s) { return mhs_from_FunPtr(s, 0, (HsFunPtr)&FREE); }
from_t mhs_iswindows(int s) { return mhs_from_Int(s, 0, iswindows()); }
from_t mhs_ismacos(int s) { return mhs_from_Int(s, 0, ismacos()); }
from_t mhs_islinux(int s) { return mhs_from_Int(s, 0, islinux()); }
from_t mhs_ismingw(int s) { return mhs_from_Int(s, 0, ismingw()); }
from_t mhs_malloc(int s) { return mhs_from_Ptr(s, 1, MALLOC(mhs_to_CSize(s, 0))); }
from_t mhs_memcpy(int s) { memcpy(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_CSize(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_strlen(int s) { return mhs_from_CSize(s, 1, strlen(mhs_to_Ptr(s, 0))); }
from_t mhs_strcpy(int s) { strcpy(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_memmove(int s) { memmove(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_CSize(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_peekPtr(int s) { return mhs_from_Ptr(s, 1, peekPtr(mhs_to_Ptr(s, 0))); }
from_t mhs_peekWord(int s) { return mhs_from_Word(s, 1, peekWord(mhs_to_Ptr(s, 0))); }
from_t mhs_pokePtr(int s) { pokePtr(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_pokeWord(int s) { pokeWord(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); return mhs_from_Unit(s, 2); }

from_t mhs_peek_uint8(int s) { return mhs_from_Word(s, 1, peek_uint8(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_uint8(int s) { poke_uint8(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_uint16(int s) { return mhs_from_Word(s, 1, peek_uint16(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_uint16(int s) { poke_uint16(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_uint32(int s) { return mhs_from_Word(s, 1, peek_uint32(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_uint32(int s) { poke_uint32(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); return mhs_from_Unit(s, 2); }
#if WANT_INT64
from_t mhs_peek_uint64(int s) { return mhs_from_Word64(s, 1, peek_uint64(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_uint64(int s) { poke_uint64(mhs_to_Ptr(s, 0), mhs_to_Word64(s, 1)); return mhs_from_Unit(s, 2); }
#endif  /* WANT_INT64 */

from_t mhs_peek_int8(int s) { return mhs_from_Int(s, 1, peek_int8(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_int8(int s) { poke_int8(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_int16(int s) { return mhs_from_Int(s, 1, peek_int16(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_int16(int s) { poke_int16(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_int32(int s) { return mhs_from_Int(s, 1, peek_int32(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_int32(int s) { poke_int32(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); return mhs_from_Unit(s, 2); }
#if WANT_INT64
from_t mhs_peek_int64(int s) { return mhs_from_Int64(s, 1, peek_int64(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_int64(int s) { poke_int64(mhs_to_Ptr(s, 0), mhs_to_Int64(s, 1)); return mhs_from_Unit(s, 2); }
#endif  /* WANT_INT64 */
from_t mhs_peek_char(int s) { return mhs_from_CChar(s, 1, peek_char(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_char(int s) { poke_char(mhs_to_Ptr(s, 0), mhs_to_CChar(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_schar(int s) { return mhs_from_CSChar(s, 1, peek_schar(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_schar(int s) { poke_schar(mhs_to_Ptr(s, 0), mhs_to_CSChar(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_uchar(int s) { return mhs_from_CUChar(s, 1, peek_uchar(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_uchar(int s) { poke_uchar(mhs_to_Ptr(s, 0), mhs_to_CUChar(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_short(int s) { return mhs_from_CShort(s, 1, peek_short(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_short(int s) { poke_short(mhs_to_Ptr(s, 0), mhs_to_CShort(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_ushort(int s) { return mhs_from_CUShort(s, 1, peek_ushort(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_ushort(int s) { poke_ushort(mhs_to_Ptr(s, 0), mhs_to_CUShort(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_int(int s) { return mhs_from_CInt(s, 1, peek_int(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_int(int s) { poke_int(mhs_to_Ptr(s, 0), mhs_to_CInt(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_uint(int s) { return mhs_from_CUInt(s, 1, peek_uint(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_uint(int s) { poke_uint(mhs_to_Ptr(s, 0), mhs_to_CUInt(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_long(int s) { return mhs_from_CLong(s, 1, peek_long(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_long(int s) { poke_long(mhs_to_Ptr(s, 0), mhs_to_CLong(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_ulong(int s) { return mhs_from_CULong(s, 1, peek_ulong(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_ulong(int s) { poke_ulong(mhs_to_Ptr(s, 0), mhs_to_CULong(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_llong(int s) { return mhs_from_CLLong(s, 1, peek_llong(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_llong(int s) { poke_llong(mhs_to_Ptr(s, 0), mhs_to_CLLong(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_ullong(int s) { return mhs_from_CULLong(s, 1, peek_ullong(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_ullong(int s) { poke_ullong(mhs_to_Ptr(s, 0), mhs_to_CULLong(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_peek_size_t(int s) { return mhs_from_CSize(s, 1, peek_size_t(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_size_t(int s) { poke_size_t(mhs_to_Ptr(s, 0), mhs_to_CSize(s, 1)); return mhs_from_Unit(s, 2); }
#if WANT_FLOAT32
from_t mhs_peek_flt32(int s) { return mhs_from_Float(s, 1, peek_flt32(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_flt32(int s) { poke_flt32(mhs_to_Ptr(s, 0), mhs_to_Float(s, 1)); return mhs_from_Unit(s, 2); }
#endif  /* WANT_FLOAT32 */
#if WANT_FLOAT64
from_t mhs_peek_flt64(int s) { return mhs_from_Double(s, 1, peek_flt64(mhs_to_Ptr(s, 0))); }
from_t mhs_poke_flt64(int s) { poke_flt64(mhs_to_Ptr(s, 0), mhs_to_Double(s, 1)); return mhs_from_Unit(s, 2); }
#endif  /* WANT_FLOAT64 */
from_t mhs_sizeof_char(int s) { return mhs_from_Int(s, 0, sizeof(char)); }
from_t mhs_sizeof_short(int s) { return mhs_from_Int(s, 0, sizeof(short)); }
from_t mhs_sizeof_int(int s) { return mhs_from_Int(s, 0, sizeof(int)); }
from_t mhs_sizeof_llong(int s) { return mhs_from_Int(s, 0, sizeof(long long)); }
from_t mhs_sizeof_long(int s) { return mhs_from_Int(s, 0, sizeof(long)); }
from_t mhs_sizeof_size_t(int s) { return mhs_from_Int(s, 0, sizeof(size_t)); }
#if WANT_DIR
from_t mhs_closedir(int s) { return mhs_from_Int(s, 1, closedir(mhs_to_Ptr(s, 0))); }
from_t mhs_opendir(int s) { return mhs_from_Ptr(s, 1, opendir(mhs_to_Ptr(s, 0))); }
from_t mhs_readdir(int s) { return mhs_from_Ptr(s, 1, readdir(mhs_to_Ptr(s, 0))); }
from_t mhs_c_d_name(int s) { return mhs_from_Ptr(s, 1, ((struct dirent *)(mhs_to_Ptr(s, 0)))->d_name); }
from_t mhs_chdir(int s) { return mhs_from_Int(s, 1, chdir(mhs_to_Ptr(s, 0))); }
from_t mhs_mkdir(int s) { return mhs_from_Int(s, 2, MKDIR(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
from_t mhs_getcwd(int s) { return mhs_from_Ptr(s, 2, getcwd(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
from_t mhs_get_permissions(int s) { return mhs_from_Int(s, 1, get_permissions(mhs_to_Ptr(s, 0))); }
from_t mhs_set_permissions(int s) { return mhs_from_Int(s, 2, set_permissions(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
from_t mhs_get_file_size(int s) { return mhs_from_Int64(s, 1, get_file_size(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_DIR */
from_t mhs_getcpu(int s) { GETCPUTIME(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
#if WANT_ENV
from_t mhs_getenv(int s) { return mhs_from_Ptr(s, 1, getenv(mhs_to_Ptr(s, 0))); }
from_t mhs_environ(int s) { return mhs_from_Ptr(s, 0, environ); }
from_t mhs_unsetenv(int s) { return mhs_from_Int(s, 1, unsetenv(mhs_to_Ptr(s, 0))); }
from_t mhs_setenv(int s) { return mhs_from_Int(s, 3, setenv(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2))); }
#endif  /* WANT_ENV */

/* Use this to detect if we have (and want) GMP/imath or not. */
from_t mhs_want_gmp(int s) { return mhs_from_Int(s, 0, WANT_GMP); }
from_t mhs_want_imath(int s) { return mhs_from_Int(s, 0, WANT_IMATH); }

#if WANT_GMP || WANT_IMATH
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
  mpz_ptr p = mmalloc(sizeof(*p));
  mpz_init(p);
  struct forptr *fp = mkForPtrP(p);
  fp->finalizer->final = (HsFunPtr)free_mpz;
  fp->finalizer->fptype = FP_MPZ;
  /*  printf("new_mpz %p %p\n", p, fp); */
  return fp;
}

intptr_t
mpz_get_si_(mpz_t op)
{
  intptr_t r = mpz_get_ui(op);
  if (mpz_sgn(op) < 0) {
    r = -r;
  }
  return r;
}

#if 0
void
print_mpz(mpz_ptr p)
{
  mpz_out_str(stdout, 10, p);
}
#endif

#if WANT_INT64 && (WORD_SIZE < 64 || LONG_MAX < INT64_MAX)
/* GMP lacks 64 bit support on 32 bit platforms, and where long is 32 bits (LLP64) */
void
mpz_init_set_ui64(mpz_t rop, uint64_t op)
{
  mpz_init_set_ui(rop, op >> 32);
  mpz_mul_2exp(rop, rop, 32);
  mpz_add_ui(rop, rop, op & 0xffffffff);
}
void
mpz_init_set_si64(mpz_t rop, int64_t op)
{
  if (op >= 0) {
    mpz_init_set_ui64(rop, op);
  } else {
    mpz_init_set_ui64(rop, -op);
    mpz_neg(rop, rop);
  }
}
int64_t
mpz_get_si64(mpz_t op)
{
  mpz_t t;
  mpz_init_set(t, op);
  mpz_tdiv_q_2exp(t, t, 32);
  uint64_t hi = mpz_get_ui(t);
  uint64_t lo = mpz_get_ui(op);
  mpz_clear(t);
  uint64_t r = (hi << 32) | lo;
  if (mpz_sgn(op) < 0) {
    r = -r;
  }
  return r;
}
#endif  /* WANT_INT64 */
#if WORD_SIZE == 64 && LONG_MAX >= INT64_MAX
#define mpz_init_set_ui64 mpz_init_set_ui
#define mpz_init_set_si64 mpz_init_set_si
#define mpz_get_si64 mpz_get_si_
#endif

from_t mhs_new_mpz(int s) { return mhs_from_ForPtr(s, 0, new_mpz()); }

/* Stubs for GMP functions */
from_t mhs_mpz_abs(int s) { mpz_abs(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_mpz_add(int s) { mpz_add(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_mpz_and(int s) { mpz_and(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_mpz_cmp(int s) { return mhs_from_Int(s, 2, mpz_cmp(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }
#if WANT_FLOAT64
from_t mhs_mpz_get_d(int s) { return mhs_from_Double(s, 1, mpz_get_d(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_FLOAT64 */
#if WANT_FLOAT32
from_t mhs_mpz_get_f(int s) { return mhs_from_Float(s, 1, (float)mpz_get_d(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_FLOAT32 */
#if WANT_INT64 && WORD_SIZE == 64 && LONG_MAX < INT64_MAX
/* LLP64: Int and Word are wider than the long the GMP interface takes. */
from_t mhs_mpz_get_si(int s) { return mhs_from_Int(s, 1, mpz_get_si64(mhs_to_Ptr(s, 0))); }
from_t mhs_mpz_init_set_si(int s) { mpz_init_set_si64(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_mpz_init_set_ui(int s) { mpz_init_set_ui64(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); return mhs_from_Unit(s, 2); }
#else
from_t mhs_mpz_get_si(int s) { return mhs_from_Int(s, 1, mpz_get_si_(mhs_to_Ptr(s, 0))); }
from_t mhs_mpz_init_set_si(int s) { mpz_init_set_si(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_mpz_init_set_ui(int s) { mpz_init_set_ui(mhs_to_Ptr(s, 0), mhs_to_Word(s, 1)); return mhs_from_Unit(s, 2); }
#endif
from_t mhs_mpz_ior(int s) { mpz_ior(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_mpz_mul(int s) { mpz_mul(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_mpz_mul_2exp(int s) { mpz_mul_2exp(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_mpz_neg(int s) { mpz_neg(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_mpz_popcount(int s) {
  mpz_ptr a = mhs_to_Ptr(s, 0);
  from_t r;
  if (mpz_sgn(a) < 0) {
    mpz_t neg_a;
    mpz_init(neg_a);
    mpz_neg(neg_a, a);
    r = mhs_from_Int(s, 1, -mpz_popcount(neg_a));
    mpz_clear(neg_a);
  } else {
    r = mhs_from_Int(s, 1, mpz_popcount(a));
  }
  return r;
}
from_t mhs_mpz_sub(int s) { mpz_sub(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_mpz_fdiv_q_2exp(int s) { mpz_fdiv_q_2exp(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2)); return mhs_from_Unit(s, 3); }
from_t mhs_mpz_tdiv_qr(int s) { mpz_tdiv_qr(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2), mhs_to_Ptr(s, 3)); return mhs_from_Unit(s, 4); }
from_t mhs_mpz_tstbit(int s) { return mhs_from_Int(s, 2, mpz_tstbit(mhs_to_Ptr(s, 0), mhs_to_Int(s, 1))); }
from_t mhs_mpz_xor(int s) { mpz_xor(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2)); return mhs_from_Unit(s, 3); }
#if WANT_INT64
from_t mhs_mpz_init_set_si64(int s) { mpz_init_set_si64(mhs_to_Ptr(s, 0), mhs_to_Int64(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_mpz_init_set_ui64(int s) { mpz_init_set_ui64(mhs_to_Ptr(s, 0), mhs_to_Word64(s, 1)); return mhs_from_Unit(s, 2); }
from_t mhs_mpz_get_si64(int s) { return mhs_from_Int64(s, 1, mpz_get_si64(mhs_to_Ptr(s, 0))); }
#endif  /* WANT_INT64 */
from_t mhs_mpz_log2(int s) {
  mpz_ptr a = mhs_to_Ptr(s, 0);
  return mhs_from_Int(s, 1, mpz_sizeinbase(a, 2) - 1);
}
#endif  /* WANT_GMP || WANT_IMATH */
#if WANT_TIME
from_t mhs_gettimeofday(int s) { return mhs_from_Int(s, 2, gettimeofday(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1))); }
#endif
from_t mhs_get_executable_path(int s) { return mhs_from_Ptr(s, 0, get_executable_path()); }

#if WANT_SOCKET
#include <sys/socket.h>
from_t mhs_F_SETFL(int s) { return mhs_from_Int(s, 0, F_SETFL); }
from_t mhs_O_NONBLOCK(int s) { return mhs_from_Int(s, 0, O_NONBLOCK); }
from_t mhs_SOL_SOCKET(int s) { return mhs_from_Int(s, 0, SOL_SOCKET); }
from_t mhs_SO_DEBUG(int s) { return mhs_from_Int(s, 0, SO_DEBUG); }
from_t mhs_SO_ERROR(int s) { return mhs_from_Int(s, 0, SO_ERROR); }
from_t mhs_SO_REUSEADDR(int s) { return mhs_from_Int(s, 0, SO_REUSEADDR); }
from_t mhs_SO_TYPE(int s) { return mhs_from_Int(s, 0, SO_TYPE); }
from_t mhs_accept(int s) { return mhs_from_Int(s, 3, accept(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1), mhs_to_Ptr(s, 2))); }
from_t mhs_bind(int s) { return mhs_from_Int(s, 3, bind(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2))); }
from_t mhs_close(int s) { return mhs_from_Int(s, 1, close(mhs_to_Int(s, 0))); }
from_t mhs_connect(int s) { return mhs_from_Int(s, 3, connect(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1), mhs_to_Int(s, 2))); }
from_t mhs_fcntl(int s) { return mhs_from_Int(s, 3, fcntl(mhs_to_Int(s, 0), mhs_to_Int(s, 1), mhs_to_Int(s, 2))); }
from_t mhs_getsockopt(int s) { return mhs_from_Int(s, 5, getsockopt(mhs_to_Int(s, 0), mhs_to_Int(s, 1), mhs_to_Int(s, 2), mhs_to_Ptr(s, 3), mhs_to_Ptr(s, 4))); }
from_t mhs_listen(int s) { return mhs_from_Int(s, 2, listen(mhs_to_Int(s, 0), mhs_to_Int(s, 1))); }
from_t mhs_recv(int s) { return mhs_from_Int(s, 4, recv(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1), mhs_to_Word(s, 2), mhs_to_Int(s, 3))); }
from_t mhs_send(int s) { return mhs_from_Int(s, 4, send(mhs_to_Int(s, 0), mhs_to_Ptr(s, 1), mhs_to_Word(s, 2), mhs_to_Int(s, 3))); }
from_t mhs_setsockopt(int s) { return mhs_from_Int(s, 5, setsockopt(mhs_to_Int(s, 0), mhs_to_Int(s, 1), mhs_to_Int(s, 2), mhs_to_Ptr(s, 3), mhs_to_Int(s, 4))); }
from_t mhs_socket(int s) { return mhs_from_Int(s, 3, socket(mhs_to_Int(s, 0), mhs_to_Int(s, 1), mhs_to_Int(s, 2))); }
#endif  /* WANT_SOCKET */

const struct ffi_entry ffi_table[] = {
  { "GETRAW", 0, mhs_GETRAW},
  { "GETTIMEMICRO", 0, mhs_GETTIMEMICRO},
  { "GETBOOTTIMEMICRO", 0, mhs_GETBOOTTIMEMICRO},
#if WANT_MATH
#if WANT_FLOAT64
  { "acos", 1, mhs_acos},
  { "asin", 1, mhs_asin},
  { "atan", 1, mhs_atan},
  { "atan2", 2, mhs_atan2},
  { "cos", 1, mhs_cos},
  { "exp", 1, mhs_exp},
  { "log", 1, mhs_log},
  { "sin", 1, mhs_sin},
  { "sqrt", 1, mhs_sqrt},
  { "tan", 1, mhs_tan},
  { "scalbn", 2, mhs_scalbn},
  { "pow", 2, mhs_pow},
  { "poke_flt64", 2, mhs_poke_flt64 },
  { "peek_flt64", 1, mhs_peek_flt64 },
#endif  /* WANT_FLOAT64 */
#if WANT_FLOAT32
  { "acosf", 1, mhs_acosf},
  { "asinf", 1, mhs_asinf},
  { "atanf", 1, mhs_atanf},
  { "atan2f", 2, mhs_atan2f},
  { "cosf", 1, mhs_cosf},
  { "expf", 1, mhs_expf},
  { "logf", 1, mhs_logf},
  { "sinf", 1, mhs_sinf},
  { "sqrtf", 1, mhs_sqrtf},
  { "tanf", 1, mhs_tanf},
  { "scalbnf", 2, mhs_scalbnf},
  { "powf", 2, mhs_powf},
  { "poke_flt32", 2, mhs_poke_flt32 },
  { "peek_flt32", 1, mhs_peek_flt32 },
#endif  /* WANT_FLOAT32 */
#endif  /* WANT_MATH */

#if defined(__EMSCRIPTEN__)
  { "js_debug", 1, mhs_js_debug},
  { "js_eval_run", 1, mhs_js_eval_run},
  { "js_eval_call", 1, mhs_js_eval_call},
  { "js_set_haskellCallback", 1, mhs_js_set_haskellCallback},
#endif

#if WANT_STDIO
  { "add_FILE", 1, mhs_add_FILE},
  { "putchar", 1, mhs_putchar},
  { "fopen", 2, mhs_fopen},
  { "tmpname", 2, mhs_tmpname},
  { "remove", 1, mhs_remove},
  { "system", 1, mhs_system},
#endif  /* WANT_STDIO */
#if WANT_FD
  { "add_fd", 1, mhs_add_fd},
  { "open", 3, mhs_open},
#endif  /* WANT_FD */
#if WANT_BUF
  { "add_buf", 2, mhs_add_buf},
#endif  /* WANT_BUF */
#if WANT_CRLF
  { "add_crlf", 1, mhs_add_crlf},
#endif  /* WANT_CRLF */
#if WANT_UTF8
  { "add_utf8", 1, mhs_add_utf8},
#endif  /* WANT_UTF8 */
#if WANT_BASE64
  { "add_base64_encoder", 1, mhs_add_base64_encoder},
  { "add_base64_decoder", 1, mhs_add_base64_decoder},
#endif  /* WANT_BASE64 */
  { "closeb", 1, mhs_closeb},
  { "&closeb", 0, mhs_addr_closeb},
  { "flushb", 1, mhs_flushb},
  { "getb", 1, mhs_getb},
  { "putb", 2, mhs_putb},
  { "ungetb", 2, mhs_ungetb},
  { "openb_wr_mem", 0, mhs_openwrmem},
  { "openb_rd_mem", 2, mhs_openrdmem},
  { "get_mem", 3, mhs_getmem},
  { "readb", 3, mhs_readb},
  { "writeb", 3, mhs_writeb},
  { "isattyb", 1, mhs_isattyb},
#if WANT_INT64
  { "filesizeb", 1, mhs_filesizeb},
#endif  /* WANT_INT64 */

#if WANT_MD5
  { "md5Array", 3, mhs_md5Array},
  { "md5BFILE", 2, mhs_md5BFILE},
  { "md5String", 2, mhs_md5String},
#endif  /* WANT_MD5 */

#if WANT_LZ77
  { "add_lz77_compressor", 1, mhs_add_lz77_compressor},
  { "add_lz77_decompressor", 1, mhs_add_lz77_decompressor},
  { "lz77c", 3, mhs_lz77c},
#endif  /* WANT_LZ77 */

#if WANT_LZMA
  { "add_lzma_compressor", 1, mhs_add_lzma_compressor},
  { "add_lzma_decompressor", 1, mhs_add_lzma_decompressor},
#endif  /* WANT_LZ77 */

#if WANT_RLE
  { "add_rle_compressor", 1, mhs_add_rle_compressor},
  { "add_rle_decompressor", 1, mhs_add_rle_decompressor},
#endif  /* WANT_RLE */

#if WANT_BWT
  { "add_bwt_compressor", 1, mhs_add_bwt_compressor},
  { "add_bwt_decompressor", 1, mhs_add_bwt_decompressor},
#endif  /* WANT_RLE */

  { "calloc", 2, mhs_calloc},
  { "realloc", 2, mhs_realloc},
  { "free", 1, mhs_free},
  { "&free", 0, mhs_addr_free},
  { "iswindows", 0, mhs_iswindows},
  { "ismacos", 0, mhs_ismacos},
  { "islinux", 0, mhs_islinux},
  { "ismingw", 0, mhs_ismingw},
  { "malloc", 1, mhs_malloc},
  { "memcpy", 3, mhs_memcpy},
  { "memmove", 3, mhs_memmove},
  { "strlen", 1, mhs_strlen},
  { "strcpy", 2, mhs_strcpy},
  { "peekPtr", 1, mhs_peekPtr},
  { "peekWord", 1, mhs_peekWord},
  { "pokePtr", 2, mhs_pokePtr},
  { "pokeWord", 2, mhs_pokeWord},

  { "peek_uint8", 1, mhs_peek_uint8},
  { "poke_uint8", 2, mhs_poke_uint8},
  { "peek_uint16", 1, mhs_peek_uint16},
  { "poke_uint16", 2, mhs_poke_uint16},
  { "peek_uint32", 1, mhs_peek_uint32},
  { "poke_uint32", 2, mhs_poke_uint32},
#if WANT_INT64
  { "peek_uint64", 1, mhs_peek_uint64},
  { "poke_uint64", 2, mhs_poke_uint64},
#endif  /* WANT_INT64 */
  { "peek_uint", 1, mhs_peek_uint},
  { "poke_uint", 2, mhs_poke_uint},

  { "peek_int8", 1, mhs_peek_int8},
  { "poke_int8", 2, mhs_poke_int8},
  { "peek_int16", 1, mhs_peek_int16},
  { "poke_int16", 2, mhs_poke_int16},
  { "peek_int32", 1, mhs_peek_int32},
  { "poke_int32", 2, mhs_poke_int32},
#if WANT_INT64
  { "peek_int64", 1, mhs_peek_int64},
  { "poke_int64", 2, mhs_poke_int64},
#endif  /* WANT_INT64 */
  { "peek_int", 1, mhs_peek_int},
  { "poke_int", 2, mhs_poke_int},
  { "peek_char", 1, mhs_peek_char},
  { "poke_char", 2, mhs_poke_char},
  { "peek_schar", 1, mhs_peek_schar},
  { "poke_schar", 2, mhs_poke_schar},
  { "peek_uchar", 1, mhs_peek_uchar},
  { "poke_uchar", 2, mhs_poke_uchar},
  { "peek_short", 1, mhs_peek_short},
  { "poke_short", 2, mhs_poke_short},
  { "peek_ushort", 1, mhs_peek_ushort},
  { "poke_ushort", 2, mhs_poke_ushort},
  { "peek_llong", 1, mhs_peek_llong},
  { "peek_long", 1, mhs_peek_long},
  { "peek_ullong", 1, mhs_peek_ullong},
  { "peek_ulong", 1, mhs_peek_ulong},
  { "peek_size_t", 1, mhs_peek_size_t},
  { "poke_llong", 2, mhs_poke_llong},
  { "poke_long", 2, mhs_poke_long},
  { "poke_ullong", 2, mhs_poke_ullong},
  { "poke_ulong", 2, mhs_poke_ulong},
  { "poke_size_t", 2, mhs_poke_size_t},
  { "sizeof_char", 0, mhs_sizeof_char},
  { "sizeof_short", 0, mhs_sizeof_short},
  { "sizeof_int", 0, mhs_sizeof_int},
  { "sizeof_llong", 0, mhs_sizeof_llong},
  { "sizeof_long", 0, mhs_sizeof_long},
  { "sizeof_size_t", 0, mhs_sizeof_size_t},
#if WANT_DIR
  { "c_d_name", 1, mhs_c_d_name},
  { "closedir", 1, mhs_closedir},
  { "opendir", 1, mhs_opendir},
  { "readdir", 1, mhs_readdir},
  { "chdir", 1, mhs_chdir},
  { "mkdir", 2, mhs_mkdir},
  { "getcwd", 2, mhs_getcwd},
  { "set_permissions", 2, mhs_set_permissions},
  { "get_permissions", 1, mhs_get_permissions},
  { "get_file_size", 1, mhs_get_file_size},
#endif  /* WANT_DIR */
  { "getcpu", 2, mhs_getcpu},
  { "want_gmp", 0, mhs_want_gmp},
  { "want_imath", 0, mhs_want_imath},
#if WANT_GMP || WANT_IMATH
  { "new_mpz", 0, mhs_new_mpz},
  { "mpz_abs", 2, mhs_mpz_abs},
  { "mpz_add", 3, mhs_mpz_add},
  { "mpz_and", 3, mhs_mpz_and},
  { "mpz_cmp", 2, mhs_mpz_cmp},
#if WANT_FLOAT64
  { "mpz_get_d", 1, mhs_mpz_get_d},
#endif  /* WANT_FLOAT64 */
  { "mpz_get_si", 1, mhs_mpz_get_si},
  { "mpz_init_set_si", 2, mhs_mpz_init_set_si},
  { "mpz_init_set_ui", 2, mhs_mpz_init_set_ui},
  { "mpz_ior", 3, mhs_mpz_ior},
  { "mpz_mul", 3, mhs_mpz_mul},
  { "mpz_mul_2exp", 3, mhs_mpz_mul_2exp},
  { "mpz_neg", 2, mhs_mpz_neg},
  { "mpz_popcount", 1, mhs_mpz_popcount},
  { "mpz_sub", 3, mhs_mpz_sub},
  { "mpz_fdiv_q_2exp", 3, mhs_mpz_fdiv_q_2exp},
  { "mpz_tdiv_qr", 4, mhs_mpz_tdiv_qr},
  { "mpz_tstbit", 2, mhs_mpz_tstbit},
  { "mpz_xor", 3, mhs_mpz_xor},
#if WANT_FLOAT32
  { "mpz_get_f", 1, mhs_mpz_get_f},
#endif  /* WANT_FLOAT32 */
#if WANT_INT64
  { "mpz_init_set_si64", 2, mhs_mpz_init_set_si64},
  { "mpz_init_set_ui64", 2, mhs_mpz_init_set_ui64},
  { "mpz_get_si64", 1, mhs_mpz_get_si64},
#endif  /* WANT_INT64 */
  { "mpz_log2", 1, mhs_mpz_log2},
#endif  /* WANT_GMP || WANT_IMATH */
#if WANT_TIME
  { "gettimeofday", 2, mhs_gettimeofday},
#endif
#if WANT_ERRNO
  { "E2BIG", 0, mhs_E2BIG},
  { "EACCES", 0, mhs_EACCES},
  { "EADDRINUSE", 0, mhs_EADDRINUSE},
  { "EADDRNOTAVAIL", 0, mhs_EADDRNOTAVAIL},
  { "EADV", 0, mhs_EADV},
  { "EAFNOSUPPORT", 0, mhs_EAFNOSUPPORT},
  { "EAGAIN", 0, mhs_EAGAIN},
  { "EALREADY", 0, mhs_EALREADY},
  { "EBADF", 0, mhs_EBADF},
  { "EBADMSG", 0, mhs_EBADMSG},
  { "EBADRPC", 0, mhs_EBADRPC},
  { "EBUSY", 0, mhs_EBUSY},
  { "ECHILD", 0, mhs_ECHILD},
  { "ECOMM", 0, mhs_ECOMM},
  { "ECONNABORTED", 0, mhs_ECONNABORTED},
  { "ECONNREFUSED", 0, mhs_ECONNREFUSED},
  { "ECONNRESET", 0, mhs_ECONNRESET},
  { "EDEADLK", 0, mhs_EDEADLK},
  { "EDESTADDRREQ", 0, mhs_EDESTADDRREQ},
  { "EDIRTY", 0, mhs_EDIRTY},
  { "EDOM", 0, mhs_EDOM},
  { "EDQUOT", 0, mhs_EDQUOT},
  { "EEXIST", 0, mhs_EEXIST},
  { "EFAULT", 0, mhs_EFAULT},
  { "EFBIG", 0, mhs_EFBIG},
  { "EFTYPE", 0, mhs_EFTYPE},
  { "EHOSTDOWN", 0, mhs_EHOSTDOWN},
  { "EHOSTUNREACH", 0, mhs_EHOSTUNREACH},
  { "EIDRM", 0, mhs_EIDRM},
  { "EILSEQ", 0, mhs_EILSEQ},
  { "EINPROGRESS", 0, mhs_EINPROGRESS},
  { "EINTR", 0, mhs_EINTR},
  { "EINVAL", 0, mhs_EINVAL},
  { "EIO", 0, mhs_EIO},
  { "EISCONN", 0, mhs_EISCONN},
  { "EISDIR", 0, mhs_EISDIR},
  { "ELOOP", 0, mhs_ELOOP},
  { "EMFILE", 0, mhs_EMFILE},
  { "EMLINK", 0, mhs_EMLINK},
  { "EMSGSIZE", 0, mhs_EMSGSIZE},
  { "EMULTIHOP", 0, mhs_EMULTIHOP},
  { "ENAMETOOLONG", 0, mhs_ENAMETOOLONG},
  { "ENETDOWN", 0, mhs_ENETDOWN},
  { "ENETRESET", 0, mhs_ENETRESET},
  { "ENETUNREACH", 0, mhs_ENETUNREACH},
  { "ENFILE", 0, mhs_ENFILE},
  { "ENOBUFS", 0, mhs_ENOBUFS},
  { "ENODATA", 0, mhs_ENODATA},
  { "ENODEV", 0, mhs_ENODEV},
  { "ENOENT", 0, mhs_ENOENT},
  { "ENOEXEC", 0, mhs_ENOEXEC},
  { "ENOLCK", 0, mhs_ENOLCK},
  { "ENOLINK", 0, mhs_ENOLINK},
  { "ENOMEM", 0, mhs_ENOMEM},
  { "ENOMSG", 0, mhs_ENOMSG},
  { "ENONET", 0, mhs_ENONET},
  { "ENOPROTOOPT", 0, mhs_ENOPROTOOPT},
  { "ENOSPC", 0, mhs_ENOSPC},
  { "ENOSR", 0, mhs_ENOSR},
  { "ENOSTR", 0, mhs_ENOSTR},
  { "ENOSYS", 0, mhs_ENOSYS},
  { "ENOTBLK", 0, mhs_ENOTBLK},
  { "ENOTCONN", 0, mhs_ENOTCONN},
  { "ENOTDIR", 0, mhs_ENOTDIR},
  { "ENOTEMPTY", 0, mhs_ENOTEMPTY},
  { "ENOTSOCK", 0, mhs_ENOTSOCK},
  { "ENOTSUP", 0, mhs_ENOTSUP},
  { "ENOTTY", 0, mhs_ENOTTY},
  { "ENXIO", 0, mhs_ENXIO},
  { "EOPNOTSUPP", 0, mhs_EOPNOTSUPP},
  { "EPERM", 0, mhs_EPERM},
  { "EPFNOSUPPORT", 0, mhs_EPFNOSUPPORT},
  { "EPIPE", 0, mhs_EPIPE},
  { "EPROCLIM", 0, mhs_EPROCLIM},
  { "EPROCUNAVAIL", 0, mhs_EPROCUNAVAIL},
  { "EPROGMISMATCH", 0, mhs_EPROGMISMATCH},
  { "EPROGUNAVAIL", 0, mhs_EPROGUNAVAIL},
  { "EPROTO", 0, mhs_EPROTO},
  { "EPROTONOSUPPORT", 0, mhs_EPROTONOSUPPORT},
  { "EPROTOTYPE", 0, mhs_EPROTOTYPE},
  { "ERANGE", 0, mhs_ERANGE},
  { "EREMCHG", 0, mhs_EREMCHG},
  { "EREMOTE", 0, mhs_EREMOTE},
  { "EROFS", 0, mhs_EROFS},
  { "ERPCMISMATCH", 0, mhs_ERPCMISMATCH},
  { "ERREMOTE", 0, mhs_ERREMOTE},
  { "ESHUTDOWN", 0, mhs_ESHUTDOWN},
  { "ESOCKTNOSUPPORT", 0, mhs_ESOCKTNOSUPPORT},
  { "ESPIPE", 0, mhs_ESPIPE},
  { "ESRCH", 0, mhs_ESRCH},
  { "ESRMNT", 0, mhs_ESRMNT},
  { "ESTALE", 0, mhs_ESTALE},
  { "ETIME", 0, mhs_ETIME},
  { "ETIMEDOUT", 0, mhs_ETIMEDOUT},
  { "ETOOMANYREFS", 0, mhs_ETOOMANYREFS},
  { "ETXTBSY", 0, mhs_ETXTBSY},
  { "EUSERS", 0, mhs_EUSERS},
  { "EWOULDBLOCK", 0, mhs_EWOULDBLOCK},
  { "EXDEV", 0, mhs_EXDEV},
  { "&errno", 0, mhs_addr_errno},
  { "strerror_r", 3, mhs_strerror_r},
#endif
  { "get_executable_path", 0, mhs_get_executable_path},
#if WANT_ENV
  { "getenv", 1, mhs_getenv},
  { "environ", 0, mhs_environ},
  { "unsetenv", 1, mhs_unsetenv},
  { "setenv", 3, mhs_setenv},
#endif  /* WANT_ENV */
#if WANT_SOCKET
  { "F_SETFL", 0, mhs_F_SETFL},
  { "O_NONBLOCK", 0, mhs_O_NONBLOCK},
  { "SOL_SOCKET", 0, mhs_SOL_SOCKET},
  { "SO_DEBUG", 0, mhs_SO_DEBUG},
  { "SO_ERROR", 0, mhs_SO_ERROR},
  { "SO_REUSEADDR", 0, mhs_SO_REUSEADDR},
  { "SO_TYPE", 0, mhs_SO_TYPE},
  { "accept", 3, mhs_accept},
  { "bind", 3, mhs_bind},
  { "close", 1, mhs_close},
  { "connect", 3, mhs_connect},
  { "fcntl", 3, mhs_fcntl},
  { "getsockopt", 5, mhs_getsockopt},
  { "listen", 2, mhs_listen},
  { "recv", 4, mhs_recv},
  { "send", 4, mhs_send},
  { "setsockopt", 5, mhs_setsockopt},
  { "socket", 3, mhs_socket},
#endif  /* WANT_SOCKET */
  { 0,0 }
};

int num_ffi = sizeof(ffi_table) / sizeof(ffi_table[0]);



/*******************************/
/* HsFFI.h API */

void
hs_init(int *argc, char **argv[])
{
  (void)mhs_main(*argc, *argv);
}

void
hs_exit(void)
{
  extern void _exit(int);       /* XXX why do we need this */
  _exit(0);
}

void
hs_set_argv(int argc, char *argv[])
{
  ERR("hs_set_argv not implemented");
}

void
hs_perform_gc(void)
{
  gc();
}

void
hs_free_stable_ptr(void *sp)
{
  free_stableptr((uvalue_t)sp);
}

void
hs_free_fun_ptr(HsFunPtr fp)
{
  ERR("hs_free_fun_ptr not implemented");
}
