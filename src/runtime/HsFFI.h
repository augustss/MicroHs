#include <inttypes.h>
#include <limits.h>
#include <float.h>
#include <unistd.h>
#include <string.h>

typedef char          HsChar;
typedef	intptr_t       HsInt;
typedef int8_t         HsInt8;
typedef int16_t        HsInt16;
typedef int32_t        HsInt32;
typedef int64_t        HsInt64;
typedef uint8_t        HsWord8;
typedef uint16_t       HsWord16;
typedef uint32_t       HsWord32;
typedef uint64_t       HsWord64;
typedef float          HsFloat;
typedef double         HsDouble;
typedef int            HsBool;
typedef void *         HsPtr;
typedef void           (*HsFunPtr)(void);
typedef	void * 	       StablePtr;

#define HS_CHAR_MIN	CHAR_MIN
#define HS_CHAR_MAX	CHAR_MAX
#define HS_INT_MIN	INT_MIN
#define HS_INT_MAX      INT_MAX
#define HS_INT8_MIN     INT8_MIN
#define HS_INT8_MAX     INT8_MAX
#define HS_INT16_MIN    INT16_MIN
#define HS_INT16_MAX    INT16_MAX
#define HS_INT32_MIN    INT32_MIN
#define HS_INT32_MAX    INT32_MAX
#define HS_INT64_MIN    INT64_MIN
#define HS_INT64_MAX    INT64_MAX
#define HS_WORD8_MAX    UINT8_MAX
#define HS_WORD16_MAX   UINT16_MAX
#define HS_WORD32_MAX   UINT32_MAX
#define HS_WORD64_MAX   UINT64_MAX

#define HS_FLOAT_RADIX	     FLT_RADIX
#define HS_FLOAT_ROUND	     ?????
#define HS_FLOAT_EPSILON     FLT_EPSILON
#define HS_DOUBLE_EPSILON    DBL_EPSILON
#define HS_FLOAT_DIG	     FLT_DIG
#define HS_DOUBLE_DIG	     DBL_DIG
#define HS_FLOAT_MANT_DIG    FLT_MANT_DIG
#define HS_DOUBLE_MANT_DIG   DBL_MAN_DIG
#define HS_FLOAT_MIN	     FLT_MIN
#define HS_DOUBLE_MIN	     DBL_MIN
#define HS_FLOAT_MIN_EXP     FLT_MIN_EXP
#define HS_DOUBLE_MIN_EXP    DBL_MIN_EXP
#define HS_FLOAT_MIN_10_EXP  FLT_MIN_10_EXP
#define HS_DOUBLE_MIN_10_EXP DBL_MIN_10_EXP
#define HS_FLOAT_MAX	     FLT_MAX
#define HS_DOUBLE_MAX	     DBL_MAX
#define HS_FLOAT_MAX_EXP     FLT_MAX_EXP
#define HS_DOUBLE_MAX_EXP    DBL_MAX_EXP
#define HS_FLOAT_MAX_10_EXP  FLT_MAX_10_EXP
#define HS_DOUBLE_MAX_10_EXP DBL_MAX_10_EXP
#define HS_BOOL_FALSE	     0
#define HS_BOOL_TRUE	     1

/*
void hs_init     (int *argc, char **argv[]);  
void hs_exit     (void);  
void hs_set_argv (int argc, char *argv[]);  
 
void hs_perform_gc (void);  
 
void hs_free_stable_ptr (HsStablePtr sp);  
void hs_free_fun_ptr    (HsFunPtr fp);
*/
/* Using prototypes and defining these in eval.c causes linker problems
 * for the MicroHs library.
 * Use this temporary fix.
 */

/*******************************/
/* HsFFI.h API */


static void ERR(const char *msg)
{
  (void)write(2, msg, strlen(msg));
  _exit(1);
}

static void hs_init(int *argc, char **argv[])
{
  extern int mhs_main(int argc, char **argv);
  (void)mhs_main(*argc, *argv);
}

static void hs_exit(void)
{
  _exit(0);
}

static void hs_set_argv(int argc, char *argv[])
{
  ERR("hs_set_argv not implemented");
}
 
static void hs_perform_gc(void)
{
  extern void gc(void);
  gc();
}

void xhs_free_stable_ptr(void *sp)
{
  /* XXX need to change representation of stablepointers */
}

void hs_free_fun_ptr(HsFunPtr fp)
{
  ERR("hs_free_fun_ptr not implemented");
}
