#include "config.h"

#include <inttypes.h>
#include <stdlib.h>
#include <time.h>
#if WANT_MATH
#include <math.h>
#endif  /* WANT_MATH */

#if INTPTR_MAX == 0x7fff
#define WORD_SIZE 16
#elif INTPTR_MAX == 0x7fffffff
#define WORD_SIZE 32
#elif INTPTR_MAX == 0x7fffffffffffffff
#define WORD_SIZE 64
#else
#error Unknown WORD_SIZE
#endif

typedef double flt64_t;
typedef float flt32_t;

struct node;
typedef intptr_t value_t;       /* Make value the same size as pointers, since they are in a union */
typedef uintptr_t uvalue_t;     /* Make unsigned value the same size as pointers, since they are in a union */
typedef intptr_t stackptr_t;    /* Index into stack */
typedef struct node* NODEPTR;
typedef void (*HsFunPtr)(void);

typedef int from_t;
typedef from_t (*funptr_t)(int);
struct ffi_entry {
  const char *ffi_name;
  int         ffi_arity;
  funptr_t    ffi_fun;
};
extern const struct ffi_entry *xffi_table;

struct ffe_entry {
  const char  *ffe_name;
  NODEPTR      ffe_value;
};
extern struct ffe_entry *xffe_table;

from_t mhs_from_Double(intptr_t, int, flt64_t);
from_t mhs_from_Float(intptr_t, int, flt32_t);
from_t mhs_from_Int(intptr_t, int, intptr_t);
from_t mhs_from_Word(intptr_t, int, uintptr_t);
from_t mhs_from_Word8(intptr_t, int, uintptr_t);
from_t mhs_from_Ptr(intptr_t, int, void *);
from_t mhs_from_FunPtr(intptr_t, int, HsFunPtr);
from_t mhs_from_CChar(intptr_t, int, char);
from_t mhs_from_CSChar(intptr_t, int, signed char);
from_t mhs_from_CUChar(intptr_t, int, unsigned char);
from_t mhs_from_CShort(intptr_t, int, short);
from_t mhs_from_CUShort(intptr_t, int, unsigned short);
from_t mhs_from_CInt(intptr_t, int, int);
from_t mhs_from_CUInt(intptr_t, int, unsigned int);
from_t mhs_from_CLong(intptr_t, int, long);
from_t mhs_from_CULong(intptr_t, int, unsigned long);
from_t mhs_from_CLLong(intptr_t, int, long long);
from_t mhs_from_CULLong(intptr_t, int, unsigned long long);
from_t mhs_from_CSize(intptr_t, int, size_t);
from_t mhs_from_CTime(intptr_t, int, time_t); /* XXX wrong */
// not on MacOS void mhs_from_CSSize(intptr_t, int, ssize_t);
from_t mhs_from_CIntPtr(intptr_t, int, intptr_t);
from_t mhs_from_CUIntPtr(intptr_t, int, uintptr_t);
from_t mhs_from_Unit(intptr_t, int);

flt64_t            mhs_to_Double(intptr_t, int);
flt32_t            mhs_to_Float(intptr_t, int);
intptr_t           mhs_to_Int(intptr_t, int);
uintptr_t          mhs_to_Word(intptr_t, int);
uint8_t            mhs_to_Word8(intptr_t, int);
void*              mhs_to_Ptr(intptr_t, int);
HsFunPtr           mhs_to_FunPtr(intptr_t, int);
char               mhs_to_CChar(intptr_t, int);
signed char        mhs_to_CSChar(intptr_t, int);
unsigned char      mhs_to_CUChar(intptr_t, int);
short              mhs_to_CShort(intptr_t, int);
unsigned short     mhs_to_CUShort(intptr_t, int);
int                mhs_to_CInt(intptr_t, int);
unsigned int       mhs_to_CUInt(intptr_t, int);
long               mhs_to_CLong(intptr_t, int);
unsigned long      mhs_to_CULong(intptr_t, int);
long long          mhs_to_CLLong(intptr_t, int);
unsigned long long mhs_to_CULLong(intptr_t, int);
size_t             mhs_to_CSize(intptr_t, int);
time_t             mhs_to_CTime(intptr_t, int); /* XXX wrong */
// ssize_t            mhs_to_CSSize(intptr_t, int);
intptr_t           mhs_to_CIntPtr(intptr_t, int);
uintptr_t          mhs_to_CUIntPtr(intptr_t, int);

void       ffe_push(NODEPTR);
void       ffe_pop(void);
stackptr_t ffe_alloc(void);
void       ffe_apply(void);
stackptr_t ffe_eval(void);
stackptr_t ffe_exec(void);
void       gc_check(size_t);

void *     apply_sp(uvalue_t, void *);
