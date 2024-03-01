#include <inttypes.h>
#include <stdlib.h>

typedef void (*funptr_t)(int);
struct ffi_entry {
  const char *ffi_name;
  funptr_t ffi_fun;
};
extern struct ffi_entry *xffi_table;

void mhs_from_Double(intptr_t, int, double);
void mhs_from_Int(intptr_t, int, intptr_t);
void mhs_from_Word(intptr_t, int, uintptr_t);
void mhs_from_Word8(intptr_t, int, uintptr_t);
void mhs_from_Ptr(intptr_t, int, void *);
void mhs_from_CChar(intptr_t, int, char);
void mhs_from_CSChar(intptr_t, int, signed char);
void mhs_from_CUChar(intptr_t, int, unsigned char);
void mhs_from_CSHORT(intptr_t, int, short);
void mhs_from_CUSHORT(intptr_t, int, unsigned short);
void mhs_from_CINT(intptr_t, int, int);
void mhs_from_CUINT(intptr_t, int, unsigned int);
void mhs_from_CLONG(intptr_t, int, long);
void mhs_from_CULONG(intptr_t, int, unsigned long);
void mhs_from_CLLONG(intptr_t, int, long long);
void mhs_from_CULLONG(intptr_t, int, unsigned long long);
void mhs_from_CSize(intptr_t, int, size_t);
// not on MacOS void mhs_from_CSSize(intptr_t, int, ssize_t);
void mhs_from_CIntPtr(intptr_t, int, intptr_t);
void mhs_from_CUIntPtr(intptr_t, int, uintptr_t);
void mhs_from_Unit(intptr_t, int);

double             mhs_to_Double(intptr_t, int);
intptr_t           mhs_to_Int(intptr_t, int);
uintptr_t          mhs_to_Word(intptr_t, int);
uint8_t            mhs_to_Word8(intptr_t, int);
void*              mhs_to_Ptr(intptr_t, int);
char               mhs_to_CChar(intptr_t, int);
signed char        mhs_to_CSChar(intptr_t, int);
unsigned char      mhs_to_CUChar(intptr_t, int);
short              mhs_to_CSHORT(intptr_t, int);
unsigned short     mhs_to_CUSHORT(intptr_t, int);
int                mhs_to_CINT(intptr_t, int);
unsigned int       mhs_to_CUINT(intptr_t, int);
long               mhs_to_CLONG(intptr_t, int);
unsigned long      mhs_to_CULONG(intptr_t, int);
long long          mhs_to_CLLONG(intptr_t, int);
unsigned long long mhs_to_CULLONG(intptr_t, int);
size_t             mhs_to_CSize(intptr_t, int);
// ssize_t            mhs_to_CSSize(intptr_t, int);
intptr_t           mhs_to_CIntPtr(intptr_t, int);
uintptr_t          mhs_to_CUIntPtr(intptr_t, int);
