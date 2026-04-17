#ifndef _LZMA_H_
#define _LZMA_H_

#define LZMA_HEADER_SIZE 13

#ifndef __APPLE__
#ifndef _7ZIP_AFFINITY_DISABLE
// _GNU_SOURCE can be required for pthread_setaffinity_np() / CPU_ZERO / CPU_SET
#define _GNU_SOURCE
#endif
#endif

/* LzmaDec.h -- LZMA Decoder
2020-03-19 : Igor Pavlov : Public domain */

#ifndef __LZMA_DEC_H
#define __LZMA_DEC_H

/* 7zTypes.h -- Basic types
2022-04-01 : Igor Pavlov : Public domain */

#ifndef __7Z_TYPES_H
#define __7Z_TYPES_H

#ifdef _WIN32
/* #include <windows.h> */
#else
#include <errno.h>
#endif

#include <stddef.h>
#include <string.h>

#ifndef EXTERN_C_BEGIN
#ifdef __cplusplus
#define EXTERN_C_BEGIN \
    extern "C"         \
    {
#define EXTERN_C_END }
#else
#define EXTERN_C_BEGIN
#define EXTERN_C_END
#endif
#endif

EXTERN_C_BEGIN

#define SZ_OK 0

#define SZ_ERROR_DATA 1
#define SZ_ERROR_MEM 2
#define SZ_ERROR_CRC 3
#define SZ_ERROR_UNSUPPORTED 4
#define SZ_ERROR_PARAM 5
#define SZ_ERROR_INPUT_EOF 6
#define SZ_ERROR_OUTPUT_EOF 7
#define SZ_ERROR_READ 8
#define SZ_ERROR_WRITE 9
#define SZ_ERROR_PROGRESS 10
#define SZ_ERROR_FAIL 11
#define SZ_ERROR_THREAD 12

#define SZ_ERROR_ARCHIVE 16
#define SZ_ERROR_NO_ARCHIVE 17

typedef int SRes;

#ifdef _MSC_VER
#if _MSC_VER > 1200
#define MY_ALIGN(n) __declspec(align(n))
#else
#define MY_ALIGN(n)
#endif
#else
#define MY_ALIGN(n) __attribute__((aligned(n)))
#endif

#ifdef _WIN32

/* typedef DWORD WRes; */
typedef unsigned WRes;
#define MY_SRes_HRESULT_FROM_WRes(x) HRESULT_FROM_WIN32(x)

// #define MY_HRES_ERROR__INTERNAL_ERROR  MY_SRes_HRESULT_FROM_WRes(ERROR_INTERNAL_ERROR)

#else // _WIN32

// #define ENV_HAVE_LSTAT
typedef int WRes;

// (FACILITY_ERRNO = 0x800) is 7zip's FACILITY constant to represent (errno) errors in HRESULT
#define MY__FACILITY_ERRNO 0x800
#define MY__FACILITY_WIN32 7
#define MY__FACILITY__WRes MY__FACILITY_ERRNO

#define MY_HRESULT_FROM_errno_CONST_ERROR(x) ((HRESULT)(((HRESULT)(x)&0x0000FFFF) | (MY__FACILITY__WRes << 16) | (HRESULT)0x80000000))

#define MY_SRes_HRESULT_FROM_WRes(x) \
    ((HRESULT)(x) <= 0 ? ((HRESULT)(x)) : MY_HRESULT_FROM_errno_CONST_ERROR(x))

// we call macro HRESULT_FROM_WIN32 for system errors (WRes) that are (errno)
#define HRESULT_FROM_WIN32(x) MY_SRes_HRESULT_FROM_WRes(x)

/*
#define ERROR_FILE_NOT_FOUND             2L
#define ERROR_ACCESS_DENIED              5L
#define ERROR_NO_MORE_FILES              18L
#define ERROR_LOCK_VIOLATION             33L
#define ERROR_FILE_EXISTS                80L
#define ERROR_DISK_FULL                  112L
#define ERROR_NEGATIVE_SEEK              131L
#define ERROR_ALREADY_EXISTS             183L
#define ERROR_DIRECTORY                  267L
#define ERROR_TOO_MANY_POSTS             298L

#define ERROR_INTERNAL_ERROR             1359L
#define ERROR_INVALID_REPARSE_DATA       4392L
#define ERROR_REPARSE_TAG_INVALID        4393L
#define ERROR_REPARSE_TAG_MISMATCH       4394L
*/

// we use errno equivalents for some WIN32 errors:

#define ERROR_INVALID_PARAMETER EINVAL
#define ERROR_INVALID_FUNCTION EINVAL
#define ERROR_ALREADY_EXISTS EEXIST
#define ERROR_FILE_EXISTS EEXIST
#define ERROR_PATH_NOT_FOUND ENOENT
#define ERROR_FILE_NOT_FOUND ENOENT
#define ERROR_DISK_FULL ENOSPC
// #define ERROR_INVALID_HANDLE        EBADF

// we use FACILITY_WIN32 for errors that has no errno equivalent
// Too many posts were made to a semaphore.
#define ERROR_TOO_MANY_POSTS ((HRESULT)0x8007012AL)
#define ERROR_INVALID_REPARSE_DATA ((HRESULT)0x80071128L)
#define ERROR_REPARSE_TAG_INVALID ((HRESULT)0x80071129L)

// if (MY__FACILITY__WRes != FACILITY_WIN32),
// we use FACILITY_WIN32 for COM errors:
#define E_OUTOFMEMORY ((HRESULT)0x8007000EL)
#define E_INVALIDARG ((HRESULT)0x80070057L)
#define MY__E_ERROR_NEGATIVE_SEEK ((HRESULT)0x80070083L)

/*
// we can use FACILITY_ERRNO for some COM errors, that have errno equivalents:
#define E_OUTOFMEMORY             MY_HRESULT_FROM_errno_CONST_ERROR(ENOMEM)
#define E_INVALIDARG              MY_HRESULT_FROM_errno_CONST_ERROR(EINVAL)
#define MY__E_ERROR_NEGATIVE_SEEK MY_HRESULT_FROM_errno_CONST_ERROR(EINVAL)
*/

#define TEXT(quote) quote

#define FILE_ATTRIBUTE_READONLY 0x0001
#define FILE_ATTRIBUTE_HIDDEN 0x0002
#define FILE_ATTRIBUTE_SYSTEM 0x0004
#define FILE_ATTRIBUTE_DIRECTORY 0x0010
#define FILE_ATTRIBUTE_ARCHIVE 0x0020
#define FILE_ATTRIBUTE_DEVICE 0x0040
#define FILE_ATTRIBUTE_NORMAL 0x0080
#define FILE_ATTRIBUTE_TEMPORARY 0x0100
#define FILE_ATTRIBUTE_SPARSE_FILE 0x0200
#define FILE_ATTRIBUTE_REPARSE_POINT 0x0400
#define FILE_ATTRIBUTE_COMPRESSED 0x0800
#define FILE_ATTRIBUTE_OFFLINE 0x1000
#define FILE_ATTRIBUTE_NOT_CONTENT_INDEXED 0x2000
#define FILE_ATTRIBUTE_ENCRYPTED 0x4000

#define FILE_ATTRIBUTE_UNIX_EXTENSION 0x8000 /* trick for Unix */

#endif

#ifndef RINOK
#define RINOK(x)               \
    {                          \
        int __result__ = (x);  \
        if (__result__ != 0)   \
            return __result__; \
    }
#endif

#ifndef RINOK_WRes
#define RINOK_WRes(x)          \
    {                          \
        WRes __result__ = (x); \
        if (__result__ != 0)   \
            return __result__; \
    }
#endif

typedef unsigned char Byte;
typedef short Int16;
typedef unsigned short UInt16;

#ifdef _LZMA_UINT32_IS_ULONG
typedef long Int32;
typedef unsigned long UInt32;
#else
typedef int Int32;
typedef unsigned int UInt32;
#endif

#ifndef _WIN32

typedef int INT;
typedef Int32 INT32;
typedef unsigned int UINT;
typedef UInt32 UINT32;
typedef INT32 LONG; // LONG, ULONG and DWORD must be 32-bit for _WIN32 compatibility
typedef UINT32 ULONG;

#undef DWORD
typedef UINT32 DWORD;

#define VOID void

#define HRESULT LONG

typedef void *LPVOID;
// typedef void VOID;
// typedef ULONG_PTR DWORD_PTR, *PDWORD_PTR;
// gcc / clang on Unix  : sizeof(long==sizeof(void*) in 32 or 64 bits)
typedef long INT_PTR;
typedef unsigned long UINT_PTR;
typedef long LONG_PTR;
typedef unsigned long DWORD_PTR;

typedef size_t SIZE_T;

#endif //  _WIN32

#define MY_HRES_ERROR__INTERNAL_ERROR ((HRESULT)0x8007054FL)

#ifdef _SZ_NO_INT_64

/* define _SZ_NO_INT_64, if your compiler doesn't support 64-bit integers.
   NOTES: Some code will work incorrectly in that case! */

typedef long Int64;
typedef unsigned long UInt64;

#else

#if defined(_MSC_VER) || defined(__BORLANDC__)
typedef __int64 Int64;
typedef unsigned __int64 UInt64;
#define UINT64_CONST(n) n
#else
typedef long long int Int64;
typedef unsigned long long int UInt64;
#define UINT64_CONST(n) n##ULL
#endif

#endif

#ifdef _LZMA_NO_SYSTEM_SIZE_T
typedef UInt32 SizeT;
#else
typedef size_t SizeT;
#endif

typedef int BoolInt;
/* typedef BoolInt Bool; */
#define True 1
#define False 0

#ifdef _WIN32
#define MY_STD_CALL __stdcall
#else
#define MY_STD_CALL
#endif

#ifdef _MSC_VER

#if _MSC_VER >= 1300
#define MY_NO_INLINE __declspec(noinline)
#else
#define MY_NO_INLINE
#endif

#define MY_FORCE_INLINE __forceinline

#define MY_CDECL __cdecl
#define MY_FAST_CALL __fastcall

#else //  _MSC_VER

#if (defined(__GNUC__) && (__GNUC__ >= 4)) || (defined(__clang__) && (__clang_major__ >= 4)) || defined(__INTEL_COMPILER) || defined(__xlC__)
#define MY_NO_INLINE __attribute__((noinline))
// #define MY_FORCE_INLINE __attribute__((always_inline)) inline
#else
#define MY_NO_INLINE
#endif

#define MY_FORCE_INLINE

#define MY_CDECL

#if defined(_M_IX86) || defined(__i386__)
// #define MY_FAST_CALL __attribute__((fastcall))
// #define MY_FAST_CALL __attribute__((cdecl))
#define MY_FAST_CALL
#elif defined(MY_CPU_AMD64)
// #define MY_FAST_CALL __attribute__((ms_abi))
#define MY_FAST_CALL
#else
#define MY_FAST_CALL
#endif

#endif //  _MSC_VER

/* The following interfaces use first parameter as pointer to structure */

typedef struct IByteIn IByteIn;
struct IByteIn
{
    Byte (*Read)(const IByteIn *p); /* reads one byte, returns 0 in case of EOF or error */
};
#define IByteIn_Read(p) (p)->Read(p)

typedef struct IByteOut IByteOut;
struct IByteOut
{
    void (*Write)(const IByteOut *p, Byte b);
};
#define IByteOut_Write(p, b) (p)->Write(p, b)

typedef struct ISeqInStream ISeqInStream;
struct ISeqInStream
{
    SRes (*Read)(const ISeqInStream *p, void *buf, size_t *size);
    /* if (input(*size) != 0 && output(*size) == 0) means end_of_stream.
       (output(*size) < input(*size)) is allowed */
};
#define ISeqInStream_Read(p, buf, size) (p)->Read(p, buf, size)

/* it can return SZ_ERROR_INPUT_EOF */
SRes SeqInStream_Read(const ISeqInStream *stream, void *buf, size_t size);
SRes SeqInStream_Read2(const ISeqInStream *stream, void *buf, size_t size, SRes errorType);
SRes SeqInStream_ReadByte(const ISeqInStream *stream, Byte *buf);

typedef struct ISeqOutStream ISeqOutStream;
struct ISeqOutStream
{
    size_t (*Write)(const ISeqOutStream *p, const void *buf, size_t size);
    /* Returns: result - the number of actually written bytes.
       (result < size) means error */
};
#define ISeqOutStream_Write(p, buf, size) (p)->Write(p, buf, size)

typedef enum
{
    SZ_SEEK_SET = 0,
    SZ_SEEK_CUR = 1,
    SZ_SEEK_END = 2
} ESzSeek;

typedef struct ISeekInStream ISeekInStream;
struct ISeekInStream
{
    SRes (*Read)(const ISeekInStream *p, void *buf, size_t *size); /* same as ISeqInStream::Read */
    SRes (*Seek)(const ISeekInStream *p, Int64 *pos, ESzSeek origin);
};
#define ISeekInStream_Read(p, buf, size) (p)->Read(p, buf, size)
#define ISeekInStream_Seek(p, pos, origin) (p)->Seek(p, pos, origin)

typedef struct ILookInStream ILookInStream;
struct ILookInStream
{
    SRes (*Look)(const ILookInStream *p, const void **buf, size_t *size);
    /* if (input(*size) != 0 && output(*size) == 0) means end_of_stream.
       (output(*size) > input(*size)) is not allowed
       (output(*size) < input(*size)) is allowed */
    SRes (*Skip)(const ILookInStream *p, size_t offset);
    /* offset must be <= output(*size) of Look */

    SRes (*Read)(const ILookInStream *p, void *buf, size_t *size);
    /* reads directly (without buffer). It's same as ISeqInStream::Read */
    SRes (*Seek)(const ILookInStream *p, Int64 *pos, ESzSeek origin);
};

#define ILookInStream_Look(p, buf, size) (p)->Look(p, buf, size)
#define ILookInStream_Skip(p, offset) (p)->Skip(p, offset)
#define ILookInStream_Read(p, buf, size) (p)->Read(p, buf, size)
#define ILookInStream_Seek(p, pos, origin) (p)->Seek(p, pos, origin)

SRes LookInStream_LookRead(const ILookInStream *stream, void *buf, size_t *size);
SRes LookInStream_SeekTo(const ILookInStream *stream, UInt64 offset);

/* reads via ILookInStream::Read */
SRes LookInStream_Read2(const ILookInStream *stream, void *buf, size_t size, SRes errorType);
SRes LookInStream_Read(const ILookInStream *stream, void *buf, size_t size);

typedef struct
{
    ILookInStream vt;
    const ISeekInStream *realStream;

    size_t pos;
    size_t size; /* it's data size */

    /* the following variables must be set outside */
    Byte *buf;
    size_t bufSize;
} CLookToRead2;

void LookToRead2_CreateVTable(CLookToRead2 *p, int lookahead);

#define LookToRead2_Init(p)       \
    {                             \
        (p)->pos = (p)->size = 0; \
    }

typedef struct
{
    ISeqInStream vt;
    const ILookInStream *realStream;
} CSecToLook;

void SecToLook_CreateVTable(CSecToLook *p);

typedef struct
{
    ISeqInStream vt;
    const ILookInStream *realStream;
} CSecToRead;

void SecToRead_CreateVTable(CSecToRead *p);

typedef struct ICompressProgress ICompressProgress;

struct ICompressProgress
{
    SRes (*Progress)(const ICompressProgress *p, UInt64 inSize, UInt64 outSize);
    /* Returns: result. (result != SZ_OK) means break.
       Value (UInt64)(Int64)-1 for size means unknown value. */
};
#define ICompressProgress_Progress(p, inSize, outSize) (p)->Progress(p, inSize, outSize)

typedef struct ISzAlloc ISzAlloc;
typedef const ISzAlloc *ISzAllocPtr;

struct ISzAlloc
{
    void *(*Alloc)(ISzAllocPtr p, size_t size);
    void (*Free)(ISzAllocPtr p, void *address); /* address can be 0 */
};

#define ISzAlloc_Alloc(p, size) (p)->Alloc(p, size)
#define ISzAlloc_Free(p, a) (p)->Free(p, a)

/* deprecated */
#define IAlloc_Alloc(p, size) ISzAlloc_Alloc(p, size)
#define IAlloc_Free(p, a) ISzAlloc_Free(p, a)

#ifndef MY_offsetof
#ifdef offsetof
#define MY_offsetof(type, m) offsetof(type, m)
/*
#define MY_offsetof(type, m) FIELD_OFFSET(type, m)
*/
#else
#define MY_offsetof(type, m) ((size_t) & (((type *)0)->m))
#endif
#endif

#ifndef MY_container_of

/*
#define MY_container_of(ptr, type, m) container_of(ptr, type, m)
#define MY_container_of(ptr, type, m) CONTAINING_RECORD(ptr, type, m)
#define MY_container_of(ptr, type, m) ((type *)((char *)(ptr) - offsetof(type, m)))
#define MY_container_of(ptr, type, m) (&((type *)0)->m == (ptr), ((type *)(((char *)(ptr)) - MY_offsetof(type, m))))
*/

/*
  GCC shows warning: "perhaps the 'offsetof' macro was used incorrectly"
    GCC 3.4.4 : classes with constructor
    GCC 4.8.1 : classes with non-public variable members"
*/

#define MY_container_of(ptr, type, m) ((type *)(void *)((char *)(void *)(1 ? (ptr) : &((type *)0)->m) - MY_offsetof(type, m)))

#endif

#define CONTAINER_FROM_VTBL_SIMPLE(ptr, type, m) ((type *)(void *)(ptr))

/*
#define CONTAINER_FROM_VTBL(ptr, type, m) CONTAINER_FROM_VTBL_SIMPLE(ptr, type, m)
*/
#define CONTAINER_FROM_VTBL(ptr, type, m) MY_container_of(ptr, type, m)

#define CONTAINER_FROM_VTBL_CLS(ptr, type, m) CONTAINER_FROM_VTBL_SIMPLE(ptr, type, m)
/*
#define CONTAINER_FROM_VTBL_CLS(ptr, type, m) CONTAINER_FROM_VTBL(ptr, type, m)
*/

#define MY_memset_0_ARRAY(a) memset((a), 0, sizeof(a))

#ifdef _WIN32

#define CHAR_PATH_SEPARATOR '\\'
#define WCHAR_PATH_SEPARATOR L'\\'
#define STRING_PATH_SEPARATOR "\\"
#define WSTRING_PATH_SEPARATOR L"\\"

#else

#define CHAR_PATH_SEPARATOR '/'
#define WCHAR_PATH_SEPARATOR L'/'
#define STRING_PATH_SEPARATOR "/"
#define WSTRING_PATH_SEPARATOR L"/"

#endif

#define k_PropVar_TimePrec_0 0
#define k_PropVar_TimePrec_Unix 1
#define k_PropVar_TimePrec_DOS 2
#define k_PropVar_TimePrec_HighPrec 3
#define k_PropVar_TimePrec_Base 16
#define k_PropVar_TimePrec_100ns (k_PropVar_TimePrec_Base + 7)
#define k_PropVar_TimePrec_1ns (k_PropVar_TimePrec_Base + 9)

EXTERN_C_END

#endif

EXTERN_C_BEGIN

/* #define _LZMA_PROB32 */
/* _LZMA_PROB32 can increase the speed on some CPUs,
   but memory usage for CLzmaDec::probs will be doubled in that case */

typedef
#ifdef _LZMA_PROB32
    UInt32
#else
    UInt16
#endif
        CLzmaProb;

/* ---------- LZMA Properties ---------- */

#define LZMA_PROPS_SIZE 5

typedef struct _CLzmaProps
{
    Byte lc;
    Byte lp;
    Byte pb;
    Byte _pad_;
    UInt32 dicSize;
} CLzmaProps;

/* LzmaProps_Decode - decodes properties
Returns:
  SZ_OK
  SZ_ERROR_UNSUPPORTED - Unsupported properties
*/

SRes LzmaProps_Decode(CLzmaProps *p, const Byte *data, unsigned size);

/* ---------- LZMA Decoder state ---------- */

/* LZMA_REQUIRED_INPUT_MAX = number of required input bytes for worst case.
   Num bits = log2((2^11 / 31) ^ 22) + 26 < 134 + 26 = 160; */

#define LZMA_REQUIRED_INPUT_MAX 20

typedef struct
{
    /* Don't change this structure. ASM code can use it. */
    CLzmaProps prop;
    CLzmaProb *probs;
    CLzmaProb *probs_1664;
    Byte *dic;
    SizeT dicBufSize;
    SizeT dicPos;
    const Byte *buf;
    UInt32 range;
    UInt32 code;
    UInt32 processedPos;
    UInt32 checkDicSize;
    UInt32 reps[4];
    UInt32 state;
    UInt32 remainLen;

    UInt32 numProbs;
    unsigned tempBufSize;
    Byte tempBuf[LZMA_REQUIRED_INPUT_MAX];
} CLzmaDec;

#define LzmaDec_Construct(p) \
    {                        \
        (p)->dic = NULL;     \
        (p)->probs = NULL;   \
    }

void LzmaDec_Init(CLzmaDec *p);

/* There are two types of LZMA streams:
     - Stream with end mark. That end mark adds about 6 bytes to compressed size.
     - Stream without end mark. You must know exact uncompressed size to decompress such stream. */

typedef enum
{
    LZMA_FINISH_ANY, /* finish at any point */
    LZMA_FINISH_END  /* block must be finished at the end */
} ELzmaFinishMode;

/* ELzmaFinishMode has meaning only if the decoding reaches output limit !!!

   You must use LZMA_FINISH_END, when you know that current output buffer
   covers last bytes of block. In other cases you must use LZMA_FINISH_ANY.

   If LZMA decoder sees end marker before reaching output limit, it returns SZ_OK,
   and output value of destLen will be less than output buffer size limit.
   You can check status result also.

   You can use multiple checks to test data integrity after full decompression:
     1) Check Result and "status" variable.
     2) Check that output(destLen) = uncompressedSize, if you know real uncompressedSize.
     3) Check that output(srcLen) = compressedSize, if you know real compressedSize.
        You must use correct finish mode in that case. */

typedef enum
{
    LZMA_STATUS_NOT_SPECIFIED,              /* use main error code instead */
    LZMA_STATUS_FINISHED_WITH_MARK,         /* stream was finished with end mark. */
    LZMA_STATUS_NOT_FINISHED,               /* stream was not finished */
    LZMA_STATUS_NEEDS_MORE_INPUT,           /* you must provide more input bytes */
    LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK /* there is probability that stream was finished without end mark */
} ELzmaStatus;

/* ELzmaStatus is used only as output value for function call */

/* ---------- Interfaces ---------- */

/* There are 3 levels of interfaces:
     1) Dictionary Interface
     2) Buffer Interface
     3) One Call Interface
   You can select any of these interfaces, but don't mix functions from different
   groups for same object. */

/* There are two variants to allocate state for Dictionary Interface:
     1) LzmaDec_Allocate / LzmaDec_Free
     2) LzmaDec_AllocateProbs / LzmaDec_FreeProbs
   You can use variant 2, if you set dictionary buffer manually.
   For Buffer Interface you must always use variant 1.

LzmaDec_Allocate* can return:
  SZ_OK
  SZ_ERROR_MEM         - Memory allocation error
  SZ_ERROR_UNSUPPORTED - Unsupported properties
*/

SRes LzmaDec_AllocateProbs(CLzmaDec *p, const Byte *props, unsigned propsSize, ISzAllocPtr alloc);
void LzmaDec_FreeProbs(CLzmaDec *p, ISzAllocPtr alloc);

SRes LzmaDec_Allocate(CLzmaDec *p, const Byte *props, unsigned propsSize, ISzAllocPtr alloc);
void LzmaDec_Free(CLzmaDec *p, ISzAllocPtr alloc);

/* ---------- Dictionary Interface ---------- */

/* You can use it, if you want to eliminate the overhead for data copying from
   dictionary to some other external buffer.
   You must work with CLzmaDec variables directly in this interface.

   STEPS:
     LzmaDec_Construct()
     LzmaDec_Allocate()
     for (each new stream)
     {
       LzmaDec_Init()
       while (it needs more decompression)
       {
         LzmaDec_DecodeToDic()
         use data from CLzmaDec::dic and update CLzmaDec::dicPos
       }
     }
     LzmaDec_Free()
*/

/* LzmaDec_DecodeToDic

   The decoding to internal dictionary buffer (CLzmaDec::dic).
   You must manually update CLzmaDec::dicPos, if it reaches CLzmaDec::dicBufSize !!!

finishMode:
  It has meaning only if the decoding reaches output limit (dicLimit).
  LZMA_FINISH_ANY - Decode just dicLimit bytes.
  LZMA_FINISH_END - Stream must be finished after dicLimit.

Returns:
  SZ_OK
    status:
      LZMA_STATUS_FINISHED_WITH_MARK
      LZMA_STATUS_NOT_FINISHED
      LZMA_STATUS_NEEDS_MORE_INPUT
      LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
  SZ_ERROR_DATA - Data error
  SZ_ERROR_FAIL - Some unexpected error: internal error of code, memory corruption or hardware failure
*/

SRes LzmaDec_DecodeToDic(CLzmaDec *p, SizeT dicLimit,
                         const Byte *src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status);

/* ---------- Buffer Interface ---------- */

/* It's zlib-like interface.
   See LzmaDec_DecodeToDic description for information about STEPS and return results,
   but you must use LzmaDec_DecodeToBuf instead of LzmaDec_DecodeToDic and you don't need
   to work with CLzmaDec variables manually.

finishMode:
  It has meaning only if the decoding reaches output limit (*destLen).
  LZMA_FINISH_ANY - Decode just destLen bytes.
  LZMA_FINISH_END - Stream must be finished after (*destLen).
*/

SRes LzmaDec_DecodeToBuf(CLzmaDec *p, Byte *dest, SizeT *destLen,
                         const Byte *src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status);

/* ---------- One Call Interface ---------- */

/* LzmaDecode

finishMode:
  It has meaning only if the decoding reaches output limit (*destLen).
  LZMA_FINISH_ANY - Decode just destLen bytes.
  LZMA_FINISH_END - Stream must be finished after (*destLen).

Returns:
  SZ_OK
    status:
      LZMA_STATUS_FINISHED_WITH_MARK
      LZMA_STATUS_NOT_FINISHED
      LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
  SZ_ERROR_DATA - Data error
  SZ_ERROR_MEM  - Memory allocation error
  SZ_ERROR_UNSUPPORTED - Unsupported properties
  SZ_ERROR_INPUT_EOF - It needs more bytes in input buffer (src).
  SZ_ERROR_FAIL - Some unexpected error: internal error of code, memory corruption or hardware failure
*/

int lzma_uncompress(unsigned char *dest, size_t *destLen, const unsigned char *src, size_t *srcLen);

SRes LzmaDecode(Byte *dest, SizeT *destLen, const Byte *src, SizeT *srcLen,
                const Byte *propData, unsigned propSize, ELzmaFinishMode finishMode,
                ELzmaStatus *status, ISzAllocPtr alloc);

EXTERN_C_END

#endif

/*  LzmaEnc.h -- LZMA Encoder
2019-10-30 : Igor Pavlov : Public domain */

#ifndef __LZMA_ENC_H
#define __LZMA_ENC_H

EXTERN_C_BEGIN

#define LZMA_PROPS_SIZE 5

typedef struct _CLzmaEncProps
{
    int level;             /* 0 <= level <= 9 */
    UInt32 dictSize;       /* (1 << 12) <= dictSize <= (1 << 27) for 32-bit version
                              (1 << 12) <= dictSize <= (3 << 29) for 64-bit version
                              default = (1 << 24) */
    int lc;                /* 0 <= lc <= 8, default = 3 */
    int lp;                /* 0 <= lp <= 4, default = 0 */
    int pb;                /* 0 <= pb <= 4, default = 2 */
    int algo;              /* 0 - fast, 1 - normal, default = 1 */
    int fb;                /* 5 <= fb <= 273, default = 32 */
    int btMode;            /* 0 - hashChain Mode, 1 - binTree mode - normal, default = 1 */
    int numHashBytes;      /* 2, 3 or 4, default = 4 */
    UInt32 mc;             /* 1 <= mc <= (1 << 30), default = 32 */
    unsigned writeEndMark; /* 0 - do not write EOPM, 1 - write EOPM, default = 0 */
    int numThreads;        /* 1 or 2, default = 2 */

    UInt64 reduceSize; /* estimated size of data that will be compressed. default = (UInt64)(Int64)-1.
                          Encoder uses this value to reduce dictionary size */

    UInt64 affinity;
} CLzmaEncProps;

void LzmaEncProps_Init(CLzmaEncProps *p);
void LzmaEncProps_Normalize(CLzmaEncProps *p);
UInt32 LzmaEncProps_GetDictSize(const CLzmaEncProps *props2);

/* ---------- CLzmaEncHandle Interface ---------- */

/* LzmaEnc* functions can return the following exit codes:
SRes:
  SZ_OK           - OK
  SZ_ERROR_MEM    - Memory allocation error
  SZ_ERROR_PARAM  - Incorrect paramater in props
  SZ_ERROR_WRITE  - ISeqOutStream write callback error
  SZ_ERROR_OUTPUT_EOF - output buffer overflow - version with (Byte *) output
  SZ_ERROR_PROGRESS - some break from progress callback
  SZ_ERROR_THREAD - error in multithreading functions (only for Mt version)
*/

typedef void *CLzmaEncHandle;

CLzmaEncHandle LzmaEnc_Create(ISzAllocPtr alloc);
void LzmaEnc_Destroy(CLzmaEncHandle p, ISzAllocPtr alloc, ISzAllocPtr allocBig);

SRes LzmaEnc_SetProps(CLzmaEncHandle p, const CLzmaEncProps *props);
void LzmaEnc_SetDataSize(CLzmaEncHandle p, UInt64 expectedDataSiize);
SRes LzmaEnc_WriteProperties(CLzmaEncHandle p, Byte *properties, SizeT *size);
unsigned LzmaEnc_IsWriteEndMark(CLzmaEncHandle p);

SRes LzmaEnc_Encode(CLzmaEncHandle p, ISeqOutStream *outStream, ISeqInStream *inStream,
                    ICompressProgress *progress, ISzAllocPtr alloc, ISzAllocPtr allocBig);
SRes LzmaEnc_MemEncode(CLzmaEncHandle p, Byte *dest, SizeT *destLen, const Byte *src, SizeT srcLen,
                       int writeEndMark, ICompressProgress *progress, ISzAllocPtr alloc, ISzAllocPtr allocBig);

/* ---------- One Call Interface ---------- */

SRes LzmaEncode(Byte *dest, SizeT *destLen, const Byte *src, SizeT srcLen,
                const CLzmaEncProps *props, Byte *propsEncoded, SizeT *propsSize, int writeEndMark,
                ICompressProgress *progress, ISzAllocPtr alloc, ISzAllocPtr allocBig);

int lzma_compress(unsigned char *dest, size_t *destLen, const unsigned char *src, size_t srcLen);

EXTERN_C_END

#endif

#ifdef LZMA_IMPLEMENTATION

#ifndef NOSTDLIB
#ifndef LZMA_ALLOC

#include <stdlib.h>
#define LZMA_ALLOC g_Alloc

static void *SzAlloc(ISzAllocPtr p, size_t size)
{
    (void)p;
    return malloc(size);
}

static void SzFree(ISzAllocPtr p, void *address)
{
    (void)p;
    free(address);
}

static const ISzAlloc g_Alloc = {SzAlloc, SzFree};

#endif // LZMA_ALLOC
#endif // NOSTDLIB

/* LzmaEnc.c -- LZMA Encoder
2022-07-15: Igor Pavlov : Public domain */

/* Compiler.h
2021-01-05 : Igor Pavlov : Public domain */

#ifndef __7Z_COMPILER_H
#define __7Z_COMPILER_H

#ifdef __clang__
#pragma clang diagnostic ignored "-Wunused-private-field"
#endif

#ifdef _MSC_VER

#ifdef UNDER_CE
#define RPC_NO_WINDOWS_H
/* #pragma warning(disable : 4115) // '_RPC_ASYNC_STATE' : named type definition in parentheses */
#pragma warning(disable : 4201) // nonstandard extension used : nameless struct/union
#pragma warning(disable : 4214) // nonstandard extension used : bit field types other than int
#endif

#if _MSC_VER >= 1300
#pragma warning(disable : 4996) // This function or variable may be unsafe
#else
#pragma warning(disable : 4511) // copy constructor could not be generated
#pragma warning(disable : 4512) // assignment operator could not be generated
#pragma warning(disable : 4514) // unreferenced inline function has been removed
#pragma warning(disable : 4702) // unreachable code
#pragma warning(disable : 4710) // not inlined
#pragma warning(disable : 4714) // function marked as __forceinline not inlined
#pragma warning(disable : 4786) // identifier was truncated to '255' characters in the debug information
#endif

#ifdef __clang__
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#pragma clang diagnostic ignored "-Wmicrosoft-exception-spec"
// #pragma clang diagnostic ignored "-Wreserved-id-macro"
#endif

#endif

#define UNUSED_VAR(x) (void)x;
/* #define UNUSED_VAR(x) x=x; */

#endif

#include <string.h>

/* #define SHOW_STAT */
/* #define SHOW_STAT2 */

#if defined(SHOW_STAT) || defined(SHOW_STAT2)
#include <stdio.h>
#endif

/* CpuArch.h -- CPU specific code
2022-07-15 : Igor Pavlov : Public domain */

#ifndef __CPU_ARCH_H
#define __CPU_ARCH_H

EXTERN_C_BEGIN

/*
MY_CPU_LE means that CPU is LITTLE ENDIAN.
MY_CPU_BE means that CPU is BIG ENDIAN.
If MY_CPU_LE and MY_CPU_BE are not defined, we don't know about ENDIANNESS of platform.

MY_CPU_LE_UNALIGN means that CPU is LITTLE ENDIAN and CPU supports unaligned memory accesses.

MY_CPU_64BIT means that processor can work with 64-bit registers.
  MY_CPU_64BIT can be used to select fast code branch
  MY_CPU_64BIT doesn't mean that (sizeof(void *) == 8)
*/

#if defined(_M_X64) || defined(_M_AMD64) || defined(__x86_64__) || defined(__AMD64__) || defined(__amd64__)
#define MY_CPU_AMD64
#ifdef __ILP32__
#define MY_CPU_NAME "x32"
#define MY_CPU_SIZEOF_POINTER 4
#else
#define MY_CPU_NAME "x64"
#define MY_CPU_SIZEOF_POINTER 8
#endif
#define MY_CPU_64BIT
#endif

#if defined(_M_IX86) || defined(__i386__)
#define MY_CPU_X86
#define MY_CPU_NAME "x86"
/* #define MY_CPU_32BIT */
#define MY_CPU_SIZEOF_POINTER 4
#endif

#if defined(_M_ARM64) || defined(__AARCH64EL__) || defined(__AARCH64EB__) || defined(__aarch64__)
#define MY_CPU_ARM64
#define MY_CPU_NAME "arm64"
#define MY_CPU_64BIT
#endif

#if defined(_M_ARM) || defined(_M_ARM_NT) || defined(_M_ARMT) || defined(__arm__) || defined(__thumb__) || defined(__ARMEL__) || defined(__ARMEB__) || defined(__THUMBEL__) || defined(__THUMBEB__)
#define MY_CPU_ARM

#if defined(__thumb__) || defined(__THUMBEL__) || defined(_M_ARMT)
#define MY_CPU_NAME "armt"
#else
#define MY_CPU_NAME "arm"
#endif
/* #define MY_CPU_32BIT */
#define MY_CPU_SIZEOF_POINTER 4
#endif

#if defined(_M_IA64) || defined(__ia64__)
#define MY_CPU_IA64
#define MY_CPU_NAME "ia64"
#define MY_CPU_64BIT
#endif

#if defined(__mips64) || defined(__mips64__) || (defined(__mips) && (__mips == 64 || __mips == 4 || __mips == 3))
#define MY_CPU_NAME "mips64"
#define MY_CPU_64BIT
#elif defined(__mips__)
#define MY_CPU_NAME "mips"
/* #define MY_CPU_32BIT */
#endif

#if defined(__ppc64__) || defined(__powerpc64__) || defined(__ppc__) || defined(__powerpc__) || defined(__PPC__) || defined(_POWER)

#if defined(__ppc64__) || defined(__powerpc64__) || defined(_LP64) || defined(__64BIT__)
#ifdef __ILP32__
#define MY_CPU_NAME "ppc64-32"
#define MY_CPU_SIZEOF_POINTER 4
#else
#define MY_CPU_NAME "ppc64"
#define MY_CPU_SIZEOF_POINTER 8
#endif
#define MY_CPU_64BIT
#else
#define MY_CPU_NAME "ppc"
#define MY_CPU_SIZEOF_POINTER 4
/* #define MY_CPU_32BIT */
#endif
#endif

#if defined(__riscv) || defined(__riscv__)
#if __riscv_xlen == 32
#define MY_CPU_NAME "riscv32"
#elif __riscv_xlen == 64
#define MY_CPU_NAME "riscv64"
#else
#define MY_CPU_NAME "riscv"
#endif
#endif

#if defined(MY_CPU_X86) || defined(MY_CPU_AMD64)
#define MY_CPU_X86_OR_AMD64
#endif

#if defined(MY_CPU_ARM) || defined(MY_CPU_ARM64)
#define MY_CPU_ARM_OR_ARM64
#endif

#ifdef _WIN32

#ifdef MY_CPU_ARM
#define MY_CPU_ARM_LE
#endif

#ifdef MY_CPU_ARM64
#define MY_CPU_ARM64_LE
#endif

#ifdef _M_IA64
#define MY_CPU_IA64_LE
#endif

#endif

#if defined(MY_CPU_X86_OR_AMD64) || defined(MY_CPU_ARM_LE) || defined(MY_CPU_ARM64_LE) || defined(MY_CPU_IA64_LE) || defined(__LITTLE_ENDIAN__) || defined(__ARMEL__) || defined(__THUMBEL__) || defined(__AARCH64EL__) || defined(__MIPSEL__) || defined(__MIPSEL) || defined(_MIPSEL) || defined(__BFIN__) || (defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__))
#define MY_CPU_LE
#endif

#if defined(__BIG_ENDIAN__) || defined(__ARMEB__) || defined(__THUMBEB__) || defined(__AARCH64EB__) || defined(__MIPSEB__) || defined(__MIPSEB) || defined(_MIPSEB) || defined(__m68k__) || defined(__s390__) || defined(__s390x__) || defined(__zarch__) || (defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__))
#define MY_CPU_BE
#endif

#if defined(MY_CPU_LE) && defined(MY_CPU_BE)
#error Stop_Compiling_Bad_Endian
#endif

#if defined(MY_CPU_32BIT) && defined(MY_CPU_64BIT)
#error Stop_Compiling_Bad_32_64_BIT
#endif

#ifdef __SIZEOF_POINTER__
#ifdef MY_CPU_SIZEOF_POINTER
#if MY_CPU_SIZEOF_POINTER != __SIZEOF_POINTER__
#error Stop_Compiling_Bad_MY_CPU_PTR_SIZE
#endif
#else
#define MY_CPU_SIZEOF_POINTER __SIZEOF_POINTER__
#endif
#endif

#if defined(MY_CPU_SIZEOF_POINTER) && (MY_CPU_SIZEOF_POINTER == 4)
#if defined(_LP64)
#error Stop_Compiling_Bad_MY_CPU_PTR_SIZE
#endif
#endif

#ifdef _MSC_VER
#if _MSC_VER >= 1300
#define MY_CPU_pragma_pack_push_1 __pragma(pack(push, 1))
#define MY_CPU_pragma_pop __pragma(pack(pop))
#else
#define MY_CPU_pragma_pack_push_1
#define MY_CPU_pragma_pop
#endif
#else
#ifdef __xlC__
#define MY_CPU_pragma_pack_push_1 _Pragma("pack(1)")
#define MY_CPU_pragma_pop _Pragma("pack()")
#else
#define MY_CPU_pragma_pack_push_1 _Pragma("pack(push, 1)")
#define MY_CPU_pragma_pop _Pragma("pack(pop)")
#endif
#endif

#ifndef MY_CPU_NAME
#ifdef MY_CPU_LE
#define MY_CPU_NAME "LE"
#elif defined(MY_CPU_BE)
#define MY_CPU_NAME "BE"
#else
/*
#define MY_CPU_NAME ""
*/
#endif
#endif

#ifdef MY_CPU_LE
#if defined(MY_CPU_X86_OR_AMD64) || defined(MY_CPU_ARM64)
#define MY_CPU_LE_UNALIGN
#define MY_CPU_LE_UNALIGN_64
#elif defined(__ARM_FEATURE_UNALIGNED)
/* gcc9 for 32-bit arm can use LDRD instruction that requires 32-bit alignment.
   So we can't use unaligned 64-bit operations. */
#define MY_CPU_LE_UNALIGN
#endif
#endif

#ifdef MY_CPU_LE_UNALIGN

#define GetUi16(p) (*(const UInt16 *)(const void *)(p))
#define GetUi32(p) (*(const UInt32 *)(const void *)(p))
#ifdef MY_CPU_LE_UNALIGN_64
#define GetUi64(p) (*(const UInt64 *)(const void *)(p))
#endif

#define SetUi16(p, v)                 \
    {                                 \
        *(UInt16 *)(void *)(p) = (v); \
    }
#define SetUi32(p, v)                 \
    {                                 \
        *(UInt32 *)(void *)(p) = (v); \
    }
#ifdef MY_CPU_LE_UNALIGN_64
#define SetUi64(p, v)                 \
    {                                 \
        *(UInt64 *)(void *)(p) = (v); \
    }
#endif

#else

#define GetUi16(p) ((UInt16)(((const Byte *)(p))[0] | \
                             ((UInt16)((const Byte *)(p))[1] << 8)))

#define GetUi32(p) (                         \
    ((const Byte *)(p))[0] |                 \
    ((UInt32)((const Byte *)(p))[1] << 8) |  \
    ((UInt32)((const Byte *)(p))[2] << 16) | \
    ((UInt32)((const Byte *)(p))[3] << 24))

#define SetUi16(p, v)                  \
    {                                  \
        Byte *_ppp_ = (Byte *)(p);     \
        UInt32 _vvv_ = (v);            \
        _ppp_[0] = (Byte)_vvv_;        \
        _ppp_[1] = (Byte)(_vvv_ >> 8); \
    }

#define SetUi32(p, v)                   \
    {                                   \
        Byte *_ppp_ = (Byte *)(p);      \
        UInt32 _vvv_ = (v);             \
        _ppp_[0] = (Byte)_vvv_;         \
        _ppp_[1] = (Byte)(_vvv_ >> 8);  \
        _ppp_[2] = (Byte)(_vvv_ >> 16); \
        _ppp_[3] = (Byte)(_vvv_ >> 24); \
    }

#endif

#ifndef MY_CPU_LE_UNALIGN_64

#define GetUi64(p) (GetUi32(p) | ((UInt64)GetUi32(((const Byte *)(p)) + 4) << 32))

#define SetUi64(p, v)                                \
    {                                                \
        Byte *_ppp2_ = (Byte *)(p);                  \
        UInt64 _vvv2_ = (v);                         \
        SetUi32(_ppp2_, (UInt32)_vvv2_);             \
        SetUi32(_ppp2_ + 4, (UInt32)(_vvv2_ >> 32)); \
    }

#endif

#ifdef __has_builtin
#define MY__has_builtin(x) __has_builtin(x)
#else
#define MY__has_builtin(x) 0
#endif

#if defined(MY_CPU_LE_UNALIGN) && /* defined(_WIN64) && */ defined(_MSC_VER) && (_MSC_VER >= 1300)

/* Note: we use bswap instruction, that is unsupported in 386 cpu */

#include <stdlib.h>

#pragma intrinsic(_byteswap_ushort)
#pragma intrinsic(_byteswap_ulong)
#pragma intrinsic(_byteswap_uint64)

/* #define GetBe16(p) _byteswap_ushort(*(const UInt16 *)(const Byte *)(p)) */
#define GetBe32(p) _byteswap_ulong(*(const UInt32 *)(const void *)(p))
#define GetBe64(p) _byteswap_uint64(*(const UInt64 *)(const void *)(p))

#define SetBe32(p, v) (*(UInt32 *)(void *)(p)) = _byteswap_ulong(v)

#elif defined(MY_CPU_LE_UNALIGN) && ((defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3))) || (defined(__clang__) && MY__has_builtin(__builtin_bswap16)))

/* #define GetBe16(p) __builtin_bswap16(*(const UInt16 *)(const void *)(p)) */
#define GetBe32(p) __builtin_bswap32(*(const UInt32 *)(const void *)(p))
#define GetBe64(p) __builtin_bswap64(*(const UInt64 *)(const void *)(p))

#define SetBe32(p, v) (*(UInt32 *)(void *)(p)) = __builtin_bswap32(v)

#else

#define GetBe32(p) (                         \
    ((UInt32)((const Byte *)(p))[0] << 24) | \
    ((UInt32)((const Byte *)(p))[1] << 16) | \
    ((UInt32)((const Byte *)(p))[2] << 8) |  \
    ((const Byte *)(p))[3])

#define GetBe64(p) (((UInt64)GetBe32(p) << 32) | GetBe32(((const Byte *)(p)) + 4))

#define SetBe32(p, v)                   \
    {                                   \
        Byte *_ppp_ = (Byte *)(p);      \
        UInt32 _vvv_ = (v);             \
        _ppp_[0] = (Byte)(_vvv_ >> 24); \
        _ppp_[1] = (Byte)(_vvv_ >> 16); \
        _ppp_[2] = (Byte)(_vvv_ >> 8);  \
        _ppp_[3] = (Byte)_vvv_;         \
    }

#endif

#ifndef GetBe16

#define GetBe16(p) ((UInt16)(((UInt16)((const Byte *)(p))[0] << 8) | \
                             ((const Byte *)(p))[1]))

#endif

#ifdef MY_CPU_X86_OR_AMD64

typedef struct
{
    UInt32 maxFunc;
    UInt32 vendor[3];
    UInt32 ver;
    UInt32 b;
    UInt32 c;
    UInt32 d;
} Cx86cpuid;

enum
{
    CPU_FIRM_INTEL,
    CPU_FIRM_AMD,
    CPU_FIRM_VIA
};

void MyCPUID(UInt32 function, UInt32 *a, UInt32 *b, UInt32 *c, UInt32 *d);

BoolInt x86cpuid_CheckAndRead(Cx86cpuid *p);
int x86cpuid_GetFirm(const Cx86cpuid *p);

#define x86cpuid_GetFamily(ver) (((ver >> 16) & 0xFF0) | ((ver >> 8) & 0xF))
#define x86cpuid_GetModel(ver) (((ver >> 12) & 0xF0) | ((ver >> 4) & 0xF))
#define x86cpuid_GetStepping(ver) (ver & 0xF)

BoolInt CPU_Is_InOrder(void);

BoolInt CPU_IsSupported_AES(void);
BoolInt CPU_IsSupported_AVX2(void);
BoolInt CPU_IsSupported_VAES_AVX2(void);
BoolInt CPU_IsSupported_SSSE3(void);
BoolInt CPU_IsSupported_SSE41(void);
BoolInt CPU_IsSupported_SHA(void);
BoolInt CPU_IsSupported_PageGB(void);

#elif defined(MY_CPU_ARM_OR_ARM64)

BoolInt CPU_IsSupported_CRC32(void);
BoolInt CPU_IsSupported_NEON(void);

#if defined(_WIN32)
BoolInt CPU_IsSupported_CRYPTO(void);
#define CPU_IsSupported_SHA1 CPU_IsSupported_CRYPTO
#define CPU_IsSupported_SHA2 CPU_IsSupported_CRYPTO
#define CPU_IsSupported_AES CPU_IsSupported_CRYPTO
#else
BoolInt CPU_IsSupported_SHA1(void);
BoolInt CPU_IsSupported_SHA2(void);
BoolInt CPU_IsSupported_AES(void);
#endif

#endif

#if defined(__APPLE__)
int My_sysctlbyname_Get(const char *name, void *buf, size_t *bufSize);
int My_sysctlbyname_Get_UInt32(const char *name, UInt32 *val);
#endif

EXTERN_C_END

#endif

/* CpuArch.c -- CPU specific code
2021-07-13 : Igor Pavlov : Public domain */

#ifdef MY_CPU_X86_OR_AMD64

#if (defined(_MSC_VER) && !defined(MY_CPU_AMD64)) || defined(__GNUC__)
#define USE_ASM
#endif

#if !defined(USE_ASM) && _MSC_VER >= 1500
#include <intrin.h>
#endif

#if defined(USE_ASM) && !defined(MY_CPU_AMD64)
static UInt32 CheckFlag(UInt32 flag)
{
#ifdef _MSC_VER
    __asm pushfd;
    __asm pop EAX;
    __asm mov EDX, EAX;
    __asm xor EAX, flag;
    __asm push EAX;
    __asm popfd;
    __asm pushfd;
    __asm pop EAX;
    __asm xor EAX, EDX;
    __asm push EDX;
    __asm popfd;
    __asm and flag, EAX;
#else
    __asm__ __volatile__(
        "pushf\n\t"
        "pop  %%EAX\n\t"
        "movl %%EAX,%%EDX\n\t"
        "xorl %0,%%EAX\n\t"
        "push %%EAX\n\t"
        "popf\n\t"
        "pushf\n\t"
        "pop  %%EAX\n\t"
        "xorl %%EDX,%%EAX\n\t"
        "push %%EDX\n\t"
        "popf\n\t"
        "andl %%EAX, %0\n\t"
        : "=c"(flag)
        : "c"(flag)
        : "%eax", "%edx");
#endif
    return flag;
}
#define CHECK_CPUID_IS_SUPPORTED                            \
    if (CheckFlag(1 << 18) == 0 || CheckFlag(1 << 21) == 0) \
        return False;
#else
#define CHECK_CPUID_IS_SUPPORTED
#endif

#ifndef USE_ASM
#ifdef _MSC_VER
#if _MSC_VER >= 1600
#define MY__cpuidex __cpuidex
#else

/*
 __cpuid (function == 4) requires subfunction number in ECX.
  MSDN: The __cpuid intrinsic clears the ECX register before calling the cpuid instruction.
   __cpuid() in new MSVC clears ECX.
   __cpuid() in old MSVC (14.00) doesn't clear ECX
 We still can use __cpuid for low (function) values that don't require ECX,
 but __cpuid() in old MSVC will be incorrect for some function values: (function == 4).
 So here we use the hack for old MSVC to send (subFunction) in ECX register to cpuid instruction,
 where ECX value is first parameter for FAST_CALL / NO_INLINE function,
 So the caller of MY__cpuidex_HACK() sets ECX as subFunction, and
 old MSVC for __cpuid() doesn't change ECX and cpuid instruction gets (subFunction) value.

 DON'T remove MY_NO_INLINE and MY_FAST_CALL for MY__cpuidex_HACK() !!!
*/

static MY_NO_INLINE void MY_FAST_CALL MY__cpuidex_HACK(UInt32 subFunction, int *CPUInfo, UInt32 function)
{
    UNUSED_VAR(subFunction);
    __cpuid(CPUInfo, function);
}

#define MY__cpuidex(info, func, func2) MY__cpuidex_HACK(func2, info, func)
#pragma message("======== MY__cpuidex_HACK WAS USED ========")
#endif
#else
#define MY__cpuidex(info, func, func2) __cpuid(info, func)
#pragma message("======== (INCORRECT ?) cpuid WAS USED ========")
#endif
#endif

void MyCPUID(UInt32 function, UInt32 *a, UInt32 *b, UInt32 *c, UInt32 *d)
{
#ifdef USE_ASM

#ifdef _MSC_VER

    UInt32 a2, b2, c2, d2;
    __asm xor EBX, EBX;
    __asm xor ECX, ECX;
    __asm xor EDX, EDX;
    __asm mov EAX, function;
    __asm cpuid;
    __asm mov a2, EAX;
    __asm mov b2, EBX;
    __asm mov c2, ECX;
    __asm mov d2, EDX;

    *a = a2;
    *b = b2;
    *c = c2;
    *d = d2;

#else

    __asm__ __volatile__(
#if defined(MY_CPU_AMD64) && defined(__PIC__)
        "mov %%rbx, %%rdi;"
        "cpuid;"
        "xchg %%rbx, %%rdi;"
        : "=a"(*a),
          "=D"(*b),
#elif defined(MY_CPU_X86) && defined(__PIC__)
        "mov %%ebx, %%edi;"
        "cpuid;"
        "xchgl %%ebx, %%edi;"
        : "=a"(*a),
          "=D"(*b),
#else
        "cpuid"
        : "=a"(*a),
          "=b"(*b),
#endif
          "=c"(*c),
          "=d"(*d)
        : "0"(function), "c"(0));

#endif

#else

    int CPUInfo[4];

    MY__cpuidex(CPUInfo, (int)function, 0);

    *a = (UInt32)CPUInfo[0];
    *b = (UInt32)CPUInfo[1];
    *c = (UInt32)CPUInfo[2];
    *d = (UInt32)CPUInfo[3];

#endif
}

BoolInt x86cpuid_CheckAndRead(Cx86cpuid *p)
{
    CHECK_CPUID_IS_SUPPORTED
    MyCPUID(0, &p->maxFunc, &p->vendor[0], &p->vendor[2], &p->vendor[1]);
    MyCPUID(1, &p->ver, &p->b, &p->c, &p->d);
    return True;
}

static const UInt32 kVendors[][3] =
    {
        {0x756E6547, 0x49656E69, 0x6C65746E},
        {0x68747541, 0x69746E65, 0x444D4163},
        {0x746E6543, 0x48727561, 0x736C7561}};

int x86cpuid_GetFirm(const Cx86cpuid *p)
{
    unsigned i;
    for (i = 0; i < sizeof(kVendors) / sizeof(kVendors[i]); i++)
    {
        const UInt32 *v = kVendors[i];
        if (v[0] == p->vendor[0] &&
            v[1] == p->vendor[1] &&
            v[2] == p->vendor[2])
            return (int)i;
    }
    return -1;
}

BoolInt CPU_Is_InOrder()
{
    Cx86cpuid p;
    int firm;
    UInt32 family, model;
    if (!x86cpuid_CheckAndRead(&p))
        return True;

    family = x86cpuid_GetFamily(p.ver);
    model = x86cpuid_GetModel(p.ver);

    firm = x86cpuid_GetFirm(&p);

    switch (firm)
    {
    case CPU_FIRM_INTEL:
        return (family < 6 || (family == 6 && (
                                                  /* In-Order Atom CPU */
                                                  model == 0x1C    /* 45 nm, N4xx, D4xx, N5xx, D5xx, 230, 330 */
                                                  || model == 0x26 /* 45 nm, Z6xx */
                                                  || model == 0x27 /* 32 nm, Z2460 */
                                                  || model == 0x35 /* 32 nm, Z2760 */
                                                  || model == 0x36 /* 32 nm, N2xxx, D2xxx */
                                                  )));
    case CPU_FIRM_AMD:
        return (family < 5 || (family == 5 && (model < 6 || model == 0xA)));
    case CPU_FIRM_VIA:
        return (family < 6 || (family == 6 && model < 0xF));
    }
    return True;
}

#if !defined(MY_CPU_AMD64) && defined(_WIN32)
#include <Windows.h>
static BoolInt CPU_Sys_Is_SSE_Supported()
{
    OSVERSIONINFO vi;
    vi.dwOSVersionInfoSize = sizeof(vi);
    if (!GetVersionEx(&vi))
        return False;
    return (vi.dwMajorVersion >= 5);
}
#define CHECK_SYS_SSE_SUPPORT        \
    if (!CPU_Sys_Is_SSE_Supported()) \
        return False;
#else
#define CHECK_SYS_SSE_SUPPORT
#endif

static UInt32 X86_CPUID_ECX_Get_Flags()
{
    Cx86cpuid p;
    CHECK_SYS_SSE_SUPPORT
    if (!x86cpuid_CheckAndRead(&p))
        return 0;
    return p.c;
}

BoolInt CPU_IsSupported_AES()
{
    return (X86_CPUID_ECX_Get_Flags() >> 25) & 1;
}

BoolInt CPU_IsSupported_SSSE3()
{
    return (X86_CPUID_ECX_Get_Flags() >> 9) & 1;
}

BoolInt CPU_IsSupported_SSE41()
{
    return (X86_CPUID_ECX_Get_Flags() >> 19) & 1;
}

BoolInt CPU_IsSupported_SHA()
{
    Cx86cpuid p;
    CHECK_SYS_SSE_SUPPORT
    if (!x86cpuid_CheckAndRead(&p))
        return False;

    if (p.maxFunc < 7)
        return False;
    {
        UInt32 d[4] = {0};
        MyCPUID(7, &d[0], &d[1], &d[2], &d[3]);
        return (d[1] >> 29) & 1;
    }
}

// #include <stdio.h>

#ifdef _WIN32
#include <Windows.h>
#endif

BoolInt CPU_IsSupported_AVX2()
{
    Cx86cpuid p;
    CHECK_SYS_SSE_SUPPORT

#ifdef _WIN32
#define MY__PF_XSAVE_ENABLED 17
    if (!IsProcessorFeaturePresent(MY__PF_XSAVE_ENABLED))
        return False;
#endif

    if (!x86cpuid_CheckAndRead(&p))
        return False;
    if (p.maxFunc < 7)
        return False;
    {
        UInt32 d[4] = {0};
        MyCPUID(7, &d[0], &d[1], &d[2], &d[3]);
        // printf("\ncpuid(7): ebx=%8x ecx=%8x\n", d[1], d[2]);
        return 1 & (d[1] >> 5); // avx2
    }
}

BoolInt CPU_IsSupported_VAES_AVX2()
{
    Cx86cpuid p;
    CHECK_SYS_SSE_SUPPORT

#ifdef _WIN32
#define MY__PF_XSAVE_ENABLED 17
    if (!IsProcessorFeaturePresent(MY__PF_XSAVE_ENABLED))
        return False;
#endif

    if (!x86cpuid_CheckAndRead(&p))
        return False;
    if (p.maxFunc < 7)
        return False;
    {
        UInt32 d[4] = {0};
        MyCPUID(7, &d[0], &d[1], &d[2], &d[3]);
        // printf("\ncpuid(7): ebx=%8x ecx=%8x\n", d[1], d[2]);
        return 1 & (d[1] >> 5) // avx2
                               // & (d[1] >> 31) // avx512vl
               & (d[2] >> 9); // vaes // VEX-256/EVEX
    }
}

BoolInt CPU_IsSupported_PageGB()
{
    Cx86cpuid cpuid;
    if (!x86cpuid_CheckAndRead(&cpuid))
        return False;
    {
        UInt32 d[4] = {0};
        MyCPUID(0x80000000, &d[0], &d[1], &d[2], &d[3]);
        if (d[0] < 0x80000001)
            return False;
    }
    {
        UInt32 d[4] = {0};
        MyCPUID(0x80000001, &d[0], &d[1], &d[2], &d[3]);
        return (d[3] >> 26) & 1;
    }
}

#elif defined(MY_CPU_ARM_OR_ARM64)

#ifdef _WIN32

#include <Windows.h>

BoolInt CPU_IsSupported_CRC32()
{
    return IsProcessorFeaturePresent(PF_ARM_V8_CRC32_INSTRUCTIONS_AVAILABLE) ? 1 : 0;
}
BoolInt CPU_IsSupported_CRYPTO()
{
    return IsProcessorFeaturePresent(PF_ARM_V8_CRYPTO_INSTRUCTIONS_AVAILABLE) ? 1 : 0;
}
BoolInt CPU_IsSupported_NEON()
{
    return IsProcessorFeaturePresent(PF_ARM_NEON_INSTRUCTIONS_AVAILABLE) ? 1 : 0;
}

#else

#if defined(__APPLE__)

/*
#include <stdio.h>
#include <string.h>
static void Print_sysctlbyname(const char *name)
{
  size_t bufSize = 256;
  char buf[256];
  int res = sysctlbyname(name, &buf, &bufSize, NULL, 0);
  {
    int i;
    printf("\nres = %d : %s : '%s' : bufSize = %d, numeric", res, name, buf, (unsigned)bufSize);
    for (i = 0; i < 20; i++)
      printf(" %2x", (unsigned)(Byte)buf[i]);

  }
}
*/

static BoolInt My_sysctlbyname_Get_BoolInt(const char *name)
{
    UInt32 val = 0;
    if (My_sysctlbyname_Get_UInt32(name, &val) == 0 && val == 1)
        return 1;
    return 0;
}

/*
Print_sysctlbyname("hw.pagesize");
Print_sysctlbyname("machdep.cpu.brand_string");
*/

BoolInt CPU_IsSupported_CRC32(void)
{
    return My_sysctlbyname_Get_BoolInt("hw.optional.armv8_crc32");
}

BoolInt CPU_IsSupported_NEON(void)
{
    return My_sysctlbyname_Get_BoolInt("hw.optional.neon");
}

#ifdef MY_CPU_ARM64
#define APPLE_CRYPTO_SUPPORT_VAL 1
#else
#define APPLE_CRYPTO_SUPPORT_VAL 0
#endif

BoolInt CPU_IsSupported_SHA1(void)
{
    return APPLE_CRYPTO_SUPPORT_VAL;
}
BoolInt CPU_IsSupported_SHA2(void)
{
    return APPLE_CRYPTO_SUPPORT_VAL;
}
BoolInt CPU_IsSupported_AES(void)
{
    return APPLE_CRYPTO_SUPPORT_VAL;
}

#else // __APPLE__

#include <sys/auxv.h>

#define USE_HWCAP

#ifdef USE_HWCAP

#include <asm/hwcap.h>

#define MY_HWCAP_CHECK_FUNC_2(name1, name2)                     \
    BoolInt CPU_IsSupported_##name1()                           \
    {                                                           \
        return (getauxval(AT_HWCAP) & (HWCAP_##name2)) ? 1 : 0; \
    }

#ifdef MY_CPU_ARM64
#define MY_HWCAP_CHECK_FUNC(name) \
    MY_HWCAP_CHECK_FUNC_2(name, name)
MY_HWCAP_CHECK_FUNC_2(NEON, ASIMD)
// MY_HWCAP_CHECK_FUNC (ASIMD)
#elif defined(MY_CPU_ARM)
#define MY_HWCAP_CHECK_FUNC(name)                                \
    BoolInt CPU_IsSupported_##name()                             \
    {                                                            \
        return (getauxval(AT_HWCAP2) & (HWCAP2_##name)) ? 1 : 0; \
    }
MY_HWCAP_CHECK_FUNC_2(NEON, NEON)
#endif

#else // USE_HWCAP

#define MY_HWCAP_CHECK_FUNC(name)    \
    BoolInt CPU_IsSupported_##name() \
    {                                \
        return 0;                    \
    }
MY_HWCAP_CHECK_FUNC(NEON)

#endif // USE_HWCAP

MY_HWCAP_CHECK_FUNC(CRC32)
MY_HWCAP_CHECK_FUNC(SHA1)
MY_HWCAP_CHECK_FUNC(SHA2)
MY_HWCAP_CHECK_FUNC(AES)

#endif // __APPLE__
#endif // _WIN32

#endif // MY_CPU_ARM_OR_ARM64

#ifdef __APPLE__

#include <sys/sysctl.h>

int My_sysctlbyname_Get(const char *name, void *buf, size_t *bufSize)
{
    return sysctlbyname(name, buf, bufSize, NULL, 0);
}

int My_sysctlbyname_Get_UInt32(const char *name, UInt32 *val)
{
    size_t bufSize = sizeof(*val);
    int res = My_sysctlbyname_Get(name, val, &bufSize);
    if (res == 0 && bufSize != sizeof(*val))
        return EFAULT;
    return res;
}

#endif

/* LzFind.h -- Match finder for LZ algorithms
2021-07-13 : Igor Pavlov : Public domain */

#ifndef __LZ_FIND_H
#define __LZ_FIND_H

EXTERN_C_BEGIN

typedef UInt32 CLzRef;

typedef struct _CMatchFinder
{
    Byte *buffer;
    UInt32 pos;
    UInt32 posLimit;
    UInt32 streamPos; /* wrap over Zero is allowed (streamPos < pos). Use (UInt32)(streamPos - pos) */
    UInt32 lenLimit;

    UInt32 cyclicBufferPos;
    UInt32 cyclicBufferSize; /* it must be = (historySize + 1) */

    Byte streamEndWasReached;
    Byte btMode;
    Byte bigHash;
    Byte directInput;

    UInt32 matchMaxLen;
    CLzRef *hash;
    CLzRef *son;
    UInt32 hashMask;
    UInt32 cutValue;

    Byte *bufferBase;
    ISeqInStream *stream;

    UInt32 blockSize;
    UInt32 keepSizeBefore;
    UInt32 keepSizeAfter;

    UInt32 numHashBytes;
    size_t directInputRem;
    UInt32 historySize;
    UInt32 fixedHashSize;
    UInt32 hashSizeSum;
    SRes result;
    UInt32 crc[256];
    size_t numRefs;

    UInt64 expectedDataSize;
} CMatchFinder;

#define Inline_MatchFinder_GetPointerToCurrentPos(p) ((const Byte *)(p)->buffer)

#define Inline_MatchFinder_GetNumAvailableBytes(p) ((UInt32)((p)->streamPos - (p)->pos))

/*
#define Inline_MatchFinder_IsFinishedOK(p) \
    ((p)->streamEndWasReached \
        && (p)->streamPos == (p)->pos \
        && (!(p)->directInput || (p)->directInputRem == 0))
*/

int MatchFinder_NeedMove(CMatchFinder *p);
/* Byte *MatchFinder_GetPointerToCurrentPos(CMatchFinder *p); */
void MatchFinder_MoveBlock(CMatchFinder *p);
void MatchFinder_ReadIfRequired(CMatchFinder *p);

void MatchFinder_Construct(CMatchFinder *p);

/* Conditions:
     historySize <= 3 GB
     keepAddBufferBefore + matchMaxLen + keepAddBufferAfter < 511MB
*/
int MatchFinder_Create(CMatchFinder *p, UInt32 historySize,
                       UInt32 keepAddBufferBefore, UInt32 matchMaxLen, UInt32 keepAddBufferAfter,
                       ISzAllocPtr alloc);
void MatchFinder_Free(CMatchFinder *p, ISzAllocPtr alloc);
void MatchFinder_Normalize3(UInt32 subValue, CLzRef *items, size_t numItems);
// void MatchFinder_ReduceOffsets(CMatchFinder *p, UInt32 subValue);

/*
#define Inline_MatchFinder_InitPos(p, val) \
    (p)->pos = (val); \
    (p)->streamPos = (val);
*/

#define Inline_MatchFinder_ReduceOffsets(p, subValue) \
    (p)->pos -= (subValue);                           \
    (p)->streamPos -= (subValue);

UInt32 *GetMatchesSpec1(UInt32 lenLimit, UInt32 curMatch, UInt32 pos, const Byte *buffer, CLzRef *son,
                        size_t _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 _cutValue,
                        UInt32 *distances, UInt32 maxLen);

/*
Conditions:
  Mf_GetNumAvailableBytes_Func must be called before each Mf_GetMatchLen_Func.
  Mf_GetPointerToCurrentPos_Func's result must be used only before any other function
*/

typedef void (*Mf_Init_Func)(void *object);
typedef UInt32 (*Mf_GetNumAvailableBytes_Func)(void *object);
typedef const Byte *(*Mf_GetPointerToCurrentPos_Func)(void *object);
typedef UInt32 *(*Mf_GetMatches_Func)(void *object, UInt32 *distances);
typedef void (*Mf_Skip_Func)(void *object, UInt32);

typedef struct _IMatchFinder
{
    Mf_Init_Func Init;
    Mf_GetNumAvailableBytes_Func GetNumAvailableBytes;
    Mf_GetPointerToCurrentPos_Func GetPointerToCurrentPos;
    Mf_GetMatches_Func GetMatches;
    Mf_Skip_Func Skip;
} IMatchFinder2;

void MatchFinder_CreateVTable(CMatchFinder *p, IMatchFinder2 *vTable);

void MatchFinder_Init_LowHash(CMatchFinder *p);
void MatchFinder_Init_HighHash(CMatchFinder *p);
void MatchFinder_Init_4(CMatchFinder *p);
void MatchFinder_Init(CMatchFinder *p);

UInt32 *Bt3Zip_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances);
UInt32 *Hc3Zip_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances);

void Bt3Zip_MatchFinder_Skip(CMatchFinder *p, UInt32 num);
void Hc3Zip_MatchFinder_Skip(CMatchFinder *p, UInt32 num);

void LzFindPrepare(void);

EXTERN_C_END

#endif

/* LzFindOpt.c -- multithreaded Match finder for LZ algorithms
2021-07-13 : Igor Pavlov : Public domain */

// #include "LzFindMt.h"

// #define LOG_ITERS

// #define LOG_THREAD

#ifdef LOG_THREAD
#include <stdio.h>
#define PRF(x) x
#else
#define PRF(x)
#endif

#ifdef LOG_ITERS
#include <stdio.h>
UInt64 g_NumIters_Tree;
UInt64 g_NumIters_Loop;
UInt64 g_NumIters_Bytes;
#define LOG_ITER(x) x
#else
#define LOG_ITER(x)
#endif

// ---------- BT THREAD ----------

#define USE_SON_PREFETCH
#define USE_LONG_MATCH_OPT

#define kEmptyHashValue 0

UInt32 *MY_FAST_CALL GetMatchesSpecN_2(const Byte *lenLimit, size_t pos, const Byte *cur, CLzRef *son,
                                       UInt32 _cutValue, UInt32 *d, size_t _maxLen, const UInt32 *hash, const UInt32 *limit, const UInt32 *size,
                                       size_t _cyclicBufferPos, UInt32 _cyclicBufferSize,
                                       UInt32 *posRes);

MY_NO_INLINE
UInt32 *MY_FAST_CALL GetMatchesSpecN_2(const Byte *lenLimit, size_t pos, const Byte *cur, CLzRef *son,
                                       UInt32 _cutValue, UInt32 *d, size_t _maxLen, const UInt32 *hash, const UInt32 *limit, const UInt32 *size,
                                       size_t _cyclicBufferPos, UInt32 _cyclicBufferSize,
                                       UInt32 *posRes)
{
    do // while (hash != size)
    {
        UInt32 delta;

#ifndef cbs
        UInt32 cbs;
#endif

        if (hash == size)
            break;

        delta = *hash++;

        if (delta == 0)
            return NULL;

        lenLimit++;

#ifndef cbs
        cbs = _cyclicBufferSize;
        if ((UInt32)pos < cbs)
        {
            if (delta > (UInt32)pos)
                return NULL;
            cbs = (UInt32)pos;
        }
#endif

        if (delta >= cbs)
        {
            CLzRef *ptr1 = son + ((size_t)_cyclicBufferPos << 1);
            *d++ = 0;
            ptr1[0] = kEmptyHashValue;
            ptr1[1] = kEmptyHashValue;
        }
        else
        {
            UInt32 *_distances = ++d;

            CLzRef *ptr0 = son + ((size_t)_cyclicBufferPos << 1) + 1;
            CLzRef *ptr1 = son + ((size_t)_cyclicBufferPos << 1);

            UInt32 cutValue = _cutValue;
            const Byte *len0 = cur, *len1 = cur;
            const Byte *maxLen = cur + _maxLen;

            // if (cutValue == 0) { *ptr0 = *ptr1 = kEmptyHashValue; } else
            for (LOG_ITER(g_NumIters_Tree++);;)
            {
                LOG_ITER(g_NumIters_Loop++);
                {
                    // SPEC code
                    CLzRef *pair = son + ((size_t)((ptrdiff_t)_cyclicBufferPos - (ptrdiff_t)delta + (ptrdiff_t)(UInt32)(_cyclicBufferPos < delta ? cbs : 0)) << 1);

                    const ptrdiff_t diff = (ptrdiff_t)0 - (ptrdiff_t)delta;
                    const Byte *len = (len0 < len1 ? len0 : len1);

#ifdef USE_SON_PREFETCH
                    const UInt32 pair0 = *pair;
#endif

                    if (len[diff] == len[0])
                    {
                        if (++len != lenLimit && len[diff] == len[0])
                            while (++len != lenLimit)
                            {
                                LOG_ITER(g_NumIters_Bytes++);
                                if (len[diff] != len[0])
                                    break;
                            }
                        if (maxLen < len)
                        {
                            maxLen = len;
                            *d++ = (UInt32)(len - cur);
                            *d++ = delta - 1;

                            if (len == lenLimit)
                            {
                                const UInt32 pair1 = pair[1];
                                *ptr1 =
#ifdef USE_SON_PREFETCH
                                    pair0;
#else
                                    pair[0];
#endif
                                *ptr0 = pair1;

                                _distances[-1] = (UInt32)(d - _distances);

#ifdef USE_LONG_MATCH_OPT

                                if (hash == size || *hash != delta || lenLimit[diff] != lenLimit[0] || d >= limit)
                                    break;

                                {
                                    for (;;)
                                    {
                                        *d++ = 2;
                                        *d++ = (UInt32)(lenLimit - cur);
                                        *d++ = delta - 1;
                                        cur++;
                                        lenLimit++;
                                        // SPEC
                                        _cyclicBufferPos++;
                                        {
                                            // SPEC code
                                            CLzRef *dest = son + ((size_t)(_cyclicBufferPos) << 1);
                                            const CLzRef *src = dest + ((diff + (ptrdiff_t)(UInt32)((_cyclicBufferPos < delta) ? cbs : 0)) << 1);
// CLzRef *ptr = son + ((size_t)(pos) << 1) - CYC_TO_POS_OFFSET * 2;
#if 0
                                            *(UInt64 *)(void *)dest = *((const UInt64 *)(const void *)src);
#else
                                            const UInt32 p0 = src[0];
                                            const UInt32 p1 = src[1];
                                            dest[0] = p0;
                                            dest[1] = p1;
#endif
                                        }
                                        pos++;
                                        hash++;
                                        if (hash == size || *hash != delta || lenLimit[diff] != lenLimit[0] || d >= limit)
                                            break;
                                    } // for() end for long matches
                                }
#endif

                                break; // break from TREE iterations
                            }
                        }
                    }
                    {
                        const UInt32 curMatch = (UInt32)pos - delta; // (UInt32)(pos + diff);
                        if (len[diff] < len[0])
                        {
                            delta = pair[1];
                            *ptr1 = curMatch;
                            ptr1 = pair + 1;
                            len1 = len;
                            if (delta >= curMatch)
                                return NULL;
                        }
                        else
                        {
                            delta = *pair;
                            *ptr0 = curMatch;
                            ptr0 = pair;
                            len0 = len;
                            if (delta >= curMatch)
                                return NULL;
                        }
                        delta = (UInt32)pos - delta;

                        if (--cutValue == 0 || delta >= cbs)
                        {
                            *ptr0 = *ptr1 = kEmptyHashValue;
                            _distances[-1] = (UInt32)(d - _distances);
                            break;
                        }
                    }
                }
            } // for (tree iterations)
        }
        pos++;
        _cyclicBufferPos++;
        cur++;
    } while (d < limit);
    *posRes = (UInt32)pos;
    return d;
}

/* LzFind.c -- Match finder for LZ algorithms
2021-11-29 : Igor Pavlov : Public domain */

#include <string.h>
// #include <stdio.h>

/* LzHash.h -- HASH functions for LZ algorithms
2019-10-30 : Igor Pavlov : Public domain */

#ifndef __LZ_HASH_H
#define __LZ_HASH_H

/*
  (kHash2Size >= (1 <<  8)) : Required
  (kHash3Size >= (1 << 16)) : Required
*/

#define kHash2Size (1 << 10)
#define kHash3Size (1 << 16)
// #define kHash4Size (1 << 20)

#define kFix3HashSize (kHash2Size)
#define kFix4HashSize (kHash2Size + kHash3Size)
// #define kFix5HashSize (kHash2Size + kHash3Size + kHash4Size)

/*
  We use up to 3 crc values for hash:
    crc0
    crc1 << Shift_1
    crc2 << Shift_2
  (Shift_1 = 5) and (Shift_2 = 10) is good tradeoff.
  Small values for Shift are not good for collision rate.
  Big value for Shift_2 increases the minimum size
  of hash table, that will be slow for small files.
*/

#define kLzHash_CrcShift_1 5
#define kLzHash_CrcShift_2 10

#endif

#define kBlockMoveAlign (1 << 7)       // alignment for memmove()
#define kBlockSizeAlign (1 << 16)      // alignment for block allocation
#define kBlockSizeReserveMin (1 << 24) // it's 1/256 from 4 GB dictinary

#define kEmptyHashValue 0

#define kMaxValForNormalize ((UInt32)0)
// #define kMaxValForNormalize ((UInt32)(1 << 20) + 0xFFF) // for debug

// #define kNormalizeAlign (1 << 7) // alignment for speculated accesses

#define GET_AVAIL_BYTES(p) \
    Inline_MatchFinder_GetNumAvailableBytes(p)

// #define kFix5HashSize (kHash2Size + kHash3Size + kHash4Size)
#define kFix5HashSize kFix4HashSize

/*
 HASH2_CALC:
   if (hv) match, then cur[0] and cur[1] also match
*/
#define HASH2_CALC hv = GetUi16(cur);

// (crc[0 ... 255] & 0xFF) provides one-to-one correspondence to [0 ... 255]

/*
 HASH3_CALC:
   if (cur[0]) and (h2) match, then cur[1]            also match
   if (cur[0]) and (hv) match, then cur[1] and cur[2] also match
*/
#define HASH3_CALC                                         \
    {                                                      \
        UInt32 temp = p->crc[cur[0]] ^ cur[1];             \
        h2 = temp & (kHash2Size - 1);                      \
        hv = (temp ^ ((UInt32)cur[2] << 8)) & p->hashMask; \
    }

#define HASH4_CALC                                                          \
    {                                                                       \
        UInt32 temp = p->crc[cur[0]] ^ cur[1];                              \
        h2 = temp & (kHash2Size - 1);                                       \
        temp ^= ((UInt32)cur[2] << 8);                                      \
        h3 = temp & (kHash3Size - 1);                                       \
        hv = (temp ^ (p->crc[cur[3]] << kLzHash_CrcShift_1)) & p->hashMask; \
    }

#define HASH5_CALC                                                          \
    {                                                                       \
        UInt32 temp = p->crc[cur[0]] ^ cur[1];                              \
        h2 = temp & (kHash2Size - 1);                                       \
        temp ^= ((UInt32)cur[2] << 8);                                      \
        h3 = temp & (kHash3Size - 1);                                       \
        temp ^= (p->crc[cur[3]] << kLzHash_CrcShift_1);                     \
        /* h4 = temp & p->hash4Mask; */ /* (kHash4Size - 1); */             \
        hv = (temp ^ (p->crc[cur[4]] << kLzHash_CrcShift_2)) & p->hashMask; \
    }

#define HASH_ZIP_CALC hv = ((cur[2] | ((UInt32)cur[0] << 8)) ^ p->crc[cur[1]]) & 0xFFFF;

static void LzInWindow_Free(CMatchFinder *p, ISzAllocPtr alloc)
{
    if (!p->directInput)
    {
        ISzAlloc_Free(alloc, p->bufferBase);
        p->bufferBase = NULL;
    }
}

static int LzInWindow_Create2(CMatchFinder *p, UInt32 blockSize, ISzAllocPtr alloc)
{
    if (blockSize == 0)
        return 0;
    if (!p->bufferBase || p->blockSize != blockSize)
    {
        // size_t blockSizeT;
        LzInWindow_Free(p, alloc);
        p->blockSize = blockSize;
        // blockSizeT = blockSize;

        // printf("\nblockSize = 0x%x\n", blockSize);
        /*
        #if defined _WIN64
        // we can allocate 4GiB, but still use UInt32 for (p->blockSize)
        // we use UInt32 type for (p->blockSize), because
        // we don't want to wrap over 4 GiB,
        // when we use (p->streamPos - p->pos) that is UInt32.
        if (blockSize >= (UInt32)0 - (UInt32)kBlockSizeAlign)
        {
          blockSizeT = ((size_t)1 << 32);
          printf("\nchanged to blockSizeT = 4GiB\n");
        }
        #endif
        */

        p->bufferBase = (Byte *)ISzAlloc_Alloc(alloc, blockSize);
        // printf("\nbufferBase = %p\n", p->bufferBase);
        // return 0; // for debug
    }
    return (p->bufferBase != NULL);
}

static const Byte *MatchFinder_GetPointerToCurrentPos(CMatchFinder *p)
{
    return p->buffer;
}

static UInt32 MatchFinder_GetNumAvailableBytes(CMatchFinder *p)
{
    return GET_AVAIL_BYTES(p);
}

MY_NO_INLINE
static void MatchFinder_ReadBlock(CMatchFinder *p)
{
    if (p->streamEndWasReached || p->result != SZ_OK)
        return;

    /* We use (p->streamPos - p->pos) value.
       (p->streamPos < p->pos) is allowed. */

    if (p->directInput)
    {
        UInt32 curSize = 0xFFFFFFFF - GET_AVAIL_BYTES(p);
        if (curSize > p->directInputRem)
            curSize = (UInt32)p->directInputRem;
        p->directInputRem -= curSize;
        p->streamPos += curSize;
        if (p->directInputRem == 0)
            p->streamEndWasReached = 1;
        return;
    }

    for (;;)
    {
        Byte *dest = p->buffer + GET_AVAIL_BYTES(p);
        size_t size = (size_t)(p->bufferBase + p->blockSize - dest);
        if (size == 0)
        {
            /* we call ReadBlock() after NeedMove() and MoveBlock().
               NeedMove() and MoveBlock() povide more than (keepSizeAfter)
               to the end of (blockSize).
               So we don't execute this branch in normal code flow.
               We can go here, if we will call ReadBlock() before NeedMove(), MoveBlock().
            */
            // p->result = SZ_ERROR_FAIL; // we can show error here
            return;
        }

        // #define kRead 3
        // if (size > kRead) size = kRead; // for debug

        p->result = ISeqInStream_Read(p->stream, dest, &size);
        if (p->result != SZ_OK)
            return;
        if (size == 0)
        {
            p->streamEndWasReached = 1;
            return;
        }
        p->streamPos += (UInt32)size;
        if (GET_AVAIL_BYTES(p) > p->keepSizeAfter)
            return;
        /* here and in another (p->keepSizeAfter) checks we keep on 1 byte more than was requested by Create() function
             (GET_AVAIL_BYTES(p) >= p->keepSizeAfter) - minimal required size */
    }

    // on exit: (p->result != SZ_OK || p->streamEndWasReached || GET_AVAIL_BYTES(p) > p->keepSizeAfter)
}

MY_NO_INLINE
void MatchFinder_MoveBlock(CMatchFinder *p)
{
    const size_t offset = (size_t)(p->buffer - p->bufferBase) - p->keepSizeBefore;
    const size_t keepBefore = (offset & (kBlockMoveAlign - 1)) + p->keepSizeBefore;
    p->buffer = p->bufferBase + keepBefore;
    memmove(p->bufferBase,
            p->bufferBase + (offset & ~((size_t)kBlockMoveAlign - 1)),
            keepBefore + (size_t)GET_AVAIL_BYTES(p));
}

/* We call MoveBlock() before ReadBlock().
   So MoveBlock() can be wasteful operation, if the whole input data
   can fit in current block even without calling MoveBlock().
   in important case where (dataSize <= historySize)
     condition (p->blockSize > dataSize + p->keepSizeAfter) is met
     So there is no MoveBlock() in that case case.
*/

int MatchFinder_NeedMove(CMatchFinder *p)
{
    if (p->directInput)
        return 0;
    if (p->streamEndWasReached || p->result != SZ_OK)
        return 0;
    return ((size_t)(p->bufferBase + p->blockSize - p->buffer) <= p->keepSizeAfter);
}

void MatchFinder_ReadIfRequired(CMatchFinder *p)
{
    if (p->keepSizeAfter >= GET_AVAIL_BYTES(p))
        MatchFinder_ReadBlock(p);
}

static void MatchFinder_SetDefaultSettings(CMatchFinder *p)
{
    p->cutValue = 32;
    p->btMode = 1;
    p->numHashBytes = 4;
    p->bigHash = 0;
}

#define kCrcPoly 0xEDB88320

void MatchFinder_Construct(CMatchFinder *p)
{
    unsigned i;
    p->bufferBase = NULL;
    p->directInput = 0;
    p->hash = NULL;
    p->expectedDataSize = (UInt64)(Int64)-1;
    MatchFinder_SetDefaultSettings(p);

    for (i = 0; i < 256; i++)
    {
        UInt32 r = (UInt32)i;
        unsigned j;
        for (j = 0; j < 8; j++)
            r = (r >> 1) ^ (kCrcPoly & ((UInt32)0 - (r & 1)));
        p->crc[i] = r;
    }
}

static void MatchFinder_FreeThisClassMemory(CMatchFinder *p, ISzAllocPtr alloc)
{
    ISzAlloc_Free(alloc, p->hash);
    p->hash = NULL;
}

void MatchFinder_Free(CMatchFinder *p, ISzAllocPtr alloc)
{
    MatchFinder_FreeThisClassMemory(p, alloc);
    LzInWindow_Free(p, alloc);
}

static CLzRef *AllocRefs(size_t num, ISzAllocPtr alloc)
{
    size_t sizeInBytes = (size_t)num * sizeof(CLzRef);
    if (sizeInBytes / sizeof(CLzRef) != num)
        return NULL;
    return (CLzRef *)ISzAlloc_Alloc(alloc, sizeInBytes);
}

#if (kBlockSizeReserveMin < kBlockSizeAlign * 2)
#error Stop_Compiling_Bad_Reserve
#endif

static UInt32 GetBlockSize(CMatchFinder *p, UInt32 historySize)
{
    UInt32 blockSize = (p->keepSizeBefore + p->keepSizeAfter);
    /*
    if (historySize > kMaxHistorySize)
      return 0;
    */
    // printf("\nhistorySize == 0x%x\n", historySize);

    if (p->keepSizeBefore < historySize || blockSize < p->keepSizeBefore) // if 32-bit overflow
        return 0;

    {
        const UInt32 kBlockSizeMax = (UInt32)0 - (UInt32)kBlockSizeAlign;
        const UInt32 rem = kBlockSizeMax - blockSize;
        const UInt32 reserve = (blockSize >> (blockSize < ((UInt32)1 << 30) ? 1 : 2)) + (1 << 12) + kBlockMoveAlign + kBlockSizeAlign; // do not overflow 32-bit here
        if (blockSize >= kBlockSizeMax || rem < kBlockSizeReserveMin)                                                                  // we reject settings that will be slow
            return 0;
        if (reserve >= rem)
            blockSize = kBlockSizeMax;
        else
        {
            blockSize += reserve;
            blockSize &= ~(UInt32)(kBlockSizeAlign - 1);
        }
    }
    // printf("\n LzFind_blockSize = %x\n", blockSize);
    // printf("\n LzFind_blockSize = %d\n", blockSize >> 20);
    return blockSize;
}

int MatchFinder_Create(CMatchFinder *p, UInt32 historySize,
                       UInt32 keepAddBufferBefore, UInt32 matchMaxLen, UInt32 keepAddBufferAfter,
                       ISzAllocPtr alloc)
{
    /* we need one additional byte in (p->keepSizeBefore),
       since we use MoveBlock() after (p->pos++) and before dictionary using */
    // keepAddBufferBefore = (UInt32)0xFFFFFFFF - (1 << 22); // for debug
    p->keepSizeBefore = historySize + keepAddBufferBefore + 1;

    keepAddBufferAfter += matchMaxLen;
    /* we need (p->keepSizeAfter >= p->numHashBytes) */
    if (keepAddBufferAfter < p->numHashBytes)
        keepAddBufferAfter = p->numHashBytes;
    // keepAddBufferAfter -= 2; // for debug
    p->keepSizeAfter = keepAddBufferAfter;

    if (p->directInput)
        p->blockSize = 0;
    if (p->directInput || LzInWindow_Create2(p, GetBlockSize(p, historySize), alloc))
    {
        const UInt32 newCyclicBufferSize = historySize + 1; // do not change it
        UInt32 hs;
        p->matchMaxLen = matchMaxLen;
        {
            // UInt32 hs4;
            p->fixedHashSize = 0;
            hs = (1 << 16) - 1;
            if (p->numHashBytes != 2)
            {
                hs = historySize;
                if (hs > p->expectedDataSize)
                    hs = (UInt32)p->expectedDataSize;
                if (hs != 0)
                    hs--;
                hs |= (hs >> 1);
                hs |= (hs >> 2);
                hs |= (hs >> 4);
                hs |= (hs >> 8);
                // we propagated 16 bits in (hs). Low 16 bits must be set later
                hs >>= 1;
                if (hs >= (1 << 24))
                {
                    if (p->numHashBytes == 3)
                        hs = (1 << 24) - 1;
                    else
                        hs >>= 1;
                    /* if (bigHash) mode, GetHeads4b() in LzFindMt.c needs (hs >= ((1 << 24) - 1))) */
                }

                // hs = ((UInt32)1 << 25) - 1; // for test

                // (hash_size >= (1 << 16)) : Required for (numHashBytes > 2)
                hs |= (1 << 16) - 1; /* don't change it! */

                // bt5: we adjust the size with recommended minimum size
                if (p->numHashBytes >= 5)
                    hs |= (256 << kLzHash_CrcShift_2) - 1;
            }
            p->hashMask = hs;
            hs++;

            /*
            hs4 = (1 << 20);
            if (hs4 > hs)
              hs4 = hs;
            // hs4 = (1 << 16); // for test
            p->hash4Mask = hs4 - 1;
            */

            if (p->numHashBytes > 2)
                p->fixedHashSize += kHash2Size;
            if (p->numHashBytes > 3)
                p->fixedHashSize += kHash3Size;
            // if (p->numHashBytes > 4) p->fixedHashSize += hs4; // kHash4Size;
            hs += p->fixedHashSize;
        }

        {
            size_t newSize;
            size_t numSons;
            p->historySize = historySize;
            p->hashSizeSum = hs;
            p->cyclicBufferSize = newCyclicBufferSize; // it must be = (historySize + 1)

            numSons = newCyclicBufferSize;
            if (p->btMode)
                numSons <<= 1;
            newSize = hs + numSons;

// aligned size is not required here, but it can be better for some loops
#define NUM_REFS_ALIGN_MASK 0xF
            newSize = (newSize + NUM_REFS_ALIGN_MASK) & ~(size_t)NUM_REFS_ALIGN_MASK;

            if (p->hash && p->numRefs == newSize)
                return 1;

            MatchFinder_FreeThisClassMemory(p, alloc);
            p->numRefs = newSize;
            p->hash = AllocRefs(newSize, alloc);

            if (p->hash)
            {
                p->son = p->hash + p->hashSizeSum;
                return 1;
            }
        }
    }

    MatchFinder_Free(p, alloc);
    return 0;
}

static void MatchFinder_SetLimits(CMatchFinder *p)
{
    UInt32 k;
    UInt32 n = kMaxValForNormalize - p->pos;
    if (n == 0)
        n = (UInt32)(Int32)-1; // we allow (pos == 0) at start even with (kMaxValForNormalize == 0)

    k = p->cyclicBufferSize - p->cyclicBufferPos;
    if (k < n)
        n = k;

    k = GET_AVAIL_BYTES(p);
    {
        const UInt32 ksa = p->keepSizeAfter;
        UInt32 mm = p->matchMaxLen;
        if (k > ksa)
            k -= ksa; // we must limit exactly to keepSizeAfter for ReadBlock
        else if (k >= mm)
        {
            // the limitation for (p->lenLimit) update
            k -= mm; // optimization : to reduce the number of checks
            k++;
            // k = 1; // non-optimized version : for debug
        }
        else
        {
            mm = k;
            if (k != 0)
                k = 1;
        }
        p->lenLimit = mm;
    }
    if (k < n)
        n = k;

    p->posLimit = p->pos + n;
}

void MatchFinder_Init_LowHash(CMatchFinder *p)
{
    size_t i;
    CLzRef *items = p->hash;
    const size_t numItems = p->fixedHashSize;
    for (i = 0; i < numItems; i++)
        items[i] = kEmptyHashValue;
}

void MatchFinder_Init_HighHash(CMatchFinder *p)
{
    size_t i;
    CLzRef *items = p->hash + p->fixedHashSize;
    const size_t numItems = (size_t)p->hashMask + 1;
    for (i = 0; i < numItems; i++)
        items[i] = kEmptyHashValue;
}

void MatchFinder_Init_4(CMatchFinder *p)
{
    p->buffer = p->bufferBase;
    {
        /* kEmptyHashValue = 0 (Zero) is used in hash tables as NO-VALUE marker.
           the code in CMatchFinderMt expects (pos = 1) */
        p->pos =
            p->streamPos =
                1; // it's smallest optimal value. do not change it
        // 0; // for debug
    }
    p->result = SZ_OK;
    p->streamEndWasReached = 0;
}

// (CYC_TO_POS_OFFSET == 0) is expected by some optimized code
#define CYC_TO_POS_OFFSET 0
// #define CYC_TO_POS_OFFSET 1 // for debug

void MatchFinder_Init(CMatchFinder *p)
{
    MatchFinder_Init_HighHash(p);
    MatchFinder_Init_LowHash(p);
    MatchFinder_Init_4(p);
    // if (readData)
    MatchFinder_ReadBlock(p);

    /* if we init (cyclicBufferPos = pos), then we can use one variable
       instead of both (cyclicBufferPos) and (pos) : only before (cyclicBufferPos) wrapping */
    p->cyclicBufferPos = (p->pos - CYC_TO_POS_OFFSET); // init with relation to (pos)
    // p->cyclicBufferPos = 0; // smallest value
    // p->son[0] = p->son[1] = 0; // unused: we can init skipped record for speculated accesses.
    MatchFinder_SetLimits(p);
}

#ifdef MY_CPU_X86_OR_AMD64
#if defined(__clang__) && (__clang_major__ >= 8) || defined(__GNUC__) && (__GNUC__ >= 8) || defined(__INTEL_COMPILER) && (__INTEL_COMPILER >= 1900)
#define USE_SATUR_SUB_128
#define USE_AVX2
#define ATTRIB_SSE41 __attribute__((__target__("sse4.1")))
#define ATTRIB_AVX2 __attribute__((__target__("avx2")))
#elif defined(_MSC_VER)
#if (_MSC_VER >= 1600)
#define USE_SATUR_SUB_128
#if (_MSC_VER >= 1900)
#define USE_AVX2
#include <immintrin.h> // avx
#endif
#endif
#endif

// #elif defined(MY_CPU_ARM_OR_ARM64)
#elif defined(MY_CPU_ARM64)

#if defined(__clang__) && (__clang_major__ >= 8) || defined(__GNUC__) && (__GNUC__ >= 8)
#define USE_SATUR_SUB_128
#ifdef MY_CPU_ARM64
// #define ATTRIB_SSE41 __attribute__((__target__("")))
#else
// #define ATTRIB_SSE41 __attribute__((__target__("fpu=crypto-neon-fp-armv8")))
#endif

#elif defined(_MSC_VER)
#if (_MSC_VER >= 1910)
#define USE_SATUR_SUB_128
#endif
#endif

#if defined(_MSC_VER) && defined(MY_CPU_ARM64)
#include <arm64_neon.h>
#else
#include <arm_neon.h>
#endif

#endif

/*
#ifndef ATTRIB_SSE41
  #define ATTRIB_SSE41
#endif
#ifndef ATTRIB_AVX2
  #define ATTRIB_AVX2
#endif
*/

#ifdef USE_SATUR_SUB_128

// #define _SHOW_HW_STATUS

#ifdef _SHOW_HW_STATUS
#include <stdio.h>
#define _PRF(x) x
_PRF(;)
#else
#define _PRF(x)
#endif

#ifdef MY_CPU_ARM_OR_ARM64

#ifdef MY_CPU_ARM64
// #define FORCE_SATUR_SUB_128
#endif

typedef uint32x4_t v128;
#define SASUB_128(i)                   \
    *(v128 *)(void *)(items + (i)*4) = \
        vsubq_u32(vmaxq_u32(*(const v128 *)(const void *)(items + (i)*4), sub2), sub2);

#else

#include <smmintrin.h> // sse4.1

typedef __m128i v128;
#define SASUB_128(i)                   \
    *(v128 *)(void *)(items + (i)*4) = \
        _mm_sub_epi32(_mm_max_epu32(*(const v128 *)(const void *)(items + (i)*4), sub2), sub2); // SSE 4.1

#endif

MY_NO_INLINE
static
#ifdef ATTRIB_SSE41
    ATTRIB_SSE41
#endif
    void
        MY_FAST_CALL
        LzFind_SaturSub_128(UInt32 subValue, CLzRef *items, const CLzRef *lim)
{
    v128 sub2 =
#ifdef MY_CPU_ARM_OR_ARM64
        vdupq_n_u32(subValue);
#else
        _mm_set_epi32((Int32)subValue, (Int32)subValue, (Int32)subValue, (Int32)subValue);
#endif
    do
    {
        SASUB_128(0)
        SASUB_128(1)
        SASUB_128(2)
        SASUB_128(3)
        items += 4 * 4;
    } while (items != lim);
}

#ifdef USE_AVX2

#include <immintrin.h> // avx

#define SASUB_256(i) *(__m256i *)(void *)(items + (i)*8) = _mm256_sub_epi32(_mm256_max_epu32(*(const __m256i *)(const void *)(items + (i)*8), sub2), sub2); // AVX2

MY_NO_INLINE
static
#ifdef ATTRIB_AVX2
    ATTRIB_AVX2
#endif
    void
        MY_FAST_CALL
        LzFind_SaturSub_256(UInt32 subValue, CLzRef *items, const CLzRef *lim)
{
    __m256i sub2 = _mm256_set_epi32(
        (Int32)subValue, (Int32)subValue, (Int32)subValue, (Int32)subValue,
        (Int32)subValue, (Int32)subValue, (Int32)subValue, (Int32)subValue);
    do
    {
        SASUB_256(0)
        SASUB_256(1)
        items += 2 * 8;
    } while (items != lim);
}
#endif // USE_AVX2

#ifndef FORCE_SATUR_SUB_128
typedef void(MY_FAST_CALL *LZFIND_SATUR_SUB_CODE_FUNC)(
    UInt32 subValue, CLzRef *items, const CLzRef *lim);
static LZFIND_SATUR_SUB_CODE_FUNC g_LzFind_SaturSub;
#endif // FORCE_SATUR_SUB_128

#endif // USE_SATUR_SUB_128

// kEmptyHashValue must be zero
// #define SASUB_32(i) v = items[i];  m = v - subValue;  if (v < subValue) m = kEmptyHashValue;  items[i] = m;
#define SASUB_32(i)   \
    v = items[i];     \
    if (v < subValue) \
        v = subValue; \
    items[i] = v - subValue;

#ifdef FORCE_SATUR_SUB_128

#define DEFAULT_SaturSub LzFind_SaturSub_128

#else

#define DEFAULT_SaturSub LzFind_SaturSub_32

MY_NO_INLINE
static void
    MY_FAST_CALL
    LzFind_SaturSub_32(UInt32 subValue, CLzRef *items, const CLzRef *lim)
{
    do
    {
        UInt32 v;
        SASUB_32(0)
        SASUB_32(1)
        SASUB_32(2)
        SASUB_32(3)
        SASUB_32(4)
        SASUB_32(5)
        SASUB_32(6)
        SASUB_32(7)
        items += 8;
    } while (items != lim);
}

#endif

MY_NO_INLINE
void MatchFinder_Normalize3(UInt32 subValue, CLzRef *items, size_t numItems)
{
#define K_NORM_ALIGN_BLOCK_SIZE (1 << 6)

    CLzRef *lim;

    for (; numItems != 0 && ((unsigned)(ptrdiff_t)items & (K_NORM_ALIGN_BLOCK_SIZE - 1)) != 0; numItems--)
    {
        UInt32 v;
        SASUB_32(0);
        items++;
    }

    {
#define K_NORM_ALIGN_MASK (K_NORM_ALIGN_BLOCK_SIZE / 4 - 1)
        lim = items + (numItems & ~(size_t)K_NORM_ALIGN_MASK);
        numItems &= K_NORM_ALIGN_MASK;
        if (items != lim)
        {
#if defined(USE_SATUR_SUB_128) && !defined(FORCE_SATUR_SUB_128)
            if (g_LzFind_SaturSub)
                g_LzFind_SaturSub(subValue, items, lim);
            else
#endif
                DEFAULT_SaturSub(subValue, items, lim);
        }
        items = lim;
    }

    for (; numItems != 0; numItems--)
    {
        UInt32 v;
        SASUB_32(0);
        items++;
    }
}

// call MatchFinder_CheckLimits() only after (p->pos++) update

MY_NO_INLINE
static void MatchFinder_CheckLimits(CMatchFinder *p)
{
    if ( // !p->streamEndWasReached && p->result == SZ_OK &&
        p->keepSizeAfter == GET_AVAIL_BYTES(p))
    {
        // we try to read only in exact state (p->keepSizeAfter == GET_AVAIL_BYTES(p))
        if (MatchFinder_NeedMove(p))
            MatchFinder_MoveBlock(p);
        MatchFinder_ReadBlock(p);
    }

    if (p->pos == kMaxValForNormalize)
        if (GET_AVAIL_BYTES(p) >= p->numHashBytes) // optional optimization for last bytes of data.
        /*
           if we disable normalization for last bytes of data, and
           if (data_size == 4 GiB), we don't call wastfull normalization,
           but (pos) will be wrapped over Zero (0) in that case.
           And we cannot resume later to normal operation
        */
        {
            // MatchFinder_Normalize(p);
            /* after normalization we need (p->pos >= p->historySize + 1); */
            /* we can reduce subValue to aligned value, if want to keep alignment
               of (p->pos) and (p->buffer) for speculated accesses. */
            const UInt32 subValue = (p->pos - p->historySize - 1) /* & ~(UInt32)(kNormalizeAlign - 1) */;
            // const UInt32 subValue = (1 << 15); // for debug
            // printf("\nMatchFinder_Normalize() subValue == 0x%x\n", subValue);
            size_t numSonRefs = p->cyclicBufferSize;
            if (p->btMode)
                numSonRefs <<= 1;
            Inline_MatchFinder_ReduceOffsets(p, subValue);
            MatchFinder_Normalize3(subValue, p->hash, (size_t)p->hashSizeSum + numSonRefs);
        }

    if (p->cyclicBufferPos == p->cyclicBufferSize)
        p->cyclicBufferPos = 0;

    MatchFinder_SetLimits(p);
}

/*
  (lenLimit > maxLen)
*/
MY_FORCE_INLINE
static UInt32 *Hc_GetMatchesSpec(size_t lenLimit, UInt32 curMatch, UInt32 pos, const Byte *cur, CLzRef *son,
                                 size_t _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 cutValue,
                                 UInt32 *d, unsigned maxLen)
{
    const Byte *lim = cur + lenLimit;
    son[_cyclicBufferPos] = curMatch;

    do
    {
        UInt32 delta;

        if (curMatch == 0)
            break;
        // if (curMatch2 >= curMatch) return NULL;
        delta = pos - curMatch;
        if (delta >= _cyclicBufferSize)
            break;
        {
            ptrdiff_t diff;
            curMatch = son[_cyclicBufferPos - delta + ((delta > _cyclicBufferPos) ? _cyclicBufferSize : 0)];
            diff = (ptrdiff_t)0 - (ptrdiff_t)delta;
            if (cur[maxLen] == cur[(ptrdiff_t)maxLen + diff])
            {
                const Byte *c = cur;
                while (*c == c[diff])
                {
                    if (++c == lim)
                    {
                        d[0] = (UInt32)(lim - cur);
                        d[1] = delta - 1;
                        return d + 2;
                    }
                }
                {
                    const unsigned len = (unsigned)(c - cur);
                    if (maxLen < len)
                    {
                        maxLen = len;
                        d[0] = (UInt32)len;
                        d[1] = delta - 1;
                        d += 2;
                    }
                }
            }
        }
    } while (--cutValue);

    return d;
}

MY_FORCE_INLINE
UInt32 *GetMatchesSpec1(UInt32 lenLimit, UInt32 curMatch, UInt32 pos, const Byte *cur, CLzRef *son,
                        size_t _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 cutValue,
                        UInt32 *d, UInt32 maxLen)
{
    CLzRef *ptr0 = son + ((size_t)_cyclicBufferPos << 1) + 1;
    CLzRef *ptr1 = son + ((size_t)_cyclicBufferPos << 1);
    unsigned len0 = 0, len1 = 0;

    UInt32 cmCheck;

    // if (curMatch >= pos) { *ptr0 = *ptr1 = kEmptyHashValue; return NULL; }

    cmCheck = (UInt32)(pos - _cyclicBufferSize);
    if ((UInt32)pos <= _cyclicBufferSize)
        cmCheck = 0;

    if (cmCheck < curMatch)
        do
        {
            const UInt32 delta = pos - curMatch;
            {
                CLzRef *pair = son + ((size_t)(_cyclicBufferPos - delta + ((delta > _cyclicBufferPos) ? _cyclicBufferSize : 0)) << 1);
                const Byte *pb = cur - delta;
                unsigned len = (len0 < len1 ? len0 : len1);
                const UInt32 pair0 = pair[0];
                if (pb[len] == cur[len])
                {
                    if (++len != lenLimit && pb[len] == cur[len])
                        while (++len != lenLimit)
                            if (pb[len] != cur[len])
                                break;
                    if (maxLen < len)
                    {
                        maxLen = (UInt32)len;
                        *d++ = (UInt32)len;
                        *d++ = delta - 1;
                        if (len == lenLimit)
                        {
                            *ptr1 = pair0;
                            *ptr0 = pair[1];
                            return d;
                        }
                    }
                }
                if (pb[len] < cur[len])
                {
                    *ptr1 = curMatch;
                    // const UInt32 curMatch2 = pair[1];
                    // if (curMatch2 >= curMatch) { *ptr0 = *ptr1 = kEmptyHashValue;  return NULL; }
                    // curMatch = curMatch2;
                    curMatch = pair[1];
                    ptr1 = pair + 1;
                    len1 = len;
                }
                else
                {
                    *ptr0 = curMatch;
                    curMatch = pair[0];
                    ptr0 = pair;
                    len0 = len;
                }
            }
        } while (--cutValue && cmCheck < curMatch);

    *ptr0 = *ptr1 = kEmptyHashValue;
    return d;
}

static void SkipMatchesSpec(UInt32 lenLimit, UInt32 curMatch, UInt32 pos, const Byte *cur, CLzRef *son,
                            size_t _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 cutValue)
{
    CLzRef *ptr0 = son + ((size_t)_cyclicBufferPos << 1) + 1;
    CLzRef *ptr1 = son + ((size_t)_cyclicBufferPos << 1);
    unsigned len0 = 0, len1 = 0;

    UInt32 cmCheck;

    cmCheck = (UInt32)(pos - _cyclicBufferSize);
    if ((UInt32)pos <= _cyclicBufferSize)
        cmCheck = 0;

    if ( // curMatch >= pos ||  // failure
        cmCheck < curMatch)
        do
        {
            const UInt32 delta = pos - curMatch;
            {
                CLzRef *pair = son + ((size_t)(_cyclicBufferPos - delta + ((delta > _cyclicBufferPos) ? _cyclicBufferSize : 0)) << 1);
                const Byte *pb = cur - delta;
                unsigned len = (len0 < len1 ? len0 : len1);
                if (pb[len] == cur[len])
                {
                    while (++len != lenLimit)
                        if (pb[len] != cur[len])
                            break;
                    {
                        if (len == lenLimit)
                        {
                            *ptr1 = pair[0];
                            *ptr0 = pair[1];
                            return;
                        }
                    }
                }
                if (pb[len] < cur[len])
                {
                    *ptr1 = curMatch;
                    curMatch = pair[1];
                    ptr1 = pair + 1;
                    len1 = len;
                }
                else
                {
                    *ptr0 = curMatch;
                    curMatch = pair[0];
                    ptr0 = pair;
                    len0 = len;
                }
            }
        } while (--cutValue && cmCheck < curMatch);

    *ptr0 = *ptr1 = kEmptyHashValue;
    return;
}

#define MOVE_POS                        \
    ++p->cyclicBufferPos;               \
    p->buffer++;                        \
    {                                   \
        const UInt32 pos1 = p->pos + 1; \
        p->pos = pos1;                  \
        if (pos1 == p->posLimit)        \
            MatchFinder_CheckLimits(p); \
    }

#define MOVE_POS_RET MOVE_POS return distances;

MY_NO_INLINE
static void MatchFinder_MovePos(CMatchFinder *p)
{
    /* we go here at the end of stream data, when (avail < num_hash_bytes)
       We don't update sons[cyclicBufferPos << btMode].
       So (sons) record will contain junk. And we cannot resume match searching
       to normal operation, even if we will provide more input data in buffer.
       p->sons[p->cyclicBufferPos << p->btMode] = 0;  // kEmptyHashValue
       if (p->btMode)
          p->sons[(p->cyclicBufferPos << p->btMode) + 1] = 0;  // kEmptyHashValue
    */
    MOVE_POS;
}

#define GET_MATCHES_HEADER2(minLen, ret_op) \
    unsigned lenLimit;                      \
    UInt32 hv;                              \
    Byte *cur;                              \
    UInt32 curMatch;                        \
    lenLimit = (unsigned)p->lenLimit;       \
    {                                       \
        if (lenLimit < minLen)              \
        {                                   \
            MatchFinder_MovePos(p);         \
            ret_op;                         \
        }                                   \
    }                                       \
    cur = p->buffer;

#define GET_MATCHES_HEADER(minLen) GET_MATCHES_HEADER2(minLen, return distances)
#define SKIP_HEADER(minLen) \
    do                      \
    {                       \
    GET_MATCHES_HEADER2(minLen, continue)

#define MF_PARAMS(p) lenLimit, curMatch, p->pos, p->buffer, p->son, p->cyclicBufferPos, p->cyclicBufferSize, p->cutValue

#define SKIP_FOOTER                \
    SkipMatchesSpec(MF_PARAMS(p)); \
    MOVE_POS;                      \
    }                              \
    while (--num)                  \
        ;

#define GET_MATCHES_FOOTER_BASE(_maxLen_, func)    \
    distances = func(MF_PARAMS(p),                 \
                     distances, (UInt32)_maxLen_); \
    MOVE_POS_RET;

#define GET_MATCHES_FOOTER_BT(_maxLen_) \
    GET_MATCHES_FOOTER_BASE(_maxLen_, GetMatchesSpec1)

#define GET_MATCHES_FOOTER_HC(_maxLen_) \
    GET_MATCHES_FOOTER_BASE(_maxLen_, Hc_GetMatchesSpec)

#define UPDATE_maxLen                                        \
    {                                                        \
        const ptrdiff_t diff = (ptrdiff_t)0 - (ptrdiff_t)d2; \
        const Byte *c = cur + maxLen;                        \
        const Byte *lim = cur + lenLimit;                    \
        for (; c != lim; c++)                                \
            if (*(c + diff) != *c)                           \
                break;                                       \
        maxLen = (unsigned)(c - cur);                        \
    }

static UInt32 *Bt2_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
{
    GET_MATCHES_HEADER(2)
    HASH2_CALC;
    curMatch = p->hash[hv];
    p->hash[hv] = p->pos;
    GET_MATCHES_FOOTER_BT(1)
}

UInt32 *Bt3Zip_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
{
    GET_MATCHES_HEADER(3)
    HASH_ZIP_CALC;
    curMatch = p->hash[hv];
    p->hash[hv] = p->pos;
    GET_MATCHES_FOOTER_BT(2)
}

#define SET_mmm                \
    mmm = p->cyclicBufferSize; \
    if (pos < mmm)             \
        mmm = pos;

static UInt32 *Bt3_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
{
    UInt32 mmm;
    UInt32 h2, d2, pos;
    unsigned maxLen;
    UInt32 *hash;
    GET_MATCHES_HEADER(3)

    HASH3_CALC;

    hash = p->hash;
    pos = p->pos;

    d2 = pos - hash[h2];

    curMatch = (hash + kFix3HashSize)[hv];

    hash[h2] = pos;
    (hash + kFix3HashSize)[hv] = pos;

    SET_mmm

        maxLen = 2;

    if (d2 < mmm && *(cur - d2) == *cur)
    {
        UPDATE_maxLen
            distances[0] = (UInt32)maxLen;
        distances[1] = d2 - 1;
        distances += 2;
        if (maxLen == lenLimit)
        {
            SkipMatchesSpec(MF_PARAMS(p));
            MOVE_POS_RET;
        }
    }

    GET_MATCHES_FOOTER_BT(maxLen)
}

static UInt32 *Bt4_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
{
    UInt32 mmm;
    UInt32 h2, h3, d2, d3, pos;
    unsigned maxLen;
    UInt32 *hash;
    GET_MATCHES_HEADER(4)

    HASH4_CALC;

    hash = p->hash;
    pos = p->pos;

    d2 = pos - hash[h2];
    d3 = pos - (hash + kFix3HashSize)[h3];
    curMatch = (hash + kFix4HashSize)[hv];

    hash[h2] = pos;
    (hash + kFix3HashSize)[h3] = pos;
    (hash + kFix4HashSize)[hv] = pos;

    SET_mmm

        maxLen = 3;

    for (;;)
    {
        if (d2 < mmm && *(cur - d2) == *cur)
        {
            distances[0] = 2;
            distances[1] = d2 - 1;
            distances += 2;
            if (*(cur - d2 + 2) == cur[2])
            {
                // distances[-2] = 3;
            }
            else if (d3 < mmm && *(cur - d3) == *cur)
            {
                d2 = d3;
                distances[1] = d3 - 1;
                distances += 2;
            }
            else
                break;
        }
        else if (d3 < mmm && *(cur - d3) == *cur)
        {
            d2 = d3;
            distances[1] = d3 - 1;
            distances += 2;
        }
        else
            break;

        UPDATE_maxLen
            distances[-2] = (UInt32)maxLen;
        if (maxLen == lenLimit)
        {
            SkipMatchesSpec(MF_PARAMS(p));
            MOVE_POS_RET
        }
        break;
    }

    GET_MATCHES_FOOTER_BT(maxLen)
}

static UInt32 *Bt5_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
{
    UInt32 mmm;
    UInt32 h2, h3, d2, d3, maxLen, pos;
    UInt32 *hash;
    GET_MATCHES_HEADER(5)

    HASH5_CALC;

    hash = p->hash;
    pos = p->pos;

    d2 = pos - hash[h2];
    d3 = pos - (hash + kFix3HashSize)[h3];
    // d4 = pos - (hash + kFix4HashSize)[h4];

    curMatch = (hash + kFix5HashSize)[hv];

    hash[h2] = pos;
    (hash + kFix3HashSize)[h3] = pos;
    // (hash + kFix4HashSize)[h4] = pos;
    (hash + kFix5HashSize)[hv] = pos;

    SET_mmm

        maxLen = 4;

    for (;;)
    {
        if (d2 < mmm && *(cur - d2) == *cur)
        {
            distances[0] = 2;
            distances[1] = d2 - 1;
            distances += 2;
            if (*(cur - d2 + 2) == cur[2])
            {
            }
            else if (d3 < mmm && *(cur - d3) == *cur)
            {
                distances[1] = d3 - 1;
                distances += 2;
                d2 = d3;
            }
            else
                break;
        }
        else if (d3 < mmm && *(cur - d3) == *cur)
        {
            distances[1] = d3 - 1;
            distances += 2;
            d2 = d3;
        }
        else
            break;

        distances[-2] = 3;
        if (*(cur - d2 + 3) != cur[3])
            break;
        UPDATE_maxLen
            distances[-2] = (UInt32)maxLen;
        if (maxLen == lenLimit)
        {
            SkipMatchesSpec(MF_PARAMS(p));
            MOVE_POS_RET;
        }
        break;
    }

    GET_MATCHES_FOOTER_BT(maxLen)
}

static UInt32 *Hc4_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
{
    UInt32 mmm;
    UInt32 h2, h3, d2, d3, pos;
    unsigned maxLen;
    UInt32 *hash;
    GET_MATCHES_HEADER(4)

    HASH4_CALC;

    hash = p->hash;
    pos = p->pos;

    d2 = pos - hash[h2];
    d3 = pos - (hash + kFix3HashSize)[h3];
    curMatch = (hash + kFix4HashSize)[hv];

    hash[h2] = pos;
    (hash + kFix3HashSize)[h3] = pos;
    (hash + kFix4HashSize)[hv] = pos;

    SET_mmm

        maxLen = 3;

    for (;;)
    {
        if (d2 < mmm && *(cur - d2) == *cur)
        {
            distances[0] = 2;
            distances[1] = d2 - 1;
            distances += 2;
            if (*(cur - d2 + 2) == cur[2])
            {
                // distances[-2] = 3;
            }
            else if (d3 < mmm && *(cur - d3) == *cur)
            {
                d2 = d3;
                distances[1] = d3 - 1;
                distances += 2;
            }
            else
                break;
        }
        else if (d3 < mmm && *(cur - d3) == *cur)
        {
            d2 = d3;
            distances[1] = d3 - 1;
            distances += 2;
        }
        else
            break;

        UPDATE_maxLen
            distances[-2] = (UInt32)maxLen;
        if (maxLen == lenLimit)
        {
            p->son[p->cyclicBufferPos] = curMatch;
            MOVE_POS_RET;
        }
        break;
    }

    GET_MATCHES_FOOTER_HC(maxLen);
}

static UInt32 *Hc5_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
{
    UInt32 mmm;
    UInt32 h2, h3, d2, d3, maxLen, pos;
    UInt32 *hash;
    GET_MATCHES_HEADER(5)

    HASH5_CALC;

    hash = p->hash;
    pos = p->pos;

    d2 = pos - hash[h2];
    d3 = pos - (hash + kFix3HashSize)[h3];
    // d4 = pos - (hash + kFix4HashSize)[h4];

    curMatch = (hash + kFix5HashSize)[hv];

    hash[h2] = pos;
    (hash + kFix3HashSize)[h3] = pos;
    // (hash + kFix4HashSize)[h4] = pos;
    (hash + kFix5HashSize)[hv] = pos;

    SET_mmm

        maxLen = 4;

    for (;;)
    {
        if (d2 < mmm && *(cur - d2) == *cur)
        {
            distances[0] = 2;
            distances[1] = d2 - 1;
            distances += 2;
            if (*(cur - d2 + 2) == cur[2])
            {
            }
            else if (d3 < mmm && *(cur - d3) == *cur)
            {
                distances[1] = d3 - 1;
                distances += 2;
                d2 = d3;
            }
            else
                break;
        }
        else if (d3 < mmm && *(cur - d3) == *cur)
        {
            distances[1] = d3 - 1;
            distances += 2;
            d2 = d3;
        }
        else
            break;

        distances[-2] = 3;
        if (*(cur - d2 + 3) != cur[3])
            break;
        UPDATE_maxLen
            distances[-2] = maxLen;
        if (maxLen == lenLimit)
        {
            p->son[p->cyclicBufferPos] = curMatch;
            MOVE_POS_RET;
        }
        break;
    }

    GET_MATCHES_FOOTER_HC(maxLen);
}

UInt32 *Hc3Zip_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
{
    GET_MATCHES_HEADER(3)
    HASH_ZIP_CALC;
    curMatch = p->hash[hv];
    p->hash[hv] = p->pos;
    GET_MATCHES_FOOTER_HC(2)
}

static void Bt2_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
{
    SKIP_HEADER(2)
    {
        HASH2_CALC;
        curMatch = p->hash[hv];
        p->hash[hv] = p->pos;
    }
    SKIP_FOOTER
}

void Bt3Zip_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
{
    SKIP_HEADER(3)
    {
        HASH_ZIP_CALC;
        curMatch = p->hash[hv];
        p->hash[hv] = p->pos;
    }
    SKIP_FOOTER
}

static void Bt3_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
{
    SKIP_HEADER(3)
    {
        UInt32 h2;
        UInt32 *hash;
        HASH3_CALC;
        hash = p->hash;
        curMatch = (hash + kFix3HashSize)[hv];
        hash[h2] =
            (hash + kFix3HashSize)[hv] = p->pos;
    }
    SKIP_FOOTER
}

static void Bt4_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
{
    SKIP_HEADER(4)
    {
        UInt32 h2, h3;
        UInt32 *hash;
        HASH4_CALC;
        hash = p->hash;
        curMatch = (hash + kFix4HashSize)[hv];
        hash[h2] =
            (hash + kFix3HashSize)[h3] =
                (hash + kFix4HashSize)[hv] = p->pos;
    }
    SKIP_FOOTER
}

static void Bt5_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
{
    SKIP_HEADER(5)
    {
        UInt32 h2, h3;
        UInt32 *hash;
        HASH5_CALC;
        hash = p->hash;
        curMatch = (hash + kFix5HashSize)[hv];
        hash[h2] =
            (hash + kFix3HashSize)[h3] =
                // (hash + kFix4HashSize)[h4] =
            (hash + kFix5HashSize)[hv] = p->pos;
    }
    SKIP_FOOTER
}

#define HC_SKIP_HEADER(minLen)                                    \
    do                                                            \
    {                                                             \
        if (p->lenLimit < minLen)                                 \
        {                                                         \
            MatchFinder_MovePos(p);                               \
            num--;                                                \
            continue;                                             \
        }                                                         \
        {                                                         \
            Byte *cur;                                            \
            UInt32 *hash;                                         \
            UInt32 *son;                                          \
            UInt32 pos = p->pos;                                  \
            UInt32 num2 = num;                                    \
            /* (p->pos == p->posLimit) is not allowed here !!! */ \
            {                                                     \
                const UInt32 rem = p->posLimit - pos;             \
                if (num2 > rem)                                   \
                    num2 = rem;                                   \
            }                                                     \
            num -= num2;                                          \
            {                                                     \
                const UInt32 cycPos = p->cyclicBufferPos;         \
                son = p->son + cycPos;                            \
                p->cyclicBufferPos = cycPos + num2;               \
            }                                                     \
            cur = p->buffer;                                      \
            hash = p->hash;                                       \
            do                                                    \
            {                                                     \
                UInt32 curMatch;                                  \
                UInt32 hv;

#define HC_SKIP_FOOTER              \
    cur++;                          \
    pos++;                          \
    *son++ = curMatch;              \
    }                               \
    while (--num2)                  \
        ;                           \
    p->buffer = cur;                \
    p->pos = pos;                   \
    if (pos == p->posLimit)         \
        MatchFinder_CheckLimits(p); \
    }                               \
    }                               \
    while (num)                     \
        ;

static void Hc4_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
{
    HC_SKIP_HEADER(4)

    UInt32 h2, h3;
    HASH4_CALC;
    curMatch = (hash + kFix4HashSize)[hv];
    hash[h2] =
        (hash + kFix3HashSize)[h3] =
            (hash + kFix4HashSize)[hv] = pos;

    HC_SKIP_FOOTER
}

static void Hc5_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
{
    HC_SKIP_HEADER(5)

    UInt32 h2, h3;
    HASH5_CALC
    curMatch = (hash + kFix5HashSize)[hv];
    hash[h2] =
        (hash + kFix3HashSize)[h3] =
            // (hash + kFix4HashSize)[h4] =
        (hash + kFix5HashSize)[hv] = pos;

    HC_SKIP_FOOTER
}

void Hc3Zip_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
{
    HC_SKIP_HEADER(3)

    HASH_ZIP_CALC;
    curMatch = hash[hv];
    hash[hv] = pos;

    HC_SKIP_FOOTER
}

void MatchFinder_CreateVTable(CMatchFinder *p, IMatchFinder2 *vTable)
{
    vTable->Init = (Mf_Init_Func)MatchFinder_Init;
    vTable->GetNumAvailableBytes = (Mf_GetNumAvailableBytes_Func)MatchFinder_GetNumAvailableBytes;
    vTable->GetPointerToCurrentPos = (Mf_GetPointerToCurrentPos_Func)MatchFinder_GetPointerToCurrentPos;
    if (!p->btMode)
    {
        if (p->numHashBytes <= 4)
        {
            vTable->GetMatches = (Mf_GetMatches_Func)Hc4_MatchFinder_GetMatches;
            vTable->Skip = (Mf_Skip_Func)Hc4_MatchFinder_Skip;
        }
        else
        {
            vTable->GetMatches = (Mf_GetMatches_Func)Hc5_MatchFinder_GetMatches;
            vTable->Skip = (Mf_Skip_Func)Hc5_MatchFinder_Skip;
        }
    }
    else if (p->numHashBytes == 2)
    {
        vTable->GetMatches = (Mf_GetMatches_Func)Bt2_MatchFinder_GetMatches;
        vTable->Skip = (Mf_Skip_Func)Bt2_MatchFinder_Skip;
    }
    else if (p->numHashBytes == 3)
    {
        vTable->GetMatches = (Mf_GetMatches_Func)Bt3_MatchFinder_GetMatches;
        vTable->Skip = (Mf_Skip_Func)Bt3_MatchFinder_Skip;
    }
    else if (p->numHashBytes == 4)
    {
        vTable->GetMatches = (Mf_GetMatches_Func)Bt4_MatchFinder_GetMatches;
        vTable->Skip = (Mf_Skip_Func)Bt4_MatchFinder_Skip;
    }
    else
    {
        vTable->GetMatches = (Mf_GetMatches_Func)Bt5_MatchFinder_GetMatches;
        vTable->Skip = (Mf_Skip_Func)Bt5_MatchFinder_Skip;
    }
}

void LzFindPrepare()
{
#ifndef FORCE_SATUR_SUB_128
#ifdef USE_SATUR_SUB_128
    LZFIND_SATUR_SUB_CODE_FUNC f = NULL;
#ifdef MY_CPU_ARM_OR_ARM64
    {
        if (CPU_IsSupported_NEON())
        {
            // #pragma message ("=== LzFind NEON")
            _PRF(printf("\n=== LzFind NEON\n"));
            f = LzFind_SaturSub_128;
        }
        // f = 0; // for debug
    }
#else // MY_CPU_ARM_OR_ARM64
    if (CPU_IsSupported_SSE41())
    {
        // #pragma message ("=== LzFind SSE41")
        _PRF(printf("\n=== LzFind SSE41\n"));
        f = LzFind_SaturSub_128;

#ifdef USE_AVX2
        if (CPU_IsSupported_AVX2())
        {
            // #pragma message ("=== LzFind AVX2")
            _PRF(printf("\n=== LzFind AVX2\n"));
            f = LzFind_SaturSub_256;
        }
#endif
    }
#endif // MY_CPU_ARM_OR_ARM64
    g_LzFind_SaturSub = f;
#endif // USE_SATUR_SUB_128
#endif // FORCE_SATUR_SUB_128
}

#ifndef _7ZIP_ST
/* LzFindMt.h -- multithreaded Match finder for LZ algorithms
2021-07-12 : Igor Pavlov : Public domain */

#ifndef __LZ_FIND_MT_H
#define __LZ_FIND_MT_H

/* Threads.h -- multithreading library
2021-12-21 : Igor Pavlov : Public domain */

#ifndef __7Z_THREADS_H
#define __7Z_THREADS_H

#ifdef _WIN32
#include <Windows.h>
#else

#if defined(__linux__)
#if !defined(__APPLE__) && !defined(_AIX) && !defined(__ANDROID__)
#ifndef _7ZIP_AFFINITY_DISABLE
#define _7ZIP_AFFINITY_SUPPORTED
// #pragma message(" ==== _7ZIP_AFFINITY_SUPPORTED")
// #define _GNU_SOURCE
#endif
#endif
#endif

#include <pthread.h>

#endif

EXTERN_C_BEGIN

#ifdef _WIN32

WRes HandlePtr_Close(HANDLE *h);
WRes Handle_WaitObject(HANDLE h);

typedef HANDLE CThread;

#define Thread_Construct(p) \
    {                       \
        *(p) = NULL;        \
    }
#define Thread_WasCreated(p) (*(p) != NULL)
#define Thread_Close(p) HandlePtr_Close(p)
// #define Thread_Wait(p) Handle_WaitObject(*(p))

#ifdef UNDER_CE
// if (USE_THREADS_CreateThread is      defined), we use _beginthreadex()
// if (USE_THREADS_CreateThread is not definned), we use CreateThread()
#define USE_THREADS_CreateThread
#endif

typedef
#ifdef USE_THREADS_CreateThread
    DWORD
#else
    unsigned
#endif
        THREAD_FUNC_RET_TYPE;

typedef DWORD_PTR CAffinityMask;
typedef DWORD_PTR CCpuSet;

#define CpuSet_Zero(p) \
    {                  \
        *(p) = 0;      \
    }
#define CpuSet_Set(p, cpu)               \
    {                                    \
        *(p) |= ((DWORD_PTR)1 << (cpu)); \
    }

#else //  _WIN32

typedef struct _CThread
{
    pthread_t _tid;
    int _created;
} CThread;

#define Thread_Construct(p) \
    {                       \
        (p)->_tid = 0;      \
        (p)->_created = 0;  \
    }
#define Thread_WasCreated(p) ((p)->_created != 0)
WRes Thread_Close(CThread *p);
// #define Thread_Wait Thread_Wait_Close

typedef void *THREAD_FUNC_RET_TYPE;

typedef UInt64 CAffinityMask;

#ifdef _7ZIP_AFFINITY_SUPPORTED

typedef cpu_set_t CCpuSet;
#define CpuSet_Zero(p) CPU_ZERO(p)
#define CpuSet_Set(p, cpu) CPU_SET(cpu, p)
#define CpuSet_IsSet(p, cpu) CPU_ISSET(cpu, p)

#else

typedef UInt64 CCpuSet;
#define CpuSet_Zero(p) \
    {                  \
        *(p) = 0;      \
    }
#define CpuSet_Set(p, cpu)            \
    {                                 \
        *(p) |= ((UInt64)1 << (cpu)); \
    }
#define CpuSet_IsSet(p, cpu) ((*(p) & ((UInt64)1 << (cpu))) != 0)

#endif

#endif //  _WIN32

#define THREAD_FUNC_CALL_TYPE MY_STD_CALL

#if defined(_WIN32) && defined(__GNUC__)
/* GCC compiler for x86 32-bit uses the rule:
   the stack is 16-byte aligned before CALL instruction for function calling.
   But only root function main() contains instructions that
   set 16-byte alignment for stack pointer. And another functions
   just keep alignment, if it was set in some parent function.

   The problem:
    if we create new thread in MinGW (GCC) 32-bit x86 via _beginthreadex() or CreateThread(),
       the root function of thread doesn't set 16-byte alignment.
       And stack frames in all child functions also will be unaligned in that case.

   Here we set (force_align_arg_pointer) attribute for root function of new thread.
   Do we need (force_align_arg_pointer) also for another systems?  */

#define THREAD_FUNC_ATTRIB_ALIGN_ARG __attribute__((force_align_arg_pointer))
// #define THREAD_FUNC_ATTRIB_ALIGN_ARG // for debug : bad alignment in SSE functions
#else
#define THREAD_FUNC_ATTRIB_ALIGN_ARG
#endif

#define THREAD_FUNC_DECL THREAD_FUNC_ATTRIB_ALIGN_ARG THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE

typedef THREAD_FUNC_RET_TYPE(THREAD_FUNC_CALL_TYPE *THREAD_FUNC_TYPE)(void *);
WRes Thread_Create(CThread *p, THREAD_FUNC_TYPE func, LPVOID param);
WRes Thread_Create_With_Affinity(CThread *p, THREAD_FUNC_TYPE func, LPVOID param, CAffinityMask affinity);
WRes Thread_Wait_Close(CThread *p);

#ifdef _WIN32
#define Thread_Create_With_CpuSet(p, func, param, cs) \
    Thread_Create_With_Affinity(p, func, param, *cs)
#else
WRes Thread_Create_With_CpuSet(CThread *p, THREAD_FUNC_TYPE func, LPVOID param, const CCpuSet *cpuSet);
#endif

#ifdef _WIN32

typedef HANDLE CEvent;
typedef CEvent CAutoResetEvent;
typedef CEvent CManualResetEvent;
#define Event_Construct(p) *(p) = NULL
#define Event_IsCreated(p) (*(p) != NULL)
#define Event_Close(p) HandlePtr_Close(p)
#define Event_Wait(p) Handle_WaitObject(*(p))
WRes Event_Set(CEvent *p);
WRes Event_Reset(CEvent *p);
WRes ManualResetEvent_Create(CManualResetEvent *p, int signaled);
WRes ManualResetEvent_CreateNotSignaled(CManualResetEvent *p);
WRes AutoResetEvent_Create(CAutoResetEvent *p, int signaled);
WRes AutoResetEvent_CreateNotSignaled(CAutoResetEvent *p);

typedef HANDLE CSemaphore;
#define Semaphore_Construct(p) *(p) = NULL
#define Semaphore_IsCreated(p) (*(p) != NULL)
#define Semaphore_Close(p) HandlePtr_Close(p)
#define Semaphore_Wait(p) Handle_WaitObject(*(p))
WRes Semaphore_Create(CSemaphore *p, UInt32 initCount, UInt32 maxCount);
WRes Semaphore_OptCreateInit(CSemaphore *p, UInt32 initCount, UInt32 maxCount);
WRes Semaphore_ReleaseN(CSemaphore *p, UInt32 num);
WRes Semaphore_Release1(CSemaphore *p);

typedef CRITICAL_SECTION CCriticalSection;
WRes CriticalSection_Init(CCriticalSection *p);
#define CriticalSection_Delete(p) DeleteCriticalSection(p)
#define CriticalSection_Enter(p) EnterCriticalSection(p)
#define CriticalSection_Leave(p) LeaveCriticalSection(p)

#else // _WIN32

typedef struct _CEvent
{
    int _created;
    int _manual_reset;
    int _state;
    pthread_mutex_t _mutex;
    pthread_cond_t _cond;
} CEvent;

typedef CEvent CAutoResetEvent;
typedef CEvent CManualResetEvent;

#define Event_Construct(p) (p)->_created = 0
#define Event_IsCreated(p) ((p)->_created)

WRes ManualResetEvent_Create(CManualResetEvent *p, int signaled);
WRes ManualResetEvent_CreateNotSignaled(CManualResetEvent *p);
WRes AutoResetEvent_Create(CAutoResetEvent *p, int signaled);
WRes AutoResetEvent_CreateNotSignaled(CAutoResetEvent *p);
WRes Event_Set(CEvent *p);
WRes Event_Reset(CEvent *p);
WRes Event_Wait(CEvent *p);
WRes Event_Close(CEvent *p);

typedef struct _CSemaphore
{
    int _created;
    UInt32 _count;
    UInt32 _maxCount;
    pthread_mutex_t _mutex;
    pthread_cond_t _cond;
} CSemaphore;

#define Semaphore_Construct(p) (p)->_created = 0
#define Semaphore_IsCreated(p) ((p)->_created)

WRes Semaphore_Create(CSemaphore *p, UInt32 initCount, UInt32 maxCount);
WRes Semaphore_OptCreateInit(CSemaphore *p, UInt32 initCount, UInt32 maxCount);
WRes Semaphore_ReleaseN(CSemaphore *p, UInt32 num);
#define Semaphore_Release1(p) Semaphore_ReleaseN(p, 1)
WRes Semaphore_Wait(CSemaphore *p);
WRes Semaphore_Close(CSemaphore *p);

typedef struct _CCriticalSection
{
    pthread_mutex_t _mutex;
} CCriticalSection;

WRes CriticalSection_Init(CCriticalSection *p);
void CriticalSection_Delete(CCriticalSection *cs);
void CriticalSection_Enter(CCriticalSection *cs);
void CriticalSection_Leave(CCriticalSection *cs);

LONG InterlockedIncrement(LONG volatile *addend);

#endif // _WIN32

EXTERN_C_END

#endif

/* Threads.c -- multithreading library
2021-12-21 : Igor Pavlov : Public domain */

#ifdef _WIN32

#ifndef USE_THREADS_CreateThread
#include <process.h>
#endif

static WRes GetError()
{
    DWORD res = GetLastError();
    return res ? (WRes)res : 1;
}

static WRes HandleToWRes(HANDLE h)
{
    return (h != NULL) ? 0 : GetError();
}
static WRes BOOLToWRes(BOOL v)
{
    return v ? 0 : GetError();
}

WRes HandlePtr_Close(HANDLE *p)
{
    if (*p != NULL)
    {
        if (!CloseHandle(*p))
            return GetError();
        *p = NULL;
    }
    return 0;
}

WRes Handle_WaitObject(HANDLE h)
{
    DWORD dw = WaitForSingleObject(h, INFINITE);
    /*
      (dw) result:
      WAIT_OBJECT_0  // 0
      WAIT_ABANDONED // 0x00000080 : is not compatible with Win32 Error space
      WAIT_TIMEOUT   // 0x00000102 : is     compatible with Win32 Error space
      WAIT_FAILED    // 0xFFFFFFFF
    */
    if (dw == WAIT_FAILED)
    {
        dw = GetLastError();
        if (dw == 0)
            return WAIT_FAILED;
    }
    return (WRes)dw;
}

#define Thread_Wait(p) Handle_WaitObject(*(p))

WRes Thread_Wait_Close(CThread *p)
{
    WRes res = Thread_Wait(p);
    WRes res2 = Thread_Close(p);
    return (res != 0 ? res : res2);
}

WRes Thread_Create(CThread *p, THREAD_FUNC_TYPE func, LPVOID param)
{
    /* Windows Me/98/95: threadId parameter may not be NULL in _beginthreadex/CreateThread functions */

#ifdef USE_THREADS_CreateThread

    DWORD threadId;
    *p = CreateThread(NULL, 0, func, param, 0, &threadId);

#else

    unsigned threadId;
    *p = (HANDLE)(_beginthreadex(NULL, 0, func, param, 0, &threadId));

#endif

    /* maybe we must use errno here, but probably GetLastError() is also OK. */
    return HandleToWRes(*p);
}

WRes Thread_Create_With_Affinity(CThread *p, THREAD_FUNC_TYPE func, LPVOID param, CAffinityMask affinity)
{
#ifdef USE_THREADS_CreateThread

    UNUSED_VAR(affinity)
    return Thread_Create(p, func, param);

#else

    /* Windows Me/98/95: threadId parameter may not be NULL in _beginthreadex/CreateThread functions */
    HANDLE h;
    WRes wres;
    unsigned threadId;
    h = (HANDLE)(_beginthreadex(NULL, 0, func, param, CREATE_SUSPENDED, &threadId));
    *p = h;
    wres = HandleToWRes(h);
    if (h)
    {
        {
            // DWORD_PTR prevMask =
            SetThreadAffinityMask(h, (DWORD_PTR)affinity);
            /*
            if (prevMask == 0)
            {
              // affinity change is non-critical error, so we can ignore it
              // wres = GetError();
            }
            */
        }
        {
            DWORD prevSuspendCount = ResumeThread(h);
            /* ResumeThread() returns:
               0 : was_not_suspended
               1 : was_resumed
              -1 : error
            */
            if (prevSuspendCount == (DWORD)-1)
                wres = GetError();
        }
    }

    /* maybe we must use errno here, but probably GetLastError() is also OK. */
    return wres;

#endif
}

static WRes Event_Create(CEvent *p, BOOL manualReset, int signaled)
{
    *p = CreateEvent(NULL, manualReset, (signaled ? TRUE : FALSE), NULL);
    return HandleToWRes(*p);
}

WRes Event_Set(CEvent *p)
{
    return BOOLToWRes(SetEvent(*p));
}
WRes Event_Reset(CEvent *p)
{
    return BOOLToWRes(ResetEvent(*p));
}

WRes ManualResetEvent_Create(CManualResetEvent *p, int signaled)
{
    return Event_Create(p, TRUE, signaled);
}
WRes AutoResetEvent_Create(CAutoResetEvent *p, int signaled)
{
    return Event_Create(p, FALSE, signaled);
}
WRes ManualResetEvent_CreateNotSignaled(CManualResetEvent *p)
{
    return ManualResetEvent_Create(p, 0);
}
WRes AutoResetEvent_CreateNotSignaled(CAutoResetEvent *p)
{
    return AutoResetEvent_Create(p, 0);
}

WRes Semaphore_Create(CSemaphore *p, UInt32 initCount, UInt32 maxCount)
{
    // negative ((LONG)maxCount) is not supported in WIN32::CreateSemaphore()
    *p = CreateSemaphore(NULL, (LONG)initCount, (LONG)maxCount, NULL);
    return HandleToWRes(*p);
}

WRes Semaphore_OptCreateInit(CSemaphore *p, UInt32 initCount, UInt32 maxCount)
{
    // if (Semaphore_IsCreated(p))
    {
        WRes wres = Semaphore_Close(p);
        if (wres != 0)
            return wres;
    }
    return Semaphore_Create(p, initCount, maxCount);
}

static WRes Semaphore_Release(CSemaphore *p, LONG releaseCount, LONG *previousCount)
{
    return BOOLToWRes(ReleaseSemaphore(*p, releaseCount, previousCount));
}
WRes Semaphore_ReleaseN(CSemaphore *p, UInt32 num)
{
    return Semaphore_Release(p, (LONG)num, NULL);
}
WRes Semaphore_Release1(CSemaphore *p)
{
    return Semaphore_ReleaseN(p, 1);
}

WRes CriticalSection_Init(CCriticalSection *p)
{
    /* InitializeCriticalSection() can raise exception:
       Windows XP, 2003 : can raise a STATUS_NO_MEMORY exception
       Windows Vista+   : no exceptions */
#ifdef _MSC_VER
    __try
#endif
    {
        InitializeCriticalSection(p);
        /* InitializeCriticalSectionAndSpinCount(p, 0); */
    }
#ifdef _MSC_VER
    __except (EXCEPTION_EXECUTE_HANDLER)
    {
        return ERROR_NOT_ENOUGH_MEMORY;
    }
#endif
    return 0;
}

#else // _WIN32

// ---------- POSIX ----------

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#ifdef _7ZIP_AFFINITY_SUPPORTED
// #include <sched.h>
#endif

#define Print(s) PRF(printf("\n%s\n", s))

// #include <stdio.h>

WRes Thread_Create_With_CpuSet(CThread *p, THREAD_FUNC_TYPE func, LPVOID param, const CCpuSet *cpuSet)
{
    // new thread in Posix probably inherits affinity from parrent thread
    Print("Thread_Create_With_CpuSet");

    pthread_attr_t attr;
    int ret;
    // int ret2;

    p->_created = 0;

    RINOK(pthread_attr_init(&attr));

    ret = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

    if (!ret)
    {
        if (cpuSet)
        {
#ifdef _7ZIP_AFFINITY_SUPPORTED

            /*
            printf("\n affinity :");
            unsigned i;
            for (i = 0; i < sizeof(*cpuSet) && i < 8; i++)
            {
              Byte b = *((const Byte *)cpuSet + i);
              char temp[32];
  #define GET_HEX_CHAR(t) ((char)(((t < 10) ? ('0' + t) : ('A' + (t - 10)))))
              temp[0] = GET_HEX_CHAR((b & 0xF));
              temp[1] = GET_HEX_CHAR((b >> 4));
              // temp[0] = GET_HEX_CHAR((b >> 4));  // big-endian
              // temp[1] = GET_HEX_CHAR((b & 0xF));  // big-endian
              temp[2] = 0;
              printf("%s", temp);
            }
            printf("\n");
            */

            // ret2 =
            pthread_attr_setaffinity_np(&attr, sizeof(*cpuSet), cpuSet);
// if (ret2) ret = ret2;
#endif
        }

        ret = pthread_create(&p->_tid, &attr, func, param);

        if (!ret)
        {
            p->_created = 1;
            /*
            if (cpuSet)
            {
              // ret2 =
              pthread_setaffinity_np(p->_tid, sizeof(*cpuSet), cpuSet);
              // if (ret2) ret = ret2;
            }
            */
        }
    }
    // ret2 =
    pthread_attr_destroy(&attr);
    // if (ret2 != 0) ret = ret2;
    return ret;
}

WRes Thread_Create(CThread *p, THREAD_FUNC_TYPE func, LPVOID param)
{
    return Thread_Create_With_CpuSet(p, func, param, NULL);
}

WRes Thread_Create_With_Affinity(CThread *p, THREAD_FUNC_TYPE func, LPVOID param, CAffinityMask affinity)
{
    Print("Thread_Create_WithAffinity");
    CCpuSet cs;
    unsigned i;
    CpuSet_Zero(&cs);
    for (i = 0; i < sizeof(affinity) * 8; i++)
    {
        if (affinity == 0)
            break;
        if (affinity & 1)
        {
            CpuSet_Set(&cs, i);
        }
        affinity >>= 1;
    }
    return Thread_Create_With_CpuSet(p, func, param, &cs);
}

WRes Thread_Close(CThread *p)
{
    // Print("Thread_Close");
    int ret;
    if (!p->_created)
        return 0;

    ret = pthread_detach(p->_tid);
    p->_tid = 0;
    p->_created = 0;
    return ret;
}

WRes Thread_Wait_Close(CThread *p)
{
    // Print("Thread_Wait_Close");
    void *thread_return;
    int ret;
    if (!p->_created)
        return EINVAL;

    ret = pthread_join(p->_tid, &thread_return);
    // probably we can't use that (_tid) after pthread_join(), so we close thread here
    p->_created = 0;
    p->_tid = 0;
    return ret;
}

static WRes Event_Create(CEvent *p, int manualReset, int signaled)
{
    RINOK(pthread_mutex_init(&p->_mutex, NULL));
    RINOK(pthread_cond_init(&p->_cond, NULL));
    p->_manual_reset = manualReset;
    p->_state = (signaled ? True : False);
    p->_created = 1;
    return 0;
}

WRes ManualResetEvent_Create(CManualResetEvent *p, int signaled)
{
    return Event_Create(p, True, signaled);
}
WRes ManualResetEvent_CreateNotSignaled(CManualResetEvent *p)
{
    return ManualResetEvent_Create(p, 0);
}
WRes AutoResetEvent_Create(CAutoResetEvent *p, int signaled)
{
    return Event_Create(p, False, signaled);
}
WRes AutoResetEvent_CreateNotSignaled(CAutoResetEvent *p)
{
    return AutoResetEvent_Create(p, 0);
}

WRes Event_Set(CEvent *p)
{
    RINOK(pthread_mutex_lock(&p->_mutex));
    p->_state = True;
    int res1 = pthread_cond_broadcast(&p->_cond);
    int res2 = pthread_mutex_unlock(&p->_mutex);
    return (res2 ? res2 : res1);
}

WRes Event_Reset(CEvent *p)
{
    RINOK(pthread_mutex_lock(&p->_mutex));
    p->_state = False;
    return pthread_mutex_unlock(&p->_mutex);
}

WRes Event_Wait(CEvent *p)
{
    RINOK(pthread_mutex_lock(&p->_mutex));
    while (p->_state == False)
    {
        // ETIMEDOUT
        // ret =
        pthread_cond_wait(&p->_cond, &p->_mutex);
        // if (ret != 0) break;
    }
    if (p->_manual_reset == False)
    {
        p->_state = False;
    }
    return pthread_mutex_unlock(&p->_mutex);
}

WRes Event_Close(CEvent *p)
{
    if (!p->_created)
        return 0;
    p->_created = 0;
    {
        int res1 = pthread_mutex_destroy(&p->_mutex);
        int res2 = pthread_cond_destroy(&p->_cond);
        return (res1 ? res1 : res2);
    }
}

WRes Semaphore_Create(CSemaphore *p, UInt32 initCount, UInt32 maxCount)
{
    if (initCount > maxCount || maxCount < 1)
        return EINVAL;
    RINOK(pthread_mutex_init(&p->_mutex, NULL));
    RINOK(pthread_cond_init(&p->_cond, NULL));
    p->_count = initCount;
    p->_maxCount = maxCount;
    p->_created = 1;
    return 0;
}

WRes Semaphore_OptCreateInit(CSemaphore *p, UInt32 initCount, UInt32 maxCount)
{
    if (Semaphore_IsCreated(p))
    {
        /*
        WRes wres = Semaphore_Close(p);
        if (wres != 0)
          return wres;
        */
        if (initCount > maxCount || maxCount < 1)
            return EINVAL;
        // return EINVAL; // for debug
        p->_count = initCount;
        p->_maxCount = maxCount;
        return 0;
    }
    return Semaphore_Create(p, initCount, maxCount);
}

WRes Semaphore_ReleaseN(CSemaphore *p, UInt32 releaseCount)
{
    UInt32 newCount;
    int ret;

    if (releaseCount < 1)
        return EINVAL;

    RINOK(pthread_mutex_lock(&p->_mutex));

    newCount = p->_count + releaseCount;
    if (newCount > p->_maxCount)
        ret = ERROR_TOO_MANY_POSTS; // EINVAL;
    else
    {
        p->_count = newCount;
        ret = pthread_cond_broadcast(&p->_cond);
    }
    RINOK(pthread_mutex_unlock(&p->_mutex));
    return ret;
}

WRes Semaphore_Wait(CSemaphore *p)
{
    RINOK(pthread_mutex_lock(&p->_mutex));
    while (p->_count < 1)
    {
        pthread_cond_wait(&p->_cond, &p->_mutex);
    }
    p->_count--;
    return pthread_mutex_unlock(&p->_mutex);
}

WRes Semaphore_Close(CSemaphore *p)
{
    if (!p->_created)
        return 0;
    p->_created = 0;
    {
        int res1 = pthread_mutex_destroy(&p->_mutex);
        int res2 = pthread_cond_destroy(&p->_cond);
        return (res1 ? res1 : res2);
    }
}

WRes CriticalSection_Init(CCriticalSection *p)
{
    // Print("CriticalSection_Init");
    if (!p)
        return EINTR;
    return pthread_mutex_init(&p->_mutex, NULL);
}

void CriticalSection_Enter(CCriticalSection *p)
{
    // Print("CriticalSection_Enter");
    if (p)
    {
        // int ret =
        pthread_mutex_lock(&p->_mutex);
    }
}

void CriticalSection_Leave(CCriticalSection *p)
{
    // Print("CriticalSection_Leave");
    if (p)
    {
        // int ret =
        pthread_mutex_unlock(&p->_mutex);
    }
}

void CriticalSection_Delete(CCriticalSection *p)
{
    // Print("CriticalSection_Delete");
    if (p)
    {
        // int ret =
        pthread_mutex_destroy(&p->_mutex);
    }
}

LONG InterlockedIncrement(LONG volatile *addend)
{
// Print("InterlockedIncrement");
#ifdef USE_HACK_UNSAFE_ATOMIC
    LONG val = *addend + 1;
    *addend = val;
    return val;
#else
    return __sync_add_and_fetch(addend, 1);
#endif
}

#endif // _WIN32

EXTERN_C_BEGIN

typedef struct _CMtSync
{
    UInt32 numProcessedBlocks;
    CThread thread;
    UInt64 affinity;

    BoolInt wasCreated;
    BoolInt needStart;
    BoolInt csWasInitialized;
    BoolInt csWasEntered;

    BoolInt exit;
    BoolInt stopWriting;

    CAutoResetEvent canStart;
    CAutoResetEvent wasStopped;
    CSemaphore freeSemaphore;
    CSemaphore filledSemaphore;
    CCriticalSection cs;
    // UInt32 numBlocks_Sent;
} CMtSync;

typedef UInt32 *(*Mf_Mix_Matches)(void *p, UInt32 matchMinPos, UInt32 *distances);

/* kMtCacheLineDummy must be >= size_of_CPU_cache_line */
#define kMtCacheLineDummy 128

typedef void (*Mf_GetHeads)(const Byte *buffer, UInt32 pos,
                            UInt32 *hash, UInt32 hashMask, UInt32 *heads, UInt32 numHeads, const UInt32 *crc);

typedef struct _CMatchFinderMt
{
    /* LZ */
    const Byte *pointerToCurPos;
    UInt32 *btBuf;
    const UInt32 *btBufPos;
    const UInt32 *btBufPosLimit;
    UInt32 lzPos;
    UInt32 btNumAvailBytes;

    UInt32 *hash;
    UInt32 fixedHashSize;
    // UInt32 hash4Mask;
    UInt32 historySize;
    const UInt32 *crc;

    Mf_Mix_Matches MixMatchesFunc;
    UInt32 failure_LZ_BT; // failure in BT transfered to LZ
    // UInt32 failure_LZ_LZ; // failure in LZ tables
    UInt32 failureBuf[1];
    // UInt32 crc[256];

    /* LZ + BT */
    CMtSync btSync;
    Byte btDummy[kMtCacheLineDummy];

    /* BT */
    UInt32 *hashBuf;
    UInt32 hashBufPos;
    UInt32 hashBufPosLimit;
    UInt32 hashNumAvail;
    UInt32 failure_BT;

    CLzRef *son;
    UInt32 matchMaxLen;
    UInt32 numHashBytes;
    UInt32 pos;
    const Byte *buffer;
    UInt32 cyclicBufferPos;
    UInt32 cyclicBufferSize; /* it must be = (historySize + 1) */
    UInt32 cutValue;

    /* BT + Hash */
    CMtSync hashSync;
    /* Byte hashDummy[kMtCacheLineDummy]; */

    /* Hash */
    Mf_GetHeads GetHeadsFunc;
    CMatchFinder *MatchFinder;
    // CMatchFinder MatchFinder;
} CMatchFinderMt;

// only for Mt part
void MatchFinderMt_Construct(CMatchFinderMt *p);
void MatchFinderMt_Destruct(CMatchFinderMt *p, ISzAllocPtr alloc);

SRes MatchFinderMt_Create(CMatchFinderMt *p, UInt32 historySize, UInt32 keepAddBufferBefore,
                          UInt32 matchMaxLen, UInt32 keepAddBufferAfter, ISzAllocPtr alloc);
void MatchFinderMt_CreateVTable(CMatchFinderMt *p, IMatchFinder2 *vTable);

/* call MatchFinderMt_InitMt() before IMatchFinder::Init() */
SRes MatchFinderMt_InitMt(CMatchFinderMt *p);
void MatchFinderMt_ReleaseStream(CMatchFinderMt *p);

EXTERN_C_END

#endif

/* LzFindMt.c -- multithreaded Match finder for LZ algorithms
2021-12-21 : Igor Pavlov : Public domain */

// #include <stdio.h>

// #define LOG_ITERS

// #define LOG_THREAD

#ifdef LOG_ITERS
#include <stdio.h>
extern UInt64 g_NumIters_Tree;
extern UInt64 g_NumIters_Loop;
extern UInt64 g_NumIters_Bytes;
#define LOG_ITER(x) x
#else
#define LOG_ITER(x)
#endif

#define kMtHashBlockSize ((UInt32)1 << 17)
#define kMtHashNumBlocks (1 << 1)

#define GET_HASH_BLOCK_OFFSET(i) (((i) & (kMtHashNumBlocks - 1)) * kMtHashBlockSize)

#define kMtBtBlockSize ((UInt32)1 << 16)
#define kMtBtNumBlocks (1 << 4)

#define GET_BT_BLOCK_OFFSET(i) (((i) & (kMtBtNumBlocks - 1)) * (size_t)kMtBtBlockSize)

/*
  HASH functions:
  We use raw 8/16 bits from a[1] and a[2],
  xored with crc(a[0]) and crc(a[3]).
  We check a[0], a[3] only. We don't need to compare a[1] and a[2] in matches.
  our crc() function provides one-to-one correspondence for low 8-bit values:
    (crc[0...0xFF] & 0xFF) <-> [0...0xFF]
*/

#define MF(mt) ((mt)->MatchFinder)
#define MF_CRC (p->crc)

// #define MF(mt) (&(mt)->MatchFinder)
// #define MF_CRC (p->MatchFinder.crc)

#define MT_HASH2_CALC \
    h2 = (MF_CRC[cur[0]] ^ cur[1]) & (kHash2Size - 1);

#define MT_HASH3_CALC                                           \
    {                                                           \
        UInt32 temp = MF_CRC[cur[0]] ^ cur[1];                  \
        h2 = temp & (kHash2Size - 1);                           \
        h3 = (temp ^ ((UInt32)cur[2] << 8)) & (kHash3Size - 1); \
    }

/*
#define MT_HASH3_CALC__NO_2 { \
  UInt32 temp = p->crc[cur[0]] ^ cur[1]; \
  h3 = (temp ^ ((UInt32)cur[2] << 8)) & (kHash3Size - 1); }

#define __MT_HASH4_CALC { \
  UInt32 temp = p->crc[cur[0]] ^ cur[1]; \
  h2 = temp & (kHash2Size - 1); \
  temp ^= ((UInt32)cur[2] << 8); \
  h3 = temp & (kHash3Size - 1); \
  h4 = (temp ^ (p->crc[cur[3]] << kLzHash_CrcShift_1)) & p->hash4Mask; }
  // (kHash4Size - 1);
*/

MY_NO_INLINE
static void MtSync_Construct(CMtSync *p)
{
    p->affinity = 0;
    p->wasCreated = False;
    p->csWasInitialized = False;
    p->csWasEntered = False;
    Thread_Construct(&p->thread);
    Event_Construct(&p->canStart);
    Event_Construct(&p->wasStopped);
    Semaphore_Construct(&p->freeSemaphore);
    Semaphore_Construct(&p->filledSemaphore);
}

#define DEBUG_BUFFER_LOCK // define it to debug lock state

#ifdef DEBUG_BUFFER_LOCK
#include <stdlib.h>
#define BUFFER_MUST_BE_LOCKED(p) \
    if (!(p)->csWasEntered)      \
        exit(1);
#define BUFFER_MUST_BE_UNLOCKED(p) \
    if ((p)->csWasEntered)         \
        exit(1);
#else
#define BUFFER_MUST_BE_LOCKED(p)
#define BUFFER_MUST_BE_UNLOCKED(p)
#endif

#define LOCK_BUFFER(p)                   \
    {                                    \
        BUFFER_MUST_BE_UNLOCKED(p);      \
        CriticalSection_Enter(&(p)->cs); \
        (p)->csWasEntered = True;        \
    }

#define UNLOCK_BUFFER(p)                 \
    {                                    \
        BUFFER_MUST_BE_LOCKED(p);        \
        CriticalSection_Leave(&(p)->cs); \
        (p)->csWasEntered = False;       \
    }

MY_NO_INLINE
static UInt32 MtSync_GetNextBlock(CMtSync *p)
{
    UInt32 numBlocks = 0;
    if (p->needStart)
    {
        BUFFER_MUST_BE_UNLOCKED(p)
        p->numProcessedBlocks = 1;
        p->needStart = False;
        p->stopWriting = False;
        p->exit = False;
        Event_Reset(&p->wasStopped);
        Event_Set(&p->canStart);
    }
    else
    {
        UNLOCK_BUFFER(p)
        // we free current block
        numBlocks = p->numProcessedBlocks++;
        Semaphore_Release1(&p->freeSemaphore);
    }

    // buffer is UNLOCKED here
    Semaphore_Wait(&p->filledSemaphore);
    LOCK_BUFFER(p);
    return numBlocks;
}

/* if Writing (Processing) thread was started, we must call MtSync_StopWriting() */

MY_NO_INLINE
static void MtSync_StopWriting(CMtSync *p)
{
    if (!Thread_WasCreated(&p->thread) || p->needStart)
        return;

    PRF(printf("\nMtSync_StopWriting %p\n", p));

    if (p->csWasEntered)
    {
        /* we don't use buffer in this thread after StopWriting().
           So we UNLOCK buffer.
           And we restore default UNLOCKED state for stopped thread */
        UNLOCK_BUFFER(p)
    }

    /* We send (p->stopWriting) message and release freeSemaphore
       to free current block.
       So the thread will see (p->stopWriting) at some
       iteration after Wait(freeSemaphore).
       The thread doesn't need to fill all avail free blocks,
       so we can get fast thread stop.
    */

    p->stopWriting = True;
    Semaphore_Release1(&p->freeSemaphore); // check semaphore count !!!

    PRF(printf("\nMtSync_StopWriting %p : Event_Wait(&p->wasStopped)\n", p));
    Event_Wait(&p->wasStopped);
    PRF(printf("\nMtSync_StopWriting %p : Event_Wait() finsihed\n", p));

    /* 21.03 : we don't restore samaphore counters here.
       We will recreate and reinit samaphores in next start */

    p->needStart = True;
}

MY_NO_INLINE
static void MtSync_Destruct(CMtSync *p)
{
    PRF(printf("\nMtSync_Destruct %p\n", p));

    if (Thread_WasCreated(&p->thread))
    {
        /* we want thread to be in Stopped state before sending EXIT command.
           note: stop(btSync) will stop (htSync) also */
        MtSync_StopWriting(p);
        /* thread in Stopped state here : (p->needStart == true) */
        p->exit = True;
        // if (p->needStart)  // it's (true)
        Event_Set(&p->canStart);       // we send EXIT command to thread
        Thread_Wait_Close(&p->thread); // we wait thread finishing
    }

    if (p->csWasInitialized)
    {
        CriticalSection_Delete(&p->cs);
        p->csWasInitialized = False;
    }
    p->csWasEntered = False;

    Event_Close(&p->canStart);
    Event_Close(&p->wasStopped);
    Semaphore_Close(&p->freeSemaphore);
    Semaphore_Close(&p->filledSemaphore);

    p->wasCreated = False;
}

// #define RINOK_THREAD(x) { if ((x) != 0) return SZ_ERROR_THREAD; }
// we want to get real system error codes here instead of SZ_ERROR_THREAD
#define RINOK_THREAD(x) RINOK(x)

// call it before each new file (when new starting is required):
MY_NO_INLINE
static SRes MtSync_Init(CMtSync *p, UInt32 numBlocks)
{
    WRes wres;
    // BUFFER_MUST_BE_UNLOCKED(p)
    if (!p->needStart || p->csWasEntered)
        return SZ_ERROR_FAIL;
    wres = Semaphore_OptCreateInit(&p->freeSemaphore, numBlocks, numBlocks);
    if (wres == 0)
        wres = Semaphore_OptCreateInit(&p->filledSemaphore, 0, numBlocks);
    return MY_SRes_HRESULT_FROM_WRes(wres);
}

static WRes MtSync_Create_WRes(CMtSync *p, THREAD_FUNC_TYPE startAddress, void *obj)
{
    WRes wres;

    if (p->wasCreated)
        return SZ_OK;

    RINOK_THREAD(CriticalSection_Init(&p->cs));
    p->csWasInitialized = True;
    p->csWasEntered = False;

    RINOK_THREAD(AutoResetEvent_CreateNotSignaled(&p->canStart));
    RINOK_THREAD(AutoResetEvent_CreateNotSignaled(&p->wasStopped));

    p->needStart = True;
    p->exit = True; /* p->exit is unused before (canStart) Event.
      But in case of some unexpected code failure we will get fast exit from thread */

    // return ERROR_TOO_MANY_POSTS; // for debug
    // return EINVAL; // for debug

    if (p->affinity != 0)
        wres = Thread_Create_With_Affinity(&p->thread, startAddress, obj, (CAffinityMask)p->affinity);
    else
        wres = Thread_Create(&p->thread, startAddress, obj);

    RINOK_THREAD(wres);
    p->wasCreated = True;
    return SZ_OK;
}

MY_NO_INLINE
static SRes MtSync_Create(CMtSync *p, THREAD_FUNC_TYPE startAddress, void *obj)
{
    const WRes wres = MtSync_Create_WRes(p, startAddress, obj);
    if (wres == 0)
        return 0;
    MtSync_Destruct(p);
    return MY_SRes_HRESULT_FROM_WRes(wres);
}

// ---------- HASH THREAD ----------

#define kMtMaxValForNormalize 0xFFFFFFFF
// #define kMtMaxValForNormalize ((1 << 21)) // for debug
// #define kNormalizeAlign (1 << 7) // alignment for speculated accesses

#ifdef MY_CPU_LE_UNALIGN
#define GetUi24hi_from32(p) ((UInt32)GetUi32(p) >> 8)
#else
#define GetUi24hi_from32(p) ((p)[1] ^ ((UInt32)(p)[2] << 8) ^ ((UInt32)(p)[3] << 16))
#endif

#define GetHeads_DECL(name)                               \
    static void GetHeads##name(const Byte *p, UInt32 pos, \
                               UInt32 *hash, UInt32 hashMask, UInt32 *heads, UInt32 numHeads, const UInt32 *crc)

#define GetHeads_LOOP(v)              \
    for (; numHeads != 0; numHeads--) \
    {                                 \
        const UInt32 value = (v);     \
        p++;                          \
        *heads++ = pos - hash[value]; \
        hash[value] = pos++;          \
    }

#define DEF_GetHeads2(name, v, action) \
    GetHeads_DECL(name)                \
    {                                  \
        action                         \
        GetHeads_LOOP(v)               \
    }

#define DEF_GetHeads(name, v) DEF_GetHeads2(name, v, ;)

DEF_GetHeads2(2, GetUi16(p), UNUSED_VAR(hashMask); UNUSED_VAR(crc);)
    DEF_GetHeads(3, (crc[p[0]] ^ GetUi16(p + 1)) & hashMask)
        DEF_GetHeads2(3b, GetUi16(p) ^ ((UInt32)(p)[2] << 16), UNUSED_VAR(hashMask); UNUSED_VAR(crc);)
// BT3 is not good for crc collisions for big hashMask values.

/*
GetHeads_DECL(3b)
{
  UNUSED_VAR(hashMask);
  UNUSED_VAR(crc);
  {
  const Byte *pLim = p + numHeads;
  if (numHeads == 0)
    return;
  pLim--;
  while (p < pLim)
  {
    UInt32 v1 = GetUi32(p);
    UInt32 v0 = v1 & 0xFFFFFF;
    UInt32 h0, h1;
    p += 2;
    v1 >>= 8;
    h0 = hash[v0]; hash[v0] = pos; heads[0] = pos - h0; pos++;
    h1 = hash[v1]; hash[v1] = pos; heads[1] = pos - h1; pos++;
    heads += 2;
  }
  if (p == pLim)
  {
    UInt32 v0 = GetUi16(p) ^ ((UInt32)(p)[2] << 16);
    *heads = pos - hash[v0];
    hash[v0] = pos;
  }
  }
}
*/

/*
GetHeads_DECL(4)
{
  unsigned sh = 0;
  UNUSED_VAR(crc)
  while ((hashMask & 0x80000000) == 0)
  {
    hashMask <<= 1;
    sh++;
  }
  GetHeads_LOOP((GetUi32(p) * 0xa54a1) >> sh)
}
#define GetHeads4b GetHeads4
*/

#define USE_GetHeads_LOCAL_CRC

#ifdef USE_GetHeads_LOCAL_CRC

            GetHeads_DECL(4)
{
    UInt32 crc0[256];
    UInt32 crc1[256];
    {
        unsigned i;
        for (i = 0; i < 256; i++)
        {
            UInt32 v = crc[i];
            crc0[i] = v & hashMask;
            crc1[i] = (v << kLzHash_CrcShift_1) & hashMask;
            // crc1[i] = rotlFixed(v, 8) & hashMask;
        }
    }
    GetHeads_LOOP(crc0[p[0]] ^ crc1[p[3]] ^ (UInt32)GetUi16(p + 1))
}

GetHeads_DECL(4b)
{
    UInt32 crc0[256];
    {
        unsigned i;
        for (i = 0; i < 256; i++)
            crc0[i] = crc[i] & hashMask;
    }
    GetHeads_LOOP(crc0[p[0]] ^ GetUi24hi_from32(p))
}

GetHeads_DECL(5)
{
    UInt32 crc0[256];
    UInt32 crc1[256];
    UInt32 crc2[256];
    {
        unsigned i;
        for (i = 0; i < 256; i++)
        {
            UInt32 v = crc[i];
            crc0[i] = v & hashMask;
            crc1[i] = (v << kLzHash_CrcShift_1) & hashMask;
            crc2[i] = (v << kLzHash_CrcShift_2) & hashMask;
        }
    }
    GetHeads_LOOP(crc0[p[0]] ^ crc1[p[3]] ^ crc2[p[4]] ^ (UInt32)GetUi16(p + 1))
}

GetHeads_DECL(5b)
{
    UInt32 crc0[256];
    UInt32 crc1[256];
    {
        unsigned i;
        for (i = 0; i < 256; i++)
        {
            UInt32 v = crc[i];
            crc0[i] = v & hashMask;
            crc1[i] = (v << kLzHash_CrcShift_1) & hashMask;
        }
    }
    GetHeads_LOOP(crc0[p[0]] ^ crc1[p[4]] ^ GetUi24hi_from32(p))
}

#else

            DEF_GetHeads(4, (crc[p[0]] ^ (crc[p[3]] << kLzHash_CrcShift_1) ^ (UInt32)GetUi16(p + 1)) & hashMask)
                DEF_GetHeads(4b, (crc[p[0]] ^ GetUi24hi_from32(p)) & hashMask)
                    DEF_GetHeads(5, (crc[p[0]] ^ (crc[p[3]] << kLzHash_CrcShift_1) ^ (crc[p[4]] << kLzHash_CrcShift_2) ^ (UInt32)GetUi16(p + 1)) & hashMask)
                        DEF_GetHeads(5b, (crc[p[0]] ^ (crc[p[4]] << kLzHash_CrcShift_1) ^ GetUi24hi_from32(p)) & hashMask)

#endif

static void HashThreadFunc(CMatchFinderMt *mt)
{
    CMtSync *p = &mt->hashSync;
    PRF(printf("\nHashThreadFunc\n"));

    for (;;)
    {
        UInt32 blockIndex = 0;
        PRF(printf("\nHashThreadFunc : Event_Wait(&p->canStart)\n"));
        Event_Wait(&p->canStart);
        PRF(printf("\nHashThreadFunc : Event_Wait(&p->canStart) : after \n"));
        if (p->exit)
        {
            PRF(printf("\nHashThreadFunc : exit \n"));
            return;
        }

        MatchFinder_Init_HighHash(MF(mt));

        for (;;)
        {
            PRF(printf("Hash thread block = %d pos = %d\n", (unsigned)blockIndex, mt->MatchFinder->pos));

            {
                CMatchFinder *mf = MF(mt);
                if (MatchFinder_NeedMove(mf))
                {
                    CriticalSection_Enter(&mt->btSync.cs);
                    CriticalSection_Enter(&mt->hashSync.cs);
                    {
                        const Byte *beforePtr = Inline_MatchFinder_GetPointerToCurrentPos(mf);
                        ptrdiff_t offset;
                        MatchFinder_MoveBlock(mf);
                        offset = beforePtr - Inline_MatchFinder_GetPointerToCurrentPos(mf);
                        mt->pointerToCurPos -= offset;
                        mt->buffer -= offset;
                    }
                    CriticalSection_Leave(&mt->hashSync.cs);
                    CriticalSection_Leave(&mt->btSync.cs);
                    continue;
                }

                Semaphore_Wait(&p->freeSemaphore);

                if (p->exit) // exit is unexpected here. But we check it here for some failure case
                    return;

                // for faster stop : we check (p->stopWriting) after Wait(freeSemaphore)
                if (p->stopWriting)
                    break;

                MatchFinder_ReadIfRequired(mf);
                {
                    UInt32 *heads = mt->hashBuf + GET_HASH_BLOCK_OFFSET(blockIndex++);
                    UInt32 num = Inline_MatchFinder_GetNumAvailableBytes(mf);
                    heads[0] = 2;
                    heads[1] = num;

                    /* heads[1] contains the number of avail bytes:
                       if (avail < mf->numHashBytes) :
                       {
                         it means that stream was finished
                         HASH_THREAD and BT_TREAD must move position for heads[1] (avail) bytes.
                         HASH_THREAD doesn't stop,
                         HASH_THREAD fills only the header (2 numbers) for all next blocks:
                         {2, NumHashBytes - 1}, {2,0}, {2,0}, ... , {2,0}
                       }
                       else
                       {
                         HASH_THREAD and BT_TREAD must move position for (heads[0] - 2) bytes;
                       }
                    */

                    if (num >= mf->numHashBytes)
                    {
                        num = num - mf->numHashBytes + 1;
                        if (num > kMtHashBlockSize - 2)
                            num = kMtHashBlockSize - 2;

                        if (mf->pos > (UInt32)kMtMaxValForNormalize - num)
                        {
                            const UInt32 subValue = (mf->pos - mf->historySize - 1); // & ~(UInt32)(kNormalizeAlign - 1);
                            Inline_MatchFinder_ReduceOffsets(mf, subValue);
                            MatchFinder_Normalize3(subValue, mf->hash + mf->fixedHashSize, (size_t)mf->hashMask + 1);
                        }

                        heads[0] = 2 + num;
                        mt->GetHeadsFunc(mf->buffer, mf->pos, mf->hash + mf->fixedHashSize, mf->hashMask, heads + 2, num, mf->crc);
                    }

                    mf->pos += num; // wrap over zero is allowed at the end of stream
                    mf->buffer += num;
                }
            }

            Semaphore_Release1(&p->filledSemaphore);
        } // for() processing end

        // p->numBlocks_Sent = blockIndex;
        Event_Set(&p->wasStopped);
    } // for() thread end
}

// ---------- BT THREAD ----------

/* we use one variable instead of two (cyclicBufferPos == pos) before CyclicBuf wrap.
   here we define fixed offset of (p->pos) from (p->cyclicBufferPos) */
#define CYC_TO_POS_OFFSET 0
// #define CYC_TO_POS_OFFSET 1 // for debug

#define MFMT_GM_INLINE

#ifdef MFMT_GM_INLINE

/*
  we use size_t for (pos) instead of UInt32
  to eliminate "movsx" BUG in old MSVC x64 compiler.
*/

UInt32 *MY_FAST_CALL GetMatchesSpecN_2(const Byte *lenLimit, size_t pos, const Byte *cur, CLzRef *son,
                                       UInt32 _cutValue, UInt32 *d, size_t _maxLen, const UInt32 *hash, const UInt32 *limit, const UInt32 *size,
                                       size_t _cyclicBufferPos, UInt32 _cyclicBufferSize,
                                       UInt32 *posRes);

#endif

static void BtGetMatches(CMatchFinderMt *p, UInt32 *d)
{
    UInt32 numProcessed = 0;
    UInt32 curPos = 2;

    /* GetMatchesSpec() functions don't create (len = 1)
       in [len, dist] match pairs, if (p->numHashBytes >= 2)
       Also we suppose here that (matchMaxLen >= 2).
       So the following code for (reserve) is not required
       UInt32 reserve = (p->matchMaxLen * 2);
       const UInt32 kNumHashBytes_Max = 5; // BT_HASH_BYTES_MAX
       if (reserve < kNumHashBytes_Max - 1)
          reserve = kNumHashBytes_Max - 1;
       const UInt32 limit = kMtBtBlockSize - (reserve);
    */

    const UInt32 limit = kMtBtBlockSize - (p->matchMaxLen * 2);

    d[1] = p->hashNumAvail;

    if (p->failure_BT)
    {
        // printf("\n == 1 BtGetMatches() p->failure_BT\n");
        d[0] = 0;
        // d[1] = 0;
        return;
    }

    while (curPos < limit)
    {
        if (p->hashBufPos == p->hashBufPosLimit)
        {
            // MatchFinderMt_GetNextBlock_Hash(p);
            UInt32 avail;
            {
                const UInt32 bi = MtSync_GetNextBlock(&p->hashSync);
                const UInt32 k = GET_HASH_BLOCK_OFFSET(bi);
                const UInt32 *h = p->hashBuf + k;
                avail = h[1];
                p->hashBufPosLimit = k + h[0];
                p->hashNumAvail = avail;
                p->hashBufPos = k + 2;
            }

            {
                /* we must prevent UInt32 overflow for avail total value,
                   if avail was increased with new hash block */
                UInt32 availSum = numProcessed + avail;
                if (availSum < numProcessed)
                    availSum = (UInt32)(Int32)-1;
                d[1] = availSum;
            }

            if (avail >= p->numHashBytes)
                continue;

            // if (p->hashBufPos != p->hashBufPosLimit) exit(1);

            /* (avail < p->numHashBytes)
               It means that stream was finished.
               And (avail) - is a number of remaining bytes,
               we fill (d) for (avail) bytes for LZ_THREAD (receiver).
               but we don't update (p->pos) and (p->cyclicBufferPos) here in BT_THREAD */

            /* here we suppose that we have space enough:
               (kMtBtBlockSize - curPos >= p->hashNumAvail) */
            p->hashNumAvail = 0;
            d[0] = curPos + avail;
            d += curPos;
            for (; avail != 0; avail--)
                *d++ = 0;
            return;
        }
        {
            UInt32 size = p->hashBufPosLimit - p->hashBufPos;
            UInt32 pos = p->pos;
            UInt32 cyclicBufferPos = p->cyclicBufferPos;
            UInt32 lenLimit = p->matchMaxLen;
            if (lenLimit >= p->hashNumAvail)
                lenLimit = p->hashNumAvail;
            {
                UInt32 size2 = p->hashNumAvail - lenLimit + 1;
                if (size2 < size)
                    size = size2;
                size2 = p->cyclicBufferSize - cyclicBufferPos;
                if (size2 < size)
                    size = size2;
            }

            if (pos > (UInt32)kMtMaxValForNormalize - size)
            {
                const UInt32 subValue = (pos - p->cyclicBufferSize); // & ~(UInt32)(kNormalizeAlign - 1);
                pos -= subValue;
                p->pos = pos;
                MatchFinder_Normalize3(subValue, p->son, (size_t)p->cyclicBufferSize * 2);
            }

#ifndef MFMT_GM_INLINE
            while (curPos < limit && size-- != 0)
            {
                UInt32 *startDistances = d + curPos;
                UInt32 num = (UInt32)(GetMatchesSpec1(lenLimit, pos - p->hashBuf[p->hashBufPos++],
                                                      pos, p->buffer, p->son, cyclicBufferPos, p->cyclicBufferSize, p->cutValue,
                                                      startDistances + 1, p->numHashBytes - 1) -
                                      startDistances);
                *startDistances = num - 1;
                curPos += num;
                cyclicBufferPos++;
                pos++;
                p->buffer++;
            }
#else
            {
                UInt32 posRes = pos;
                const UInt32 *d_end;
                {
                    d_end = GetMatchesSpecN_2(
                        p->buffer + lenLimit - 1,
                        pos, p->buffer, p->son, p->cutValue, d + curPos,
                        p->numHashBytes - 1, p->hashBuf + p->hashBufPos,
                        d + limit, p->hashBuf + p->hashBufPos + size,
                        cyclicBufferPos, p->cyclicBufferSize,
                        &posRes);
                }
                {
                    if (!d_end)
                    {
                        // printf("\n == 2 BtGetMatches() p->failure_BT\n");
                        // internal data failure
                        p->failure_BT = True;
                        d[0] = 0;
                        // d[1] = 0;
                        return;
                    }
                }
                curPos = (UInt32)(d_end - d);
                {
                    const UInt32 processed = posRes - pos;
                    pos = posRes;
                    p->hashBufPos += processed;
                    cyclicBufferPos += processed;
                    p->buffer += processed;
                }
            }
#endif

            {
                const UInt32 processed = pos - p->pos;
                numProcessed += processed;
                p->hashNumAvail -= processed;
                p->pos = pos;
            }
            if (cyclicBufferPos == p->cyclicBufferSize)
                cyclicBufferPos = 0;
            p->cyclicBufferPos = cyclicBufferPos;
        }
    }

    d[0] = curPos;
}

static void BtFillBlock(CMatchFinderMt *p, UInt32 globalBlockIndex)
{
    CMtSync *sync = &p->hashSync;

    BUFFER_MUST_BE_UNLOCKED(sync)

    if (!sync->needStart)
    {
        LOCK_BUFFER(sync)
    }

    BtGetMatches(p, p->btBuf + GET_BT_BLOCK_OFFSET(globalBlockIndex));

    /* We suppose that we have called GetNextBlock() from start.
       So buffer is LOCKED */

    UNLOCK_BUFFER(sync)
}

MY_NO_INLINE
static void BtThreadFunc(CMatchFinderMt *mt)
{
    CMtSync *p = &mt->btSync;
    for (;;)
    {
        UInt32 blockIndex = 0;
        Event_Wait(&p->canStart);

        for (;;)
        {
            PRF(printf("  BT thread block = %d  pos = %d\n", (unsigned)blockIndex, mt->pos));
            /* (p->exit == true) is possible after (p->canStart) at first loop iteration
               and is unexpected after more Wait(freeSemaphore) iterations */
            if (p->exit)
                return;

            Semaphore_Wait(&p->freeSemaphore);

            // for faster stop : we check (p->stopWriting) after Wait(freeSemaphore)
            if (p->stopWriting)
                break;

            BtFillBlock(mt, blockIndex++);

            Semaphore_Release1(&p->filledSemaphore);
        }

        // we stop HASH_THREAD here
        MtSync_StopWriting(&mt->hashSync);

        // p->numBlocks_Sent = blockIndex;
        Event_Set(&p->wasStopped);
    }
}

void MatchFinderMt_Construct(CMatchFinderMt *p)
{
    p->hashBuf = NULL;
    MtSync_Construct(&p->hashSync);
    MtSync_Construct(&p->btSync);
}

static void MatchFinderMt_FreeMem(CMatchFinderMt *p, ISzAllocPtr alloc)
{
    ISzAlloc_Free(alloc, p->hashBuf);
    p->hashBuf = NULL;
}

void MatchFinderMt_Destruct(CMatchFinderMt *p, ISzAllocPtr alloc)
{
    /*
       HASH_THREAD can use CriticalSection(s) btSync.cs and hashSync.cs.
       So we must be sure that HASH_THREAD will not use CriticalSection(s)
       after deleting CriticalSection here.

       we call ReleaseStream(p)
         that calls StopWriting(btSync)
           that calls StopWriting(hashSync), if it's required to stop HASH_THREAD.
       after StopWriting() it's safe to destruct MtSync(s) in any order */

    MatchFinderMt_ReleaseStream(p);

    MtSync_Destruct(&p->btSync);
    MtSync_Destruct(&p->hashSync);

    LOG_ITER(
        printf("\nTree %9d * %7d iter = %9d = sum  :  bytes = %9d\n",
               (UInt32)(g_NumIters_Tree / 1000),
               (UInt32)(((UInt64)g_NumIters_Loop * 1000) / (g_NumIters_Tree + 1)),
               (UInt32)(g_NumIters_Loop / 1000),
               (UInt32)(g_NumIters_Bytes / 1000)));

    MatchFinderMt_FreeMem(p, alloc);
}

#define kHashBufferSize (kMtHashBlockSize * kMtHashNumBlocks)
#define kBtBufferSize (kMtBtBlockSize * kMtBtNumBlocks)

static THREAD_FUNC_DECL HashThreadFunc2(void *p)
{
    HashThreadFunc((CMatchFinderMt *)p);
    return 0;
}
static THREAD_FUNC_DECL BtThreadFunc2(void *p)
{
    Byte allocaDummy[0x180];
    unsigned i = 0;
    for (i = 0; i < 16; i++)
        allocaDummy[i] = (Byte)0;
    if (allocaDummy[0] == 0)
        BtThreadFunc((CMatchFinderMt *)p);
    return 0;
}

SRes MatchFinderMt_Create(CMatchFinderMt *p, UInt32 historySize, UInt32 keepAddBufferBefore,
                          UInt32 matchMaxLen, UInt32 keepAddBufferAfter, ISzAllocPtr alloc)
{
    CMatchFinder *mf = MF(p);
    p->historySize = historySize;
    if (kMtBtBlockSize <= matchMaxLen * 4)
        return SZ_ERROR_PARAM;
    if (!p->hashBuf)
    {
        p->hashBuf = (UInt32 *)ISzAlloc_Alloc(alloc, ((size_t)kHashBufferSize + (size_t)kBtBufferSize) * sizeof(UInt32));
        if (!p->hashBuf)
            return SZ_ERROR_MEM;
        p->btBuf = p->hashBuf + kHashBufferSize;
    }
    keepAddBufferBefore += (kHashBufferSize + kBtBufferSize);
    keepAddBufferAfter += kMtHashBlockSize;
    if (!MatchFinder_Create(mf, historySize, keepAddBufferBefore, matchMaxLen, keepAddBufferAfter, alloc))
        return SZ_ERROR_MEM;

    RINOK(MtSync_Create(&p->hashSync, HashThreadFunc2, p));
    RINOK(MtSync_Create(&p->btSync, BtThreadFunc2, p));
    return SZ_OK;
}

SRes MatchFinderMt_InitMt(CMatchFinderMt *p)
{
    RINOK(MtSync_Init(&p->hashSync, kMtHashNumBlocks));
    return MtSync_Init(&p->btSync, kMtBtNumBlocks);
}

static void MatchFinderMt_Init(CMatchFinderMt *p)
{
    CMatchFinder *mf = MF(p);

    p->btBufPos =
        p->btBufPosLimit = NULL;
    p->hashBufPos =
        p->hashBufPosLimit = 0;
    p->hashNumAvail = 0; // 21.03

    p->failure_BT = False;

    /* Init without data reading. We don't want to read data in this thread */
    MatchFinder_Init_4(mf);

    MatchFinder_Init_LowHash(mf);

    p->pointerToCurPos = Inline_MatchFinder_GetPointerToCurrentPos(mf);
    p->btNumAvailBytes = 0;
    p->failure_LZ_BT = False;
    // p->failure_LZ_LZ = False;

    p->lzPos =
        1; // optimal smallest value
    // 0; // for debug: ignores match to start
    // kNormalizeAlign; // for debug

    p->hash = mf->hash;
    p->fixedHashSize = mf->fixedHashSize;
    // p->hash4Mask = mf->hash4Mask;
    p->crc = mf->crc;
    // memcpy(p->crc, mf->crc, sizeof(mf->crc));

    p->son = mf->son;
    p->matchMaxLen = mf->matchMaxLen;
    p->numHashBytes = mf->numHashBytes;

    /* (mf->pos) and (mf->streamPos) were already initialized to 1 in MatchFinder_Init_4() */
    // mf->streamPos = mf->pos = 1; // optimal smallest value
    // 0; // for debug: ignores match to start
    // kNormalizeAlign; // for debug

    /* we must init (p->pos = mf->pos) for BT, because
       BT code needs (p->pos == delta_value_for_empty_hash_record == mf->pos) */
    p->pos = mf->pos; // do not change it

    p->cyclicBufferPos = (p->pos - CYC_TO_POS_OFFSET);
    p->cyclicBufferSize = mf->cyclicBufferSize;
    p->buffer = mf->buffer;
    p->cutValue = mf->cutValue;
    // p->son[0] = p->son[1] = 0; // unused: to init skipped record for speculated accesses.
}

/* ReleaseStream is required to finish multithreading */
void MatchFinderMt_ReleaseStream(CMatchFinderMt *p)
{
    // Sleep(1); // for debug
    MtSync_StopWriting(&p->btSync);
    // Sleep(200); // for debug
    /* p->MatchFinder->ReleaseStream(); */
}

MY_NO_INLINE
static UInt32 MatchFinderMt_GetNextBlock_Bt(CMatchFinderMt *p)
{
    if (p->failure_LZ_BT)
        p->btBufPos = p->failureBuf;
    else
    {
        const UInt32 bi = MtSync_GetNextBlock(&p->btSync);
        const UInt32 *bt = p->btBuf + GET_BT_BLOCK_OFFSET(bi);
        {
            const UInt32 numItems = bt[0];
            p->btBufPosLimit = bt + numItems;
            p->btNumAvailBytes = bt[1];
            p->btBufPos = bt + 2;
            if (numItems < 2 || numItems > kMtBtBlockSize)
            {
                p->failureBuf[0] = 0;
                p->btBufPos = p->failureBuf;
                p->btBufPosLimit = p->failureBuf + 1;
                p->failure_LZ_BT = True;
                // p->btNumAvailBytes = 0;
                /* we don't want to decrease AvailBytes, that was load before.
                    that can be unxepected for the code that have loaded anopther value before */
            }
        }

        if (p->lzPos >= (UInt32)kMtMaxValForNormalize - (UInt32)kMtBtBlockSize)
        {
            /* we don't check (lzPos) over exact avail bytes in (btBuf).
               (fixedHashSize) is small, so normalization is fast */
            const UInt32 subValue = (p->lzPos - p->historySize - 1); // & ~(UInt32)(kNormalizeAlign - 1);
            p->lzPos -= subValue;
            MatchFinder_Normalize3(subValue, p->hash, p->fixedHashSize);
        }
    }
    return p->btNumAvailBytes;
}

static const Byte *MatchFinderMt_GetPointerToCurrentPos(CMatchFinderMt *p)
{
    return p->pointerToCurPos;
}

#define GET_NEXT_BLOCK_IF_REQUIRED       \
    if (p->btBufPos == p->btBufPosLimit) \
        MatchFinderMt_GetNextBlock_Bt(p);

static UInt32 MatchFinderMt_GetNumAvailableBytes(CMatchFinderMt *p)
{
    if (p->btBufPos != p->btBufPosLimit)
        return p->btNumAvailBytes;
    return MatchFinderMt_GetNextBlock_Bt(p);
}

// #define CHECK_FAILURE_LZ(_match_, _pos_) if (_match_ >= _pos_) { p->failure_LZ_LZ = True;  return d; }
#define CHECK_FAILURE_LZ(_match_, _pos_)

static UInt32 *MixMatches2(CMatchFinderMt *p, UInt32 matchMinPos, UInt32 *d)
{
    UInt32 h2, c2;
    UInt32 *hash = p->hash;
    const Byte *cur = p->pointerToCurPos;
    const UInt32 m = p->lzPos;
    MT_HASH2_CALC

    c2 = hash[h2];
    hash[h2] = m;

    if (c2 >= matchMinPos)
    {
        CHECK_FAILURE_LZ(c2, m)
        if (cur[(ptrdiff_t)c2 - (ptrdiff_t)m] == cur[0])
        {
            *d++ = 2;
            *d++ = m - c2 - 1;
        }
    }

    return d;
}

static UInt32 *MixMatches3(CMatchFinderMt *p, UInt32 matchMinPos, UInt32 *d)
{
    UInt32 h2, h3, c2, c3;
    UInt32 *hash = p->hash;
    const Byte *cur = p->pointerToCurPos;
    const UInt32 m = p->lzPos;
    MT_HASH3_CALC

    c2 = hash[h2];
    c3 = (hash + kFix3HashSize)[h3];

    hash[h2] = m;
    (hash + kFix3HashSize)[h3] = m;

    if (c2 >= matchMinPos)
    {
        CHECK_FAILURE_LZ(c2, m)
        if (cur[(ptrdiff_t)c2 - (ptrdiff_t)m] == cur[0])
        {
            d[1] = m - c2 - 1;
            if (cur[(ptrdiff_t)c2 - (ptrdiff_t)m + 2] == cur[2])
            {
                d[0] = 3;
                return d + 2;
            }
            d[0] = 2;
            d += 2;
        }
    }

    if (c3 >= matchMinPos)
    {
        CHECK_FAILURE_LZ(c3, m)
        if (cur[(ptrdiff_t)c3 - (ptrdiff_t)m] == cur[0])
        {
            *d++ = 3;
            *d++ = m - c3 - 1;
        }
    }

    return d;
}

#define INCREASE_LZ_POS \
    p->lzPos++;         \
    p->pointerToCurPos++;

static UInt32 *MixMatches4(CMatchFinderMt *p, UInt32 matchMinPos, UInt32 *d)
{
    UInt32 h2, h3, /* h4, */ c2, c3 /* , c4 */;
    UInt32 *hash = p->hash;
    const Byte *cur = p->pointerToCurPos;
    const UInt32 m = p->lzPos;
    MT_HASH3_CALC
    // MT_HASH4_CALC
    c2 = hash[h2];
    c3 = (hash + kFix3HashSize)[h3];
    // c4 = (hash + kFix4HashSize)[h4];

    hash[h2] = m;
    (hash + kFix3HashSize)[h3] = m;
    // (hash + kFix4HashSize)[h4] = m;

#define _USE_H2

#ifdef _USE_H2
    if (c2 >= matchMinPos && cur[(ptrdiff_t)c2 - (ptrdiff_t)m] == cur[0])
    {
        d[1] = m - c2 - 1;
        if (cur[(ptrdiff_t)c2 - (ptrdiff_t)m + 2] == cur[2])
        {
            // d[0] = (cur[(ptrdiff_t)c2 - (ptrdiff_t)m + 3] == cur[3]) ? 4 : 3;
            // return d + 2;

            if (cur[(ptrdiff_t)c2 - (ptrdiff_t)m + 3] == cur[3])
            {
                d[0] = 4;
                return d + 2;
            }
            d[0] = 3;
            d += 2;

#ifdef _USE_H4
            if (c4 >= matchMinPos)
                if (
                    cur[(ptrdiff_t)c4 - (ptrdiff_t)m] == cur[0] &&
                    cur[(ptrdiff_t)c4 - (ptrdiff_t)m + 3] == cur[3])
                {
                    *d++ = 4;
                    *d++ = m - c4 - 1;
                }
#endif
            return d;
        }
        d[0] = 2;
        d += 2;
    }
#endif

    if (c3 >= matchMinPos && cur[(ptrdiff_t)c3 - (ptrdiff_t)m] == cur[0])
    {
        d[1] = m - c3 - 1;
        if (cur[(ptrdiff_t)c3 - (ptrdiff_t)m + 3] == cur[3])
        {
            d[0] = 4;
            return d + 2;
        }
        d[0] = 3;
        d += 2;
    }

#ifdef _USE_H4
    if (c4 >= matchMinPos)
        if (
            cur[(ptrdiff_t)c4 - (ptrdiff_t)m] == cur[0] &&
            cur[(ptrdiff_t)c4 - (ptrdiff_t)m + 3] == cur[3])
        {
            *d++ = 4;
            *d++ = m - c4 - 1;
        }
#endif

    return d;
}

static UInt32 *MatchFinderMt2_GetMatches(CMatchFinderMt *p, UInt32 *d)
{
    const UInt32 *bt = p->btBufPos;
    const UInt32 len = *bt++;
    const UInt32 *btLim = bt + len;
    p->btBufPos = btLim;
    p->btNumAvailBytes--;
    INCREASE_LZ_POS
    {
        while (bt != btLim)
        {
            const UInt32 v0 = bt[0];
            const UInt32 v1 = bt[1];
            bt += 2;
            d[0] = v0;
            d[1] = v1;
            d += 2;
        }
    }
    return d;
}

static UInt32 *MatchFinderMt_GetMatches(CMatchFinderMt *p, UInt32 *d)
{
    const UInt32 *bt = p->btBufPos;
    UInt32 len = *bt++;
    const UInt32 avail = p->btNumAvailBytes - 1;
    p->btNumAvailBytes = avail;
    p->btBufPos = bt + len;
    if (len == 0)
    {
#define BT_HASH_BYTES_MAX 5
        if (avail >= (BT_HASH_BYTES_MAX - 1) - 1)
        {
            UInt32 m = p->lzPos;
            if (m > p->historySize)
                m -= p->historySize;
            else
                m = 1;
            d = p->MixMatchesFunc(p, m, d);
        }
    }
    else
    {
        /*
          first match pair from BinTree: (match_len, match_dist),
          (match_len >= numHashBytes).
          MixMatchesFunc() inserts only hash matches that are nearer than (match_dist)
        */
        d = p->MixMatchesFunc(p, p->lzPos - bt[1], d);
        // if (d) // check for failure
        do
        {
            const UInt32 v0 = bt[0];
            const UInt32 v1 = bt[1];
            bt += 2;
            d[0] = v0;
            d[1] = v1;
            d += 2;
        } while (len -= 2);
    }
    INCREASE_LZ_POS
    return d;
}

#define SKIP_HEADER2_MT \
    do                  \
    {                   \
    GET_NEXT_BLOCK_IF_REQUIRED
#define SKIP_HEADER_MT(n)                            \
    SKIP_HEADER2_MT if (p->btNumAvailBytes-- >= (n)) \
    {                                                \
        const Byte *cur = p->pointerToCurPos;        \
        UInt32 *hash = p->hash;
#define SKIP_FOOTER_MT                                       \
    }                                                        \
    INCREASE_LZ_POS p->btBufPos += (size_t)*p->btBufPos + 1; \
    }                                                        \
    while (--num != 0)                                       \
        ;

static void MatchFinderMt0_Skip(CMatchFinderMt *p, UInt32 num)
{
    SKIP_HEADER2_MT
    {
        p->btNumAvailBytes--;
        SKIP_FOOTER_MT
    }

    static void MatchFinderMt2_Skip(CMatchFinderMt * p, UInt32 num)
    {
        SKIP_HEADER_MT(2)
        UInt32 h2;
        MT_HASH2_CALC
        hash[h2] = p->lzPos;
        SKIP_FOOTER_MT
    }

    static void MatchFinderMt3_Skip(CMatchFinderMt * p, UInt32 num)
    {
        SKIP_HEADER_MT(3)
        UInt32 h2, h3;
        MT_HASH3_CALC(hash + kFix3HashSize)
        [h3] =
            hash[h2] =
                p->lzPos;
        SKIP_FOOTER_MT
    }

    /*
    // MatchFinderMt4_Skip() is similar to MatchFinderMt3_Skip().
    // The difference is that MatchFinderMt3_Skip() updates hash for last 3 bytes of stream.

    static void MatchFinderMt4_Skip(CMatchFinderMt *p, UInt32 num)
    {
      SKIP_HEADER_MT(4)
          UInt32 h2, h3; // h4
          MT_HASH3_CALC
          // MT_HASH4_CALC
          // (hash + kFix4HashSize)[h4] =
          (hash + kFix3HashSize)[h3] =
          hash[                h2] =
            p->lzPos;
      SKIP_FOOTER_MT
    }
    */

    void MatchFinderMt_CreateVTable(CMatchFinderMt * p, IMatchFinder2 * vTable)
    {
        vTable->Init = (Mf_Init_Func)MatchFinderMt_Init;
        vTable->GetNumAvailableBytes = (Mf_GetNumAvailableBytes_Func)MatchFinderMt_GetNumAvailableBytes;
        vTable->GetPointerToCurrentPos = (Mf_GetPointerToCurrentPos_Func)MatchFinderMt_GetPointerToCurrentPos;
        vTable->GetMatches = (Mf_GetMatches_Func)MatchFinderMt_GetMatches;

        switch (MF(p)->numHashBytes)
        {
        case 2:
            p->GetHeadsFunc = GetHeads2;
            p->MixMatchesFunc = (Mf_Mix_Matches)NULL;
            vTable->Skip = (Mf_Skip_Func)MatchFinderMt0_Skip;
            vTable->GetMatches = (Mf_GetMatches_Func)MatchFinderMt2_GetMatches;
            break;
        case 3:
            p->GetHeadsFunc = MF(p)->bigHash ? GetHeads3b : GetHeads3;
            p->MixMatchesFunc = (Mf_Mix_Matches)MixMatches2;
            vTable->Skip = (Mf_Skip_Func)MatchFinderMt2_Skip;
            break;
        case 4:
            p->GetHeadsFunc = MF(p)->bigHash ? GetHeads4b : GetHeads4;

            // it's fast inline version of GetMatches()
            // vTable->GetMatches = (Mf_GetMatches_Func)MatchFinderMt_GetMatches_Bt4;

            p->MixMatchesFunc = (Mf_Mix_Matches)MixMatches3;
            vTable->Skip = (Mf_Skip_Func)MatchFinderMt3_Skip;
            break;
        default:
            p->GetHeadsFunc = MF(p)->bigHash ? GetHeads5b : GetHeads5;
            p->MixMatchesFunc = (Mf_Mix_Matches)MixMatches4;
            vTable->Skip =
                (Mf_Skip_Func)MatchFinderMt3_Skip;
            // (Mf_Skip_Func)MatchFinderMt4_Skip;
            break;
        }
    }

#endif

    /* the following LzmaEnc_* declarations is internal LZMA interface for LZMA2 encoder */

    SRes LzmaEnc_PrepareForLzma2(CLzmaEncHandle pp, ISeqInStream * inStream, UInt32 keepWindowSize,
                                 ISzAllocPtr alloc, ISzAllocPtr allocBig);
    SRes LzmaEnc_MemPrepare(CLzmaEncHandle pp, const Byte *src, SizeT srcLen,
                            UInt32 keepWindowSize, ISzAllocPtr alloc, ISzAllocPtr allocBig);
    SRes LzmaEnc_CodeOneMemBlock(CLzmaEncHandle pp, BoolInt reInit,
                                 Byte * dest, size_t * destLen, UInt32 desiredPackSize, UInt32 * unpackSize);
    const Byte *LzmaEnc_GetCurBuf(CLzmaEncHandle pp);
    void LzmaEnc_Finish(CLzmaEncHandle pp);
    void LzmaEnc_SaveState(CLzmaEncHandle pp);
    void LzmaEnc_RestoreState(CLzmaEncHandle pp);

#ifdef SHOW_STAT
    static unsigned g_STAT_OFFSET = 0;
#endif

    /* for good normalization speed we still reserve 256 MB before 4 GB range */
#define kLzmaMaxHistorySize ((UInt32)15 << 28)

#define kNumTopBits 24
#define kTopValue ((UInt32)1 << kNumTopBits)

#define kNumBitModelTotalBits 11
#define kBitModelTotal (1 << kNumBitModelTotalBits)
#define kNumMoveBits 5
#define kProbInitValue (kBitModelTotal >> 1)

#define kNumMoveReducingBits 4
#define kNumBitPriceShiftBits 4
    // #define kBitPrice (1 << kNumBitPriceShiftBits)

#define REP_LEN_COUNT 64

    void LzmaEncProps_Init(CLzmaEncProps * p)
    {
        p->level = 5;
        p->dictSize = p->mc = 0;
        p->reduceSize = (UInt64)(Int64)-1;
        p->lc = p->lp = p->pb = p->algo = p->fb = p->btMode = p->numHashBytes = p->numThreads = -1;
        p->writeEndMark = 0;
        p->affinity = 0;
    }

    void LzmaEncProps_Normalize(CLzmaEncProps * p)
    {
        int level = p->level;
        if (level < 0)
            level = 5;
        p->level = level;

        if (p->dictSize == 0)
            p->dictSize =
                (level <= 3 ? ((UInt32)1 << (level * 2 + 16)) : (level <= 6 ? ((UInt32)1 << (level + 19)) : (level <= 7 ? ((UInt32)1 << 25) : ((UInt32)1 << 26))));

        if (p->dictSize > p->reduceSize)
        {
            UInt32 v = (UInt32)p->reduceSize;
            const UInt32 kReduceMin = ((UInt32)1 << 12);
            if (v < kReduceMin)
                v = kReduceMin;
            if (p->dictSize > v)
                p->dictSize = v;
        }

        if (p->lc < 0)
            p->lc = 3;
        if (p->lp < 0)
            p->lp = 0;
        if (p->pb < 0)
            p->pb = 2;

        if (p->algo < 0)
            p->algo = (level < 5 ? 0 : 1);
        if (p->fb < 0)
            p->fb = (level < 7 ? 32 : 64);
        if (p->btMode < 0)
            p->btMode = (p->algo == 0 ? 0 : 1);
        if (p->numHashBytes < 0)
            p->numHashBytes = (p->btMode ? 4 : 5);
        if (p->mc == 0)
            p->mc = (16 + ((unsigned)p->fb >> 1)) >> (p->btMode ? 0 : 1);

        if (p->numThreads < 0)
            p->numThreads =
#ifndef _7ZIP_ST
                ((p->btMode && p->algo) ? 2 : 1);
#else
            1;
#endif
    }

    UInt32 LzmaEncProps_GetDictSize(const CLzmaEncProps *props2)
    {
        CLzmaEncProps props = *props2;
        LzmaEncProps_Normalize(&props);
        return props.dictSize;
    }

    /*
    x86/x64:

    BSR:
      IF (SRC == 0) ZF = 1, DEST is undefined;
                      AMD : DEST is unchanged;
      IF (SRC != 0) ZF = 0; DEST is index of top non-zero bit
      BSR is slow in some processors

    LZCNT:
      IF (SRC  == 0) CF = 1, DEST is size_in_bits_of_register(src) (32 or 64)
      IF (SRC  != 0) CF = 0, DEST = num_lead_zero_bits
      IF (DEST == 0) ZF = 1;

    LZCNT works only in new processors starting from Haswell.
    if LZCNT is not supported by processor, then it's executed as BSR.
    LZCNT can be faster than BSR, if supported.
    */

    // #define LZMA_LOG_BSR

#if defined(MY_CPU_ARM_OR_ARM64) /* || defined(MY_CPU_X86_OR_AMD64) */

#if (defined(__clang__) && (__clang_major__ >= 6)) || (defined(__GNUC__) && (__GNUC__ >= 6))
#define LZMA_LOG_BSR
#elif defined(_MSC_VER) && (_MSC_VER >= 1300)
    // #if defined(MY_CPU_ARM_OR_ARM64)
#define LZMA_LOG_BSR
    // #endif
#endif
#endif

    // #include <intrin.h>

#ifdef LZMA_LOG_BSR

#if defined(__clang__) || defined(__GNUC__)

    /*
      C code:                  : (30 - __builtin_clz(x))
        gcc9/gcc10 for x64 /x86  : 30 - (bsr(x) xor 31)
        clang10 for x64          : 31 + (bsr(x) xor -32)
    */

#define MY_clz(x) ((unsigned)__builtin_clz(x))
    // __lzcnt32
    // __builtin_ia32_lzcnt_u32

#else // #if defined(_MSC_VER)

#ifdef MY_CPU_ARM_OR_ARM64

#define MY_clz _CountLeadingZeros

#else // if defined(MY_CPU_X86_OR_AMD64)

// #define MY_clz  __lzcnt  // we can use lzcnt (unsupported by old CPU)
// _BitScanReverse code is not optimal for some MSVC compilers
#define BSR2_RET(pos, res)             \
    {                                  \
        unsigned long zz;              \
        _BitScanReverse(&zz, (pos));   \
        zz--;                          \
        res = (zz + zz) + (pos >> zz); \
    }

#endif // MY_CPU_X86_OR_AMD64

#endif // _MSC_VER

#ifndef BSR2_RET

#define BSR2_RET(pos, res)              \
    {                                   \
        unsigned zz = 30 - MY_clz(pos); \
        res = (zz + zz) + (pos >> zz);  \
    }

#endif

    unsigned GetPosSlot1(UInt32 pos);
    unsigned GetPosSlot1(UInt32 pos)
    {
        unsigned res;
        BSR2_RET(pos, res);
        return res;
    }
#define GetPosSlot2(pos, res) \
    {                         \
        BSR2_RET(pos, res);   \
    }
#define GetPosSlot(pos, res)    \
    {                           \
        if (pos < 2)            \
            res = pos;          \
        else                    \
            BSR2_RET(pos, res); \
    }

#else // ! LZMA_LOG_BSR

#define kNumLogBits (11 + sizeof(size_t) / 8 * 3)

#define kDicLogSizeMaxCompress ((kNumLogBits - 1) * 2 + 7)

static void LzmaEnc_FastPosInit(Byte *g_FastPos)
{
    unsigned slot;
    g_FastPos[0] = 0;
    g_FastPos[1] = 1;
    g_FastPos += 2;

    for (slot = 2; slot < kNumLogBits * 2; slot++)
    {
        size_t k = ((size_t)1 << ((slot >> 1) - 1));
        size_t j;
        for (j = 0; j < k; j++)
            g_FastPos[j] = (Byte)slot;
        g_FastPos += k;
    }
}

/* we can use ((limit - pos) >> 31) only if (pos < ((UInt32)1 << 31)) */
/*
#define BSR2_RET(pos, res) { unsigned zz = 6 + ((kNumLogBits - 1) & \
(0 - (((((UInt32)1 << (kNumLogBits + 6)) - 1) - pos) >> 31))); \
res = p->g_FastPos[pos >> zz] + (zz * 2); }
*/

/*
#define BSR2_RET(pos, res) { unsigned zz = 6 + ((kNumLogBits - 1) & \
(0 - (((((UInt32)1 << (kNumLogBits)) - 1) - (pos >> 6)) >> 31))); \
res = p->g_FastPos[pos >> zz] + (zz * 2); }
*/

#define BSR2_RET(pos, res)                                                        \
    {                                                                             \
        unsigned zz = (pos < (1 << (kNumLogBits + 6))) ? 6 : 6 + kNumLogBits - 1; \
        res = p->g_FastPos[pos >> zz] + (zz * 2);                                 \
    }

/*
#define BSR2_RET(pos, res) { res = (pos < (1 << (kNumLogBits + 6))) ? \
p->g_FastPos[pos >> 6] + 12 : \
p->g_FastPos[pos >> (6 + kNumLogBits - 1)] + (6 + (kNumLogBits - 1)) * 2; }
*/

#define GetPosSlot1(pos) p->g_FastPos[pos]
#define GetPosSlot2(pos, res) \
    {                         \
        BSR2_RET(pos, res);   \
    }
#define GetPosSlot(pos, res)                                   \
    {                                                          \
        if (pos < kNumFullDistances)                           \
            res = p->g_FastPos[pos & (kNumFullDistances - 1)]; \
        else                                                   \
            BSR2_RET(pos, res);                                \
    }

#endif // LZMA_LOG_BSR

#define LZMA_NUM_REPS 4

    typedef UInt16 CState;
    typedef UInt16 CExtra;

    typedef struct
    {
        UInt32 price;
        CState state;
        CExtra extra;
        // 0   : normal
        // 1   : LIT : MATCH
        // > 1 : MATCH (extra-1) : LIT : REP0 (len)
        UInt32 len;
        UInt32 dist;
        UInt32 reps[LZMA_NUM_REPS];
    } COptimal;

// 18.06
#define kNumOpts (1 << 11)
#define kPackReserve (kNumOpts * 8)
    // #define kNumOpts (1 << 12)
    // #define kPackReserve (1 + kNumOpts * 2)

#define kNumLenToPosStates 4
#define kNumPosSlotBits 6
// #define kDicLogSizeMin 0
#define kDicLogSizeMax 32
#define kDistTableSizeMax (kDicLogSizeMax * 2)

#define kNumAlignBits 4
#define kAlignTableSize (1 << kNumAlignBits)
#define kAlignMask (kAlignTableSize - 1)

#define kStartPosModelIndex 4
#define kEndPosModelIndex 14
#define kNumFullDistances (1 << (kEndPosModelIndex >> 1))

#define LZMA_PB_MAX 4
#define LZMA_LC_MAX 8
#define LZMA_LP_MAX 4

#define LZMA_NUM_PB_STATES_MAX (1 << LZMA_PB_MAX)

#define kLenNumLowBits 3
#define kLenNumLowSymbols (1 << kLenNumLowBits)
#define kLenNumHighBits 8
#define kLenNumHighSymbols (1 << kLenNumHighBits)
#define kLenNumSymbolsTotal (kLenNumLowSymbols * 2 + kLenNumHighSymbols)

#define LZMA_MATCH_LEN_MIN 2
#define LZMA_MATCH_LEN_MAX (LZMA_MATCH_LEN_MIN + kLenNumSymbolsTotal - 1)

#define kNumStates 12

    typedef struct
    {
        CLzmaProb low[LZMA_NUM_PB_STATES_MAX << (kLenNumLowBits + 1)];
        CLzmaProb high[kLenNumHighSymbols];
    } CLenEnc;

    typedef struct
    {
        unsigned tableSize;
        UInt32 prices[LZMA_NUM_PB_STATES_MAX][kLenNumSymbolsTotal];
        // UInt32 prices1[LZMA_NUM_PB_STATES_MAX][kLenNumLowSymbols * 2];
        // UInt32 prices2[kLenNumSymbolsTotal];
    } CLenPriceEnc;

#define GET_PRICE_LEN(p, posState, len) \
    ((p)->prices[posState][(size_t)(len)-LZMA_MATCH_LEN_MIN])

    /*
    #define GET_PRICE_LEN(p, posState, len) \
        ((p)->prices2[(size_t)(len) - 2] + ((p)->prices1[posState][((len) - 2) & (kLenNumLowSymbols * 2 - 1)] & (((len) - 2 - kLenNumLowSymbols * 2) >> 9)))
    */

    typedef struct
    {
        UInt32 range;
        unsigned cache;
        UInt64 low;
        UInt64 cacheSize;
        Byte *buf;
        Byte *bufLim;
        Byte *bufBase;
        ISeqOutStream *outStream;
        UInt64 processed;
        SRes res;
    } CRangeEnc;

    typedef struct
    {
        CLzmaProb *litProbs;

        unsigned state;
        UInt32 reps[LZMA_NUM_REPS];

        CLzmaProb posAlignEncoder[1 << kNumAlignBits];
        CLzmaProb isRep[kNumStates];
        CLzmaProb isRepG0[kNumStates];
        CLzmaProb isRepG1[kNumStates];
        CLzmaProb isRepG2[kNumStates];
        CLzmaProb isMatch[kNumStates][LZMA_NUM_PB_STATES_MAX];
        CLzmaProb isRep0Long[kNumStates][LZMA_NUM_PB_STATES_MAX];

        CLzmaProb posSlotEncoder[kNumLenToPosStates][1 << kNumPosSlotBits];
        CLzmaProb posEncoders[kNumFullDistances];

        CLenEnc lenProbs;
        CLenEnc repLenProbs;

    } CSaveState;

    typedef UInt32 CProbPrice;

    typedef struct
    {
        void *matchFinderObj;
        IMatchFinder2 matchFinder;

        unsigned optCur;
        unsigned optEnd;

        unsigned longestMatchLen;
        unsigned numPairs;
        UInt32 numAvail;

        unsigned state;
        unsigned numFastBytes;
        unsigned additionalOffset;
        UInt32 reps[LZMA_NUM_REPS];
        unsigned lpMask, pbMask;
        CLzmaProb *litProbs;
        CRangeEnc rc;

        UInt32 backRes;

        unsigned lc, lp, pb;
        unsigned lclp;

        BoolInt fastMode;
        BoolInt writeEndMark;
        BoolInt finished;
        BoolInt multiThread;
        BoolInt needInit;
        // BoolInt _maxMode;

        UInt64 nowPos64;

        unsigned matchPriceCount;
        // unsigned alignPriceCount;
        int repLenEncCounter;

        unsigned distTableSize;

        UInt32 dictSize;
        SRes result;

#ifndef _7ZIP_ST
        BoolInt mtMode;
        // begin of CMatchFinderMt is used in LZ thread
        CMatchFinderMt matchFinderMt;
// end of CMatchFinderMt is used in BT and HASH threads
// #else
// CMatchFinder matchFinderBase;
#endif
        CMatchFinder matchFinderBase;

        // we suppose that we have 8-bytes alignment after CMatchFinder

#ifndef _7ZIP_ST
        Byte pad[128];
#endif

        // LZ thread
        CProbPrice ProbPrices[kBitModelTotal >> kNumMoveReducingBits];

        // we want {len , dist} pairs to be 8-bytes aligned in matches array
        UInt32 matches[LZMA_MATCH_LEN_MAX * 2 + 2];

        // we want 8-bytes alignment here
        UInt32 alignPrices[kAlignTableSize];
        UInt32 posSlotPrices[kNumLenToPosStates][kDistTableSizeMax];
        UInt32 distancesPrices[kNumLenToPosStates][kNumFullDistances];

        CLzmaProb posAlignEncoder[1 << kNumAlignBits];
        CLzmaProb isRep[kNumStates];
        CLzmaProb isRepG0[kNumStates];
        CLzmaProb isRepG1[kNumStates];
        CLzmaProb isRepG2[kNumStates];
        CLzmaProb isMatch[kNumStates][LZMA_NUM_PB_STATES_MAX];
        CLzmaProb isRep0Long[kNumStates][LZMA_NUM_PB_STATES_MAX];
        CLzmaProb posSlotEncoder[kNumLenToPosStates][1 << kNumPosSlotBits];
        CLzmaProb posEncoders[kNumFullDistances];

        CLenEnc lenProbs;
        CLenEnc repLenProbs;

#ifndef LZMA_LOG_BSR
        Byte g_FastPos[1 << kNumLogBits];
#endif

        CLenPriceEnc lenEnc;
        CLenPriceEnc repLenEnc;

        COptimal opt[kNumOpts];

        CSaveState saveState;

// BoolInt mf_Failure;
#ifndef _7ZIP_ST
        Byte pad2[128];
#endif
    } CLzmaEnc;

#define MFB (p->matchFinderBase)
    /*
    #ifndef _7ZIP_ST
    #define MFB (p->matchFinderMt.MatchFinder)
    #endif
    */

#define COPY_ARR(dest, src, arr) memcpy(dest->arr, src->arr, sizeof(src->arr));

    void LzmaEnc_SaveState(CLzmaEncHandle pp)
    {
        CLzmaEnc *p = (CLzmaEnc *)pp;
        CSaveState *dest = &p->saveState;

        dest->state = p->state;

        dest->lenProbs = p->lenProbs;
        dest->repLenProbs = p->repLenProbs;

        COPY_ARR(dest, p, reps);

        COPY_ARR(dest, p, posAlignEncoder);
        COPY_ARR(dest, p, isRep);
        COPY_ARR(dest, p, isRepG0);
        COPY_ARR(dest, p, isRepG1);
        COPY_ARR(dest, p, isRepG2);
        COPY_ARR(dest, p, isMatch);
        COPY_ARR(dest, p, isRep0Long);
        COPY_ARR(dest, p, posSlotEncoder);
        COPY_ARR(dest, p, posEncoders);

        memcpy(dest->litProbs, p->litProbs, ((UInt32)0x300 << p->lclp) * sizeof(CLzmaProb));
    }

    void LzmaEnc_RestoreState(CLzmaEncHandle pp)
    {
        CLzmaEnc *dest = (CLzmaEnc *)pp;
        const CSaveState *p = &dest->saveState;

        dest->state = p->state;

        dest->lenProbs = p->lenProbs;
        dest->repLenProbs = p->repLenProbs;

        COPY_ARR(dest, p, reps);

        COPY_ARR(dest, p, posAlignEncoder);
        COPY_ARR(dest, p, isRep);
        COPY_ARR(dest, p, isRepG0);
        COPY_ARR(dest, p, isRepG1);
        COPY_ARR(dest, p, isRepG2);
        COPY_ARR(dest, p, isMatch);
        COPY_ARR(dest, p, isRep0Long);
        COPY_ARR(dest, p, posSlotEncoder);
        COPY_ARR(dest, p, posEncoders);

        memcpy(dest->litProbs, p->litProbs, ((UInt32)0x300 << dest->lclp) * sizeof(CLzmaProb));
    }

    SRes LzmaEnc_SetProps(CLzmaEncHandle pp, const CLzmaEncProps *props2)
    {
        CLzmaEnc *p = (CLzmaEnc *)pp;
        CLzmaEncProps props = *props2;
        LzmaEncProps_Normalize(&props);

        if (props.lc > LZMA_LC_MAX || props.lp > LZMA_LP_MAX || props.pb > LZMA_PB_MAX)
            return SZ_ERROR_PARAM;

        if (props.dictSize > kLzmaMaxHistorySize)
            props.dictSize = kLzmaMaxHistorySize;

#ifndef LZMA_LOG_BSR
        {
            const UInt64 dict64 = props.dictSize;
            if (dict64 > ((UInt64)1 << kDicLogSizeMaxCompress))
                return SZ_ERROR_PARAM;
        }
#endif

        p->dictSize = props.dictSize;
        {
            unsigned fb = (unsigned)props.fb;
            if (fb < 5)
                fb = 5;
            if (fb > LZMA_MATCH_LEN_MAX)
                fb = LZMA_MATCH_LEN_MAX;
            p->numFastBytes = fb;
        }
        p->lc = (unsigned)props.lc;
        p->lp = (unsigned)props.lp;
        p->pb = (unsigned)props.pb;
        p->fastMode = (props.algo == 0);
        // p->_maxMode = True;
        MFB.btMode = (Byte)(props.btMode ? 1 : 0);
        {
            unsigned numHashBytes = 4;
            if (props.btMode)
            {
                if (props.numHashBytes < 2)
                    numHashBytes = 2;
                else if (props.numHashBytes < 4)
                    numHashBytes = (unsigned)props.numHashBytes;
            }
            if (props.numHashBytes >= 5)
                numHashBytes = 5;

            MFB.numHashBytes = numHashBytes;
        }

        MFB.cutValue = props.mc;

        p->writeEndMark = (BoolInt)props.writeEndMark;

#ifndef _7ZIP_ST
        /*
        if (newMultiThread != _multiThread)
        {
          ReleaseMatchFinder();
          _multiThread = newMultiThread;
        }
        */
        p->multiThread = (props.numThreads > 1);
        p->matchFinderMt.btSync.affinity =
            p->matchFinderMt.hashSync.affinity = props.affinity;
#endif

        return SZ_OK;
    }

    void LzmaEnc_SetDataSize(CLzmaEncHandle pp, UInt64 expectedDataSiize)
    {
        CLzmaEnc *p = (CLzmaEnc *)pp;
        MFB.expectedDataSize = expectedDataSiize;
    }

#define kState_Start 0
#define kState_LitAfterMatch 4
#define kState_LitAfterRep 5
#define kState_MatchAfterLit 7
#define kState_RepAfterLit 8

    static const Byte kLiteralNextStates[kNumStates] = {0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 4, 5};
    static const Byte kMatchNextStates[kNumStates] = {7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10};
    static const Byte kRepNextStates[kNumStates] = {8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11};
    static const Byte kShortRepNextStates[kNumStates] = {9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11};

#define IsLitState(s) ((s) < 7)
#define GetLenToPosState2(len) (((len) < kNumLenToPosStates - 1) ? (len) : kNumLenToPosStates - 1)
#define GetLenToPosState(len) (((len) < kNumLenToPosStates + 1) ? (len)-2 : kNumLenToPosStates - 1)

#define kInfinityPrice (1 << 30)

    static void RangeEnc_Construct(CRangeEnc * p)
    {
        p->outStream = NULL;
        p->bufBase = NULL;
    }

#define RangeEnc_GetProcessed(p) ((p)->processed + (size_t)((p)->buf - (p)->bufBase) + (p)->cacheSize)
#define RangeEnc_GetProcessed_sizet(p) ((size_t)(p)->processed + (size_t)((p)->buf - (p)->bufBase) + (size_t)(p)->cacheSize)

#define RC_BUF_SIZE (1 << 16)

    static int RangeEnc_Alloc(CRangeEnc * p, ISzAllocPtr alloc)
    {
        if (!p->bufBase)
        {
            p->bufBase = (Byte *)ISzAlloc_Alloc(alloc, RC_BUF_SIZE);
            if (!p->bufBase)
                return 0;
            p->bufLim = p->bufBase + RC_BUF_SIZE;
        }
        return 1;
    }

    static void RangeEnc_Free(CRangeEnc * p, ISzAllocPtr alloc)
    {
        ISzAlloc_Free(alloc, p->bufBase);
        p->bufBase = NULL;
    }

    static void RangeEnc_Init(CRangeEnc * p)
    {
        p->range = 0xFFFFFFFF;
        p->cache = 0;
        p->low = 0;
        p->cacheSize = 0;

        p->buf = p->bufBase;

        p->processed = 0;
        p->res = SZ_OK;
    }

    MY_NO_INLINE static void RangeEnc_FlushStream(CRangeEnc * p)
    {
        const size_t num = (size_t)(p->buf - p->bufBase);
        if (p->res == SZ_OK)
        {
            if (num != ISeqOutStream_Write(p->outStream, p->bufBase, num))
                p->res = SZ_ERROR_WRITE;
        }
        p->processed += num;
        p->buf = p->bufBase;
    }

    MY_NO_INLINE static void MY_FAST_CALL RangeEnc_ShiftLow(CRangeEnc * p)
    {
        UInt32 low = (UInt32)p->low;
        unsigned high = (unsigned)(p->low >> 32);
        p->low = (UInt32)(low << 8);
        if (low < (UInt32)0xFF000000 || high != 0)
        {
            {
                Byte *buf = p->buf;
                *buf++ = (Byte)(p->cache + high);
                p->cache = (unsigned)(low >> 24);
                p->buf = buf;
                if (buf == p->bufLim)
                    RangeEnc_FlushStream(p);
                if (p->cacheSize == 0)
                    return;
            }
            high += 0xFF;
            for (;;)
            {
                Byte *buf = p->buf;
                *buf++ = (Byte)(high);
                p->buf = buf;
                if (buf == p->bufLim)
                    RangeEnc_FlushStream(p);
                if (--p->cacheSize == 0)
                    return;
            }
        }
        p->cacheSize++;
    }

    static void RangeEnc_FlushData(CRangeEnc * p)
    {
        int i;
        for (i = 0; i < 5; i++)
            RangeEnc_ShiftLow(p);
    }

#define RC_NORM(p)            \
    if (range < kTopValue)    \
    {                         \
        range <<= 8;          \
        RangeEnc_ShiftLow(p); \
    }

#define RC_BIT_PRE(p, prob) \
    ttt = *(prob);          \
    newBound = (range >> kNumBitModelTotalBits) * ttt;

    // #define _LZMA_ENC_USE_BRANCH

#ifdef _LZMA_ENC_USE_BRANCH

#define RC_BIT(p, prob, bit)                               \
    {                                                      \
        RC_BIT_PRE(p, prob)                                \
        if (bit == 0)                                      \
        {                                                  \
            range = newBound;                              \
            ttt += (kBitModelTotal - ttt) >> kNumMoveBits; \
        }                                                  \
        else                                               \
        {                                                  \
            (p)->low += newBound;                          \
            range -= newBound;                             \
            ttt -= ttt >> kNumMoveBits;                    \
        }                                                  \
        *(prob) = (CLzmaProb)ttt;                          \
        RC_NORM(p)                                         \
    }

#else

#define RC_BIT(p, prob, bit)                                  \
    {                                                         \
        UInt32 mask;                                          \
        RC_BIT_PRE(p, prob)                                   \
        mask = 0 - (UInt32)bit;                               \
        range &= mask;                                        \
        mask &= newBound;                                     \
        range -= mask;                                        \
        (p)->low += mask;                                     \
        mask = (UInt32)bit - 1;                               \
        range += newBound & mask;                             \
        mask &= (kBitModelTotal - ((1 << kNumMoveBits) - 1)); \
        mask += ((1 << kNumMoveBits) - 1);                    \
        ttt += (UInt32)((Int32)(mask - ttt) >> kNumMoveBits); \
        *(prob) = (CLzmaProb)ttt;                             \
        RC_NORM(p)                                            \
    }

#endif

#define RC_BIT_0_BASE(p, prob) \
    range = newBound;          \
    *(prob) = (CLzmaProb)(ttt + ((kBitModelTotal - ttt) >> kNumMoveBits));

#define RC_BIT_1_BASE(p, prob) \
    range -= newBound;         \
    (p)->low += newBound;      \
    *(prob) = (CLzmaProb)(ttt - (ttt >> kNumMoveBits));

#define RC_BIT_0(p, prob)  \
    RC_BIT_0_BASE(p, prob) \
    RC_NORM(p)

#define RC_BIT_1(p, prob)  \
    RC_BIT_1_BASE(p, prob) \
    RC_NORM(p)

    static void RangeEnc_EncodeBit_0(CRangeEnc * p, CLzmaProb * prob)
    {
        UInt32 range, ttt, newBound;
        range = p->range;
        RC_BIT_PRE(p, prob)
        RC_BIT_0(p, prob)
        p->range = range;
    }

    static void LitEnc_Encode(CRangeEnc * p, CLzmaProb * probs, UInt32 sym)
    {
        UInt32 range = p->range;
        sym |= 0x100;
        do
        {
            UInt32 ttt, newBound;
            // RangeEnc_EncodeBit(p, probs + (sym >> 8), (sym >> 7) & 1);
            CLzmaProb *prob = probs + (sym >> 8);
            UInt32 bit = (sym >> 7) & 1;
            sym <<= 1;
            RC_BIT(p, prob, bit);
        } while (sym < 0x10000);
        p->range = range;
    }

    static void LitEnc_EncodeMatched(CRangeEnc * p, CLzmaProb * probs, UInt32 sym, UInt32 matchByte)
    {
        UInt32 range = p->range;
        UInt32 offs = 0x100;
        sym |= 0x100;
        do
        {
            UInt32 ttt, newBound;
            CLzmaProb *prob;
            UInt32 bit;
            matchByte <<= 1;
            // RangeEnc_EncodeBit(p, probs + (offs + (matchByte & offs) + (sym >> 8)), (sym >> 7) & 1);
            prob = probs + (offs + (matchByte & offs) + (sym >> 8));
            bit = (sym >> 7) & 1;
            sym <<= 1;
            offs &= ~(matchByte ^ sym);
            RC_BIT(p, prob, bit);
        } while (sym < 0x10000);
        p->range = range;
    }

    static void LzmaEnc_InitPriceTables(CProbPrice * ProbPrices)
    {
        UInt32 i;
        for (i = 0; i < (kBitModelTotal >> kNumMoveReducingBits); i++)
        {
            const unsigned kCyclesBits = kNumBitPriceShiftBits;
            UInt32 w = (i << kNumMoveReducingBits) + (1 << (kNumMoveReducingBits - 1));
            unsigned bitCount = 0;
            unsigned j;
            for (j = 0; j < kCyclesBits; j++)
            {
                w = w * w;
                bitCount <<= 1;
                while (w >= ((UInt32)1 << 16))
                {
                    w >>= 1;
                    bitCount++;
                }
            }
            ProbPrices[i] = (CProbPrice)(((unsigned)kNumBitModelTotalBits << kCyclesBits) - 15 - bitCount);
            // printf("\n%3d: %5d", i, ProbPrices[i]);
        }
    }

#define GET_PRICE(prob, bit) \
    p->ProbPrices[((prob) ^ (unsigned)(((-(int)(bit))) & (kBitModelTotal - 1))) >> kNumMoveReducingBits];

#define GET_PRICEa(prob, bit) \
    ProbPrices[((prob) ^ (unsigned)((-((int)(bit))) & (kBitModelTotal - 1))) >> kNumMoveReducingBits];

#define GET_PRICE_0(prob) p->ProbPrices[(prob) >> kNumMoveReducingBits]
#define GET_PRICE_1(prob) p->ProbPrices[((prob) ^ (kBitModelTotal - 1)) >> kNumMoveReducingBits]

#define GET_PRICEa_0(prob) ProbPrices[(prob) >> kNumMoveReducingBits]
#define GET_PRICEa_1(prob) ProbPrices[((prob) ^ (kBitModelTotal - 1)) >> kNumMoveReducingBits]

    static UInt32 LitEnc_GetPrice(const CLzmaProb *probs, UInt32 sym, const CProbPrice *ProbPrices)
    {
        UInt32 price = 0;
        sym |= 0x100;
        do
        {
            unsigned bit = sym & 1;
            sym >>= 1;
            price += GET_PRICEa(probs[sym], bit);
        } while (sym >= 2);
        return price;
    }

    static UInt32 LitEnc_Matched_GetPrice(const CLzmaProb *probs, UInt32 sym, UInt32 matchByte, const CProbPrice *ProbPrices)
    {
        UInt32 price = 0;
        UInt32 offs = 0x100;
        sym |= 0x100;
        do
        {
            matchByte <<= 1;
            price += GET_PRICEa(probs[offs + (matchByte & offs) + (sym >> 8)], (sym >> 7) & 1);
            sym <<= 1;
            offs &= ~(matchByte ^ sym);
        } while (sym < 0x10000);
        return price;
    }

    static void RcTree_ReverseEncode(CRangeEnc * rc, CLzmaProb * probs, unsigned numBits, unsigned sym)
    {
        UInt32 range = rc->range;
        unsigned m = 1;
        do
        {
            UInt32 ttt, newBound;
            unsigned bit = sym & 1;
            // RangeEnc_EncodeBit(rc, probs + m, bit);
            sym >>= 1;
            RC_BIT(rc, probs + m, bit);
            m = (m << 1) | bit;
        } while (--numBits);
        rc->range = range;
    }

    static void LenEnc_Init(CLenEnc * p)
    {
        unsigned i;
        for (i = 0; i < (LZMA_NUM_PB_STATES_MAX << (kLenNumLowBits + 1)); i++)
            p->low[i] = kProbInitValue;
        for (i = 0; i < kLenNumHighSymbols; i++)
            p->high[i] = kProbInitValue;
    }

    static void LenEnc_Encode(CLenEnc * p, CRangeEnc * rc, unsigned sym, unsigned posState)
    {
        UInt32 range, ttt, newBound;
        CLzmaProb *probs = p->low;
        range = rc->range;
        RC_BIT_PRE(rc, probs);
        if (sym >= kLenNumLowSymbols)
        {
            RC_BIT_1(rc, probs);
            probs += kLenNumLowSymbols;
            RC_BIT_PRE(rc, probs);
            if (sym >= kLenNumLowSymbols * 2)
            {
                RC_BIT_1(rc, probs);
                rc->range = range;
                // RcTree_Encode(rc, p->high, kLenNumHighBits, sym - kLenNumLowSymbols * 2);
                LitEnc_Encode(rc, p->high, sym - kLenNumLowSymbols * 2);
                return;
            }
            sym -= kLenNumLowSymbols;
        }

        // RcTree_Encode(rc, probs + (posState << kLenNumLowBits), kLenNumLowBits, sym);
        {
            unsigned m;
            unsigned bit;
            RC_BIT_0(rc, probs);
            probs += (posState << (1 + kLenNumLowBits));
            bit = (sym >> 2);
            RC_BIT(rc, probs + 1, bit);
            m = (1 << 1) + bit;
            bit = (sym >> 1) & 1;
            RC_BIT(rc, probs + m, bit);
            m = (m << 1) + bit;
            bit = sym & 1;
            RC_BIT(rc, probs + m, bit);
            rc->range = range;
        }
    }

    static void SetPrices_3(const CLzmaProb *probs, UInt32 startPrice, UInt32 *prices, const CProbPrice *ProbPrices)
    {
        unsigned i;
        for (i = 0; i < 8; i += 2)
        {
            UInt32 price = startPrice;
            UInt32 prob;
            price += GET_PRICEa(probs[1], (i >> 2));
            price += GET_PRICEa(probs[2 + (i >> 2)], (i >> 1) & 1);
            prob = probs[4 + (i >> 1)];
            prices[i] = price + GET_PRICEa_0(prob);
            prices[i + 1] = price + GET_PRICEa_1(prob);
        }
    }

    MY_NO_INLINE static void MY_FAST_CALL LenPriceEnc_UpdateTables(
        CLenPriceEnc * p,
        unsigned numPosStates,
        const CLenEnc *enc,
        const CProbPrice *ProbPrices)
    {
        UInt32 b;

        {
            unsigned prob = enc->low[0];
            UInt32 a, c;
            unsigned posState;
            b = GET_PRICEa_1(prob);
            a = GET_PRICEa_0(prob);
            c = b + GET_PRICEa_0(enc->low[kLenNumLowSymbols]);
            for (posState = 0; posState < numPosStates; posState++)
            {
                UInt32 *prices = p->prices[posState];
                const CLzmaProb *probs = enc->low + (posState << (1 + kLenNumLowBits));
                SetPrices_3(probs, a, prices, ProbPrices);
                SetPrices_3(probs + kLenNumLowSymbols, c, prices + kLenNumLowSymbols, ProbPrices);
            }
        }

        /*
        {
          unsigned i;
          UInt32 b;
          a = GET_PRICEa_0(enc->low[0]);
          for (i = 0; i < kLenNumLowSymbols; i++)
            p->prices2[i] = a;
          a = GET_PRICEa_1(enc->low[0]);
          b = a + GET_PRICEa_0(enc->low[kLenNumLowSymbols]);
          for (i = kLenNumLowSymbols; i < kLenNumLowSymbols * 2; i++)
            p->prices2[i] = b;
          a += GET_PRICEa_1(enc->low[kLenNumLowSymbols]);
        }
        */

        // p->counter = numSymbols;
        // p->counter = 64;

        {
            unsigned i = p->tableSize;

            if (i > kLenNumLowSymbols * 2)
            {
                const CLzmaProb *probs = enc->high;
                UInt32 *prices = p->prices[0] + kLenNumLowSymbols * 2;
                i -= kLenNumLowSymbols * 2 - 1;
                i >>= 1;
                b += GET_PRICEa_1(enc->low[kLenNumLowSymbols]);
                do
                {
                    /*
                    p->prices2[i] = a +
                    // RcTree_GetPrice(enc->high, kLenNumHighBits, i - kLenNumLowSymbols * 2, ProbPrices);
                    LitEnc_GetPrice(probs, i - kLenNumLowSymbols * 2, ProbPrices);
                    */
                    // UInt32 price = a + RcTree_GetPrice(probs, kLenNumHighBits - 1, sym, ProbPrices);
                    unsigned sym = --i + (1 << (kLenNumHighBits - 1));
                    UInt32 price = b;
                    do
                    {
                        unsigned bit = sym & 1;
                        sym >>= 1;
                        price += GET_PRICEa(probs[sym], bit);
                    } while (sym >= 2);

                    {
                        unsigned prob = probs[(size_t)i + (1 << (kLenNumHighBits - 1))];
                        prices[(size_t)i * 2] = price + GET_PRICEa_0(prob);
                        prices[(size_t)i * 2 + 1] = price + GET_PRICEa_1(prob);
                    }
                } while (i);

                {
                    unsigned posState;
                    size_t num = (p->tableSize - kLenNumLowSymbols * 2) * sizeof(p->prices[0][0]);
                    for (posState = 1; posState < numPosStates; posState++)
                        memcpy(p->prices[posState] + kLenNumLowSymbols * 2, p->prices[0] + kLenNumLowSymbols * 2, num);
                }
            }
        }
    }

    /*
      #ifdef SHOW_STAT
      g_STAT_OFFSET += num;
      printf("\n MovePos %u", num);
      #endif
    */

#define MOVE_POS_2(p, num)                                     \
    {                                                          \
        p->additionalOffset += (num);                          \
        p->matchFinder.Skip(p->matchFinderObj, (UInt32)(num)); \
    }

    static unsigned ReadMatchDistances(CLzmaEnc * p, unsigned *numPairsRes)
    {
        unsigned numPairs;

        p->additionalOffset++;
        p->numAvail = p->matchFinder.GetNumAvailableBytes(p->matchFinderObj);
        {
            const UInt32 *d = p->matchFinder.GetMatches(p->matchFinderObj, p->matches);
            // if (!d) { p->mf_Failure = True; *numPairsRes = 0;  return 0; }
            numPairs = (unsigned)(d - p->matches);
        }
        *numPairsRes = numPairs;

#ifdef SHOW_STAT
        printf("\n i = %u numPairs = %u    ", g_STAT_OFFSET, numPairs / 2);
        g_STAT_OFFSET++;
        {
            unsigned i;
            for (i = 0; i < numPairs; i += 2)
                printf("%2u %6u   | ", p->matches[i], p->matches[i + 1]);
        }
#endif

        if (numPairs == 0)
            return 0;
        {
            const unsigned len = p->matches[(size_t)numPairs - 2];
            if (len != p->numFastBytes)
                return len;
            {
                UInt32 numAvail = p->numAvail;
                if (numAvail > LZMA_MATCH_LEN_MAX)
                    numAvail = LZMA_MATCH_LEN_MAX;
                {
                    const Byte *p1 = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;
                    const Byte *p2 = p1 + len;
                    const ptrdiff_t dif = (ptrdiff_t)-1 - (ptrdiff_t)p->matches[(size_t)numPairs - 1];
                    const Byte *lim = p1 + numAvail;
                    for (; p2 != lim && *p2 == p2[dif]; p2++)
                    {
                    }
                    return (unsigned)(p2 - p1);
                }
            }
        }
    }

#define MARK_LIT ((UInt32)(Int32)-1)

#define MakeAs_Lit(p)         \
    {                         \
        (p)->dist = MARK_LIT; \
        (p)->extra = 0;       \
    }
#define MakeAs_ShortRep(p) \
    {                      \
        (p)->dist = 0;     \
        (p)->extra = 0;    \
    }
#define IsShortRep(p) ((p)->dist == 0)

#define GetPrice_ShortRep(p, state, posState) \
    (GET_PRICE_0(p->isRepG0[state]) + GET_PRICE_0(p->isRep0Long[state][posState]))

#define GetPrice_Rep_0(p, state, posState) (                                                                                             \
                                               GET_PRICE_1(p->isMatch[state][posState]) + GET_PRICE_1(p->isRep0Long[state][posState])) + \
                                               GET_PRICE_1(p->isRep[state]) + GET_PRICE_0(p->isRepG0[state])

    MY_FORCE_INLINE
    static UInt32 GetPrice_PureRep(const CLzmaEnc *p, unsigned repIndex, size_t state, size_t posState)
    {
        UInt32 price;
        UInt32 prob = p->isRepG0[state];
        if (repIndex == 0)
        {
            price = GET_PRICE_0(prob);
            price += GET_PRICE_1(p->isRep0Long[state][posState]);
        }
        else
        {
            price = GET_PRICE_1(prob);
            prob = p->isRepG1[state];
            if (repIndex == 1)
                price += GET_PRICE_0(prob);
            else
            {
                price += GET_PRICE_1(prob);
                price += GET_PRICE(p->isRepG2[state], repIndex - 2);
            }
        }
        return price;
    }

    static unsigned Backward(CLzmaEnc * p, unsigned cur)
    {
        unsigned wr = cur + 1;
        p->optEnd = wr;

        for (;;)
        {
            UInt32 dist = p->opt[cur].dist;
            unsigned len = (unsigned)p->opt[cur].len;
            unsigned extra = (unsigned)p->opt[cur].extra;
            cur -= len;

            if (extra)
            {
                wr--;
                p->opt[wr].len = (UInt32)len;
                cur -= extra;
                len = extra;
                if (extra == 1)
                {
                    p->opt[wr].dist = dist;
                    dist = MARK_LIT;
                }
                else
                {
                    p->opt[wr].dist = 0;
                    len--;
                    wr--;
                    p->opt[wr].dist = MARK_LIT;
                    p->opt[wr].len = 1;
                }
            }

            if (cur == 0)
            {
                p->backRes = dist;
                p->optCur = wr;
                return len;
            }

            wr--;
            p->opt[wr].dist = dist;
            p->opt[wr].len = (UInt32)len;
        }
    }

#define LIT_PROBS(pos, prevByte) \
    (p->litProbs + (UInt32)3 * (((((pos) << 8) + (prevByte)) & p->lpMask) << p->lc))

    static unsigned GetOptimum(CLzmaEnc * p, UInt32 position)
    {
        unsigned last, cur;
        UInt32 reps[LZMA_NUM_REPS];
        unsigned repLens[LZMA_NUM_REPS];
        UInt32 *matches;

        {
            UInt32 numAvail;
            unsigned numPairs, mainLen, repMaxIndex, i, posState;
            UInt32 matchPrice, repMatchPrice;
            const Byte *data;
            Byte curByte, matchByte;

            p->optCur = p->optEnd = 0;

            if (p->additionalOffset == 0)
                mainLen = ReadMatchDistances(p, &numPairs);
            else
            {
                mainLen = p->longestMatchLen;
                numPairs = p->numPairs;
            }

            numAvail = p->numAvail;
            if (numAvail < 2)
            {
                p->backRes = MARK_LIT;
                return 1;
            }
            if (numAvail > LZMA_MATCH_LEN_MAX)
                numAvail = LZMA_MATCH_LEN_MAX;

            data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;
            repMaxIndex = 0;

            for (i = 0; i < LZMA_NUM_REPS; i++)
            {
                unsigned len;
                const Byte *data2;
                reps[i] = p->reps[i];
                data2 = data - reps[i];
                if (data[0] != data2[0] || data[1] != data2[1])
                {
                    repLens[i] = 0;
                    continue;
                }
                for (len = 2; len < numAvail && data[len] == data2[len]; len++)
                {
                }
                repLens[i] = len;
                if (len > repLens[repMaxIndex])
                    repMaxIndex = i;
                if (len == LZMA_MATCH_LEN_MAX) // 21.03 : optimization
                    break;
            }

            if (repLens[repMaxIndex] >= p->numFastBytes)
            {
                unsigned len;
                p->backRes = (UInt32)repMaxIndex;
                len = repLens[repMaxIndex];
                MOVE_POS_2(p, len - 1)
                return len;
            }

            matches = p->matches;
#define MATCHES matches
            // #define MATCHES  p->matches

            if (mainLen >= p->numFastBytes)
            {
                p->backRes = MATCHES[(size_t)numPairs - 1] + LZMA_NUM_REPS;
                MOVE_POS_2(p, mainLen - 1)
                return mainLen;
            }

            curByte = *data;
            matchByte = *(data - reps[0]);

            last = repLens[repMaxIndex];
            if (last <= mainLen)
                last = mainLen;

            if (last < 2 && curByte != matchByte)
            {
                p->backRes = MARK_LIT;
                return 1;
            }

            p->opt[0].state = (CState)p->state;

            posState = (position & p->pbMask);

            {
                const CLzmaProb *probs = LIT_PROBS(position, *(data - 1));
                p->opt[1].price = GET_PRICE_0(p->isMatch[p->state][posState]) +
                                  (!IsLitState(p->state) ? LitEnc_Matched_GetPrice(probs, curByte, matchByte, p->ProbPrices) : LitEnc_GetPrice(probs, curByte, p->ProbPrices));
            }

            MakeAs_Lit(&p->opt[1]);

            matchPrice = GET_PRICE_1(p->isMatch[p->state][posState]);
            repMatchPrice = matchPrice + GET_PRICE_1(p->isRep[p->state]);

            // 18.06
            if (matchByte == curByte && repLens[0] == 0)
            {
                UInt32 shortRepPrice = repMatchPrice + GetPrice_ShortRep(p, p->state, posState);
                if (shortRepPrice < p->opt[1].price)
                {
                    p->opt[1].price = shortRepPrice;
                    MakeAs_ShortRep(&p->opt[1]);
                }
                if (last < 2)
                {
                    p->backRes = p->opt[1].dist;
                    return 1;
                }
            }

            p->opt[1].len = 1;

            p->opt[0].reps[0] = reps[0];
            p->opt[0].reps[1] = reps[1];
            p->opt[0].reps[2] = reps[2];
            p->opt[0].reps[3] = reps[3];

            // ---------- REP ----------

            for (i = 0; i < LZMA_NUM_REPS; i++)
            {
                unsigned repLen = repLens[i];
                UInt32 price;
                if (repLen < 2)
                    continue;
                price = repMatchPrice + GetPrice_PureRep(p, i, p->state, posState);
                do
                {
                    UInt32 price2 = price + GET_PRICE_LEN(&p->repLenEnc, posState, repLen);
                    COptimal *opt = &p->opt[repLen];
                    if (price2 < opt->price)
                    {
                        opt->price = price2;
                        opt->len = (UInt32)repLen;
                        opt->dist = (UInt32)i;
                        opt->extra = 0;
                    }
                } while (--repLen >= 2);
            }

            // ---------- MATCH ----------
            {
                unsigned len = repLens[0] + 1;
                if (len <= mainLen)
                {
                    unsigned offs = 0;
                    UInt32 normalMatchPrice = matchPrice + GET_PRICE_0(p->isRep[p->state]);

                    if (len < 2)
                        len = 2;
                    else
                        while (len > MATCHES[offs])
                            offs += 2;

                    for (;; len++)
                    {
                        COptimal *opt;
                        UInt32 dist = MATCHES[(size_t)offs + 1];
                        UInt32 price = normalMatchPrice + GET_PRICE_LEN(&p->lenEnc, posState, len);
                        unsigned lenToPosState = GetLenToPosState(len);

                        if (dist < kNumFullDistances)
                            price += p->distancesPrices[lenToPosState][dist & (kNumFullDistances - 1)];
                        else
                        {
                            unsigned slot;
                            GetPosSlot2(dist, slot);
                            price += p->alignPrices[dist & kAlignMask];
                            price += p->posSlotPrices[lenToPosState][slot];
                        }

                        opt = &p->opt[len];

                        if (price < opt->price)
                        {
                            opt->price = price;
                            opt->len = (UInt32)len;
                            opt->dist = dist + LZMA_NUM_REPS;
                            opt->extra = 0;
                        }

                        if (len == MATCHES[offs])
                        {
                            offs += 2;
                            if (offs == numPairs)
                                break;
                        }
                    }
                }
            }

            cur = 0;

#ifdef SHOW_STAT2
            /* if (position >= 0) */
            {
                unsigned i;
                printf("\n pos = %4X", position);
                for (i = cur; i <= last; i++)
                    printf("\nprice[%4X] = %u", position - cur + i, p->opt[i].price);
            }
#endif
        }

        // ---------- Optimal Parsing ----------

        for (;;)
        {
            unsigned numAvail;
            UInt32 numAvailFull;
            unsigned newLen, numPairs, prev, state, posState, startLen;
            UInt32 litPrice, matchPrice, repMatchPrice;
            BoolInt nextIsLit;
            Byte curByte, matchByte;
            const Byte *data;
            COptimal *curOpt, *nextOpt;

            if (++cur == last)
                break;

            // 18.06
            if (cur >= kNumOpts - 64)
            {
                unsigned j, best;
                UInt32 price = p->opt[cur].price;
                best = cur;
                for (j = cur + 1; j <= last; j++)
                {
                    UInt32 price2 = p->opt[j].price;
                    if (price >= price2)
                    {
                        price = price2;
                        best = j;
                    }
                }
                {
                    unsigned delta = best - cur;
                    if (delta != 0)
                    {
                        MOVE_POS_2(p, delta);
                    }
                }
                cur = best;
                break;
            }

            newLen = ReadMatchDistances(p, &numPairs);

            if (newLen >= p->numFastBytes)
            {
                p->numPairs = numPairs;
                p->longestMatchLen = newLen;
                break;
            }

            curOpt = &p->opt[cur];

            position++;

            // we need that check here, if skip_items in p->opt are possible
            /*
            if (curOpt->price >= kInfinityPrice)
              continue;
            */

            prev = cur - curOpt->len;

            if (curOpt->len == 1)
            {
                state = (unsigned)p->opt[prev].state;
                if (IsShortRep(curOpt))
                    state = kShortRepNextStates[state];
                else
                    state = kLiteralNextStates[state];
            }
            else
            {
                const COptimal *prevOpt;
                UInt32 b0;
                UInt32 dist = curOpt->dist;

                if (curOpt->extra)
                {
                    prev -= (unsigned)curOpt->extra;
                    state = kState_RepAfterLit;
                    if (curOpt->extra == 1)
                        state = (dist < LZMA_NUM_REPS ? kState_RepAfterLit : kState_MatchAfterLit);
                }
                else
                {
                    state = (unsigned)p->opt[prev].state;
                    if (dist < LZMA_NUM_REPS)
                        state = kRepNextStates[state];
                    else
                        state = kMatchNextStates[state];
                }

                prevOpt = &p->opt[prev];
                b0 = prevOpt->reps[0];

                if (dist < LZMA_NUM_REPS)
                {
                    if (dist == 0)
                    {
                        reps[0] = b0;
                        reps[1] = prevOpt->reps[1];
                        reps[2] = prevOpt->reps[2];
                        reps[3] = prevOpt->reps[3];
                    }
                    else
                    {
                        reps[1] = b0;
                        b0 = prevOpt->reps[1];
                        if (dist == 1)
                        {
                            reps[0] = b0;
                            reps[2] = prevOpt->reps[2];
                            reps[3] = prevOpt->reps[3];
                        }
                        else
                        {
                            reps[2] = b0;
                            reps[0] = prevOpt->reps[dist];
                            reps[3] = prevOpt->reps[dist ^ 1];
                        }
                    }
                }
                else
                {
                    reps[0] = (dist - LZMA_NUM_REPS + 1);
                    reps[1] = b0;
                    reps[2] = prevOpt->reps[1];
                    reps[3] = prevOpt->reps[2];
                }
            }

            curOpt->state = (CState)state;
            curOpt->reps[0] = reps[0];
            curOpt->reps[1] = reps[1];
            curOpt->reps[2] = reps[2];
            curOpt->reps[3] = reps[3];

            data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;
            curByte = *data;
            matchByte = *(data - reps[0]);

            posState = (position & p->pbMask);

            /*
            The order of Price checks:
               <  LIT
               <= SHORT_REP
               <  LIT : REP_0
               <  REP    [ : LIT : REP_0 ]
               <  MATCH  [ : LIT : REP_0 ]
            */

            {
                UInt32 curPrice = curOpt->price;
                unsigned prob = p->isMatch[state][posState];
                matchPrice = curPrice + GET_PRICE_1(prob);
                litPrice = curPrice + GET_PRICE_0(prob);
            }

            nextOpt = &p->opt[(size_t)cur + 1];
            nextIsLit = False;

            // here we can allow skip_items in p->opt, if we don't check (nextOpt->price < kInfinityPrice)
            // 18.new.06
            if ((nextOpt->price < kInfinityPrice
                 // && !IsLitState(state)
                 && matchByte == curByte) ||
                litPrice > nextOpt->price)
                litPrice = 0;
            else
            {
                const CLzmaProb *probs = LIT_PROBS(position, *(data - 1));
                litPrice += (!IsLitState(state) ? LitEnc_Matched_GetPrice(probs, curByte, matchByte, p->ProbPrices) : LitEnc_GetPrice(probs, curByte, p->ProbPrices));

                if (litPrice < nextOpt->price)
                {
                    nextOpt->price = litPrice;
                    nextOpt->len = 1;
                    MakeAs_Lit(nextOpt);
                    nextIsLit = True;
                }
            }

            repMatchPrice = matchPrice + GET_PRICE_1(p->isRep[state]);

            numAvailFull = p->numAvail;
            {
                unsigned temp = kNumOpts - 1 - cur;
                if (numAvailFull > temp)
                    numAvailFull = (UInt32)temp;
            }

            // 18.06
            // ---------- SHORT_REP ----------
            if (IsLitState(state)) // 18.new
                if (matchByte == curByte)
                    if (repMatchPrice < nextOpt->price) // 18.new
                        // if (numAvailFull < 2 || data[1] != *(data - reps[0] + 1))
                        if (
                            // nextOpt->price >= kInfinityPrice ||
                            nextOpt->len < 2 // we can check nextOpt->len, if skip items are not allowed in p->opt
                            || (nextOpt->dist != 0
                                // && nextOpt->extra <= 1 // 17.old
                                ))
                        {
                            UInt32 shortRepPrice = repMatchPrice + GetPrice_ShortRep(p, state, posState);
                            // if (shortRepPrice <= nextOpt->price) // 17.old
                            if (shortRepPrice < nextOpt->price) // 18.new
                            {
                                nextOpt->price = shortRepPrice;
                                nextOpt->len = 1;
                                MakeAs_ShortRep(nextOpt);
                                nextIsLit = False;
                            }
                        }

            if (numAvailFull < 2)
                continue;
            numAvail = (numAvailFull <= p->numFastBytes ? numAvailFull : p->numFastBytes);

            // numAvail <= p->numFastBytes

            // ---------- LIT : REP_0 ----------

            if (!nextIsLit && litPrice != 0 // 18.new
                && matchByte != curByte && numAvailFull > 2)
            {
                const Byte *data2 = data - reps[0];
                if (data[1] == data2[1] && data[2] == data2[2])
                {
                    unsigned len;
                    unsigned limit = p->numFastBytes + 1;
                    if (limit > numAvailFull)
                        limit = numAvailFull;
                    for (len = 3; len < limit && data[len] == data2[len]; len++)
                    {
                    }

                    {
                        unsigned state2 = kLiteralNextStates[state];
                        unsigned posState2 = (position + 1) & p->pbMask;
                        UInt32 price = litPrice + GetPrice_Rep_0(p, state2, posState2);
                        {
                            unsigned offset = cur + len;

                            if (last < offset)
                                last = offset;

                            // do
                            {
                                UInt32 price2;
                                COptimal *opt;
                                len--;
                                // price2 = price + GetPrice_Len_Rep_0(p, len, state2, posState2);
                                price2 = price + GET_PRICE_LEN(&p->repLenEnc, posState2, len);

                                opt = &p->opt[offset];
                                // offset--;
                                if (price2 < opt->price)
                                {
                                    opt->price = price2;
                                    opt->len = (UInt32)len;
                                    opt->dist = 0;
                                    opt->extra = 1;
                                }
                            }
                            // while (len >= 3);
                        }
                    }
                }
            }

            startLen = 2; /* speed optimization */

            {
                // ---------- REP ----------
                unsigned repIndex = 0; // 17.old
                // unsigned repIndex = IsLitState(state) ? 0 : 1; // 18.notused
                for (; repIndex < LZMA_NUM_REPS; repIndex++)
                {
                    unsigned len;
                    UInt32 price;
                    const Byte *data2 = data - reps[repIndex];
                    if (data[0] != data2[0] || data[1] != data2[1])
                        continue;

                    for (len = 2; len < numAvail && data[len] == data2[len]; len++)
                    {
                    }

                    // if (len < startLen) continue; // 18.new: speed optimization

                    {
                        unsigned offset = cur + len;
                        if (last < offset)
                            last = offset;
                    }
                    {
                        unsigned len2 = len;
                        price = repMatchPrice + GetPrice_PureRep(p, repIndex, state, posState);
                        do
                        {
                            UInt32 price2 = price + GET_PRICE_LEN(&p->repLenEnc, posState, len2);
                            COptimal *opt = &p->opt[cur + len2];
                            if (price2 < opt->price)
                            {
                                opt->price = price2;
                                opt->len = (UInt32)len2;
                                opt->dist = (UInt32)repIndex;
                                opt->extra = 0;
                            }
                        } while (--len2 >= 2);
                    }

                    if (repIndex == 0)
                        startLen = len + 1; // 17.old
                    // startLen = len + 1; // 18.new

                    /* if (_maxMode) */
                    {
                        // ---------- REP : LIT : REP_0 ----------
                        // numFastBytes + 1 + numFastBytes

                        unsigned len2 = len + 1;
                        unsigned limit = len2 + p->numFastBytes;
                        if (limit > numAvailFull)
                            limit = numAvailFull;

                        len2 += 2;
                        if (len2 <= limit)
                            if (data[len2 - 2] == data2[len2 - 2])
                                if (data[len2 - 1] == data2[len2 - 1])
                                {
                                    unsigned state2 = kRepNextStates[state];
                                    unsigned posState2 = (position + len) & p->pbMask;
                                    price += GET_PRICE_LEN(&p->repLenEnc, posState, len) + GET_PRICE_0(p->isMatch[state2][posState2]) + LitEnc_Matched_GetPrice(LIT_PROBS(position + len, data[(size_t)len - 1]), data[len], data2[len], p->ProbPrices);

                                    // state2 = kLiteralNextStates[state2];
                                    state2 = kState_LitAfterRep;
                                    posState2 = (posState2 + 1) & p->pbMask;

                                    price += GetPrice_Rep_0(p, state2, posState2);

                                    for (; len2 < limit && data[len2] == data2[len2]; len2++)
                                    {
                                    }

                                    len2 -= len;
                                    // if (len2 >= 3)
                                    {
                                        {
                                            unsigned offset = cur + len + len2;

                                            if (last < offset)
                                                last = offset;
                                            // do
                                            {
                                                UInt32 price2;
                                                COptimal *opt;
                                                len2--;
                                                // price2 = price + GetPrice_Len_Rep_0(p, len2, state2, posState2);
                                                price2 = price + GET_PRICE_LEN(&p->repLenEnc, posState2, len2);

                                                opt = &p->opt[offset];
                                                // offset--;
                                                if (price2 < opt->price)
                                                {
                                                    opt->price = price2;
                                                    opt->len = (UInt32)len2;
                                                    opt->extra = (CExtra)(len + 1);
                                                    opt->dist = (UInt32)repIndex;
                                                }
                                            }
                                            // while (len2 >= 3);
                                        }
                                    }
                                }
                    }
                }
            }

            // ---------- MATCH ----------
            /* for (unsigned len = 2; len <= newLen; len++) */
            if (newLen > numAvail)
            {
                newLen = numAvail;
                for (numPairs = 0; newLen > MATCHES[numPairs]; numPairs += 2)
                    ;
                MATCHES[numPairs] = (UInt32)newLen;
                numPairs += 2;
            }

            // startLen = 2; /* speed optimization */

            if (newLen >= startLen)
            {
                UInt32 normalMatchPrice = matchPrice + GET_PRICE_0(p->isRep[state]);
                UInt32 dist;
                unsigned offs, posSlot, len;

                {
                    unsigned offset = cur + newLen;
                    if (last < offset)
                        last = offset;
                }

                offs = 0;
                while (startLen > MATCHES[offs])
                    offs += 2;
                dist = MATCHES[(size_t)offs + 1];

                // if (dist >= kNumFullDistances)
                GetPosSlot2(dist, posSlot);

                for (len = /*2*/ startLen;; len++)
                {
                    UInt32 price = normalMatchPrice + GET_PRICE_LEN(&p->lenEnc, posState, len);
                    {
                        COptimal *opt;
                        unsigned lenNorm = len - 2;
                        lenNorm = GetLenToPosState2(lenNorm);
                        if (dist < kNumFullDistances)
                            price += p->distancesPrices[lenNorm][dist & (kNumFullDistances - 1)];
                        else
                            price += p->posSlotPrices[lenNorm][posSlot] + p->alignPrices[dist & kAlignMask];

                        opt = &p->opt[cur + len];
                        if (price < opt->price)
                        {
                            opt->price = price;
                            opt->len = (UInt32)len;
                            opt->dist = dist + LZMA_NUM_REPS;
                            opt->extra = 0;
                        }
                    }

                    if (len == MATCHES[offs])
                    {
                        // if (p->_maxMode) {
                        // MATCH : LIT : REP_0

                        const Byte *data2 = data - dist - 1;
                        unsigned len2 = len + 1;
                        unsigned limit = len2 + p->numFastBytes;
                        if (limit > numAvailFull)
                            limit = numAvailFull;

                        len2 += 2;
                        if (len2 <= limit)
                            if (data[len2 - 2] == data2[len2 - 2])
                                if (data[len2 - 1] == data2[len2 - 1])
                                {
                                    for (; len2 < limit && data[len2] == data2[len2]; len2++)
                                    {
                                    }

                                    len2 -= len;

                                    // if (len2 >= 3)
                                    {
                                        unsigned state2 = kMatchNextStates[state];
                                        unsigned posState2 = (position + len) & p->pbMask;
                                        unsigned offset;
                                        price += GET_PRICE_0(p->isMatch[state2][posState2]);
                                        price += LitEnc_Matched_GetPrice(LIT_PROBS(position + len, data[(size_t)len - 1]),
                                                                         data[len], data2[len], p->ProbPrices);

                                        // state2 = kLiteralNextStates[state2];
                                        state2 = kState_LitAfterMatch;

                                        posState2 = (posState2 + 1) & p->pbMask;
                                        price += GetPrice_Rep_0(p, state2, posState2);

                                        offset = cur + len + len2;

                                        if (last < offset)
                                            last = offset;
                                        // do
                                        {
                                            UInt32 price2;
                                            COptimal *opt;
                                            len2--;
                                            // price2 = price + GetPrice_Len_Rep_0(p, len2, state2, posState2);
                                            price2 = price + GET_PRICE_LEN(&p->repLenEnc, posState2, len2);
                                            opt = &p->opt[offset];
                                            // offset--;
                                            if (price2 < opt->price)
                                            {
                                                opt->price = price2;
                                                opt->len = (UInt32)len2;
                                                opt->extra = (CExtra)(len + 1);
                                                opt->dist = dist + LZMA_NUM_REPS;
                                            }
                                        }
                                        // while (len2 >= 3);
                                    }
                                }

                        offs += 2;
                        if (offs == numPairs)
                            break;
                        dist = MATCHES[(size_t)offs + 1];
                        // if (dist >= kNumFullDistances)
                        GetPosSlot2(dist, posSlot);
                    }
                }
            }
        }

        do
            p->opt[last].price = kInfinityPrice;
        while (--last);

        return Backward(p, cur);
    }

#define ChangePair(smallDist, bigDist) (((bigDist) >> 7) > (smallDist))

    static unsigned GetOptimumFast(CLzmaEnc * p)
    {
        UInt32 numAvail, mainDist;
        unsigned mainLen, numPairs, repIndex, repLen, i;
        const Byte *data;

        if (p->additionalOffset == 0)
            mainLen = ReadMatchDistances(p, &numPairs);
        else
        {
            mainLen = p->longestMatchLen;
            numPairs = p->numPairs;
        }

        numAvail = p->numAvail;
        p->backRes = MARK_LIT;
        if (numAvail < 2)
            return 1;
        // if (mainLen < 2 && p->state == 0) return 1; // 18.06.notused
        if (numAvail > LZMA_MATCH_LEN_MAX)
            numAvail = LZMA_MATCH_LEN_MAX;
        data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;
        repLen = repIndex = 0;

        for (i = 0; i < LZMA_NUM_REPS; i++)
        {
            unsigned len;
            const Byte *data2 = data - p->reps[i];
            if (data[0] != data2[0] || data[1] != data2[1])
                continue;
            for (len = 2; len < numAvail && data[len] == data2[len]; len++)
            {
            }
            if (len >= p->numFastBytes)
            {
                p->backRes = (UInt32)i;
                MOVE_POS_2(p, len - 1)
                return len;
            }
            if (len > repLen)
            {
                repIndex = i;
                repLen = len;
            }
        }

        if (mainLen >= p->numFastBytes)
        {
            p->backRes = p->matches[(size_t)numPairs - 1] + LZMA_NUM_REPS;
            MOVE_POS_2(p, mainLen - 1)
            return mainLen;
        }

        mainDist = 0; /* for GCC */

        if (mainLen >= 2)
        {
            mainDist = p->matches[(size_t)numPairs - 1];
            while (numPairs > 2)
            {
                UInt32 dist2;
                if (mainLen != p->matches[(size_t)numPairs - 4] + 1)
                    break;
                dist2 = p->matches[(size_t)numPairs - 3];
                if (!ChangePair(dist2, mainDist))
                    break;
                numPairs -= 2;
                mainLen--;
                mainDist = dist2;
            }
            if (mainLen == 2 && mainDist >= 0x80)
                mainLen = 1;
        }

        if (repLen >= 2)
            if (repLen + 1 >= mainLen || (repLen + 2 >= mainLen && mainDist >= (1 << 9)) || (repLen + 3 >= mainLen && mainDist >= (1 << 15)))
            {
                p->backRes = (UInt32)repIndex;
                MOVE_POS_2(p, repLen - 1)
                return repLen;
            }

        if (mainLen < 2 || numAvail <= 2)
            return 1;

        {
            unsigned len1 = ReadMatchDistances(p, &p->numPairs);
            p->longestMatchLen = len1;

            if (len1 >= 2)
            {
                UInt32 newDist = p->matches[(size_t)p->numPairs - 1];
                if ((len1 >= mainLen && newDist < mainDist) || (len1 == mainLen + 1 && !ChangePair(mainDist, newDist)) || (len1 > mainLen + 1) || (len1 + 1 >= mainLen && mainLen >= 3 && ChangePair(newDist, mainDist)))
                    return 1;
            }
        }

        data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;

        for (i = 0; i < LZMA_NUM_REPS; i++)
        {
            unsigned len, limit;
            const Byte *data2 = data - p->reps[i];
            if (data[0] != data2[0] || data[1] != data2[1])
                continue;
            limit = mainLen - 1;
            for (len = 2;; len++)
            {
                if (len >= limit)
                    return 1;
                if (data[len] != data2[len])
                    break;
            }
        }

        p->backRes = mainDist + LZMA_NUM_REPS;
        if (mainLen != 2)
        {
            MOVE_POS_2(p, mainLen - 2)
        }
        return mainLen;
    }

    static void WriteEndMarker(CLzmaEnc * p, unsigned posState)
    {
        UInt32 range;
        range = p->rc.range;
        {
            UInt32 ttt, newBound;
            CLzmaProb *prob = &p->isMatch[p->state][posState];
            RC_BIT_PRE(&p->rc, prob)
            RC_BIT_1(&p->rc, prob)
            prob = &p->isRep[p->state];
            RC_BIT_PRE(&p->rc, prob)
            RC_BIT_0(&p->rc, prob)
        }
        p->state = kMatchNextStates[p->state];

        p->rc.range = range;
        LenEnc_Encode(&p->lenProbs, &p->rc, 0, posState);
        range = p->rc.range;

        {
            // RcTree_Encode_PosSlot(&p->rc, p->posSlotEncoder[0], (1 << kNumPosSlotBits) - 1);
            CLzmaProb *probs = p->posSlotEncoder[0];
            unsigned m = 1;
            do
            {
                UInt32 ttt, newBound;
                RC_BIT_PRE(p, probs + m)
                RC_BIT_1(&p->rc, probs + m);
                m = (m << 1) + 1;
            } while (m < (1 << kNumPosSlotBits));
        }
        {
            // RangeEnc_EncodeDirectBits(&p->rc, ((UInt32)1 << (30 - kNumAlignBits)) - 1, 30 - kNumAlignBits);    UInt32 range = p->range;
            unsigned numBits = 30 - kNumAlignBits;
            do
            {
                range >>= 1;
                p->rc.low += range;
                RC_NORM(&p->rc)
            } while (--numBits);
        }

        {
            // RcTree_ReverseEncode(&p->rc, p->posAlignEncoder, kNumAlignBits, kAlignMask);
            CLzmaProb *probs = p->posAlignEncoder;
            unsigned m = 1;
            do
            {
                UInt32 ttt, newBound;
                RC_BIT_PRE(p, probs + m)
                RC_BIT_1(&p->rc, probs + m);
                m = (m << 1) + 1;
            } while (m < kAlignTableSize);
        }
        p->rc.range = range;
    }

    static SRes CheckErrors(CLzmaEnc * p)
    {
        if (p->result != SZ_OK)
            return p->result;
        if (p->rc.res != SZ_OK)
            p->result = SZ_ERROR_WRITE;

#ifndef _7ZIP_ST
        if (
            // p->mf_Failure ||
            (p->mtMode &&
             ( // p->matchFinderMt.failure_LZ_LZ ||
                 p->matchFinderMt.failure_LZ_BT)))
        {
            p->result = MY_HRES_ERROR__INTERNAL_ERROR;
            // printf("\nCheckErrors p->matchFinderMt.failureLZ\n");
        }
#endif

        if (MFB.result != SZ_OK)
            p->result = SZ_ERROR_READ;

        if (p->result != SZ_OK)
            p->finished = True;
        return p->result;
    }

    MY_NO_INLINE static SRes Flush(CLzmaEnc * p, UInt32 nowPos)
    {
        /* ReleaseMFStream(); */
        p->finished = True;
        if (p->writeEndMark)
            WriteEndMarker(p, nowPos & p->pbMask);
        RangeEnc_FlushData(&p->rc);
        RangeEnc_FlushStream(&p->rc);
        return CheckErrors(p);
    }

    MY_NO_INLINE static void FillAlignPrices(CLzmaEnc * p)
    {
        unsigned i;
        const CProbPrice *ProbPrices = p->ProbPrices;
        const CLzmaProb *probs = p->posAlignEncoder;
        // p->alignPriceCount = 0;
        for (i = 0; i < kAlignTableSize / 2; i++)
        {
            UInt32 price = 0;
            unsigned sym = i;
            unsigned m = 1;
            unsigned bit;
            UInt32 prob;
            bit = sym & 1;
            sym >>= 1;
            price += GET_PRICEa(probs[m], bit);
            m = (m << 1) + bit;
            bit = sym & 1;
            sym >>= 1;
            price += GET_PRICEa(probs[m], bit);
            m = (m << 1) + bit;
            bit = sym & 1;
            sym >>= 1;
            price += GET_PRICEa(probs[m], bit);
            m = (m << 1) + bit;
            prob = probs[m];
            p->alignPrices[i] = price + GET_PRICEa_0(prob);
            p->alignPrices[i + 8] = price + GET_PRICEa_1(prob);
            // p->alignPrices[i] = RcTree_ReverseGetPrice(p->posAlignEncoder, kNumAlignBits, i, p->ProbPrices);
        }
    }

    MY_NO_INLINE static void FillDistancesPrices(CLzmaEnc * p)
    {
        // int y; for (y = 0; y < 100; y++) {

        UInt32 tempPrices[kNumFullDistances];
        unsigned i, lps;

        const CProbPrice *ProbPrices = p->ProbPrices;
        p->matchPriceCount = 0;

        for (i = kStartPosModelIndex / 2; i < kNumFullDistances / 2; i++)
        {
            unsigned posSlot = GetPosSlot1(i);
            unsigned footerBits = (posSlot >> 1) - 1;
            unsigned base = ((2 | (posSlot & 1)) << footerBits);
            const CLzmaProb *probs = p->posEncoders + (size_t)base * 2;
            // tempPrices[i] = RcTree_ReverseGetPrice(p->posEncoders + base, footerBits, i - base, p->ProbPrices);
            UInt32 price = 0;
            unsigned m = 1;
            unsigned sym = i;
            unsigned offset = (unsigned)1 << footerBits;
            base += i;

            if (footerBits)
                do
                {
                    unsigned bit = sym & 1;
                    sym >>= 1;
                    price += GET_PRICEa(probs[m], bit);
                    m = (m << 1) + bit;
                } while (--footerBits);

            {
                unsigned prob = probs[m];
                tempPrices[base] = price + GET_PRICEa_0(prob);
                tempPrices[base + offset] = price + GET_PRICEa_1(prob);
            }
        }

        for (lps = 0; lps < kNumLenToPosStates; lps++)
        {
            unsigned slot;
            unsigned distTableSize2 = (p->distTableSize + 1) >> 1;
            UInt32 *posSlotPrices = p->posSlotPrices[lps];
            const CLzmaProb *probs = p->posSlotEncoder[lps];

            for (slot = 0; slot < distTableSize2; slot++)
            {
                // posSlotPrices[slot] = RcTree_GetPrice(encoder, kNumPosSlotBits, slot, p->ProbPrices);
                UInt32 price;
                unsigned bit;
                unsigned sym = slot + (1 << (kNumPosSlotBits - 1));
                unsigned prob;
                bit = sym & 1;
                sym >>= 1;
                price = GET_PRICEa(probs[sym], bit);
                bit = sym & 1;
                sym >>= 1;
                price += GET_PRICEa(probs[sym], bit);
                bit = sym & 1;
                sym >>= 1;
                price += GET_PRICEa(probs[sym], bit);
                bit = sym & 1;
                sym >>= 1;
                price += GET_PRICEa(probs[sym], bit);
                bit = sym & 1;
                sym >>= 1;
                price += GET_PRICEa(probs[sym], bit);
                prob = probs[(size_t)slot + (1 << (kNumPosSlotBits - 1))];
                posSlotPrices[(size_t)slot * 2] = price + GET_PRICEa_0(prob);
                posSlotPrices[(size_t)slot * 2 + 1] = price + GET_PRICEa_1(prob);
            }

            {
                UInt32 delta = ((UInt32)((kEndPosModelIndex / 2 - 1) - kNumAlignBits) << kNumBitPriceShiftBits);
                for (slot = kEndPosModelIndex / 2; slot < distTableSize2; slot++)
                {
                    posSlotPrices[(size_t)slot * 2] += delta;
                    posSlotPrices[(size_t)slot * 2 + 1] += delta;
                    delta += ((UInt32)1 << kNumBitPriceShiftBits);
                }
            }

            {
                UInt32 *dp = p->distancesPrices[lps];

                dp[0] = posSlotPrices[0];
                dp[1] = posSlotPrices[1];
                dp[2] = posSlotPrices[2];
                dp[3] = posSlotPrices[3];

                for (i = 4; i < kNumFullDistances; i += 2)
                {
                    UInt32 slotPrice = posSlotPrices[GetPosSlot1(i)];
                    dp[i] = slotPrice + tempPrices[i];
                    dp[i + 1] = slotPrice + tempPrices[i + 1];
                }
            }
        }
        // }
    }

    static void LzmaEnc_Construct(CLzmaEnc * p)
    {
        RangeEnc_Construct(&p->rc);
        MatchFinder_Construct(&MFB);

#ifndef _7ZIP_ST
        p->matchFinderMt.MatchFinder = &MFB;
        MatchFinderMt_Construct(&p->matchFinderMt);
#endif

        {
            CLzmaEncProps props;
            LzmaEncProps_Init(&props);
            LzmaEnc_SetProps(p, &props);
        }

#ifndef LZMA_LOG_BSR
        LzmaEnc_FastPosInit(p->g_FastPos);
#endif

        LzmaEnc_InitPriceTables(p->ProbPrices);
        p->litProbs = NULL;
        p->saveState.litProbs = NULL;
    }

    CLzmaEncHandle LzmaEnc_Create(ISzAllocPtr alloc)
    {
        void *p;
        p = ISzAlloc_Alloc(alloc, sizeof(CLzmaEnc));
        if (p)
            LzmaEnc_Construct((CLzmaEnc *)p);
        return p;
    }

    static void LzmaEnc_FreeLits(CLzmaEnc * p, ISzAllocPtr alloc)
    {
        ISzAlloc_Free(alloc, p->litProbs);
        ISzAlloc_Free(alloc, p->saveState.litProbs);
        p->litProbs = NULL;
        p->saveState.litProbs = NULL;
    }

    static void LzmaEnc_Destruct(CLzmaEnc * p, ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
#ifndef _7ZIP_ST
        MatchFinderMt_Destruct(&p->matchFinderMt, allocBig);
#endif

        MatchFinder_Free(&MFB, allocBig);
        LzmaEnc_FreeLits(p, alloc);
        RangeEnc_Free(&p->rc, alloc);
    }

    void LzmaEnc_Destroy(CLzmaEncHandle p, ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        LzmaEnc_Destruct((CLzmaEnc *)p, alloc, allocBig);
        ISzAlloc_Free(alloc, p);
    }

    MY_NO_INLINE
    static SRes LzmaEnc_CodeOneBlock(CLzmaEnc * p, UInt32 maxPackSize, UInt32 maxUnpackSize)
    {
        UInt32 nowPos32, startPos32;
        if (p->needInit)
        {
#ifndef _7ZIP_ST
            if (p->mtMode)
            {
                RINOK(MatchFinderMt_InitMt(&p->matchFinderMt));
            }
#endif
            p->matchFinder.Init(p->matchFinderObj);
            p->needInit = 0;
        }

        if (p->finished)
            return p->result;
        RINOK(CheckErrors(p));

        nowPos32 = (UInt32)p->nowPos64;
        startPos32 = nowPos32;

        if (p->nowPos64 == 0)
        {
            unsigned numPairs;
            Byte curByte;
            if (p->matchFinder.GetNumAvailableBytes(p->matchFinderObj) == 0)
                return Flush(p, nowPos32);
            ReadMatchDistances(p, &numPairs);
            RangeEnc_EncodeBit_0(&p->rc, &p->isMatch[kState_Start][0]);
            // p->state = kLiteralNextStates[p->state];
            curByte = *(p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - p->additionalOffset);
            LitEnc_Encode(&p->rc, p->litProbs, curByte);
            p->additionalOffset--;
            nowPos32++;
        }

        if (p->matchFinder.GetNumAvailableBytes(p->matchFinderObj) != 0)

            for (;;)
            {
                UInt32 dist;
                unsigned len, posState;
                UInt32 range, ttt, newBound;
                CLzmaProb *probs;

                if (p->fastMode)
                    len = GetOptimumFast(p);
                else
                {
                    unsigned oci = p->optCur;
                    if (p->optEnd == oci)
                        len = GetOptimum(p, nowPos32);
                    else
                    {
                        const COptimal *opt = &p->opt[oci];
                        len = opt->len;
                        p->backRes = opt->dist;
                        p->optCur = oci + 1;
                    }
                }

                posState = (unsigned)nowPos32 & p->pbMask;
                range = p->rc.range;
                probs = &p->isMatch[p->state][posState];

                RC_BIT_PRE(&p->rc, probs)

                dist = p->backRes;

#ifdef SHOW_STAT2
                printf("\n pos = %6X, len = %3u  pos = %6u", nowPos32, len, dist);
#endif

                if (dist == MARK_LIT)
                {
                    Byte curByte;
                    const Byte *data;
                    unsigned state;

                    RC_BIT_0(&p->rc, probs);
                    p->rc.range = range;
                    data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - p->additionalOffset;
                    probs = LIT_PROBS(nowPos32, *(data - 1));
                    curByte = *data;
                    state = p->state;
                    p->state = kLiteralNextStates[state];
                    if (IsLitState(state))
                        LitEnc_Encode(&p->rc, probs, curByte);
                    else
                        LitEnc_EncodeMatched(&p->rc, probs, curByte, *(data - p->reps[0]));
                }
                else
                {
                    RC_BIT_1(&p->rc, probs);
                    probs = &p->isRep[p->state];
                    RC_BIT_PRE(&p->rc, probs)

                    if (dist < LZMA_NUM_REPS)
                    {
                        RC_BIT_1(&p->rc, probs);
                        probs = &p->isRepG0[p->state];
                        RC_BIT_PRE(&p->rc, probs)
                        if (dist == 0)
                        {
                            RC_BIT_0(&p->rc, probs);
                            probs = &p->isRep0Long[p->state][posState];
                            RC_BIT_PRE(&p->rc, probs)
                            if (len != 1)
                            {
                                RC_BIT_1_BASE(&p->rc, probs);
                            }
                            else
                            {
                                RC_BIT_0_BASE(&p->rc, probs);
                                p->state = kShortRepNextStates[p->state];
                            }
                        }
                        else
                        {
                            RC_BIT_1(&p->rc, probs);
                            probs = &p->isRepG1[p->state];
                            RC_BIT_PRE(&p->rc, probs)
                            if (dist == 1)
                            {
                                RC_BIT_0_BASE(&p->rc, probs);
                                dist = p->reps[1];
                            }
                            else
                            {
                                RC_BIT_1(&p->rc, probs);
                                probs = &p->isRepG2[p->state];
                                RC_BIT_PRE(&p->rc, probs)
                                if (dist == 2)
                                {
                                    RC_BIT_0_BASE(&p->rc, probs);
                                    dist = p->reps[2];
                                }
                                else
                                {
                                    RC_BIT_1_BASE(&p->rc, probs);
                                    dist = p->reps[3];
                                    p->reps[3] = p->reps[2];
                                }
                                p->reps[2] = p->reps[1];
                            }
                            p->reps[1] = p->reps[0];
                            p->reps[0] = dist;
                        }

                        RC_NORM(&p->rc)

                        p->rc.range = range;

                        if (len != 1)
                        {
                            LenEnc_Encode(&p->repLenProbs, &p->rc, len - LZMA_MATCH_LEN_MIN, posState);
                            --p->repLenEncCounter;
                            p->state = kRepNextStates[p->state];
                        }
                    }
                    else
                    {
                        unsigned posSlot;
                        RC_BIT_0(&p->rc, probs);
                        p->rc.range = range;
                        p->state = kMatchNextStates[p->state];

                        LenEnc_Encode(&p->lenProbs, &p->rc, len - LZMA_MATCH_LEN_MIN, posState);
                        // --p->lenEnc.counter;

                        dist -= LZMA_NUM_REPS;
                        p->reps[3] = p->reps[2];
                        p->reps[2] = p->reps[1];
                        p->reps[1] = p->reps[0];
                        p->reps[0] = dist + 1;

                        p->matchPriceCount++;
                        GetPosSlot(dist, posSlot);
                        // RcTree_Encode_PosSlot(&p->rc, p->posSlotEncoder[GetLenToPosState(len)], posSlot);
                        {
                            UInt32 sym = (UInt32)posSlot + (1 << kNumPosSlotBits);
                            range = p->rc.range;
                            probs = p->posSlotEncoder[GetLenToPosState(len)];
                            do
                            {
                                CLzmaProb *prob = probs + (sym >> kNumPosSlotBits);
                                UInt32 bit = (sym >> (kNumPosSlotBits - 1)) & 1;
                                sym <<= 1;
                                RC_BIT(&p->rc, prob, bit);
                            } while (sym < (1 << kNumPosSlotBits * 2));
                            p->rc.range = range;
                        }

                        if (dist >= kStartPosModelIndex)
                        {
                            unsigned footerBits = ((posSlot >> 1) - 1);

                            if (dist < kNumFullDistances)
                            {
                                unsigned base = ((2 | (posSlot & 1)) << footerBits);
                                RcTree_ReverseEncode(&p->rc, p->posEncoders + base, footerBits, (unsigned)(dist /* - base */));
                            }
                            else
                            {
                                UInt32 pos2 = (dist | 0xF) << (32 - footerBits);
                                range = p->rc.range;
                                // RangeEnc_EncodeDirectBits(&p->rc, posReduced >> kNumAlignBits, footerBits - kNumAlignBits);
                                /*
                                do
                                {
                                  range >>= 1;
                                  p->rc.low += range & (0 - ((dist >> --footerBits) & 1));
                                  RC_NORM(&p->rc)
                                }
                                while (footerBits > kNumAlignBits);
                                */
                                do
                                {
                                    range >>= 1;
                                    p->rc.low += range & (0 - (pos2 >> 31));
                                    pos2 += pos2;
                                    RC_NORM(&p->rc)
                                } while (pos2 != 0xF0000000);

                                // RcTree_ReverseEncode(&p->rc, p->posAlignEncoder, kNumAlignBits, posReduced & kAlignMask);

                                {
                                    unsigned m = 1;
                                    unsigned bit;
                                    bit = dist & 1;
                                    dist >>= 1;
                                    RC_BIT(&p->rc, p->posAlignEncoder + m, bit);
                                    m = (m << 1) + bit;
                                    bit = dist & 1;
                                    dist >>= 1;
                                    RC_BIT(&p->rc, p->posAlignEncoder + m, bit);
                                    m = (m << 1) + bit;
                                    bit = dist & 1;
                                    dist >>= 1;
                                    RC_BIT(&p->rc, p->posAlignEncoder + m, bit);
                                    m = (m << 1) + bit;
                                    bit = dist & 1;
                                    RC_BIT(&p->rc, p->posAlignEncoder + m, bit);
                                    p->rc.range = range;
                                    // p->alignPriceCount++;
                                }
                            }
                        }
                    }
                }

                nowPos32 += (UInt32)len;
                p->additionalOffset -= len;

                if (p->additionalOffset == 0)
                {
                    UInt32 processed;

                    if (!p->fastMode)
                    {
                        /*
                        if (p->alignPriceCount >= 16) // kAlignTableSize
                          FillAlignPrices(p);
                        if (p->matchPriceCount >= 128)
                          FillDistancesPrices(p);
                        if (p->lenEnc.counter <= 0)
                          LenPriceEnc_UpdateTables(&p->lenEnc, 1 << p->pb, &p->lenProbs, p->ProbPrices);
                        */
                        if (p->matchPriceCount >= 64)
                        {
                            FillAlignPrices(p);
                            // { int y; for (y = 0; y < 100; y++) {
                            FillDistancesPrices(p);
                            // }}
                            LenPriceEnc_UpdateTables(&p->lenEnc, (unsigned)1 << p->pb, &p->lenProbs, p->ProbPrices);
                        }
                        if (p->repLenEncCounter <= 0)
                        {
                            p->repLenEncCounter = REP_LEN_COUNT;
                            LenPriceEnc_UpdateTables(&p->repLenEnc, (unsigned)1 << p->pb, &p->repLenProbs, p->ProbPrices);
                        }
                    }

                    if (p->matchFinder.GetNumAvailableBytes(p->matchFinderObj) == 0)
                        break;
                    processed = nowPos32 - startPos32;

                    if (maxPackSize)
                    {
                        if (processed + kNumOpts + 300 >= maxUnpackSize || RangeEnc_GetProcessed_sizet(&p->rc) + kPackReserve >= maxPackSize)
                            break;
                    }
                    else if (processed >= (1 << 17))
                    {
                        p->nowPos64 += nowPos32 - startPos32;
                        return CheckErrors(p);
                    }
                }
            }

        p->nowPos64 += nowPos32 - startPos32;
        return Flush(p, nowPos32);
    }

#define kBigHashDicLimit ((UInt32)1 << 24)

    static SRes LzmaEnc_Alloc(CLzmaEnc * p, UInt32 keepWindowSize, ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        UInt32 beforeSize = kNumOpts;
        UInt32 dictSize;

        if (!RangeEnc_Alloc(&p->rc, alloc))
            return SZ_ERROR_MEM;

#ifndef _7ZIP_ST
        p->mtMode = (p->multiThread && !p->fastMode && (MFB.btMode != 0));
#endif

        {
            unsigned lclp = p->lc + p->lp;
            if (!p->litProbs || !p->saveState.litProbs || p->lclp != lclp)
            {
                LzmaEnc_FreeLits(p, alloc);
                p->litProbs = (CLzmaProb *)ISzAlloc_Alloc(alloc, ((UInt32)0x300 << lclp) * sizeof(CLzmaProb));
                p->saveState.litProbs = (CLzmaProb *)ISzAlloc_Alloc(alloc, ((UInt32)0x300 << lclp) * sizeof(CLzmaProb));
                if (!p->litProbs || !p->saveState.litProbs)
                {
                    LzmaEnc_FreeLits(p, alloc);
                    return SZ_ERROR_MEM;
                }
                p->lclp = lclp;
            }
        }

        MFB.bigHash = (Byte)(p->dictSize > kBigHashDicLimit ? 1 : 0);

        dictSize = p->dictSize;
        if (dictSize == ((UInt32)2 << 30) ||
            dictSize == ((UInt32)3 << 30))
        {
            /* 21.03 : here we reduce the dictionary for 2 reasons:
               1) we don't want 32-bit back_distance matches in decoder for 2 GB dictionary.
               2) we want to elimate useless last MatchFinder_Normalize3() for corner cases,
                  where data size is aligned for 1 GB: 5/6/8 GB.
                  That reducing must be >= 1 for such corner cases. */
            dictSize -= 1;
        }

        if (beforeSize + dictSize < keepWindowSize)
            beforeSize = keepWindowSize - dictSize;

            /* in worst case we can look ahead for
                  max(LZMA_MATCH_LEN_MAX, numFastBytes + 1 + numFastBytes) bytes.
               we send larger value for (keepAfter) to MantchFinder_Create():
                  (numFastBytes + LZMA_MATCH_LEN_MAX + 1)
            */

#ifndef _7ZIP_ST
        if (p->mtMode)
        {
            RINOK(MatchFinderMt_Create(&p->matchFinderMt, dictSize, beforeSize,
                                       p->numFastBytes, LZMA_MATCH_LEN_MAX + 1 /* 18.04 */
                                       ,
                                       allocBig));
            p->matchFinderObj = &p->matchFinderMt;
            MFB.bigHash = (Byte)((p->dictSize > kBigHashDicLimit && MFB.hashMask >= 0xFFFFFF) ? 1 : 0);
            MatchFinderMt_CreateVTable(&p->matchFinderMt, &p->matchFinder);
        }
        else
#endif
        {
            if (!MatchFinder_Create(&MFB, dictSize, beforeSize,
                                    p->numFastBytes, LZMA_MATCH_LEN_MAX + 1 /* 21.03 */
                                    ,
                                    allocBig))
                return SZ_ERROR_MEM;
            p->matchFinderObj = &MFB;
            MatchFinder_CreateVTable(&MFB, &p->matchFinder);
        }

        return SZ_OK;
    }

    static void LzmaEnc_Init(CLzmaEnc * p)
    {
        unsigned i;
        p->state = 0;
        p->reps[0] =
            p->reps[1] =
                p->reps[2] =
                    p->reps[3] = 1;

        RangeEnc_Init(&p->rc);

        for (i = 0; i < (1 << kNumAlignBits); i++)
            p->posAlignEncoder[i] = kProbInitValue;

        for (i = 0; i < kNumStates; i++)
        {
            unsigned j;
            for (j = 0; j < LZMA_NUM_PB_STATES_MAX; j++)
            {
                p->isMatch[i][j] = kProbInitValue;
                p->isRep0Long[i][j] = kProbInitValue;
            }
            p->isRep[i] = kProbInitValue;
            p->isRepG0[i] = kProbInitValue;
            p->isRepG1[i] = kProbInitValue;
            p->isRepG2[i] = kProbInitValue;
        }

        {
            for (i = 0; i < kNumLenToPosStates; i++)
            {
                CLzmaProb *probs = p->posSlotEncoder[i];
                unsigned j;
                for (j = 0; j < (1 << kNumPosSlotBits); j++)
                    probs[j] = kProbInitValue;
            }
        }
        {
            for (i = 0; i < kNumFullDistances; i++)
                p->posEncoders[i] = kProbInitValue;
        }

        {
            UInt32 num = (UInt32)0x300 << (p->lp + p->lc);
            UInt32 k;
            CLzmaProb *probs = p->litProbs;
            for (k = 0; k < num; k++)
                probs[k] = kProbInitValue;
        }

        LenEnc_Init(&p->lenProbs);
        LenEnc_Init(&p->repLenProbs);

        p->optEnd = 0;
        p->optCur = 0;

        {
            for (i = 0; i < kNumOpts; i++)
                p->opt[i].price = kInfinityPrice;
        }

        p->additionalOffset = 0;

        p->pbMask = ((unsigned)1 << p->pb) - 1;
        p->lpMask = ((UInt32)0x100 << p->lp) - ((unsigned)0x100 >> p->lc);

        // p->mf_Failure = False;
    }

    static void LzmaEnc_InitPrices(CLzmaEnc * p)
    {
        if (!p->fastMode)
        {
            FillDistancesPrices(p);
            FillAlignPrices(p);
        }

        p->lenEnc.tableSize =
            p->repLenEnc.tableSize =
                p->numFastBytes + 1 - LZMA_MATCH_LEN_MIN;

        p->repLenEncCounter = REP_LEN_COUNT;

        LenPriceEnc_UpdateTables(&p->lenEnc, (unsigned)1 << p->pb, &p->lenProbs, p->ProbPrices);
        LenPriceEnc_UpdateTables(&p->repLenEnc, (unsigned)1 << p->pb, &p->repLenProbs, p->ProbPrices);
    }

    static SRes LzmaEnc_AllocAndInit(CLzmaEnc * p, UInt32 keepWindowSize, ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        unsigned i;
        for (i = kEndPosModelIndex / 2; i < kDicLogSizeMax; i++)
            if (p->dictSize <= ((UInt32)1 << i))
                break;
        p->distTableSize = i * 2;

        p->finished = False;
        p->result = SZ_OK;
        RINOK(LzmaEnc_Alloc(p, keepWindowSize, alloc, allocBig));
        LzmaEnc_Init(p);
        LzmaEnc_InitPrices(p);
        p->nowPos64 = 0;
        return SZ_OK;
    }

    static SRes LzmaEnc_Prepare(CLzmaEncHandle pp, ISeqOutStream * outStream, ISeqInStream * inStream,
                                ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        CLzmaEnc *p = (CLzmaEnc *)pp;
        MFB.stream = inStream;
        p->needInit = 1;
        p->rc.outStream = outStream;
        return LzmaEnc_AllocAndInit(p, 0, alloc, allocBig);
    }

    SRes LzmaEnc_PrepareForLzma2(CLzmaEncHandle pp,
                                 ISeqInStream * inStream, UInt32 keepWindowSize,
                                 ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        CLzmaEnc *p = (CLzmaEnc *)pp;
        MFB.stream = inStream;
        p->needInit = 1;
        return LzmaEnc_AllocAndInit(p, keepWindowSize, alloc, allocBig);
    }

    static void LzmaEnc_SetInputBuf(CLzmaEnc * p, const Byte *src, SizeT srcLen)
    {
        MFB.directInput = 1;
        MFB.bufferBase = (Byte *)src;
        MFB.directInputRem = srcLen;
    }

    SRes LzmaEnc_MemPrepare(CLzmaEncHandle pp, const Byte *src, SizeT srcLen,
                            UInt32 keepWindowSize, ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        CLzmaEnc *p = (CLzmaEnc *)pp;
        LzmaEnc_SetInputBuf(p, src, srcLen);
        p->needInit = 1;

        LzmaEnc_SetDataSize(pp, srcLen);
        return LzmaEnc_AllocAndInit(p, keepWindowSize, alloc, allocBig);
    }

    void LzmaEnc_Finish(CLzmaEncHandle pp)
    {
#ifndef _7ZIP_ST
        CLzmaEnc *p = (CLzmaEnc *)pp;
        if (p->mtMode)
            MatchFinderMt_ReleaseStream(&p->matchFinderMt);
#else
    UNUSED_VAR(pp);
#endif
    }

    typedef struct
    {
        ISeqOutStream vt;
        Byte *data;
        SizeT rem;
        BoolInt overflow;
    } CLzmaEnc_SeqOutStreamBuf;

    static size_t SeqOutStreamBuf_Write(const ISeqOutStream *pp, const void *data, size_t size)
    {
        CLzmaEnc_SeqOutStreamBuf *p = CONTAINER_FROM_VTBL(pp, CLzmaEnc_SeqOutStreamBuf, vt);
        if (p->rem < size)
        {
            size = p->rem;
            p->overflow = True;
        }
        if (size != 0)
        {
            memcpy(p->data, data, size);
            p->rem -= size;
            p->data += size;
        }
        return size;
    }

    /*
    UInt32 LzmaEnc_GetNumAvailableBytes(CLzmaEncHandle pp)
    {
      const CLzmaEnc *p = (CLzmaEnc *)pp;
      return p->matchFinder.GetNumAvailableBytes(p->matchFinderObj);
    }
    */

    const Byte *LzmaEnc_GetCurBuf(CLzmaEncHandle pp)
    {
        const CLzmaEnc *p = (CLzmaEnc *)pp;
        return p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - p->additionalOffset;
    }

    // (desiredPackSize == 0) is not allowed
    SRes LzmaEnc_CodeOneMemBlock(CLzmaEncHandle pp, BoolInt reInit,
                                 Byte * dest, size_t * destLen, UInt32 desiredPackSize, UInt32 * unpackSize)
    {
        CLzmaEnc *p = (CLzmaEnc *)pp;
        UInt64 nowPos64;
        SRes res;
        CLzmaEnc_SeqOutStreamBuf outStream;

        outStream.vt.Write = SeqOutStreamBuf_Write;
        outStream.data = dest;
        outStream.rem = *destLen;
        outStream.overflow = False;

        p->writeEndMark = False;
        p->finished = False;
        p->result = SZ_OK;

        if (reInit)
            LzmaEnc_Init(p);
        LzmaEnc_InitPrices(p);
        RangeEnc_Init(&p->rc);
        p->rc.outStream = &outStream.vt;
        nowPos64 = p->nowPos64;

        res = LzmaEnc_CodeOneBlock(p, desiredPackSize, *unpackSize);

        *unpackSize = (UInt32)(p->nowPos64 - nowPos64);
        *destLen -= outStream.rem;
        if (outStream.overflow)
            return SZ_ERROR_OUTPUT_EOF;

        return res;
    }

    MY_NO_INLINE
    static SRes LzmaEnc_Encode2(CLzmaEnc * p, ICompressProgress * progress)
    {
        SRes res = SZ_OK;

#ifndef _7ZIP_ST
        Byte allocaDummy[0x300];
        allocaDummy[0] = 0;
        allocaDummy[1] = allocaDummy[0];
#endif

        for (;;)
        {
            res = LzmaEnc_CodeOneBlock(p, 0, 0);
            if (res != SZ_OK || p->finished)
                break;
            if (progress)
            {
                res = ICompressProgress_Progress(progress, p->nowPos64, RangeEnc_GetProcessed(&p->rc));
                if (res != SZ_OK)
                {
                    res = SZ_ERROR_PROGRESS;
                    break;
                }
            }
        }

        LzmaEnc_Finish(p);

        /*
        if (res == SZ_OK && !Inline_MatchFinder_IsFinishedOK(&MFB))
          res = SZ_ERROR_FAIL;
        }
        */

        return res;
    }

    SRes LzmaEnc_Encode(CLzmaEncHandle pp, ISeqOutStream * outStream, ISeqInStream * inStream, ICompressProgress * progress,
                        ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        RINOK(LzmaEnc_Prepare(pp, outStream, inStream, alloc, allocBig));
        return LzmaEnc_Encode2((CLzmaEnc *)pp, progress);
    }

    SRes LzmaEnc_WriteProperties(CLzmaEncHandle pp, Byte * props, SizeT * size)
    {
        if (*size < LZMA_PROPS_SIZE)
            return SZ_ERROR_PARAM;
        *size = LZMA_PROPS_SIZE;
        {
            const CLzmaEnc *p = (const CLzmaEnc *)pp;
            const UInt32 dictSize = p->dictSize;
            UInt32 v;
            props[0] = (Byte)((p->pb * 5 + p->lp) * 9 + p->lc);

            // we write aligned dictionary value to properties for lzma decoder
            if (dictSize >= ((UInt32)1 << 21))
            {
                const UInt32 kDictMask = ((UInt32)1 << 20) - 1;
                v = (dictSize + kDictMask) & ~kDictMask;
                if (v < dictSize)
                    v = dictSize;
            }
            else
            {
                unsigned i = 11 * 2;
                do
                {
                    v = (UInt32)(2 + (i & 1)) << (i >> 1);
                    i++;
                } while (v < dictSize);
            }

            SetUi32(props + 1, v);
            return SZ_OK;
        }
    }

    unsigned LzmaEnc_IsWriteEndMark(CLzmaEncHandle pp)
    {
        return (unsigned)((CLzmaEnc *)pp)->writeEndMark;
    }

    SRes LzmaEnc_MemEncode(CLzmaEncHandle pp, Byte * dest, SizeT * destLen, const Byte *src, SizeT srcLen,
                           int writeEndMark, ICompressProgress *progress, ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        SRes res;
        CLzmaEnc *p = (CLzmaEnc *)pp;

        CLzmaEnc_SeqOutStreamBuf outStream;

        outStream.vt.Write = SeqOutStreamBuf_Write;
        outStream.data = dest;
        outStream.rem = *destLen;
        outStream.overflow = False;

        p->writeEndMark = writeEndMark;
        p->rc.outStream = &outStream.vt;

        res = LzmaEnc_MemPrepare(pp, src, srcLen, 0, alloc, allocBig);

        if (res == SZ_OK)
        {
            res = LzmaEnc_Encode2(p, progress);
            if (res == SZ_OK && p->nowPos64 != srcLen)
                res = SZ_ERROR_FAIL;
        }

        *destLen -= outStream.rem;
        if (outStream.overflow)
            return SZ_ERROR_OUTPUT_EOF;
        return res;
    }

    SRes LzmaEncode(Byte * dest, SizeT * destLen, const Byte *src, SizeT srcLen,
                    const CLzmaEncProps *props, Byte *propsEncoded, SizeT *propsSize, int writeEndMark,
                    ICompressProgress *progress, ISzAllocPtr alloc, ISzAllocPtr allocBig)
    {
        CLzmaEnc *p = (CLzmaEnc *)LzmaEnc_Create(alloc);
        SRes res;
        if (!p)
            return SZ_ERROR_MEM;

        res = LzmaEnc_SetProps(p, props);
        if (res == SZ_OK)
        {
            res = LzmaEnc_WriteProperties(p, propsEncoded, propsSize);
            if (res == SZ_OK)
                res = LzmaEnc_MemEncode(p, dest, destLen, src, srcLen,
                                        writeEndMark, progress, alloc, allocBig);
        }

        LzmaEnc_Destroy(p, alloc, allocBig);
        return res;
    }

    int lzma_compress(unsigned char *dest, size_t *destLen, const unsigned char *src, size_t srcLen)
    {
        size_t outPropsSize = LZMA_PROPS_SIZE;
        CLzmaEncProps props;
        LzmaEncProps_Init(&props);
        SRes res = LzmaEncode(&dest[LZMA_HEADER_SIZE], destLen, src, srcLen, &props, dest, &outPropsSize, 0, NULL, &g_Alloc, &g_Alloc);
        if (res == SZ_OK)
            *destLen += LZMA_HEADER_SIZE;
        return res;
    }

    /*
    #ifndef _7ZIP_ST
    void LzmaEnc_GetLzThreads(CLzmaEncHandle pp, HANDLE lz_threads[2])
    {
      const CLzmaEnc *p = (CLzmaEnc *)pp;
      lz_threads[0] = p->matchFinderMt.hashSync.thread;
      lz_threads[1] = p->matchFinderMt.btSync.thread;
    }
    #endif
    */

#define kNumTopBits 24
#define kTopValue ((UInt32)1 << kNumTopBits)

#define kNumBitModelTotalBits 11
#define kBitModelTotal (1 << kNumBitModelTotalBits)

#define RC_INIT_SIZE 5

#ifndef _LZMA_DEC_OPT

#define kNumMoveBits 5
#define NORMALIZE                      \
    if (range < kTopValue)             \
    {                                  \
        range <<= 8;                   \
        code = (code << 8) | (*buf++); \
    }

#define IF_BIT_0(p)                                         \
    ttt = *(p);                                             \
    NORMALIZE;                                              \
    bound = (range >> kNumBitModelTotalBits) * (UInt32)ttt; \
    if (code < bound)
#define UPDATE_0(p) \
    range = bound;  \
    *(p) = (CLzmaProb)(ttt + ((kBitModelTotal - ttt) >> kNumMoveBits));
#define UPDATE_1(p) \
    range -= bound; \
    code -= bound;  \
    *(p) = (CLzmaProb)(ttt - (ttt >> kNumMoveBits));
#define GET_BIT2(p, i, A0, A1) \
    IF_BIT_0(p)                \
    {                          \
        UPDATE_0(p);           \
        i = (i + i);           \
        A0;                    \
    }                          \
    else                       \
    {                          \
        UPDATE_1(p);           \
        i = (i + i) + 1;       \
        A1;                    \
    }

#define TREE_GET_BIT(probs, i)        \
    {                                 \
        GET_BIT2(probs + i, i, ;, ;); \
    }

#define REV_BIT(p, i, A0, A1) \
    IF_BIT_0(p + i)           \
    {                         \
        UPDATE_0(p + i);      \
        A0;                   \
    }                         \
    else                      \
    {                         \
        UPDATE_1(p + i);      \
        A1;                   \
    }
#define REV_BIT_VAR(p, i, m) REV_BIT(p, i, i += m; m += m, m += m; i += m;)
#define REV_BIT_CONST(p, i, m) REV_BIT(p, i, i += m;, i += m * 2;)
#define REV_BIT_LAST(p, i, m) REV_BIT(p, i, i -= m, ;)

#define TREE_DECODE(probs, limit, i) \
    {                                \
        i = 1;                       \
        do                           \
        {                            \
            TREE_GET_BIT(probs, i);  \
        } while (i < limit);         \
        i -= limit;                  \
    }

    /* #define _LZMA_SIZE_OPT */

#ifdef _LZMA_SIZE_OPT
#define TREE_6_DECODE(probs, i) TREE_DECODE(probs, (1 << 6), i)
#else
#define TREE_6_DECODE(probs, i) \
    {                           \
        i = 1;                  \
        TREE_GET_BIT(probs, i); \
        TREE_GET_BIT(probs, i); \
        TREE_GET_BIT(probs, i); \
        TREE_GET_BIT(probs, i); \
        TREE_GET_BIT(probs, i); \
        TREE_GET_BIT(probs, i); \
        i -= 0x40;              \
    }
#endif

#define NORMAL_LITER_DEC TREE_GET_BIT(prob, symbol)
#define MATCHED_LITER_DEC                   \
    matchByte += matchByte;                 \
    bit = offs;                             \
    offs &= matchByte;                      \
    probLit = prob + (offs + bit + symbol); \
    GET_BIT2(probLit, symbol, offs ^= bit;, ;)

#endif // _LZMA_DEC_OPT

#define NORMALIZE_CHECK                \
    if (range < kTopValue)             \
    {                                  \
        if (buf >= bufLimit)           \
            return DUMMY_INPUT_EOF;    \
        range <<= 8;                   \
        code = (code << 8) | (*buf++); \
    }

#define IF_BIT_0_CHECK(p)                                   \
    ttt = *(p);                                             \
    NORMALIZE_CHECK;                                        \
    bound = (range >> kNumBitModelTotalBits) * (UInt32)ttt; \
    if (code < bound)
#define UPDATE_0_CHECK range = bound;
#define UPDATE_1_CHECK \
    range -= bound;    \
    code -= bound;
#define GET_BIT2_CHECK(p, i, A0, A1) \
    IF_BIT_0_CHECK(p)                \
    {                                \
        UPDATE_0_CHECK;              \
        i = (i + i);                 \
        A0;                          \
    }                                \
    else                             \
    {                                \
        UPDATE_1_CHECK;              \
        i = (i + i) + 1;             \
        A1;                          \
    }
#define GET_BIT_CHECK(p, i) GET_BIT2_CHECK(p, i, ;, ;)
#define TREE_DECODE_CHECK(probs, limit, i) \
    {                                      \
        i = 1;                             \
        do                                 \
        {                                  \
            GET_BIT_CHECK(probs + i, i)    \
        } while (i < limit);               \
        i -= limit;                        \
    }

#define REV_BIT_CHECK(p, i, m) \
    IF_BIT_0_CHECK(p + i)      \
    {                          \
        UPDATE_0_CHECK;        \
        i += m;                \
        m += m;                \
    }                          \
    else                       \
    {                          \
        UPDATE_1_CHECK;        \
        m += m;                \
        i += m;                \
    }

#define kNumPosBitsMax 4
#define kNumPosStatesMax (1 << kNumPosBitsMax)

#define kLenNumLowBits 3
#define kLenNumLowSymbols (1 << kLenNumLowBits)
#define kLenNumHighBits 8
#define kLenNumHighSymbols (1 << kLenNumHighBits)

#define LenLow 0
#define LenHigh (LenLow + 2 * (kNumPosStatesMax << kLenNumLowBits))
#define kNumLenProbs (LenHigh + kLenNumHighSymbols)

#define LenChoice LenLow
#define LenChoice2 (LenLow + (1 << kLenNumLowBits))

#define kNumStates 12
#define kNumStates2 16
#define kNumLitStates 7

#define kStartPosModelIndex 4
#define kEndPosModelIndex 14
#define kNumFullDistances (1 << (kEndPosModelIndex >> 1))

#define kNumPosSlotBits 6
#define kNumLenToPosStates 4

#define kNumAlignBits 4
#define kAlignTableSize (1 << kNumAlignBits)

#define kMatchMinLen 2
#define kMatchSpecLenStart (kMatchMinLen + kLenNumLowSymbols * 2 + kLenNumHighSymbols)

#define kMatchSpecLen_Error_Data (1 << 9)
#define kMatchSpecLen_Error_Fail (kMatchSpecLen_Error_Data - 1)

    /* External ASM code needs same CLzmaProb array layout. So don't change it. */

    /* (probs_1664) is faster and better for code size at some platforms */
    /*
    #ifdef MY_CPU_X86_OR_AMD64
    */
#define kStartOffset 1664
#define GET_PROBS p->probs_1664
    /*
    #define GET_PROBS p->probs + kStartOffset
    #else
    #define kStartOffset 0
    #define GET_PROBS p->probs
    #endif
    */

#define SpecPos (-kStartOffset)
#define IsRep0Long (SpecPos + kNumFullDistances)
#define RepLenCoder (IsRep0Long + (kNumStates2 << kNumPosBitsMax))
#define LenCoder (RepLenCoder + kNumLenProbs)
#define IsMatch (LenCoder + kNumLenProbs)
#define Align (IsMatch + (kNumStates2 << kNumPosBitsMax))
#define IsRep (Align + kAlignTableSize)
#define IsRepG0 (IsRep + kNumStates)
#define IsRepG1 (IsRepG0 + kNumStates)
#define IsRepG2 (IsRepG1 + kNumStates)
#define PosSlot (IsRepG2 + kNumStates)
#define Literal (PosSlot + (kNumLenToPosStates << kNumPosSlotBits))
#define NUM_BASE_PROBS (Literal + kStartOffset)

#if Align != 0 && kStartOffset != 0
#error Stop_Compiling_Bad_LZMA_kAlign
#endif

#if NUM_BASE_PROBS != 1984
#error Stop_Compiling_Bad_LZMA_PROBS
#endif

#define LZMA_LIT_SIZE 0x300

#define LzmaProps_GetNumProbs(p) (NUM_BASE_PROBS + ((UInt32)LZMA_LIT_SIZE << ((p)->lc + (p)->lp)))

#define CALC_POS_STATE(processedPos, pbMask) (((processedPos) & (pbMask)) << 4)
#define COMBINED_PS_STATE (posState + state)
#define GET_LEN_STATE (posState)

#define LZMA_DIC_MIN (1 << 12)

    /*
    p->remainLen : shows status of LZMA decoder:
        < kMatchSpecLenStart  : the number of bytes to be copied with (p->rep0) offset
        = kMatchSpecLenStart  : the LZMA stream was finished with end mark
        = kMatchSpecLenStart + 1  : need init range coder
        = kMatchSpecLenStart + 2  : need init range coder and state
        = kMatchSpecLen_Error_Fail                : Internal Code Failure
        = kMatchSpecLen_Error_Data + [0 ... 273]  : LZMA Data Error
    */

    /* ---------- LZMA_DECODE_REAL ---------- */
    /*
    LzmaDec_DecodeReal_3() can be implemented in external ASM file.
    3 - is the code compatibility version of that function for check at link time.
    */

#define LZMA_DECODE_REAL LzmaDec_DecodeReal_3

    /*
    LZMA_DECODE_REAL()
    In:
      RangeCoder is normalized
      if (p->dicPos == limit)
      {
        LzmaDec_TryDummy() was called before to exclude LITERAL and MATCH-REP cases.
        So first symbol can be only MATCH-NON-REP. And if that MATCH-NON-REP symbol
        is not END_OF_PAYALOAD_MARKER, then the function doesn't write any byte to dictionary,
        the function returns SZ_OK, and the caller can use (p->remainLen) and (p->reps[0]) later.
      }

    Processing:
      The first LZMA symbol will be decoded in any case.
      All main checks for limits are at the end of main loop,
      It decodes additional LZMA-symbols while (p->buf < bufLimit && dicPos < limit),
      RangeCoder is still without last normalization when (p->buf < bufLimit) is being checked.
      But if (p->buf < bufLimit), the caller provided at least (LZMA_REQUIRED_INPUT_MAX + 1) bytes for
      next iteration  before limit (bufLimit + LZMA_REQUIRED_INPUT_MAX),
      that is enough for worst case LZMA symbol with one additional RangeCoder normalization for one bit.
      So that function never reads bufLimit [LZMA_REQUIRED_INPUT_MAX] byte.

    Out:
      RangeCoder is normalized
      Result:
        SZ_OK - OK
          p->remainLen:
            < kMatchSpecLenStart : the number of bytes to be copied with (p->reps[0]) offset
            = kMatchSpecLenStart : the LZMA stream was finished with end mark

        SZ_ERROR_DATA - error, when the MATCH-Symbol refers out of dictionary
          p->remainLen : undefined
          p->reps[*]    : undefined
    */

#ifdef _LZMA_DEC_OPT

    int MY_FAST_CALL LZMA_DECODE_REAL(CLzmaDec * p, SizeT limit, const Byte *bufLimit);

#else

static int MY_FAST_CALL LZMA_DECODE_REAL(CLzmaDec *p, SizeT limit, const Byte *bufLimit)
{
    CLzmaProb *probs = GET_PROBS;
    unsigned state = (unsigned)p->state;
    UInt32 rep0 = p->reps[0], rep1 = p->reps[1], rep2 = p->reps[2], rep3 = p->reps[3];
    unsigned pbMask = ((unsigned)1 << (p->prop.pb)) - 1;
    unsigned lc = p->prop.lc;
    unsigned lpMask = ((unsigned)0x100 << p->prop.lp) - ((unsigned)0x100 >> lc);

    Byte *dic = p->dic;
    SizeT dicBufSize = p->dicBufSize;
    SizeT dicPos = p->dicPos;

    UInt32 processedPos = p->processedPos;
    UInt32 checkDicSize = p->checkDicSize;
    unsigned len = 0;

    const Byte *buf = p->buf;
    UInt32 range = p->range;
    UInt32 code = p->code;

    do
    {
        CLzmaProb *prob;
        UInt32 bound;
        unsigned ttt;
        unsigned posState = CALC_POS_STATE(processedPos, pbMask);

        prob = probs + IsMatch + COMBINED_PS_STATE;
        IF_BIT_0(prob)
        {
            unsigned symbol;
            UPDATE_0(prob);
            prob = probs + Literal;
            if (processedPos != 0 || checkDicSize != 0)
                prob += (UInt32)3 * ((((processedPos << 8) + dic[(dicPos == 0 ? dicBufSize : dicPos) - 1]) & lpMask) << lc);
            processedPos++;

            if (state < kNumLitStates)
            {
                state -= (state < 4) ? state : 3;
                symbol = 1;
#ifdef _LZMA_SIZE_OPT
                do
                {
                    NORMAL_LITER_DEC
                } while (symbol < 0x100);
#else
                NORMAL_LITER_DEC
                NORMAL_LITER_DEC
                NORMAL_LITER_DEC
                NORMAL_LITER_DEC
                NORMAL_LITER_DEC
                NORMAL_LITER_DEC
                NORMAL_LITER_DEC
                NORMAL_LITER_DEC
#endif
            }
            else
            {
                unsigned matchByte = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
                unsigned offs = 0x100;
                state -= (state < 10) ? 3 : 6;
                symbol = 1;
#ifdef _LZMA_SIZE_OPT
                do
                {
                    unsigned bit;
                    CLzmaProb *probLit;
                    MATCHED_LITER_DEC
                } while (symbol < 0x100);
#else
                {
                    unsigned bit;
                    CLzmaProb *probLit;
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                }
#endif
            }

            dic[dicPos++] = (Byte)symbol;
            continue;
        }

        {
            UPDATE_1(prob);
            prob = probs + IsRep + state;
            IF_BIT_0(prob)
            {
                UPDATE_0(prob);
                state += kNumStates;
                prob = probs + LenCoder;
            }
            else
            {
                UPDATE_1(prob);
                prob = probs + IsRepG0 + state;
                IF_BIT_0(prob)
                {
                    UPDATE_0(prob);
                    prob = probs + IsRep0Long + COMBINED_PS_STATE;
                    IF_BIT_0(prob)
                    {
                        UPDATE_0(prob);

                        // that case was checked before with kBadRepCode
                        // if (checkDicSize == 0 && processedPos == 0) { len = kMatchSpecLen_Error_Data + 1; break; }
                        // The caller doesn't allow (dicPos == limit) case here
                        // so we don't need the following check:
                        // if (dicPos == limit) { state = state < kNumLitStates ? 9 : 11; len = 1; break; }

                        dic[dicPos] = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
                        dicPos++;
                        processedPos++;
                        state = state < kNumLitStates ? 9 : 11;
                        continue;
                    }
                    UPDATE_1(prob);
                }
                else
                {
                    UInt32 distance;
                    UPDATE_1(prob);
                    prob = probs + IsRepG1 + state;
                    IF_BIT_0(prob)
                    {
                        UPDATE_0(prob);
                        distance = rep1;
                    }
                    else
                    {
                        UPDATE_1(prob);
                        prob = probs + IsRepG2 + state;
                        IF_BIT_0(prob)
                        {
                            UPDATE_0(prob);
                            distance = rep2;
                        }
                        else
                        {
                            UPDATE_1(prob);
                            distance = rep3;
                            rep3 = rep2;
                        }
                        rep2 = rep1;
                    }
                    rep1 = rep0;
                    rep0 = distance;
                }
                state = state < kNumLitStates ? 8 : 11;
                prob = probs + RepLenCoder;
            }

#ifdef _LZMA_SIZE_OPT
            {
                unsigned lim, offset;
                CLzmaProb *probLen = prob + LenChoice;
                IF_BIT_0(probLen)
                {
                    UPDATE_0(probLen);
                    probLen = prob + LenLow + GET_LEN_STATE;
                    offset = 0;
                    lim = (1 << kLenNumLowBits);
                }
                else
                {
                    UPDATE_1(probLen);
                    probLen = prob + LenChoice2;
                    IF_BIT_0(probLen)
                    {
                        UPDATE_0(probLen);
                        probLen = prob + LenLow + GET_LEN_STATE + (1 << kLenNumLowBits);
                        offset = kLenNumLowSymbols;
                        lim = (1 << kLenNumLowBits);
                    }
                    else
                    {
                        UPDATE_1(probLen);
                        probLen = prob + LenHigh;
                        offset = kLenNumLowSymbols * 2;
                        lim = (1 << kLenNumHighBits);
                    }
                }
                TREE_DECODE(probLen, lim, len);
                len += offset;
            }
#else
            {
                CLzmaProb *probLen = prob + LenChoice;
                IF_BIT_0(probLen)
                {
                    UPDATE_0(probLen);
                    probLen = prob + LenLow + GET_LEN_STATE;
                    len = 1;
                    TREE_GET_BIT(probLen, len);
                    TREE_GET_BIT(probLen, len);
                    TREE_GET_BIT(probLen, len);
                    len -= 8;
                }
                else
                {
                    UPDATE_1(probLen);
                    probLen = prob + LenChoice2;
                    IF_BIT_0(probLen)
                    {
                        UPDATE_0(probLen);
                        probLen = prob + LenLow + GET_LEN_STATE + (1 << kLenNumLowBits);
                        len = 1;
                        TREE_GET_BIT(probLen, len);
                        TREE_GET_BIT(probLen, len);
                        TREE_GET_BIT(probLen, len);
                    }
                    else
                    {
                        UPDATE_1(probLen);
                        probLen = prob + LenHigh;
                        TREE_DECODE(probLen, (1 << kLenNumHighBits), len);
                        len += kLenNumLowSymbols * 2;
                    }
                }
            }
#endif

            if (state >= kNumStates)
            {
                UInt32 distance;
                prob = probs + PosSlot +
                       ((len < kNumLenToPosStates ? len : kNumLenToPosStates - 1) << kNumPosSlotBits);
                TREE_6_DECODE(prob, distance);
                if (distance >= kStartPosModelIndex)
                {
                    unsigned posSlot = (unsigned)distance;
                    unsigned numDirectBits = (unsigned)(((distance >> 1) - 1));
                    distance = (2 | (distance & 1));
                    if (posSlot < kEndPosModelIndex)
                    {
                        distance <<= numDirectBits;
                        prob = probs + SpecPos;
                        {
                            UInt32 m = 1;
                            distance++;
                            do
                            {
                                REV_BIT_VAR(prob, distance, m);
                            } while (--numDirectBits);
                            distance -= m;
                        }
                    }
                    else
                    {
                        numDirectBits -= kNumAlignBits;
                        do
                        {
                            NORMALIZE
                            range >>= 1;

                            {
                                UInt32 t;
                                code -= range;
                                t = (0 - ((UInt32)code >> 31)); /* (UInt32)((Int32)code >> 31) */
                                distance = (distance << 1) + (t + 1);
                                code += range & t;
                            }
                            /*
                            distance <<= 1;
                            if (code >= range)
                            {
                              code -= range;
                              distance |= 1;
                            }
                            */
                        } while (--numDirectBits);
                        prob = probs + Align;
                        distance <<= kNumAlignBits;
                        {
                            unsigned i = 1;
                            REV_BIT_CONST(prob, i, 1);
                            REV_BIT_CONST(prob, i, 2);
                            REV_BIT_CONST(prob, i, 4);
                            REV_BIT_LAST(prob, i, 8);
                            distance |= i;
                        }
                        if (distance == (UInt32)0xFFFFFFFF)
                        {
                            len = kMatchSpecLenStart;
                            state -= kNumStates;
                            break;
                        }
                    }
                }

                rep3 = rep2;
                rep2 = rep1;
                rep1 = rep0;
                rep0 = distance + 1;
                state = (state < kNumStates + kNumLitStates) ? kNumLitStates : kNumLitStates + 3;
                if (distance >= (checkDicSize == 0 ? processedPos : checkDicSize))
                {
                    len += kMatchSpecLen_Error_Data + kMatchMinLen;
                    // len = kMatchSpecLen_Error_Data;
                    // len += kMatchMinLen;
                    break;
                }
            }

            len += kMatchMinLen;

            {
                SizeT rem;
                unsigned curLen;
                SizeT pos;

                if ((rem = limit - dicPos) == 0)
                {
                    /*
                    We stop decoding and return SZ_OK, and we can resume decoding later.
                    Any error conditions can be tested later in caller code.
                    For more strict mode we can stop decoding with error
                    // len += kMatchSpecLen_Error_Data;
                    */
                    break;
                }

                curLen = ((rem < len) ? (unsigned)rem : len);
                pos = dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0);

                processedPos += (UInt32)curLen;

                len -= curLen;
                if (curLen <= dicBufSize - pos)
                {
                    Byte *dest = dic + dicPos;
                    ptrdiff_t src = (ptrdiff_t)pos - (ptrdiff_t)dicPos;
                    const Byte *lim = dest + curLen;
                    dicPos += (SizeT)curLen;
                    do
                        *(dest) = (Byte) * (dest + src);
                    while (++dest != lim);
                }
                else
                {
                    do
                    {
                        dic[dicPos++] = dic[pos];
                        if (++pos == dicBufSize)
                            pos = 0;
                    } while (--curLen != 0);
                }
            }
        }
    } while (dicPos < limit && buf < bufLimit);

    NORMALIZE;

    p->buf = buf;
    p->range = range;
    p->code = code;
    p->remainLen = (UInt32)len; // & (kMatchSpecLen_Error_Data - 1); // we can write real length for error matches too.
    p->dicPos = dicPos;
    p->processedPos = processedPos;
    p->reps[0] = rep0;
    p->reps[1] = rep1;
    p->reps[2] = rep2;
    p->reps[3] = rep3;
    p->state = (UInt32)state;
    if (len >= kMatchSpecLen_Error_Data)
        return SZ_ERROR_DATA;
    return SZ_OK;
}
#endif

    static void MY_FAST_CALL LzmaDec_WriteRem(CLzmaDec * p, SizeT limit)
    {
        unsigned len = (unsigned)p->remainLen;
        if (len == 0 /* || len >= kMatchSpecLenStart */)
            return;
        {
            SizeT dicPos = p->dicPos;
            Byte *dic;
            SizeT dicBufSize;
            SizeT rep0; /* we use SizeT to avoid the BUG of VC14 for AMD64 */
            {
                SizeT rem = limit - dicPos;
                if (rem < len)
                {
                    len = (unsigned)(rem);
                    if (len == 0)
                        return;
                }
            }

            if (p->checkDicSize == 0 && p->prop.dicSize - p->processedPos <= len)
                p->checkDicSize = p->prop.dicSize;

            p->processedPos += (UInt32)len;
            p->remainLen -= (UInt32)len;
            dic = p->dic;
            rep0 = p->reps[0];
            dicBufSize = p->dicBufSize;
            do
            {
                dic[dicPos] = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
                dicPos++;
            } while (--len);
            p->dicPos = dicPos;
        }
    }

    /*
    At staring of new stream we have one of the following symbols:
      - Literal        - is allowed
      - Non-Rep-Match  - is allowed only if it's end marker symbol
      - Rep-Match      - is not allowed
    We use early check of (RangeCoder:Code) over kBadRepCode to simplify main decoding code
    */

#define kRange0 0xFFFFFFFF
#define kBound0 ((kRange0 >> kNumBitModelTotalBits) << (kNumBitModelTotalBits - 1))
#define kBadRepCode (kBound0 + (((kRange0 - kBound0) >> kNumBitModelTotalBits) << (kNumBitModelTotalBits - 1)))
#if kBadRepCode != (0xC0000000 - 0x400)
#error Stop_Compiling_Bad_LZMA_Check
#endif

    /*
    LzmaDec_DecodeReal2():
      It calls LZMA_DECODE_REAL() and it adjusts limit according (p->checkDicSize).

    We correct (p->checkDicSize) after LZMA_DECODE_REAL() and in LzmaDec_WriteRem(),
    and we support the following state of (p->checkDicSize):
      if (total_processed < p->prop.dicSize) then
      {
        (total_processed == p->processedPos)
        (p->checkDicSize == 0)
      }
      else
        (p->checkDicSize == p->prop.dicSize)
    */

    static int MY_FAST_CALL LzmaDec_DecodeReal2(CLzmaDec * p, SizeT limit, const Byte *bufLimit)
    {
        if (p->checkDicSize == 0)
        {
            UInt32 rem = p->prop.dicSize - p->processedPos;
            if (limit - p->dicPos > rem)
                limit = p->dicPos + rem;
        }
        {
            int res = LZMA_DECODE_REAL(p, limit, bufLimit);
            if (p->checkDicSize == 0 && p->processedPos >= p->prop.dicSize)
                p->checkDicSize = p->prop.dicSize;
            return res;
        }
    }

    typedef enum
    {
        DUMMY_INPUT_EOF, /* need more input data */
        DUMMY_LIT,
        DUMMY_MATCH,
        DUMMY_REP
    } ELzmaDummy;

#define IS_DUMMY_END_MARKER_POSSIBLE(dummyRes) ((dummyRes) == DUMMY_MATCH)

    static ELzmaDummy LzmaDec_TryDummy(const CLzmaDec *p, const Byte *buf, const Byte **bufOut)
    {
        UInt32 range = p->range;
        UInt32 code = p->code;
        const Byte *bufLimit = *bufOut;
        const CLzmaProb *probs = GET_PROBS;
        unsigned state = (unsigned)p->state;
        ELzmaDummy res;

        for (;;)
        {
            const CLzmaProb *prob;
            UInt32 bound;
            unsigned ttt;
            unsigned posState = CALC_POS_STATE(p->processedPos, ((unsigned)1 << p->prop.pb) - 1);

            prob = probs + IsMatch + COMBINED_PS_STATE;
            IF_BIT_0_CHECK(prob)
            {
                UPDATE_0_CHECK

                prob = probs + Literal;
                if (p->checkDicSize != 0 || p->processedPos != 0)
                    prob += ((UInt32)LZMA_LIT_SIZE *
                             ((((p->processedPos) & (((unsigned)1 << (p->prop.lp)) - 1)) << p->prop.lc) +
                              ((unsigned)p->dic[(p->dicPos == 0 ? p->dicBufSize : p->dicPos) - 1] >> (8 - p->prop.lc))));

                if (state < kNumLitStates)
                {
                    unsigned symbol = 1;
                    do
                    {
                        GET_BIT_CHECK(prob + symbol, symbol)
                    } while (symbol < 0x100);
                }
                else
                {
                    unsigned matchByte = p->dic[p->dicPos - p->reps[0] +
                                                (p->dicPos < p->reps[0] ? p->dicBufSize : 0)];
                    unsigned offs = 0x100;
                    unsigned symbol = 1;
                    do
                    {
                        unsigned bit;
                        const CLzmaProb *probLit;
                        matchByte += matchByte;
                        bit = offs;
                        offs &= matchByte;
                        probLit = prob + (offs + bit + symbol);
                        GET_BIT2_CHECK(probLit, symbol, offs ^= bit;, ;)
                    } while (symbol < 0x100);
                }
                res = DUMMY_LIT;
            }
            else
            {
                unsigned len;
                UPDATE_1_CHECK;

                prob = probs + IsRep + state;
                IF_BIT_0_CHECK(prob)
                {
                    UPDATE_0_CHECK;
                    state = 0;
                    prob = probs + LenCoder;
                    res = DUMMY_MATCH;
                }
                else
                {
                    UPDATE_1_CHECK;
                    res = DUMMY_REP;
                    prob = probs + IsRepG0 + state;
                    IF_BIT_0_CHECK(prob)
                    {
                        UPDATE_0_CHECK;
                        prob = probs + IsRep0Long + COMBINED_PS_STATE;
                        IF_BIT_0_CHECK(prob)
                        {
                            UPDATE_0_CHECK;
                            break;
                        }
                        else
                        {
                            UPDATE_1_CHECK;
                        }
                    }
                    else
                    {
                        UPDATE_1_CHECK;
                        prob = probs + IsRepG1 + state;
                        IF_BIT_0_CHECK(prob)
                        {
                            UPDATE_0_CHECK;
                        }
                        else
                        {
                            UPDATE_1_CHECK;
                            prob = probs + IsRepG2 + state;
                            IF_BIT_0_CHECK(prob)
                            {
                                UPDATE_0_CHECK;
                            }
                            else
                            {
                                UPDATE_1_CHECK;
                            }
                        }
                    }
                    state = kNumStates;
                    prob = probs + RepLenCoder;
                }
                {
                    unsigned limit, offset;
                    const CLzmaProb *probLen = prob + LenChoice;
                    IF_BIT_0_CHECK(probLen)
                    {
                        UPDATE_0_CHECK;
                        probLen = prob + LenLow + GET_LEN_STATE;
                        offset = 0;
                        limit = 1 << kLenNumLowBits;
                    }
                    else
                    {
                        UPDATE_1_CHECK;
                        probLen = prob + LenChoice2;
                        IF_BIT_0_CHECK(probLen)
                        {
                            UPDATE_0_CHECK;
                            probLen = prob + LenLow + GET_LEN_STATE + (1 << kLenNumLowBits);
                            offset = kLenNumLowSymbols;
                            limit = 1 << kLenNumLowBits;
                        }
                        else
                        {
                            UPDATE_1_CHECK;
                            probLen = prob + LenHigh;
                            offset = kLenNumLowSymbols * 2;
                            limit = 1 << kLenNumHighBits;
                        }
                    }
                    TREE_DECODE_CHECK(probLen, limit, len);
                    len += offset;
                }

                if (state < 4)
                {
                    unsigned posSlot;
                    prob = probs + PosSlot +
                           ((len < kNumLenToPosStates - 1 ? len : kNumLenToPosStates - 1) << kNumPosSlotBits);
                    TREE_DECODE_CHECK(prob, 1 << kNumPosSlotBits, posSlot);
                    if (posSlot >= kStartPosModelIndex)
                    {
                        unsigned numDirectBits = ((posSlot >> 1) - 1);

                        if (posSlot < kEndPosModelIndex)
                        {
                            prob = probs + SpecPos + ((2 | (posSlot & 1)) << numDirectBits);
                        }
                        else
                        {
                            numDirectBits -= kNumAlignBits;
                            do
                            {
                                NORMALIZE_CHECK
                                range >>= 1;
                                code -= range & (((code - range) >> 31) - 1);
                                /* if (code >= range) code -= range; */
                            } while (--numDirectBits);
                            prob = probs + Align;
                            numDirectBits = kNumAlignBits;
                        }
                        {
                            unsigned i = 1;
                            unsigned m = 1;
                            do
                            {
                                REV_BIT_CHECK(prob, i, m);
                            } while (--numDirectBits);
                        }
                    }
                }
            }
            break;
        }
        NORMALIZE_CHECK;

        *bufOut = buf;
        return res;
    }

    void LzmaDec_InitDicAndState(CLzmaDec * p, BoolInt initDic, BoolInt initState);
    void LzmaDec_InitDicAndState(CLzmaDec * p, BoolInt initDic, BoolInt initState)
    {
        p->remainLen = kMatchSpecLenStart + 1;
        p->tempBufSize = 0;

        if (initDic)
        {
            p->processedPos = 0;
            p->checkDicSize = 0;
            p->remainLen = kMatchSpecLenStart + 2;
        }
        if (initState)
            p->remainLen = kMatchSpecLenStart + 2;
    }

    void LzmaDec_Init(CLzmaDec * p)
    {
        p->dicPos = 0;
        LzmaDec_InitDicAndState(p, True, True);
    }

    /*
    LZMA supports optional end_marker.
    So the decoder can lookahead for one additional LZMA-Symbol to check end_marker.
    That additional LZMA-Symbol can require up to LZMA_REQUIRED_INPUT_MAX bytes in input stream.
    When the decoder reaches dicLimit, it looks (finishMode) parameter:
      if (finishMode == LZMA_FINISH_ANY), the decoder doesn't lookahead
      if (finishMode != LZMA_FINISH_ANY), the decoder lookahead, if end_marker is possible for current position

    When the decoder lookahead, and the lookahead symbol is not end_marker, we have two ways:
      1) Strict mode (default) : the decoder returns SZ_ERROR_DATA.
      2) The relaxed mode (alternative mode) : we could return SZ_OK, and the caller
         must check (status) value. The caller can show the error,
         if the end of stream is expected, and the (status) is noit
         LZMA_STATUS_FINISHED_WITH_MARK or LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK.
    */

#define RETURN__NOT_FINISHED__FOR_FINISH \
    *status = LZMA_STATUS_NOT_FINISHED;  \
    return SZ_ERROR_DATA; // for strict mode
    // return SZ_OK; // for relaxed mode

    SRes LzmaDec_DecodeToDic(CLzmaDec * p, SizeT dicLimit, const Byte *src, SizeT *srcLen,
                             ELzmaFinishMode finishMode, ELzmaStatus *status)
    {
        SizeT inSize = *srcLen;
        (*srcLen) = 0;
        *status = LZMA_STATUS_NOT_SPECIFIED;

        if (p->remainLen > kMatchSpecLenStart)
        {
            if (p->remainLen > kMatchSpecLenStart + 2)
                return p->remainLen == kMatchSpecLen_Error_Fail ? SZ_ERROR_FAIL : SZ_ERROR_DATA;

            for (; inSize > 0 && p->tempBufSize < RC_INIT_SIZE; (*srcLen)++, inSize--)
                p->tempBuf[p->tempBufSize++] = *src++;
            if (p->tempBufSize != 0 && p->tempBuf[0] != 0)
                return SZ_ERROR_DATA;
            if (p->tempBufSize < RC_INIT_SIZE)
            {
                *status = LZMA_STATUS_NEEDS_MORE_INPUT;
                return SZ_OK;
            }
            p->code =
                ((UInt32)p->tempBuf[1] << 24) | ((UInt32)p->tempBuf[2] << 16) | ((UInt32)p->tempBuf[3] << 8) | ((UInt32)p->tempBuf[4]);

            if (p->checkDicSize == 0 && p->processedPos == 0 && p->code >= kBadRepCode)
                return SZ_ERROR_DATA;

            p->range = 0xFFFFFFFF;
            p->tempBufSize = 0;

            if (p->remainLen > kMatchSpecLenStart + 1)
            {
                SizeT numProbs = LzmaProps_GetNumProbs(&p->prop);
                SizeT i;
                CLzmaProb *probs = p->probs;
                for (i = 0; i < numProbs; i++)
                    probs[i] = kBitModelTotal >> 1;
                p->reps[0] = p->reps[1] = p->reps[2] = p->reps[3] = 1;
                p->state = 0;
            }

            p->remainLen = 0;
        }

        for (;;)
        {
            if (p->remainLen == kMatchSpecLenStart)
            {
                if (p->code != 0)
                    return SZ_ERROR_DATA;
                *status = LZMA_STATUS_FINISHED_WITH_MARK;
                return SZ_OK;
            }

            LzmaDec_WriteRem(p, dicLimit);

            {
                // (p->remainLen == 0 || p->dicPos == dicLimit)

                int checkEndMarkNow = 0;

                if (p->dicPos >= dicLimit)
                {
                    if (p->remainLen == 0 && p->code == 0)
                    {
                        *status = LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK;
                        return SZ_OK;
                    }
                    if (finishMode == LZMA_FINISH_ANY)
                    {
                        *status = LZMA_STATUS_NOT_FINISHED;
                        return SZ_OK;
                    }
                    if (p->remainLen != 0)
                    {
                        RETURN__NOT_FINISHED__FOR_FINISH;
                    }
                    checkEndMarkNow = 1;
                }

                // (p->remainLen == 0)

                if (p->tempBufSize == 0)
                {
                    const Byte *bufLimit;
                    int dummyProcessed = -1;

                    if (inSize < LZMA_REQUIRED_INPUT_MAX || checkEndMarkNow)
                    {
                        const Byte *bufOut = src + inSize;

                        ELzmaDummy dummyRes = LzmaDec_TryDummy(p, src, &bufOut);

                        if (dummyRes == DUMMY_INPUT_EOF)
                        {
                            size_t i;
                            if (inSize >= LZMA_REQUIRED_INPUT_MAX)
                                break;
                            (*srcLen) += inSize;
                            p->tempBufSize = (unsigned)inSize;
                            for (i = 0; i < inSize; i++)
                                p->tempBuf[i] = src[i];
                            *status = LZMA_STATUS_NEEDS_MORE_INPUT;
                            return SZ_OK;
                        }

                        dummyProcessed = (int)(bufOut - src);
                        if ((unsigned)dummyProcessed > LZMA_REQUIRED_INPUT_MAX)
                            break;

                        if (checkEndMarkNow && !IS_DUMMY_END_MARKER_POSSIBLE(dummyRes))
                        {
                            unsigned i;
                            (*srcLen) += (unsigned)dummyProcessed;
                            p->tempBufSize = (unsigned)dummyProcessed;
                            for (i = 0; i < (unsigned)dummyProcessed; i++)
                                p->tempBuf[i] = src[i];
                            // p->remainLen = kMatchSpecLen_Error_Data;
                            RETURN__NOT_FINISHED__FOR_FINISH;
                        }

                        bufLimit = src;
                        // we will decode only one iteration
                    }
                    else
                        bufLimit = src + inSize - LZMA_REQUIRED_INPUT_MAX;

                    p->buf = src;

                    {
                        int res = LzmaDec_DecodeReal2(p, dicLimit, bufLimit);

                        SizeT processed = (SizeT)(p->buf - src);

                        if (dummyProcessed < 0)
                        {
                            if (processed > inSize)
                                break;
                        }
                        else if ((unsigned)dummyProcessed != processed)
                            break;

                        src += processed;
                        inSize -= processed;
                        (*srcLen) += processed;

                        if (res != SZ_OK)
                        {
                            p->remainLen = kMatchSpecLen_Error_Data;
                            return SZ_ERROR_DATA;
                        }
                    }
                    continue;
                }

                {
                    // we have some data in (p->tempBuf)
                    // in strict mode: tempBufSize is not enough for one Symbol decoding.
                    // in relaxed mode: tempBufSize not larger than required for one Symbol decoding.

                    unsigned rem = p->tempBufSize;
                    unsigned ahead = 0;
                    int dummyProcessed = -1;

                    while (rem < LZMA_REQUIRED_INPUT_MAX && ahead < inSize)
                        p->tempBuf[rem++] = src[ahead++];

                    // ahead - the size of new data copied from (src) to (p->tempBuf)
                    // rem   - the size of temp buffer including new data from (src)

                    if (rem < LZMA_REQUIRED_INPUT_MAX || checkEndMarkNow)
                    {
                        const Byte *bufOut = p->tempBuf + rem;

                        ELzmaDummy dummyRes = LzmaDec_TryDummy(p, p->tempBuf, &bufOut);

                        if (dummyRes == DUMMY_INPUT_EOF)
                        {
                            if (rem >= LZMA_REQUIRED_INPUT_MAX)
                                break;
                            p->tempBufSize = rem;
                            (*srcLen) += (SizeT)ahead;
                            *status = LZMA_STATUS_NEEDS_MORE_INPUT;
                            return SZ_OK;
                        }

                        dummyProcessed = (int)(bufOut - p->tempBuf);

                        if ((unsigned)dummyProcessed < p->tempBufSize)
                            break;

                        if (checkEndMarkNow && !IS_DUMMY_END_MARKER_POSSIBLE(dummyRes))
                        {
                            (*srcLen) += (unsigned)dummyProcessed - p->tempBufSize;
                            p->tempBufSize = (unsigned)dummyProcessed;
                            // p->remainLen = kMatchSpecLen_Error_Data;
                            RETURN__NOT_FINISHED__FOR_FINISH;
                        }
                    }

                    p->buf = p->tempBuf;

                    {
                        // we decode one symbol from (p->tempBuf) here, so the (bufLimit) is equal to (p->buf)
                        int res = LzmaDec_DecodeReal2(p, dicLimit, p->buf);

                        SizeT processed = (SizeT)(p->buf - p->tempBuf);
                        rem = p->tempBufSize;

                        if (dummyProcessed < 0)
                        {
                            if (processed > LZMA_REQUIRED_INPUT_MAX)
                                break;
                            if (processed < rem)
                                break;
                        }
                        else if ((unsigned)dummyProcessed != processed)
                            break;

                        processed -= rem;

                        src += processed;
                        inSize -= processed;
                        (*srcLen) += processed;
                        p->tempBufSize = 0;

                        if (res != SZ_OK)
                        {
                            p->remainLen = kMatchSpecLen_Error_Data;
                            return SZ_ERROR_DATA;
                        }
                    }
                }
            }
        }

        /*  Some unexpected error: internal error of code, memory corruption or hardware failure */
        p->remainLen = kMatchSpecLen_Error_Fail;
        return SZ_ERROR_FAIL;
    }

    SRes LzmaDec_DecodeToBuf(CLzmaDec * p, Byte * dest, SizeT * destLen, const Byte *src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status)
    {
        SizeT outSize = *destLen;
        SizeT inSize = *srcLen;
        *srcLen = *destLen = 0;
        for (;;)
        {
            SizeT inSizeCur = inSize, outSizeCur, dicPos;
            ELzmaFinishMode curFinishMode;
            SRes res;
            if (p->dicPos == p->dicBufSize)
                p->dicPos = 0;
            dicPos = p->dicPos;
            if (outSize > p->dicBufSize - dicPos)
            {
                outSizeCur = p->dicBufSize;
                curFinishMode = LZMA_FINISH_ANY;
            }
            else
            {
                outSizeCur = dicPos + outSize;
                curFinishMode = finishMode;
            }

            res = LzmaDec_DecodeToDic(p, outSizeCur, src, &inSizeCur, curFinishMode, status);
            src += inSizeCur;
            inSize -= inSizeCur;
            *srcLen += inSizeCur;
            outSizeCur = p->dicPos - dicPos;
            memcpy(dest, p->dic + dicPos, outSizeCur);
            dest += outSizeCur;
            outSize -= outSizeCur;
            *destLen += outSizeCur;
            if (res != 0)
                return res;
            if (outSizeCur == 0 || outSize == 0)
                return SZ_OK;
        }
    }

    void LzmaDec_FreeProbs(CLzmaDec * p, ISzAllocPtr alloc)
    {
        ISzAlloc_Free(alloc, p->probs);
        p->probs = NULL;
    }

    static void LzmaDec_FreeDict(CLzmaDec * p, ISzAllocPtr alloc)
    {
        ISzAlloc_Free(alloc, p->dic);
        p->dic = NULL;
    }

    void LzmaDec_Free(CLzmaDec * p, ISzAllocPtr alloc)
    {
        LzmaDec_FreeProbs(p, alloc);
        LzmaDec_FreeDict(p, alloc);
    }

    SRes LzmaProps_Decode(CLzmaProps * p, const Byte *data, unsigned size)
    {
        UInt32 dicSize;
        Byte d;

        if (size < LZMA_PROPS_SIZE)
            return SZ_ERROR_UNSUPPORTED;
        else
            dicSize = data[1] | ((UInt32)data[2] << 8) | ((UInt32)data[3] << 16) | ((UInt32)data[4] << 24);

        if (dicSize < LZMA_DIC_MIN)
            dicSize = LZMA_DIC_MIN;
        p->dicSize = dicSize;

        d = data[0];
        if (d >= (9 * 5 * 5))
            return SZ_ERROR_UNSUPPORTED;

        p->lc = (Byte)(d % 9);
        d /= 9;
        p->pb = (Byte)(d / 5);
        p->lp = (Byte)(d % 5);

        return SZ_OK;
    }

    static SRes LzmaDec_AllocateProbs2(CLzmaDec * p, const CLzmaProps *propNew, ISzAllocPtr alloc)
    {
        UInt32 numProbs = LzmaProps_GetNumProbs(propNew);
        if (!p->probs || numProbs != p->numProbs)
        {
            LzmaDec_FreeProbs(p, alloc);
            p->probs = (CLzmaProb *)ISzAlloc_Alloc(alloc, numProbs * sizeof(CLzmaProb));
            if (!p->probs)
                return SZ_ERROR_MEM;
            p->probs_1664 = p->probs + 1664;
            p->numProbs = numProbs;
        }
        return SZ_OK;
    }

    SRes LzmaDec_AllocateProbs(CLzmaDec * p, const Byte *props, unsigned propsSize, ISzAllocPtr alloc)
    {
        CLzmaProps propNew;
        RINOK(LzmaProps_Decode(&propNew, props, propsSize));
        RINOK(LzmaDec_AllocateProbs2(p, &propNew, alloc));
        p->prop = propNew;
        return SZ_OK;
    }

    SRes LzmaDec_Allocate(CLzmaDec * p, const Byte *props, unsigned propsSize, ISzAllocPtr alloc)
    {
        CLzmaProps propNew;
        SizeT dicBufSize;
        RINOK(LzmaProps_Decode(&propNew, props, propsSize));
        RINOK(LzmaDec_AllocateProbs2(p, &propNew, alloc));

        {
            UInt32 dictSize = propNew.dicSize;
            SizeT mask = ((UInt32)1 << 12) - 1;
            if (dictSize >= ((UInt32)1 << 30))
                mask = ((UInt32)1 << 22) - 1;
            else if (dictSize >= ((UInt32)1 << 22))
                mask = ((UInt32)1 << 20) - 1;
            ;
            dicBufSize = ((SizeT)dictSize + mask) & ~mask;
            if (dicBufSize < dictSize)
                dicBufSize = dictSize;
        }

        if (!p->dic || dicBufSize != p->dicBufSize)
        {
            LzmaDec_FreeDict(p, alloc);
            p->dic = (Byte *)ISzAlloc_Alloc(alloc, dicBufSize);
            if (!p->dic)
            {
                LzmaDec_FreeProbs(p, alloc);
                return SZ_ERROR_MEM;
            }
        }
        p->dicBufSize = dicBufSize;
        p->prop = propNew;
        return SZ_OK;
    }

    SRes LzmaDecode(Byte * dest, SizeT * destLen, const Byte *src, SizeT *srcLen,
                    const Byte *propData, unsigned propSize, ELzmaFinishMode finishMode,
                    ELzmaStatus *status, ISzAllocPtr alloc)
    {
        CLzmaDec p;
        SRes res;
        SizeT outSize = *destLen, inSize = *srcLen;
        *destLen = *srcLen = 0;
        *status = LZMA_STATUS_NOT_SPECIFIED;
        if (inSize < RC_INIT_SIZE)
            return SZ_ERROR_INPUT_EOF;
        LzmaDec_Construct(&p);
        RINOK(LzmaDec_AllocateProbs(&p, propData, propSize, alloc));
        p.dic = dest;
        p.dicBufSize = outSize;
        LzmaDec_Init(&p);
        *srcLen = inSize;
        res = LzmaDec_DecodeToDic(&p, outSize, src, srcLen, finishMode, status);
        *destLen = p.dicPos;
        if (res == SZ_OK && *status == LZMA_STATUS_NEEDS_MORE_INPUT)
            res = SZ_ERROR_INPUT_EOF;
        LzmaDec_FreeProbs(&p, alloc);
        return res;
    }

    int lzma_uncompress(unsigned char *dest, size_t *destLen, const unsigned char *src, size_t *srcLen)
    {
        ELzmaStatus status;
        *srcLen -= LZMA_HEADER_SIZE;
        return LzmaDecode(dest, destLen, &src[LZMA_HEADER_SIZE], srcLen, src, LZMA_PROPS_SIZE, LZMA_FINISH_ANY, &status, &LZMA_ALLOC);
    }

#endif // LZMA_IMPLEMENTATION
#endif // _LZMA_H_
