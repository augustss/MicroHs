#include <errno.h>

#include "mhsffi.h"

#ifndef E2BIG
#define E2BIG -1
#endif
#ifndef EACCES
#define EACCES -1
#endif
#ifndef EADDRINUSE
#define EADDRINUSE -1
#endif
#ifndef EADDRNOTAVAIL
#define EADDRNOTAVAIL -1
#endif
#ifndef EADV
#define EADV -1
#endif
#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT -1
#endif
#ifndef EAGAIN
#define EAGAIN -1
#endif
#ifndef EALREADY
#define EALREADY -1
#endif
#ifndef EBADF
#define EBADF -1
#endif
#ifndef EBADMSG
#define EBADMSG -1
#endif
#ifndef EBADRPC
#define EBADRPC -1
#endif
#ifndef EBUSY
#define EBUSY -1
#endif
#ifndef ECHILD
#define ECHILD -1
#endif
#ifndef ECOMM
#define ECOMM -1
#endif
#ifndef ECONNABORTED
#define ECONNABORTED -1
#endif
#ifndef ECONNREFUSED
#define ECONNREFUSED -1
#endif
#ifndef ECONNRESET
#define ECONNRESET -1
#endif
#ifndef EDEADLK
#define EDEADLK -1
#endif
#ifndef EDESTADDRREQ
#define EDESTADDRREQ -1
#endif
#ifndef EDIRTY
#define EDIRTY -1
#endif
#ifndef EDOM
#define EDOM -1
#endif
#ifndef EDQUOT
#define EDQUOT -1
#endif
#ifndef EEXIST
#define EEXIST -1
#endif
#ifndef EFAULT
#define EFAULT -1
#endif
#ifndef EFBIG
#define EFBIG -1
#endif
#ifndef EFTYPE
#define EFTYPE -1
#endif
#ifndef EHOSTDOWN
#define EHOSTDOWN -1
#endif
#ifndef EHOSTUNREACH
#define EHOSTUNREACH -1
#endif
#ifndef EIDRM
#define EIDRM -1
#endif
#ifndef EILSEQ
#define EILSEQ -1
#endif
#ifndef EINPROGRESS
#define EINPROGRESS -1
#endif
#ifndef EINTR
#define EINTR -1
#endif
#ifndef EINVAL
#define EINVAL -1
#endif
#ifndef EIO
#define EIO -1
#endif
#ifndef EISCONN
#define EISCONN -1
#endif
#ifndef EISDIR
#define EISDIR -1
#endif
#ifndef ELOOP
#define ELOOP -1
#endif
#ifndef EMFILE
#define EMFILE -1
#endif
#ifndef EMLINK
#define EMLINK -1
#endif
#ifndef EMSGSIZE
#define EMSGSIZE -1
#endif
#ifndef EMULTIHOP
#define EMULTIHOP -1
#endif
#ifndef ENAMETOOLONG
#define ENAMETOOLONG -1
#endif
#ifndef ENETDOWN
#define ENETDOWN -1
#endif
#ifndef ENETRESET
#define ENETRESET -1
#endif
#ifndef ENETUNREACH
#define ENETUNREACH -1
#endif
#ifndef ENFILE
#define ENFILE -1
#endif
#ifndef ENOBUFS
#define ENOBUFS -1
#endif
#ifndef ENODATA
#define ENODATA -1
#endif
#ifndef ENODEV
#define ENODEV -1
#endif
#ifndef ENOENT
#define ENOENT -1
#endif
#ifndef ENOEXEC
#define ENOEXEC -1
#endif
#ifndef ENOLCK
#define ENOLCK -1
#endif
#ifndef ENOLINK
#define ENOLINK -1
#endif
#ifndef ENOMEM
#define ENOMEM -1
#endif
#ifndef ENOMSG
#define ENOMSG -1
#endif
#ifndef ENONET
#define ENONET -1
#endif
#ifndef ENOPROTOOPT
#define ENOPROTOOPT -1
#endif
#ifndef ENOSPC
#define ENOSPC -1
#endif
#ifndef ENOSR
#define ENOSR -1
#endif
#ifndef ENOSTR
#define ENOSTR -1
#endif
#ifndef ENOSYS
#define ENOSYS -1
#endif
#ifndef ENOTBLK
#define ENOTBLK -1
#endif
#ifndef ENOTCONN
#define ENOTCONN -1
#endif
#ifndef ENOTDIR
#define ENOTDIR -1
#endif
#ifndef ENOTEMPTY
#define ENOTEMPTY -1
#endif
#ifndef ENOTSOCK
#define ENOTSOCK -1
#endif
#ifndef ENOTSUP
#define ENOTSUP -1
#endif
#ifndef ENOTTY
#define ENOTTY -1
#endif
#ifndef ENXIO
#define ENXIO -1
#endif
#ifndef EOPNOTSUPP
#define EOPNOTSUPP -1
#endif
#ifndef EPERM
#define EPERM -1
#endif
#ifndef EPFNOSUPPORT
#define EPFNOSUPPORT -1
#endif
#ifndef EPIPE
#define EPIPE -1
#endif
#ifndef EPROCLIM
#define EPROCLIM -1
#endif
#ifndef EPROCUNAVAIL
#define EPROCUNAVAIL -1
#endif
#ifndef EPROGMISMATCH
#define EPROGMISMATCH -1
#endif
#ifndef EPROGUNAVAIL
#define EPROGUNAVAIL -1
#endif
#ifndef EPROTO
#define EPROTO -1
#endif
#ifndef EPROTONOSUPPORT
#define EPROTONOSUPPORT -1
#endif
#ifndef EPROTOTYPE
#define EPROTOTYPE -1
#endif
#ifndef ERANGE
#define ERANGE -1
#endif
#ifndef EREMCHG
#define EREMCHG -1
#endif
#ifndef EREMOTE
#define EREMOTE -1
#endif
#ifndef EROFS
#define EROFS -1
#endif
#ifndef ERPCMISMATCH
#define ERPCMISMATCH -1
#endif
#ifndef ERREMOTE
#define ERREMOTE -1
#endif
#ifndef ESHUTDOWN
#define ESHUTDOWN -1
#endif
#ifndef ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT -1
#endif
#ifndef ESPIPE
#define ESPIPE -1
#endif
#ifndef ESRCH
#define ESRCH -1
#endif
#ifndef ESRMNT
#define ESRMNT -1
#endif
#ifndef ESTALE
#define ESTALE -1
#endif
#ifndef ETIME
#define ETIME -1
#endif
#ifndef ETIMEDOUT
#define ETIMEDOUT -1
#endif
#ifndef ETOOMANYREFS
#define ETOOMANYREFS -1
#endif
#ifndef ETXTBSY
#define ETXTBSY -1
#endif
#ifndef EUSERS
#define EUSERS -1
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK -1
#endif
#ifndef EXDEV
#define EXDEV -1
#endif

from_t mhs_E2BIG(int s) { return mhs_from_CInt(s, 0, E2BIG); }
from_t mhs_EACCES(int s) { return mhs_from_CInt(s, 0, EACCES); }
from_t mhs_EADDRINUSE(int s) { return mhs_from_CInt(s, 0, EADDRINUSE); }
from_t mhs_EADDRNOTAVAIL(int s) { return mhs_from_CInt(s, 0, EADDRNOTAVAIL); }
from_t mhs_EADV(int s) { return mhs_from_CInt(s, 0, EADV); }
from_t mhs_EAFNOSUPPORT(int s) { return mhs_from_CInt(s, 0, EAFNOSUPPORT); }
from_t mhs_EAGAIN(int s) { return mhs_from_CInt(s, 0, EAGAIN); }
from_t mhs_EALREADY(int s) { return mhs_from_CInt(s, 0, EALREADY); }
from_t mhs_EBADF(int s) { return mhs_from_CInt(s, 0, EBADF); }
from_t mhs_EBADMSG(int s) { return mhs_from_CInt(s, 0, EBADMSG); }
from_t mhs_EBADRPC(int s) { return mhs_from_CInt(s, 0, EBADRPC); }
from_t mhs_EBUSY(int s) { return mhs_from_CInt(s, 0, EBUSY); }
from_t mhs_ECHILD(int s) { return mhs_from_CInt(s, 0, ECHILD); }
from_t mhs_ECOMM(int s) { return mhs_from_CInt(s, 0, ECOMM); }
from_t mhs_ECONNABORTED(int s) { return mhs_from_CInt(s, 0, ECONNABORTED); }
from_t mhs_ECONNREFUSED(int s) { return mhs_from_CInt(s, 0, ECONNREFUSED); }
from_t mhs_ECONNRESET(int s) { return mhs_from_CInt(s, 0, ECONNRESET); }
from_t mhs_EDEADLK(int s) { return mhs_from_CInt(s, 0, EDEADLK); }
from_t mhs_EDESTADDRREQ(int s) { return mhs_from_CInt(s, 0, EDESTADDRREQ); }
from_t mhs_EDIRTY(int s) { return mhs_from_CInt(s, 0, EDIRTY); }
from_t mhs_EDOM(int s) { return mhs_from_CInt(s, 0, EDOM); }
from_t mhs_EDQUOT(int s) { return mhs_from_CInt(s, 0, EDQUOT); }
from_t mhs_EEXIST(int s) { return mhs_from_CInt(s, 0, EEXIST); }
from_t mhs_EFAULT(int s) { return mhs_from_CInt(s, 0, EFAULT); }
from_t mhs_EFBIG(int s) { return mhs_from_CInt(s, 0, EFBIG); }
from_t mhs_EFTYPE(int s) { return mhs_from_CInt(s, 0, EFTYPE); }
from_t mhs_EHOSTDOWN(int s) { return mhs_from_CInt(s, 0, EHOSTDOWN); }
from_t mhs_EHOSTUNREACH(int s) { return mhs_from_CInt(s, 0, EHOSTUNREACH); }
from_t mhs_EIDRM(int s) { return mhs_from_CInt(s, 0, EIDRM); }
from_t mhs_EILSEQ(int s) { return mhs_from_CInt(s, 0, EILSEQ); }
from_t mhs_EINPROGRESS(int s) { return mhs_from_CInt(s, 0, EINPROGRESS); }
from_t mhs_EINTR(int s) { return mhs_from_CInt(s, 0, EINTR); }
from_t mhs_EINVAL(int s) { return mhs_from_CInt(s, 0, EINVAL); }
from_t mhs_EIO(int s) { return mhs_from_CInt(s, 0, EIO); }
from_t mhs_EISCONN(int s) { return mhs_from_CInt(s, 0, EISCONN); }
from_t mhs_EISDIR(int s) { return mhs_from_CInt(s, 0, EISDIR); }
from_t mhs_ELOOP(int s) { return mhs_from_CInt(s, 0, ELOOP); }
from_t mhs_EMFILE(int s) { return mhs_from_CInt(s, 0, EMFILE); }
from_t mhs_EMLINK(int s) { return mhs_from_CInt(s, 0, EMLINK); }
from_t mhs_EMSGSIZE(int s) { return mhs_from_CInt(s, 0, EMSGSIZE); }
from_t mhs_EMULTIHOP(int s) { return mhs_from_CInt(s, 0, EMULTIHOP); }
from_t mhs_ENAMETOOLONG(int s) { return mhs_from_CInt(s, 0, ENAMETOOLONG); }
from_t mhs_ENETDOWN(int s) { return mhs_from_CInt(s, 0, ENETDOWN); }
from_t mhs_ENETRESET(int s) { return mhs_from_CInt(s, 0, ENETRESET); }
from_t mhs_ENETUNREACH(int s) { return mhs_from_CInt(s, 0, ENETUNREACH); }
from_t mhs_ENFILE(int s) { return mhs_from_CInt(s, 0, ENFILE); }
from_t mhs_ENOBUFS(int s) { return mhs_from_CInt(s, 0, ENOBUFS); }
from_t mhs_ENODATA(int s) { return mhs_from_CInt(s, 0, ENODATA); }
from_t mhs_ENODEV(int s) { return mhs_from_CInt(s, 0, ENODEV); }
from_t mhs_ENOENT(int s) { return mhs_from_CInt(s, 0, ENOENT); }
from_t mhs_ENOEXEC(int s) { return mhs_from_CInt(s, 0, ENOEXEC); }
from_t mhs_ENOLCK(int s) { return mhs_from_CInt(s, 0, ENOLCK); }
from_t mhs_ENOLINK(int s) { return mhs_from_CInt(s, 0, ENOLINK); }
from_t mhs_ENOMEM(int s) { return mhs_from_CInt(s, 0, ENOMEM); }
from_t mhs_ENOMSG(int s) { return mhs_from_CInt(s, 0, ENOMSG); }
from_t mhs_ENONET(int s) { return mhs_from_CInt(s, 0, ENONET); }
from_t mhs_ENOPROTOOPT(int s) { return mhs_from_CInt(s, 0, ENOPROTOOPT); }
from_t mhs_ENOSPC(int s) { return mhs_from_CInt(s, 0, ENOSPC); }
from_t mhs_ENOSR(int s) { return mhs_from_CInt(s, 0, ENOSR); }
from_t mhs_ENOSTR(int s) { return mhs_from_CInt(s, 0, ENOSTR); }
from_t mhs_ENOSYS(int s) { return mhs_from_CInt(s, 0, ENOSYS); }
from_t mhs_ENOTBLK(int s) { return mhs_from_CInt(s, 0, ENOTBLK); }
from_t mhs_ENOTCONN(int s) { return mhs_from_CInt(s, 0, ENOTCONN); }
from_t mhs_ENOTDIR(int s) { return mhs_from_CInt(s, 0, ENOTDIR); }
from_t mhs_ENOTEMPTY(int s) { return mhs_from_CInt(s, 0, ENOTEMPTY); }
from_t mhs_ENOTSOCK(int s) { return mhs_from_CInt(s, 0, ENOTSOCK); }
from_t mhs_ENOTSUP(int s) { return mhs_from_CInt(s, 0, ENOTSUP); }
from_t mhs_ENOTTY(int s) { return mhs_from_CInt(s, 0, ENOTTY); }
from_t mhs_ENXIO(int s) { return mhs_from_CInt(s, 0, ENXIO); }
from_t mhs_EOPNOTSUPP(int s) { return mhs_from_CInt(s, 0, EOPNOTSUPP); }
from_t mhs_EPERM(int s) { return mhs_from_CInt(s, 0, EPERM); }
from_t mhs_EPFNOSUPPORT(int s) { return mhs_from_CInt(s, 0, EPFNOSUPPORT); }
from_t mhs_EPIPE(int s) { return mhs_from_CInt(s, 0, EPIPE); }
from_t mhs_EPROCLIM(int s) { return mhs_from_CInt(s, 0, EPROCLIM); }
from_t mhs_EPROCUNAVAIL(int s) { return mhs_from_CInt(s, 0, EPROCUNAVAIL); }
from_t mhs_EPROGMISMATCH(int s) { return mhs_from_CInt(s, 0, EPROGMISMATCH); }
from_t mhs_EPROGUNAVAIL(int s) { return mhs_from_CInt(s, 0, EPROGUNAVAIL); }
from_t mhs_EPROTO(int s) { return mhs_from_CInt(s, 0, EPROTO); }
from_t mhs_EPROTONOSUPPORT(int s) { return mhs_from_CInt(s, 0, EPROTONOSUPPORT); }
from_t mhs_EPROTOTYPE(int s) { return mhs_from_CInt(s, 0, EPROTOTYPE); }
from_t mhs_ERANGE(int s) { return mhs_from_CInt(s, 0, ERANGE); }
from_t mhs_EREMCHG(int s) { return mhs_from_CInt(s, 0, EREMCHG); }
from_t mhs_EREMOTE(int s) { return mhs_from_CInt(s, 0, EREMOTE); }
from_t mhs_EROFS(int s) { return mhs_from_CInt(s, 0, EROFS); }
from_t mhs_ERPCMISMATCH(int s) { return mhs_from_CInt(s, 0, ERPCMISMATCH); }
from_t mhs_ERREMOTE(int s) { return mhs_from_CInt(s, 0, ERREMOTE); }
from_t mhs_ESHUTDOWN(int s) { return mhs_from_CInt(s, 0, ESHUTDOWN); }
from_t mhs_ESOCKTNOSUPPORT(int s) { return mhs_from_CInt(s, 0, ESOCKTNOSUPPORT); }
from_t mhs_ESPIPE(int s) { return mhs_from_CInt(s, 0, ESPIPE); }
from_t mhs_ESRCH(int s) { return mhs_from_CInt(s, 0, ESRCH); }
from_t mhs_ESRMNT(int s) { return mhs_from_CInt(s, 0, ESRMNT); }
from_t mhs_ESTALE(int s) { return mhs_from_CInt(s, 0, ESTALE); }
from_t mhs_ETIME(int s) { return mhs_from_CInt(s, 0, ETIME); }
from_t mhs_ETIMEDOUT(int s) { return mhs_from_CInt(s, 0, ETIMEDOUT); }
from_t mhs_ETOOMANYREFS(int s) { return mhs_from_CInt(s, 0, ETOOMANYREFS); }
from_t mhs_ETXTBSY(int s) { return mhs_from_CInt(s, 0, ETXTBSY); }
from_t mhs_EUSERS(int s) { return mhs_from_CInt(s, 0, EUSERS); }
from_t mhs_EWOULDBLOCK(int s) { return mhs_from_CInt(s, 0, EWOULDBLOCK); }
from_t mhs_EXDEV(int s) { return mhs_from_CInt(s, 0, EXDEV); }

from_t mhs_addr_errno(int s) { return mhs_from_Ptr(s, 0, &errno); }
from_t mhs_strerror_r(int s) { return mhs_from_CInt(s, 3, strerror_r(mhs_to_CInt(s, 0), mhs_to_Ptr(s, 1), mhs_to_CSize(s, 2))); }
