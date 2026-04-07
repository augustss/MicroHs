-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Error
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- C-specific Marshalling support: Handling of C \"errno\" error codes.
--
-----------------------------------------------------------------------------

module Foreign.C.Error (
  -- * Haskell representations of @errno@ values

  Errno(..),

  -- ** Common @errno@ symbols
  -- | Different operating systems and\/or C libraries often support
  -- different values of @errno@.  This module defines the common values,
  -- but due to the open definition of 'Errno' users may add definitions
  -- which are not predefined.
  eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN,
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED,
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT,
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ,
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK,
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH,
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK,
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS,
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTSUP, eNOTTY, eNXIO,
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL,
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE,
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN,
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT,
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV,

  -- ** 'Errno' functions
  isValidErrno,

  -- access to the current thread's "errno" value
  --
  getErrno,
  resetErrno,

  -- conversion of an "errno" value into IO error
  --
  errnoToIOError,

  -- throw current "errno" value
  --
  throwErrno,

  -- ** Guards for IO operations that may fail

  throwErrnoIf,
  throwErrnoIf_,
  throwErrnoIfRetry,
  throwErrnoIfRetry_,
  throwErrnoIfMinus1,
  throwErrnoIfMinus1_,
  throwErrnoIfMinus1Retry,
  throwErrnoIfMinus1Retry_,
  throwErrnoIfNull,
  throwErrnoIfNullRetry,

  throwErrnoIfRetryMayBlock,
  throwErrnoIfRetryMayBlock_,
  throwErrnoIfMinus1RetryMayBlock,
  throwErrnoIfMinus1RetryMayBlock_,
  throwErrnoIfNullRetryMayBlock,

  throwErrnoPath,
  throwErrnoPathIf,
  throwErrnoPathIf_,
  throwErrnoPathIfNull,
  throwErrnoPathIfMinus1,
  throwErrnoPathIfMinus1_,
) where
import qualified Prelude(); import MiniPrelude

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Data.Functor(void)
import Data.Maybe
import System.IO.Base(Handle)
import System.IO.Error
import System.IO.Unsafe(unsafePerformIO)

newtype Errno = Errno CInt

instance Eq Errno where
  errno1@(Errno no1) == Errno no2
    | isValidErrno errno1 = no1 == no2
    | otherwise           = False

eOK :: Errno
eOK = Errno 0

foreign import capi "value E2BIG"           e2BIG           :: Errno
foreign import capi "value EACCES"          eACCES          :: Errno
foreign import capi "value EADDRINUSE"      eADDRINUSE      :: Errno
foreign import capi "value EADDRNOTAVAIL"   eADDRNOTAVAIL   :: Errno
foreign import capi "value EADV"            eADV            :: Errno
foreign import capi "value EAFNOSUPPORT"    eAFNOSUPPORT    :: Errno
foreign import capi "value EAGAIN"          eAGAIN          :: Errno
foreign import capi "value EALREADY"        eALREADY        :: Errno
foreign import capi "value EBADF"           eBADF           :: Errno
foreign import capi "value EBADMSG"         eBADMSG         :: Errno
foreign import capi "value EBADRPC"         eBADRPC         :: Errno
foreign import capi "value EBUSY"           eBUSY           :: Errno
foreign import capi "value ECHILD"          eCHILD          :: Errno
foreign import capi "value ECOMM"           eCOMM           :: Errno
foreign import capi "value ECONNABORTED"    eCONNABORTED    :: Errno
foreign import capi "value ECONNREFUSED"    eCONNREFUSED    :: Errno
foreign import capi "value ECONNRESET"      eCONNRESET      :: Errno
foreign import capi "value EDEADLK"         eDEADLK         :: Errno
foreign import capi "value EDESTADDRREQ"    eDESTADDRREQ    :: Errno
foreign import capi "value EDIRTY"          eDIRTY          :: Errno
foreign import capi "value EDOM"            eDOM            :: Errno
foreign import capi "value EDQUOT"          eDQUOT          :: Errno
foreign import capi "value EEXIST"          eEXIST          :: Errno
foreign import capi "value EFAULT"          eFAULT          :: Errno
foreign import capi "value EFBIG"           eFBIG           :: Errno
foreign import capi "value EFTYPE"          eFTYPE          :: Errno
foreign import capi "value EHOSTDOWN"       eHOSTDOWN       :: Errno
foreign import capi "value EHOSTUNREACH"    eHOSTUNREACH    :: Errno
foreign import capi "value EIDRM"           eIDRM           :: Errno
foreign import capi "value EILSEQ"          eILSEQ          :: Errno
foreign import capi "value EINPROGRESS"     eINPROGRESS     :: Errno
foreign import capi "value EINTR"           eINTR           :: Errno
foreign import capi "value EINVAL"          eINVAL          :: Errno
foreign import capi "value EIO"             eIO             :: Errno
foreign import capi "value EISCONN"         eISCONN         :: Errno
foreign import capi "value EISDIR"          eISDIR          :: Errno
foreign import capi "value ELOOP"           eLOOP           :: Errno
foreign import capi "value EMFILE"          eMFILE          :: Errno
foreign import capi "value EMLINK"          eMLINK          :: Errno
foreign import capi "value EMSGSIZE"        eMSGSIZE        :: Errno
foreign import capi "value EMULTIHOP"       eMULTIHOP       :: Errno
foreign import capi "value ENAMETOOLONG"    eNAMETOOLONG    :: Errno
foreign import capi "value ENETDOWN"        eNETDOWN        :: Errno
foreign import capi "value ENETRESET"       eNETRESET       :: Errno
foreign import capi "value ENETUNREACH"     eNETUNREACH     :: Errno
foreign import capi "value ENFILE"          eNFILE          :: Errno
foreign import capi "value ENOBUFS"         eNOBUFS         :: Errno
foreign import capi "value ENODATA"         eNODATA         :: Errno
foreign import capi "value ENODEV"          eNODEV          :: Errno
foreign import capi "value ENOENT"          eNOENT          :: Errno
foreign import capi "value ENOEXEC"         eNOEXEC         :: Errno
foreign import capi "value ENOLCK"          eNOLCK          :: Errno
foreign import capi "value ENOLINK"         eNOLINK         :: Errno
foreign import capi "value ENOMEM"          eNOMEM          :: Errno
foreign import capi "value ENOMSG"          eNOMSG          :: Errno
foreign import capi "value ENONET"          eNONET          :: Errno
foreign import capi "value ENOPROTOOPT"     eNOPROTOOPT     :: Errno
foreign import capi "value ENOSPC"          eNOSPC          :: Errno
foreign import capi "value ENOSR"           eNOSR           :: Errno
foreign import capi "value ENOSTR"          eNOSTR          :: Errno
foreign import capi "value ENOSYS"          eNOSYS          :: Errno
foreign import capi "value ENOTBLK"         eNOTBLK         :: Errno
foreign import capi "value ENOTCONN"        eNOTCONN        :: Errno
foreign import capi "value ENOTDIR"         eNOTDIR         :: Errno
foreign import capi "value ENOTEMPTY"       eNOTEMPTY       :: Errno
foreign import capi "value ENOTSOCK"        eNOTSOCK        :: Errno
foreign import capi "value ENOTSUP"         eNOTSUP         :: Errno
foreign import capi "value ENOTTY"          eNOTTY          :: Errno
foreign import capi "value ENXIO"           eNXIO           :: Errno
foreign import capi "value EOPNOTSUPP"      eOPNOTSUPP      :: Errno
foreign import capi "value EPERM"           ePERM           :: Errno
foreign import capi "value EPFNOSUPPORT"    ePFNOSUPPORT    :: Errno
foreign import capi "value EPIPE"           ePIPE           :: Errno
foreign import capi "value EPROCLIM"        ePROCLIM        :: Errno
foreign import capi "value EPROCUNAVAIL"    ePROCUNAVAIL    :: Errno
foreign import capi "value EPROGMISMATCH"   ePROGMISMATCH   :: Errno
foreign import capi "value EPROGUNAVAIL"    ePROGUNAVAIL    :: Errno
foreign import capi "value EPROTO"          ePROTO          :: Errno
foreign import capi "value EPROTONOSUPPORT" ePROTONOSUPPORT :: Errno
foreign import capi "value EPROTOTYPE"      ePROTOTYPE      :: Errno
foreign import capi "value ERANGE"          eRANGE          :: Errno
foreign import capi "value EREMCHG"         eREMCHG         :: Errno
foreign import capi "value EREMOTE"         eREMOTE         :: Errno
foreign import capi "value EROFS"           eROFS           :: Errno
foreign import capi "value ERPCMISMATCH"    eRPCMISMATCH    :: Errno
foreign import capi "value ERREMOTE"        eRREMOTE        :: Errno
foreign import capi "value ESHUTDOWN"       eSHUTDOWN       :: Errno
foreign import capi "value ESOCKTNOSUPPORT" eSOCKTNOSUPPORT :: Errno
foreign import capi "value ESPIPE"          eSPIPE          :: Errno
foreign import capi "value ESRCH"           eSRCH           :: Errno
foreign import capi "value ESRMNT"          eSRMNT          :: Errno
foreign import capi "value ESTALE"          eSTALE          :: Errno
foreign import capi "value ETIME"           eTIME           :: Errno
foreign import capi "value ETIMEDOUT"       eTIMEDOUT       :: Errno
foreign import capi "value ETOOMANYREFS"    eTOOMANYREFS    :: Errno
foreign import capi "value ETXTBSY"         eTXTBSY         :: Errno
foreign import capi "value EUSERS"          eUSERS          :: Errno
foreign import capi "value EWOULDBLOCK"     eWOULDBLOCK     :: Errno
foreign import capi "value EXDEV"           eXDEV           :: Errno

isValidErrno               :: Errno -> Bool
isValidErrno (Errno errno)  = errno /= -1

foreign import ccall unsafe "errno.h &errno" c_errno_ptr :: IO (Ptr CInt)

getErrno :: IO Errno
getErrno = do
  p <- c_errno_ptr
  e <- peek p
  return (Errno e)

resetErrno :: IO ()
resetErrno = do
  p <- c_errno_ptr
  poke p 0
  return ()

throwErrno :: String -> IO a
throwErrno loc = do
  errno <- getErrno
  ioError (errnoToIOError loc errno Nothing Nothing)

throwErrnoIf    :: (a -> Bool)  -- ^ predicate to apply to the result value
                                -- of the 'IO' operation
                -> String       -- ^ textual description of the location
                -> IO a         -- ^ the 'IO' operation to be executed
                -> IO a
throwErrnoIf pred loc f  =
  do
    res <- f
    if pred res then throwErrno loc else return res

throwErrnoIf_   :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ pred loc f  = void $ throwErrnoIf pred loc f

throwErrnoIfRetry            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry pred loc f  =
  do
    res <- f
    if pred res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoIfRetry pred loc f
          else throwErrno loc
      else return res

throwErrnoIfRetryMayBlock
                :: (a -> Bool)  -- ^ predicate to apply to the result value
                                -- of the 'IO' operation
                -> String       -- ^ textual description of the location
                -> IO a         -- ^ the 'IO' operation to be executed
                -> IO b         -- ^ action to execute before retrying if
                                -- an immediate retry would block
                -> IO a
throwErrnoIfRetryMayBlock pred loc f on_block  =
  do
    res <- f
    if pred res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoIfRetryMayBlock pred loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
                 then do _ <- on_block
                         throwErrnoIfRetryMayBlock pred loc f on_block
                 else throwErrno loc
      else return res

throwErrnoIfRetry_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ pred loc f  = void $ throwErrnoIfRetry pred loc f

throwErrnoIfRetryMayBlock_ :: (a -> Bool) -> String -> IO a -> IO b -> IO ()
throwErrnoIfRetryMayBlock_ pred loc f on_block
  = void $ throwErrnoIfRetryMayBlock pred loc f on_block

throwErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1  = throwErrnoIf (== (-1))

throwErrnoIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1_  = throwErrnoIf_ (== (-1))

throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== (-1))

throwErrnoIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_  = throwErrnoIfRetry_ (== (-1))

throwErrnoIfMinus1RetryMayBlock :: (Eq a, Num a)
                                => String -> IO a -> IO b -> IO a
throwErrnoIfMinus1RetryMayBlock  = throwErrnoIfRetryMayBlock (== (-1))

throwErrnoIfMinus1RetryMayBlock_ :: (Eq a, Num a)
                                 => String -> IO a -> IO b -> IO ()
throwErrnoIfMinus1RetryMayBlock_  = throwErrnoIfRetryMayBlock_ (== (-1))

throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull  = throwErrnoIf (== nullPtr)

throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry  = throwErrnoIfRetry (== nullPtr)

throwErrnoIfNullRetryMayBlock :: String -> IO (Ptr a) -> IO b -> IO (Ptr a)
throwErrnoIfNullRetryMayBlock  = throwErrnoIfRetryMayBlock (== nullPtr)

throwErrnoPath :: String -> FilePath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing (Just path))

throwErrnoPathIf :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIf pred loc path f =
  do
    res <- f
    if pred res then throwErrnoPath loc path else return res

throwErrnoPathIf_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIf_ pred loc path f  = void $ throwErrnoPathIf pred loc path f

throwErrnoPathIfNull :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull  = throwErrnoPathIf (== nullPtr)

throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (== (-1))

throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== (-1))


foreign import ccall "string.h strerror_r"
    c_strerror_r :: Errno -> CString -> CSize -> IO CInt

errnoToString :: Errno -> IO String
errnoToString errno =
    allocaBytes len $ \ ptr -> do
        ret <- c_strerror_r errno ptr (CSize len)
        if ret /= 0
          then return "errnoToString failed"
          else peekCString ptr
  where len :: (Num a => a); len = 512

errnoToIOError  :: String       -- ^ the location where the error occurred
                -> Errno        -- ^ the error number
                -> Maybe Handle -- ^ optional handle associated with the error
                -> Maybe String -- ^ optional filename associated with the error
                -> IOError
errnoToIOError loc errno@(Errno errno') maybeHdl maybeName = unsafePerformIO $ do
    str <- errnoToString errno
    return (IOError maybeHdl errType loc str (Just errno') maybeName)
  where
    errType
        | errno == eOK             = OtherError
        | errno == e2BIG           = ResourceExhausted
        | errno == eACCES          = PermissionDenied
        | errno == eADDRINUSE      = ResourceBusy
        | errno == eADDRNOTAVAIL   = UnsupportedOperation
        | errno == eADV            = OtherError
        | errno == eAFNOSUPPORT    = UnsupportedOperation
        | errno == eAGAIN          = ResourceExhausted
        | errno == eALREADY        = AlreadyExists
        | errno == eBADF           = InvalidArgument
        | errno == eBADMSG         = InappropriateType
        | errno == eBADRPC         = OtherError
        | errno == eBUSY           = ResourceBusy
        | errno == eCHILD          = NoSuchThing
        | errno == eCOMM           = ResourceVanished
        | errno == eCONNABORTED    = OtherError
        | errno == eCONNREFUSED    = NoSuchThing
        | errno == eCONNRESET      = ResourceVanished
        | errno == eDEADLK         = ResourceBusy
        | errno == eDESTADDRREQ    = InvalidArgument
        | errno == eDIRTY          = UnsatisfiedConstraints
        | errno == eDOM            = InvalidArgument
        | errno == eDQUOT          = PermissionDenied
        | errno == eEXIST          = AlreadyExists
        | errno == eFAULT          = OtherError
        | errno == eFBIG           = PermissionDenied
        | errno == eFTYPE          = InappropriateType
        | errno == eHOSTDOWN       = NoSuchThing
        | errno == eHOSTUNREACH    = NoSuchThing
        | errno == eIDRM           = ResourceVanished
        | errno == eILSEQ          = InvalidArgument
        | errno == eINPROGRESS     = AlreadyExists
        | errno == eINTR           = Interrupted
        | errno == eINVAL          = InvalidArgument
        | errno == eIO             = HardwareFault
        | errno == eISCONN         = AlreadyExists
        | errno == eISDIR          = InappropriateType
        | errno == eLOOP           = InvalidArgument
        | errno == eMFILE          = ResourceExhausted
        | errno == eMLINK          = ResourceExhausted
        | errno == eMSGSIZE        = ResourceExhausted
        | errno == eMULTIHOP       = UnsupportedOperation
        | errno == eNAMETOOLONG    = InvalidArgument
        | errno == eNETDOWN        = ResourceVanished
        | errno == eNETRESET       = ResourceVanished
        | errno == eNETUNREACH     = NoSuchThing
        | errno == eNFILE          = ResourceExhausted
        | errno == eNOBUFS         = ResourceExhausted
        | errno == eNODATA         = NoSuchThing
        | errno == eNODEV          = UnsupportedOperation
        | errno == eNOENT          = NoSuchThing
        | errno == eNOEXEC         = InvalidArgument
        | errno == eNOLCK          = ResourceExhausted
        | errno == eNOLINK         = ResourceVanished
        | errno == eNOMEM          = ResourceExhausted
        | errno == eNOMSG          = NoSuchThing
        | errno == eNONET          = NoSuchThing
        | errno == eNOPROTOOPT     = UnsupportedOperation
        | errno == eNOSPC          = ResourceExhausted
        | errno == eNOSR           = ResourceExhausted
        | errno == eNOSTR          = InvalidArgument
        | errno == eNOSYS          = UnsupportedOperation
        | errno == eNOTBLK         = InvalidArgument
        | errno == eNOTCONN        = InvalidArgument
        | errno == eNOTDIR         = InappropriateType
        | errno == eNOTEMPTY       = UnsatisfiedConstraints
        | errno == eNOTSOCK        = InvalidArgument
        | errno == eNOTTY          = IllegalOperation
        | errno == eNXIO           = NoSuchThing
        | errno == eOPNOTSUPP      = UnsupportedOperation
        | errno == ePERM           = PermissionDenied
        | errno == ePFNOSUPPORT    = UnsupportedOperation
        | errno == ePIPE           = ResourceVanished
        | errno == ePROCLIM        = PermissionDenied
        | errno == ePROCUNAVAIL    = UnsupportedOperation
        | errno == ePROGMISMATCH   = ProtocolError
        | errno == ePROGUNAVAIL    = UnsupportedOperation
        | errno == ePROTO          = ProtocolError
        | errno == ePROTONOSUPPORT = ProtocolError
        | errno == ePROTOTYPE      = ProtocolError
        | errno == eRANGE          = UnsupportedOperation
        | errno == eREMCHG         = ResourceVanished
        | errno == eREMOTE         = IllegalOperation
        | errno == eROFS           = PermissionDenied
        | errno == eRPCMISMATCH    = ProtocolError
        | errno == eRREMOTE        = IllegalOperation
        | errno == eSHUTDOWN       = IllegalOperation
        | errno == eSOCKTNOSUPPORT = UnsupportedOperation
        | errno == eSPIPE          = UnsupportedOperation
        | errno == eSRCH           = NoSuchThing
        | errno == eSRMNT          = UnsatisfiedConstraints
        | errno == eSTALE          = ResourceVanished
        | errno == eTIME           = TimeExpired
        | errno == eTIMEDOUT       = TimeExpired
        | errno == eTOOMANYREFS    = ResourceExhausted
        | errno == eTXTBSY         = ResourceBusy
        | errno == eUSERS          = ResourceExhausted
        | errno == eWOULDBLOCK     = OtherError
        | errno == eXDEV           = UnsupportedOperation
        | otherwise                = OtherError

