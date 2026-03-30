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
  errno1@(Errno no1) == errno2@(Errno no2)
    | isValidErrno errno1 && isValidErrno errno2 = no1 == no2
    | otherwise                                  = False

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
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV                    :: Errno

eOK             = Errno 0
e2BIG           = Errno cE2BIG;           foreign import capi "value E2BIG"           cE2BIG :: CInt
eACCES          = Errno cEACCES;          foreign import capi "value EACCES"          cEACCES :: CInt
eADDRINUSE      = Errno cEADDRINUSE;      foreign import capi "value EADDRINUSE"      cEADDRINUSE :: CInt
eADDRNOTAVAIL   = Errno cEADDRNOTAVAIL;   foreign import capi "value EADDRNOTAVAIL"   cEADDRNOTAVAIL :: CInt
eADV            = Errno cEADV;            foreign import capi "value EADV"            cEADV :: CInt
eAFNOSUPPORT    = Errno cEAFNOSUPPORT;    foreign import capi "value EAFNOSUPPORT"    cEAFNOSUPPORT :: CInt
eAGAIN          = Errno cEAGAIN;          foreign import capi "value EAGAIN"          cEAGAIN :: CInt
eALREADY        = Errno cEALREADY;        foreign import capi "value EALREADY"        cEALREADY :: CInt
eBADF           = Errno cEBADF;           foreign import capi "value EBADF"           cEBADF :: CInt
eBADMSG         = Errno cEBADMSG;         foreign import capi "value EBADMSG"         cEBADMSG :: CInt
eBADRPC         = Errno cEBADRPC;         foreign import capi "value EBADRPC"         cEBADRPC :: CInt
eBUSY           = Errno cEBUSY;           foreign import capi "value EBUSY"           cEBUSY :: CInt
eCHILD          = Errno cECHILD;          foreign import capi "value ECHILD"          cECHILD :: CInt
eCOMM           = Errno cECOMM;           foreign import capi "value ECOMM"           cECOMM :: CInt
eCONNABORTED    = Errno cECONNABORTED;    foreign import capi "value ECONNABORTED"    cECONNABORTED :: CInt
eCONNREFUSED    = Errno cECONNREFUSED;    foreign import capi "value ECONNREFUSED"    cECONNREFUSED :: CInt
eCONNRESET      = Errno cECONNRESET;      foreign import capi "value ECONNRESET"      cECONNRESET :: CInt
eDEADLK         = Errno cEDEADLK;         foreign import capi "value EDEADLK"         cEDEADLK :: CInt
eDESTADDRREQ    = Errno cEDESTADDRREQ;    foreign import capi "value EDESTADDRREQ"    cEDESTADDRREQ :: CInt
eDIRTY          = Errno cEDIRTY;          foreign import capi "value EDIRTY"          cEDIRTY :: CInt
eDOM            = Errno cEDOM;            foreign import capi "value EDOM"            cEDOM :: CInt
eDQUOT          = Errno cEDQUOT;          foreign import capi "value EDQUOT"          cEDQUOT :: CInt
eEXIST          = Errno cEEXIST;          foreign import capi "value EEXIST"          cEEXIST :: CInt
eFAULT          = Errno cEFAULT;          foreign import capi "value EFAULT"          cEFAULT :: CInt
eFBIG           = Errno cEFBIG;           foreign import capi "value EFBIG"           cEFBIG :: CInt
eFTYPE          = Errno cEFTYPE;          foreign import capi "value EFTYPE"          cEFTYPE :: CInt
eHOSTDOWN       = Errno cEHOSTDOWN;       foreign import capi "value EHOSTDOWN"       cEHOSTDOWN :: CInt
eHOSTUNREACH    = Errno cEHOSTUNREACH;    foreign import capi "value EHOSTUNREACH"    cEHOSTUNREACH :: CInt
eIDRM           = Errno cEIDRM;           foreign import capi "value EIDRM"           cEIDRM :: CInt
eILSEQ          = Errno cEILSEQ;          foreign import capi "value EILSEQ"          cEILSEQ :: CInt
eINPROGRESS     = Errno cEINPROGRESS;     foreign import capi "value EINPROGRESS"     cEINPROGRESS :: CInt
eINTR           = Errno cEINTR;           foreign import capi "value EINTR"           cEINTR :: CInt
eINVAL          = Errno cEINVAL;          foreign import capi "value EINVAL"          cEINVAL :: CInt
eIO             = Errno cEIO;             foreign import capi "value EIO"             cEIO :: CInt
eISCONN         = Errno cEISCONN;         foreign import capi "value EISCONN"         cEISCONN :: CInt
eISDIR          = Errno cEISDIR;          foreign import capi "value EISDIR"          cEISDIR :: CInt
eLOOP           = Errno cELOOP;           foreign import capi "value ELOOP"           cELOOP :: CInt
eMFILE          = Errno cEMFILE;          foreign import capi "value EMFILE"          cEMFILE :: CInt
eMLINK          = Errno cEMLINK;          foreign import capi "value EMLINK"          cEMLINK :: CInt
eMSGSIZE        = Errno cEMSGSIZE;        foreign import capi "value EMSGSIZE"        cEMSGSIZE :: CInt
eMULTIHOP       = Errno cEMULTIHOP;       foreign import capi "value EMULTIHOP"       cEMULTIHOP :: CInt
eNAMETOOLONG    = Errno cENAMETOOLONG;    foreign import capi "value ENAMETOOLONG"    cENAMETOOLONG :: CInt
eNETDOWN        = Errno cENETDOWN;        foreign import capi "value ENETDOWN"        cENETDOWN :: CInt
eNETRESET       = Errno cENETRESET;       foreign import capi "value ENETRESET"       cENETRESET :: CInt
eNETUNREACH     = Errno cENETUNREACH;     foreign import capi "value ENETUNREACH"     cENETUNREACH :: CInt
eNFILE          = Errno cENFILE;          foreign import capi "value ENFILE"          cENFILE :: CInt
eNOBUFS         = Errno cENOBUFS;         foreign import capi "value ENOBUFS"         cENOBUFS :: CInt
eNODATA         = Errno cENODATA;         foreign import capi "value ENODATA"         cENODATA :: CInt
eNODEV          = Errno cENODEV;          foreign import capi "value ENODEV"          cENODEV :: CInt
eNOENT          = Errno cENOENT;          foreign import capi "value ENOENT"          cENOENT :: CInt
eNOEXEC         = Errno cENOEXEC;         foreign import capi "value ENOEXEC"         cENOEXEC :: CInt
eNOLCK          = Errno cENOLCK;          foreign import capi "value ENOLCK"          cENOLCK :: CInt
eNOLINK         = Errno cENOLINK;         foreign import capi "value ENOLINK"         cENOLINK :: CInt
eNOMEM          = Errno cENOMEM;          foreign import capi "value ENOMEM"          cENOMEM :: CInt
eNOMSG          = Errno cENOMSG;          foreign import capi "value ENOMSG"          cENOMSG :: CInt
eNONET          = Errno cENONET;          foreign import capi "value ENONET"          cENONET :: CInt
eNOPROTOOPT     = Errno cENOPROTOOPT;     foreign import capi "value ENOPROTOOPT"     cENOPROTOOPT :: CInt
eNOSPC          = Errno cENOSPC;          foreign import capi "value ENOSPC"          cENOSPC :: CInt
eNOSR           = Errno cENOSR;           foreign import capi "value ENOSR"           cENOSR :: CInt
eNOSTR          = Errno cENOSTR;          foreign import capi "value ENOSTR"          cENOSTR :: CInt
eNOSYS          = Errno cENOSYS;          foreign import capi "value ENOSYS"          cENOSYS :: CInt
eNOTBLK         = Errno cENOTBLK;         foreign import capi "value ENOTBLK"         cENOTBLK :: CInt
eNOTCONN        = Errno cENOTCONN;        foreign import capi "value ENOTCONN"        cENOTCONN :: CInt
eNOTDIR         = Errno cENOTDIR;         foreign import capi "value ENOTDIR"         cENOTDIR :: CInt
eNOTEMPTY       = Errno cENOTEMPTY;       foreign import capi "value ENOTEMPTY"       cENOTEMPTY :: CInt
eNOTSOCK        = Errno cENOTSOCK;        foreign import capi "value ENOTSOCK"        cENOTSOCK :: CInt
eNOTSUP         = Errno cENOTSUP;         foreign import capi "value ENOTSUP"         cENOTSUP :: CInt
-- ^ @since base-4.7.0.0
eNOTTY          = Errno cENOTTY;          foreign import capi "value ENOTTY"          cENOTTY :: CInt
eNXIO           = Errno cENXIO;           foreign import capi "value ENXIO"           cENXIO :: CInt
eOPNOTSUPP      = Errno cEOPNOTSUPP;      foreign import capi "value EOPNOTSUPP"      cEOPNOTSUPP :: CInt
ePERM           = Errno cEPERM;           foreign import capi "value EPERM"           cEPERM :: CInt
ePFNOSUPPORT    = Errno cEPFNOSUPPORT;    foreign import capi "value EPFNOSUPPORT"    cEPFNOSUPPORT :: CInt
ePIPE           = Errno cEPIPE;           foreign import capi "value EPIPE"           cEPIPE :: CInt
ePROCLIM        = Errno cEPROCLIM;        foreign import capi "value EPROCLIM"        cEPROCLIM :: CInt
ePROCUNAVAIL    = Errno cEPROCUNAVAIL;    foreign import capi "value EPROCUNAVAIL"    cEPROCUNAVAIL :: CInt
ePROGMISMATCH   = Errno cEPROGMISMATCH;   foreign import capi "value EPROGMISMATCH"   cEPROGMISMATCH :: CInt
ePROGUNAVAIL    = Errno cEPROGUNAVAIL;    foreign import capi "value EPROGUNAVAIL"    cEPROGUNAVAIL :: CInt
ePROTO          = Errno cEPROTO;          foreign import capi "value EPROTO"          cEPROTO :: CInt
ePROTONOSUPPORT = Errno cEPROTONOSUPPORT; foreign import capi "value EPROTONOSUPPORT" cEPROTONOSUPPORT :: CInt
ePROTOTYPE      = Errno cEPROTOTYPE;      foreign import capi "value EPROTOTYPE"      cEPROTOTYPE :: CInt
eRANGE          = Errno cERANGE;          foreign import capi "value ERANGE"          cERANGE :: CInt
eREMCHG         = Errno cEREMCHG;         foreign import capi "value EREMCHG"         cEREMCHG :: CInt
eREMOTE         = Errno cEREMOTE;         foreign import capi "value EREMOTE"         cEREMOTE :: CInt
eROFS           = Errno cEROFS;           foreign import capi "value EROFS"           cEROFS :: CInt
eRPCMISMATCH    = Errno cERPCMISMATCH;    foreign import capi "value ERPCMISMATCH"    cERPCMISMATCH :: CInt
eRREMOTE        = Errno cERREMOTE;        foreign import capi "value ERREMOTE"        cERREMOTE :: CInt
eSHUTDOWN       = Errno cESHUTDOWN;       foreign import capi "value ESHUTDOWN"       cESHUTDOWN :: CInt
eSOCKTNOSUPPORT = Errno cESOCKTNOSUPPORT; foreign import capi "value ESOCKTNOSUPPORT" cESOCKTNOSUPPORT :: CInt
eSPIPE          = Errno cESPIPE;          foreign import capi "value ESPIPE"          cESPIPE :: CInt
eSRCH           = Errno cESRCH;           foreign import capi "value ESRCH"           cESRCH :: CInt
eSRMNT          = Errno cESRMNT;          foreign import capi "value ESRMNT"          cESRMNT :: CInt
eSTALE          = Errno cESTALE;          foreign import capi "value ESTALE"          cESTALE :: CInt
eTIME           = Errno cETIME;           foreign import capi "value ETIME"           cETIME :: CInt
eTIMEDOUT       = Errno cETIMEDOUT;       foreign import capi "value ETIMEDOUT"       cETIMEDOUT :: CInt
eTOOMANYREFS    = Errno cETOOMANYREFS;    foreign import capi "value ETOOMANYREFS"    cETOOMANYREFS :: CInt
eTXTBSY         = Errno cETXTBSY;         foreign import capi "value ETXTBSY"         cETXTBSY :: CInt
eUSERS          = Errno cEUSERS;          foreign import capi "value EUSERS"          cEUSERS :: CInt
eWOULDBLOCK     = Errno cEWOULDBLOCK;     foreign import capi "value EWOULDBLOCK"     cEWOULDBLOCK :: CInt
eXDEV           = Errno cEXDEV;           foreign import capi "value EXDEV"           cEXDEV :: CInt

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
    c_strerror_r :: CInt -> CString -> CSize -> IO CInt

errnoToString :: Errno -> IO String
errnoToString (Errno errno) =
    allocaBytes 512 $ \ ptr -> do
        ret <- c_strerror_r errno ptr (CSize 512)
        if ret /= 0
          then return "errnoToString failed"
          else peekCString ptr
  where len = 512::Int

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

