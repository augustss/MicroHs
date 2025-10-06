module System.Posix.Types (
  CDev(..),
  CIno(..),
  CMode(..),
  COff(..),
  CPid(..),
  CSsize(..),
  CGid(..),
  CNlink(..),
  CUid(..),
  CCc(..),
  CSpeed(..),
  CTcflag(..),
  CRLim(..),
  CBlkSize(..),
  CBlkCnt(..),
  CClockId(..),
  CFsBlkCnt(..),
  CFsFilCnt(..),
  CId(..),
  CKey(..),
  CTimer(..),
  CSocklen(..),
  CNfds(..),
  Fd(..),
  LinkCount,
  UserID,
  GroupID,

  ByteCount,
  ClockTick,
  EpochTime,
  FileOffset,
  ProcessID,
  ProcessGroupID,
  DeviceID,
  FileID,
  FileMode,
  Limit
 ) where

import Data.Bits
import Data.Int
import Data.Ix
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

-- The types are highly platform dependent.
-- What we have here should work on
--  Linux 64 bit
--  Linux 32 bit
--  MacOS 64 bit
-- The actual size of the representation type is >= the platform type,
-- but the storable instance has the right size.

toI32 :: Integral a => a -> Int32
toI32 = fromIntegral

fromI32 :: Integral a => Int32 -> a
fromI32 = fromIntegral

toU32 :: Integral a => a -> Word32
toU32 = fromIntegral

fromU32 :: Integral a => Word32 -> a
fromU32 = fromIntegral

toU16 :: Integral a => a -> Word16
toU16 = fromIntegral

fromU16 :: Integral a => Word16 -> a
fromU16 = fromIntegral

newtype CDev = CDev Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord)
instance Storable CDev where
  sizeOf    (CDev x) =          if _isMacOS then sizeOf              (toI32 x) else sizeOf    x
  alignment (CDev x) =          if _isMacOS then alignment           (toI32 x) else alignment x
  peek p             = CDev <$> if _isMacOS then fromI32 <$> peek (castPtr p)  else peek (castPtr p)
  poke p    (CDev x) =          if _isMacOS then poke (castPtr p)    (toI32 x) else poke (castPtr p) x

newtype CIno = CIno Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CMode = CMode Word32
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype COff = COff Int64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CPid = CPid Int32
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CSsize = CSsize Int
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CGid = CGid Word32
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CNlink = CNlink Word
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord)
instance Storable CNlink where
  sizeOf    (CNlink x) =            if _isMacOS then sizeOf              (toU16 x) else sizeOf    x
  alignment (CNlink x) =            if _isMacOS then alignment           (toU16 x) else alignment x
  peek p               = CNlink <$> if _isMacOS then fromU16 <$> peek (castPtr p)  else peek (castPtr p)
  poke p    (CNlink x) =            if _isMacOS then poke (castPtr p)    (toU16 x) else poke (castPtr p) x

newtype CUid = CUid Word32
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CCc = CCc Word8
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CSpeed = CSpeed Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord)
instance Storable CSpeed where
  sizeOf    (CSpeed x) =            if not _isMacOS then sizeOf              (toU32 x) else sizeOf    x
  alignment (CSpeed x) =            if not _isMacOS then alignment           (toU32 x) else alignment x
  peek p               = CSpeed <$> if not _isMacOS then fromU32 <$> peek (castPtr p)  else peek (castPtr p)
  poke p    (CSpeed x) =            if not _isMacOS then poke (castPtr p)    (toU32 x) else poke (castPtr p) x

newtype CTcflag = CTcflag Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord)
instance Storable CTcflag where
  sizeOf    (CTcflag x) =             if not _isMacOS then sizeOf              (toU32 x) else sizeOf    x
  alignment (CTcflag x) =             if not _isMacOS then alignment           (toU32 x) else alignment x
  peek p                = CTcflag <$> if not _isMacOS then fromU32 <$> peek (castPtr p)  else peek (castPtr p)
  poke p    (CTcflag x) =             if not _isMacOS then poke (castPtr p)    (toU32 x) else poke (castPtr p) x

newtype CRLim = CRLim Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CBlkSize = CBlkSize Word
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord)
instance Storable CBlkSize where
  sizeOf    (CBlkSize x) =              if _isMacOS then sizeOf              (toI32 x) else sizeOf    x
  alignment (CBlkSize x) =              if _isMacOS then alignment           (toI32 x) else alignment x
  peek p                 = CBlkSize <$> if _isMacOS then fromI32 <$> peek (castPtr p)  else peek (castPtr p)
  poke p    (CBlkSize x) =              if _isMacOS then poke (castPtr p)    (toI32 x) else poke (castPtr p) x

newtype CBlkCnt = CBlkCnt Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CClockId = CClockId Int
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CFsBlkCnt = CFsBlkCnt Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CFsFilCnt = CFsFilCnt Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CId = CId Int
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CKey = CKey Int
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CTimer = CTimer Word
  deriving (Show, Eq, Ord, Storable)

newtype CSocklen = CSocklen Word32
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord, Storable)

newtype CNfds = CNfds Word64
  deriving (Bits, FiniteBits, Bounded, Enum, Ix, Num, Read, Integral, Real, Show, Eq, Ord)
instance Storable CNfds where
  sizeOf    (CNfds x) =           if _isMacOS then sizeOf              (toI32 x) else sizeOf    x
  alignment (CNfds x) =           if _isMacOS then alignment           (toI32 x) else alignment x
  peek p              = CNfds <$> if _isMacOS then fromI32 <$> peek (castPtr p)  else peek (castPtr p)
  poke p    (CNfds x) =           if _isMacOS then poke (castPtr p)    (toI32 x) else poke (castPtr p) x

newtype Fd = Fd CInt
  deriving (Bits, FiniteBits, Bounded, Enum, {-Ix,-} Num, {-Read,-} Integral, Real, Show, Eq, Ord, Storable)


type LinkCount      = CNlink
type UserID         = CUid
type GroupID        = CGid

type ByteCount      = CSize
type ClockTick      = CClock
type EpochTime      = CTime
type DeviceID       = CDev
type FileID         = CIno
type FileMode       = CMode
type ProcessID      = CPid
type FileOffset     = COff
type ProcessGroupID = CPid
type Limit          = CLong

