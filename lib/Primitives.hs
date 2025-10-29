-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Primitives(module Primitives) where
import qualified Prelude()              -- do not import Prelude
import Data.Bool_Type
--import Data.List_Type
import Data.Ordering_Type

-- These fixities are hardwired
-- infixr -1 ->
-- infixr -2 =>
infix   4 ~

-- Use deriving () everywhere to avoid the automatic Data.Typeable derivation.

-- Kinds
data Constraint deriving ()
data Nat deriving ()
data Symbol deriving ()
data Type deriving ()

-- Classes
-- Type equality as a constraint.
type (~) :: forall k . k -> k -> Constraint
class a ~ b | a -> b, b -> a
-- class KnownNat in Data.TypeLits
-- class KnownSymbol in Data.TypeLits

-- Types
data AnyType deriving ()
--data Char
newtype Char = Char Word deriving ()
data Int deriving ()
data Int64 deriving ()  -- same representation as Int on a 64 bit platform
data Float deriving ()
data Double deriving ()
data IO a deriving ()
data Word deriving ()
data Word64 deriving ()
data Ptr a deriving ()
data ForeignPtr a deriving ()
data FunPtr a deriving ()
data IOArray a deriving ()
data ThreadId deriving ()
data MVar a deriving ()
data Weak v deriving ()
-- (), (,), (,,), etc are built in to the compiler

primIntAdd :: Int -> Int -> Int
primIntAdd  = _primitive "+"
primIntSub :: Int -> Int -> Int
primIntSub  = _primitive "-"
primIntMul :: Int -> Int -> Int
primIntMul  = _primitive "*"
primIntQuot :: Int -> Int -> Int
primIntQuot = _primitive "quot"
primIntRem :: Int -> Int -> Int
primIntRem  = _primitive "rem"
primIntSubR :: Int -> Int -> Int
primIntSubR = _primitive "subtract"
primIntNeg :: Int -> Int
primIntNeg = _primitive "neg"

primIntEQ   :: Int -> Int -> Bool
primIntEQ   = _primitive "=="
primIntNE   :: Int -> Int -> Bool
primIntNE   = _primitive "/="
primIntLT   :: Int -> Int -> Bool
primIntLT   = _primitive "<"
primIntLE   :: Int -> Int -> Bool
primIntLE   = _primitive "<="
primIntGT   :: Int -> Int -> Bool
primIntGT   = _primitive ">"
primIntGE   :: Int -> Int -> Bool
primIntGE   = _primitive ">="

primFloatAdd :: Float -> Float -> Float
primFloatAdd  = _primitive "f+"
primFloatSub :: Float -> Float -> Float
primFloatSub  = _primitive "f-"
primFloatMul :: Float -> Float -> Float
primFloatMul  = _primitive "f*"
primFloatDiv :: Float -> Float -> Float
primFloatDiv = _primitive "f/"
primFloatNeg :: Float -> Float
primFloatNeg = _primitive "fneg"

primFloatEQ :: Float -> Float -> Bool
primFloatEQ = _primitive "f=="
primFloatNE :: Float -> Float -> Bool
primFloatNE = _primitive "f/="
primFloatLT :: Float -> Float -> Bool
primFloatLT = _primitive "f<"
primFloatLE :: Float -> Float -> Bool
primFloatLE = _primitive "f<="
primFloatGT :: Float -> Float -> Bool
primFloatGT = _primitive "f>"
primFloatGE :: Float -> Float -> Bool
primFloatGE = _primitive "f>="
primFloatFromInt64 :: Int64 -> Float
primFloatFromInt64 = _primitive "Itof"
primFloatFromInt :: Int -> Float
primFloatFromInt = _primitive "itof"

primDoubleAdd :: Double -> Double -> Double
primDoubleAdd  = _primitive "d+"
primDoubleSub :: Double -> Double -> Double
primDoubleSub  = _primitive "d-"
primDoubleMul :: Double -> Double -> Double
primDoubleMul  = _primitive "d*"
primDoubleDiv :: Double -> Double -> Double
primDoubleDiv = _primitive "d/"
primDoubleNeg :: Double -> Double
primDoubleNeg = _primitive "dneg"

primDoubleEQ :: Double -> Double -> Bool
primDoubleEQ = _primitive "d=="
primDoubleNE :: Double -> Double -> Bool
primDoubleNE = _primitive "d/="
primDoubleLT :: Double -> Double -> Bool
primDoubleLT = _primitive "d<"
primDoubleLE :: Double -> Double -> Bool
primDoubleLE = _primitive "d<="
primDoubleGT :: Double -> Double -> Bool
primDoubleGT = _primitive "d>"
primDoubleGE :: Double -> Double -> Bool
primDoubleGE = _primitive "d>="
primDoubleFromInt64 :: Int64 -> Double
primDoubleFromInt64 = _primitive "Itod"
primDoubleFromInt :: Int -> Double
primDoubleFromInt = _primitive "itod"

primWordAdd :: Word -> Word -> Word
primWordAdd  = _primitive "+"
primWordSub :: Word -> Word -> Word
primWordSub  = _primitive "-"
primWordMul :: Word -> Word -> Word
primWordMul  = _primitive "*"
primWordQuot :: Word -> Word -> Word
primWordQuot = _primitive "uquot"
primWordRem :: Word -> Word -> Word
primWordRem  = _primitive "urem"
primWordAnd :: Word -> Word -> Word
primWordAnd  = _primitive "and"
primWordOr :: Word -> Word -> Word
primWordOr  = _primitive "or"
primWordXor :: Word -> Word -> Word
primWordXor  = _primitive "xor"
primWordShl :: Word -> Int -> Word
primWordShl  = _primitive "shl"
primWordShr :: Word -> Int -> Word
primWordShr  = _primitive "shr"
primWordAshr :: Word -> Int -> Word
primWordAshr  = _primitive "ashr"
primWordInv :: Word -> Word
primWordInv  = _primitive "inv"
primWordPopcount :: Word -> Int
primWordPopcount = _primitive "popcount"
primWordClz :: Word -> Int
primWordClz = _primitive "clz"
primWordCtz :: Word -> Int
primWordCtz = _primitive "ctz"
primWordToFloatRaw :: Word -> Float
primWordToFloatRaw = _primitive "toFlt"
primWord64ToDoubleRaw :: Word64 -> Double
primWord64ToDoubleRaw = _primitive "toDbl"
primWordFromFloatRaw :: Float -> Word
primWordFromFloatRaw = _primitive "fromFlt"
primWord64FromDoubleRaw :: Double -> Word64
primWord64FromDoubleRaw = _primitive "fromDbl"

primIntAnd :: Int -> Int -> Int
primIntAnd  = _primitive "and"
primIntOr :: Int -> Int -> Int
primIntOr  = _primitive "or"
primIntXor :: Int -> Int -> Int
primIntXor  = _primitive "xor"
primIntShl :: Int -> Int -> Int
primIntShl  = _primitive "shl"
primIntShr :: Int -> Int -> Int
primIntShr  = _primitive "ashr"
primIntInv :: Int -> Int
primIntInv  = _primitive "inv"
primIntPopcount :: Int -> Int
primIntPopcount = _primitive "popcount"
primIntClz :: Int -> Int
primIntClz = _primitive "clz"
primIntCtz :: Int -> Int
primIntCtz = _primitive "ctz"

primWordEQ  :: Word -> Word -> Bool
primWordEQ  = _primitive "=="
primWordNE  :: Word -> Word -> Bool
primWordNE  = _primitive "/="
primWordLT  :: Word -> Word -> Bool
primWordLT  = _primitive "u<"
primWordLE   :: Word -> Word -> Bool
primWordLE   = _primitive "u<="
primWordGT   :: Word -> Word -> Bool
primWordGT   = _primitive "u>"
primWordGE   :: Word -> Word -> Bool
primWordGE   = _primitive "u>="

primWordToInt :: Word -> Int
primWordToInt = _primitive "I"
primIntToWord :: Int -> Word
primIntToWord = _primitive "I"

-- Char is represented by Word
primCharEQ :: Char -> Char -> Bool
primCharEQ  = _primitive "=="
primCharNE :: Char -> Char -> Bool
primCharNE  = _primitive "/="
primCharLT :: Char -> Char -> Bool
primCharLT  = _primitive "u<"
primCharLE :: Char -> Char -> Bool
primCharLE  = _primitive "u<="
primCharGT :: Char -> Char -> Bool
primCharGT  = _primitive "u>"
primCharGE :: Char -> Char -> Bool
primCharGE  = _primitive "u>="

primFix    :: forall a . (a -> a) -> a
primFix    = _primitive "Y"

primSeq    :: forall a b . a -> b -> b
primSeq    = _primitive "seq"

primIntCompare :: Int -> Int -> Ordering
primIntCompare  = _primitive "icmp"
primInt64Compare :: Int64 -> Int64 -> Ordering
primInt64Compare  = _primitive "Iicmp"
primCharCompare :: Char -> Char -> Ordering
primCharCompare  = _primitive "icmp"
primWordCompare :: Word -> Word -> Ordering
primWordCompare  = _primitive "ucmp"
primWord64Compare :: Word64 -> Word64 -> Ordering
primWord64Compare  = _primitive "Iucmp"

primChr :: Int -> Char
primChr = _primitive "chr"
primOrd :: Char -> Int
primOrd = _primitive "ord"

primUnsafeCoerce :: forall a b . a -> b
primUnsafeCoerce = _primitive "I"

primBind         :: forall a b . IO a -> (a -> IO b) -> IO b
primBind          = _primitive "IO.>>="
primThen         :: forall a b . IO a -> IO b -> IO b
primThen          = _primitive "IO.>>"
primReturn       :: forall a . a -> IO a
primReturn        = _primitive "IO.return"
primGetArgRef    :: IO (IOArray [[Char]])
primGetArgRef     = _primitive "IO.getArgRef"
primPerformIO    :: forall a . IO a -> a
primPerformIO     = _primitive "IO.performIO"

primRnfErr       :: forall a . a -> ()
primRnfErr        = _primitive "rnf" (0::Int)

primRnfNoErr     :: forall a . a -> ()
primRnfNoErr      = _primitive "rnf" (1::Int)

primWordToPtr :: forall a . Word -> Ptr a
primWordToPtr = _primitive "toPtr"

primPtrToWord :: forall a . Ptr a -> Word
primPtrToWord = _primitive "toInt"

primIntToPtr :: forall a . Int -> Ptr a
primIntToPtr = _primitive "toPtr"

primPtrToInt :: forall a . Ptr a -> Int
primPtrToInt = _primitive "toInt"

primFunPtrToWord :: forall a . FunPtr a -> Word
primFunPtrToWord = _primitive "toInt"

primIntToFunPtr :: forall a . Int -> FunPtr a
primIntToFunPtr = _primitive "toFunPtr"

primFunPtrToPtr :: forall a b . FunPtr a -> Ptr b
primFunPtrToPtr = _primitive "toPtr"

primPtrToFunPtr :: forall a b . Ptr a -> FunPtr b
primPtrToFunPtr = _primitive "toFunPtr"

-- Size in bits of Word/Int.
-- Will get constant folded on first use.
_wordSize :: Int
_wordSize = loop (primWordInv (0::Word)) (0::Int)
  where
    loop :: Word -> Int -> Int
    loop w n = if w `primWordEQ` (0::Word) then n else loop (primWordShr w (1::Int)) (n `primIntAdd` (1::Int))

-- Is this Windows?
foreign import ccall "iswindows" c_iswindows :: IO Int
_isWindows :: Bool
_isWindows = primPerformIO c_iswindows `primIntEQ` 1

-- Is this MacOS?
foreign import ccall "ismacos" c_ismacos :: IO Int
_isMacOS :: Bool
_isMacOS = primPerformIO c_ismacos `primIntEQ` 1

-- Is this Linux?
foreign import ccall "islinux" c_islinux :: IO Int
_isLinux :: Bool
_isLinux = primPerformIO c_islinux `primIntEQ` 1

primArrAlloc :: forall a . Int -> a -> IO (IOArray a)
primArrAlloc = _primitive "A.alloc"

primArrCopy :: forall a . IOArray a -> IO (IOArray a)
primArrCopy = _primitive "A.copy"

primArrSize :: forall a . IOArray a -> IO Int
primArrSize = _primitive "A.size"

primArrRead :: forall a . IOArray a -> Int -> IO a
primArrRead = _primitive "A.read"

primArrWrite :: forall a . IOArray a -> Int -> a -> IO ()
primArrWrite = _primitive "A.write"

primArrTrunc :: forall a . IOArray a -> Int -> IO ()
primArrTrunc = _primitive "A.trunc"

-- Not referentially transparent
primArrEQ :: forall a . IOArray a -> IOArray a -> Bool
primArrEQ = _primitive "A.=="

primGC :: Int -> IO ()
primGC = _primitive "IO.gc"

primForeignPtrToPtr :: ForeignPtr a -> Ptr a
primForeignPtrToPtr = _primitive "fp2p"

primNewForeignPtr :: Ptr a -> IO (ForeignPtr a)
primNewForeignPtr = _primitive "fpnew"

primAddFinalizer :: FunPtr (Ptr a -> IO ()) -> ForeignPtr a -> IO ()
primAddFinalizer = _primitive "fpfin"

primForkIO :: IO () -> IO ThreadId
primForkIO = _primitive "IO.fork"

primMyThreadId :: IO ThreadId
primMyThreadId = _primitive "IO.thid"
primThreadNum :: ThreadId -> Word
primThreadNum = _primitive "thnum"

primYield :: IO ()
primYield = _primitive "IO.yield"

primMVarToWord :: forall a . MVar a -> Word
primMVarToWord = _primitive "toInt"

primNewEmptyMVar :: forall a . IO (MVar a)
primNewEmptyMVar = _primitive "IO.newmvar"
primTakeMVar :: forall a . MVar a -> IO a
primTakeMVar = _primitive "IO.takemvar"
primReadMVar :: forall a . MVar a -> IO a
primReadMVar = _primitive "IO.readmvar"
primPutMVar :: forall a . MVar a -> a -> IO ()
primPutMVar = _primitive "IO.putmvar"
primTryTakeMVar :: MVar a -> IO b {-(Maybe a)-}
primTryTakeMVar = _primitive "IO.trytakemvar"
primTryPutMVar :: MVar a -> a -> IO Bool
primTryPutMVar = _primitive "IO.tryputmvar"
primTryReadMVar :: MVar a -> IO b {-(Maybe a)-}
primTryReadMVar = _primitive "IO.tryreadmvar"

primThreadDelay :: Int -> IO ()
primThreadDelay = _primitive "IO.threaddelay"

primThreadStatus :: ThreadId -> IO Int
primThreadStatus = _primitive "IO.threadstatus"

primIsInt :: a -> Int
primIsInt = _primitive "isint"

primGetMaskingState :: IO Int
primGetMaskingState = _primitive "IO.getmaskingstate"
primSetMaskingState :: Int -> IO ()
primSetMaskingState = _primitive "IO.setmaskingstate"

primStats :: IO (Word, Word)
primStats = _primitive "IO.stats"

primNewStablePtr :: a -> IO Word
primNewStablePtr = _primitive "SPnew"
primDeRefStablePtr :: Word -> IO a
primDeRefStablePtr = _primitive "SPderef"
primFreeStablePtr :: Word -> IO ()
primFreeStablePtr = _primitive "SPfree"


primInt64Add :: Int64 -> Int64 -> Int64
primInt64Add  = _primitive "I+"
primInt64Sub :: Int64 -> Int64 -> Int64
primInt64Sub  = _primitive "I-"
primInt64Mul :: Int64 -> Int64 -> Int64
primInt64Mul  = _primitive "I*"
primInt64Quot :: Int64 -> Int64 -> Int64
primInt64Quot = _primitive "Iquot"
primInt64Rem :: Int64 -> Int64 -> Int64
primInt64Rem  = _primitive "Irem"
primInt64SubR :: Int64 -> Int64 -> Int64
primInt64SubR = _primitive "Isubtract"
primInt64Neg :: Int64 -> Int64
primInt64Neg = _primitive "Ineg"
primInt64EQ   :: Int64 -> Int64 -> Bool
primInt64EQ   = _primitive "I=="
primInt64NE   :: Int64 -> Int64 -> Bool
primInt64NE   = _primitive "I/="
primInt64LT   :: Int64 -> Int64 -> Bool
primInt64LT   = _primitive "I<"
primInt64LE   :: Int64 -> Int64 -> Bool
primInt64LE   = _primitive "I<="
primInt64GT   :: Int64 -> Int64 -> Bool
primInt64GT   = _primitive "I>"
primInt64GE   :: Int64 -> Int64 -> Bool
primInt64GE   = _primitive "I>="
primInt64And :: Int64 -> Int64 -> Int64
primInt64And  = _primitive "Iand"
primInt64Or :: Int64 -> Int64 -> Int64
primInt64Or  = _primitive "Ior"
primInt64Xor :: Int64 -> Int64 -> Int64
primInt64Xor  = _primitive "Ixor"
primInt64Shl :: Int64 -> Int -> Int64
primInt64Shl  = _primitive "Ishl"
primInt64Shr :: Int64 -> Int -> Int64
primInt64Shr  = _primitive "Iashr"
primInt64Inv :: Int64 -> Int64
primInt64Inv  = _primitive "Iinv"
primInt64Popcount :: Int64 -> Int
primInt64Popcount = _primitive "Ipopcount"
primInt64Clz :: Int64 -> Int
primInt64Clz = _primitive "Iclz"
primInt64Ctz :: Int64 -> Int
primInt64Ctz = _primitive "Ictz"

primWord64Add :: Word64 -> Word64 -> Word64
primWord64Add  = _primitive "I+"
primWord64Sub :: Word64 -> Word64 -> Word64
primWord64Sub  = _primitive "I-"
primWord64Mul :: Word64 -> Word64 -> Word64
primWord64Mul  = _primitive "I*"
primWord64Quot :: Word64 -> Word64 -> Word64
primWord64Quot = _primitive "Iuquot"
primWord64Rem :: Word64 -> Word64 -> Word64
primWord64Rem  = _primitive "Iurem"
primWord64And :: Word64 -> Word64 -> Word64
primWord64And  = _primitive "Iand"
primWord64Or :: Word64 -> Word64 -> Word64
primWord64Or  = _primitive "Ior"
primWord64Xor :: Word64 -> Word64 -> Word64
primWord64Xor  = _primitive "Ixor"
primWord64Shl :: Word64 -> Int -> Word64
primWord64Shl  = _primitive "Ishl"
primWord64Shr :: Word64 -> Int -> Word64
primWord64Shr  = _primitive "Ishr"
primWord64Ashr :: Word64 -> Int -> Word64
primWord64Ashr  = _primitive "Iashr"
primWord64Inv :: Word64 -> Word64
primWord64Inv  = _primitive "Iinv"
primWord64Popcount :: Word64 -> Int
primWord64Popcount = _primitive "Ipopcount"
primWord64Clz :: Word64 -> Int
primWord64Clz = _primitive "Iclz"
primWord64Ctz :: Word64 -> Int
primWord64Ctz = _primitive "Ictz"
primWord64EQ  :: Word64 -> Word64 -> Bool
primWord64EQ  = _primitive "I=="
primWord64NE  :: Word64 -> Word64 -> Bool
primWord64NE  = _primitive "I/="
primWord64LT  :: Word64 -> Word64 -> Bool
primWord64LT  = _primitive "Iu<"
primWord64LE   :: Word64 -> Word64 -> Bool
primWord64LE   = _primitive "Iu<="
primWord64GT   :: Word64 -> Word64 -> Bool
primWord64GT   = _primitive "Iu>"
primWord64GE   :: Word64 -> Word64 -> Bool
primWord64GE   = _primitive "Iu>="

primWord64ToInt64 :: Word64 -> Int64
primWord64ToInt64 = _primitive "I"
primInt64ToWord64 :: Int64 -> Word64
primInt64ToWord64 = _primitive "I"

primIntToInt64 :: Int -> Int64
primIntToInt64 = _primitive "itoI"
primInt64ToInt :: Int64 -> Int
primInt64ToInt = _primitive "Itoi"
primWordToWord64 :: Word -> Word64
primWordToWord64 = _primitive "utoU"
primWord64ToWord :: Word64 -> Word
primWord64ToWord = _primitive "Utou"
