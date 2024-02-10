-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Primitives(module Primitives) where
import Prelude()              -- do not import Prelude
import Data.Bool_Type
--import Data.List_Type
import Data.Ordering_Type

-- These fixities are hardwired
-- infixr -1 ->
-- infixr -2 =>
infix   4 ~

-- Kinds
data Constraint
data Nat
data Symbol
data Type

-- Classes
-- Type equality as a constraint.
-- class a ~ b | a -> b, b -> a
-- class KnownNat in Data.TypeLits
-- class KnownSymbol in Data.TypeLits

-- Types
data AnyType
data Char
data Int
data Double
data IO a
data Word
data Ptr a
data IOArray a

data () = ()   -- Parser hacks allows () to be used

primIntAdd :: Int -> Int -> Int
primIntAdd  = primitive "+"
primIntSub :: Int -> Int -> Int
primIntSub  = primitive "-"
primIntMul :: Int -> Int -> Int
primIntMul  = primitive "*"
primIntQuot :: Int -> Int -> Int
primIntQuot = primitive "quot"
primIntRem :: Int -> Int -> Int
primIntRem  = primitive "rem"
primIntSubR :: Int -> Int -> Int
primIntSubR = primitive "subtract"
primIntNeg :: Int -> Int
primIntNeg = primitive "neg"

primIntEQ   :: Int -> Int -> Bool
primIntEQ   = primitive "=="
primIntNE   :: Int -> Int -> Bool
primIntNE   = primitive "/="
primIntLT   :: Int -> Int -> Bool
primIntLT   = primitive "<"
primIntLE   :: Int -> Int -> Bool
primIntLE   = primitive "<="
primIntGT   :: Int -> Int -> Bool
primIntGT   = primitive ">"
primIntGE   :: Int -> Int -> Bool
primIntGE   = primitive ">="

primDoubleAdd :: Double -> Double -> Double
primDoubleAdd  = primitive "f+"
primDoubleSub :: Double -> Double -> Double
primDoubleSub  = primitive "f-"
primDoubleMul :: Double -> Double -> Double
primDoubleMul  = primitive "f*"
primDoubleDiv :: Double -> Double -> Double
primDoubleDiv = primitive "f/"
primDoubleNeg :: Double -> Double
primDoubleNeg = primitive "fneg"

primDoubleEQ :: Double -> Double -> Bool
primDoubleEQ = primitive "f=="
primDoubleNE :: Double -> Double -> Bool
primDoubleNE = primitive "f/="
primDoubleLT :: Double -> Double -> Bool
primDoubleLT = primitive "f<"
primDoubleLE :: Double -> Double -> Bool
primDoubleLE = primitive "f<="
primDoubleGT :: Double -> Double -> Bool
primDoubleGT = primitive "f>"
primDoubleGE :: Double -> Double -> Bool
primDoubleGE = primitive "f>="
primDoubleShow :: Double -> [Char]
primDoubleShow = primitive "fshow"
primDoubleRead :: [Char] -> Double
primDoubleRead = primitive "fread"
primDoubleFromInt :: Int -> Double
primDoubleFromInt = primitive "itof"

primWordAdd :: Word -> Word -> Word
primWordAdd  = primitive "+"
primWordSub :: Word -> Word -> Word
primWordSub  = primitive "-"
primWordMul :: Word -> Word -> Word
primWordMul  = primitive "*"
primWordQuot :: Word -> Word -> Word
primWordQuot = primitive "uquot"
primWordRem :: Word -> Word -> Word
primWordRem  = primitive "urem"
primWordAnd :: Word -> Word -> Word
primWordAnd  = primitive "and"
primWordOr :: Word -> Word -> Word
primWordOr  = primitive "or"
primWordXor :: Word -> Word -> Word
primWordXor  = primitive "xor"
primWordShl :: Word -> Int -> Word
primWordShl  = primitive "shl"
primWordShr :: Word -> Int -> Word
primWordShr  = primitive "shr"
primWordAshr :: Word -> Int -> Word
primWordAshr  = primitive "ashr"
primWordInv :: Word -> Word
primWordInv  = primitive "inv"
primWordToDoubleRaw :: Word -> Double
primWordToDoubleRaw = primitive "toDbl"
primWordFromDoubleRaw :: Double -> Word
primWordFromDoubleRaw = primitive "toInt"

primWordEQ  :: Word -> Word -> Bool
primWordEQ  = primitive "=="
primWordNE  :: Word -> Word -> Bool
primWordNE  = primitive "/="
primWordLT  :: Word -> Word -> Bool
primWordLT  = primitive "u<"
primWordLE   :: Word -> Word -> Bool
primWordLE   = primitive "u<="
primWordGT   :: Word -> Word -> Bool
primWordGT   = primitive "u>"
primWordGE   :: Word -> Word -> Bool
primWordGE   = primitive "u>="

primWordToInt :: Word -> Int
primWordToInt = primitive "I"
primIntToWord :: Int -> Word
primIntToWord = primitive "I"

-- Char is represented by Int
primCharEQ :: Char -> Char -> Bool
primCharEQ  = primitive "=="
primCharNE :: Char -> Char -> Bool
primCharNE  = primitive "/="
primCharLT :: Char -> Char -> Bool
primCharLT  = primitive "<"
primCharLE :: Char -> Char -> Bool
primCharLE  = primitive "<="
primCharGT :: Char -> Char -> Bool
primCharGT  = primitive ">"
primCharGE :: Char -> Char -> Bool
primCharGE  = primitive ">="

primError  :: forall a . [Char] -> a
primError  = primitive "error"

primFix    :: forall a . (a -> a) -> a
primFix    = primitive "Y"

primSeq    :: forall a b . a -> b -> b
primSeq    = primitive "seq"

--primEqual  :: forall a . a -> a -> Bool
--primEqual  = primitive "equal"

-- Works for Int, Char, String
primStringCompare :: forall a . [Char] -> [Char] -> Ordering
primStringCompare  = primitive "scmp"
primIntCompare :: forall a . Int -> Int -> Ordering
primIntCompare  = primitive "icmp"
primCharCompare :: forall a . Char -> Char -> Ordering
primCharCompare  = primitive "icmp"

primStringEQ  :: [Char] -> [Char] -> Bool
primStringEQ  = primitive "sequal"

primChr :: Int -> Char
primChr = primitive "chr"
primOrd :: Char -> Int
primOrd = primitive "ord"

primUnsafeCoerce :: forall a b . a -> b
primUnsafeCoerce = primitive "I"

primBind         :: forall a b . IO a -> (a -> IO b) -> IO b
primBind          = primitive "IO.>>="
primThen         :: forall a b . IO a -> IO b -> IO b
primThen          = primitive "IO.>>"
primReturn       :: forall a . a -> IO a
primReturn        = primitive "IO.return"
primGetArgs      :: IO [[Char]]
primGetArgs       = primitive "IO.getArgs"
primPerformIO    :: forall a . IO a -> a
primPerformIO     = primitive "IO.performIO"

-- Use string for the exception until we can do better.
primCatch        :: forall a . IO a -> ([Char] -> IO a) -> IO a
primCatch         = primitive "IO.catch"

primRnfErr       :: forall a . a -> ()
primRnfErr        = primitive "rnf" (0::Int)

primRnfNoErr     :: forall a . a -> ()
primRnfNoErr      = primitive "rnf" (1::Int)

primNewCAStringLen :: [Char] -> IO (Ptr Char, Int)
primNewCAStringLen = primitive "newCAStringLen"

primPeekCAString :: Ptr Char -> IO [Char]
primPeekCAString = primitive "peekCAString"

primPeekCAStringLen :: Ptr Char -> Int -> IO [Char]
primPeekCAStringLen = primitive "peekCAStringLen"

primWordToPtr :: forall a . Word -> Ptr a
primWordToPtr = primitive "toPtr"

primPtrToWord :: forall a . Ptr a -> Word
primPtrToWord = primitive "toInt"

primIntToPtr :: forall a . Int -> Ptr a
primIntToPtr = primitive "toPtr"

primPtrToInt :: forall a . Ptr a -> Int
primPtrToInt = primitive "toInt"

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

primPtrEQ   :: forall a b . Ptr a -> Ptr b -> Bool
primPtrEQ   = primitive "p=="

primPtrNull :: forall a . Ptr a
primPtrNull = primitive "pnull"

primPtrCast :: forall a b . Ptr a -> Ptr b
primPtrCast = primitive "pcast"

primPtrAdd :: forall a b . Ptr a -> Int -> Ptr b
primPtrAdd = primitive "p+"

primPtrSub :: forall a b . Ptr a -> Ptr b -> Int
primPtrSub = primitive "p-"

primArrAlloc :: forall a . Int -> a -> IO (IOArray a)
primArrAlloc = primitive "A.alloc"

primArrSize :: forall a . IOArray a -> IO Int
primArrSize = primitive "A.size"

primArrRead :: forall a . IOArray a -> Int -> IO a
primArrRead = primitive "A.read"

primArrWrite :: forall a . IOArray a -> Int -> a -> IO ()
primArrWrite = primitive "A.write"

primArrEQ :: forall a . IOArray a -> IOArray a -> Bool
primArrEQ = primitive "A.=="
