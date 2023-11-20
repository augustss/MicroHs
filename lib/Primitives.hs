-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Primitives(module Primitives) where
import Data.Bool_Type
--import Data.List_Type
import Data.Ordering_Type

infixr -1 ->
infixr -2 =>
infix   4 ~

data Any
data Char
data Handle
data Int
data Double
data IO a
data Word
data Ptr a

-- Type equality as a constraint.
class a ~ b {-x | a -> b, b -> a-}

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

primDoubleAdd :: Double -> Double -> Double
primDoubleAdd  = primitive "fadd"
primDoubleSub :: Double -> Double -> Double
primDoubleSub  = primitive "fsub"
primDoubleMul :: Double -> Double -> Double
primDoubleMul  = primitive "fmul"
primDoubleDiv :: Double -> Double -> Double
primDoubleDiv = primitive "fdiv"
primDoubleNeg :: Double -> Double
primDoubleNeg = primitive "fneg"
primDoubleEQ :: Double -> Double -> Bool
primDoubleEQ = primitive "feq"
primDoubleNE :: Double -> Double -> Bool
primDoubleNE = primitive "fne"
primDoubleLT :: Double -> Double -> Bool
primDoubleLT = primitive "flt"
primDoubleLE :: Double -> Double -> Bool
primDoubleLE = primitive "fle"
primDoubleGT :: Double -> Double -> Bool
primDoubleGT = primitive "fgt"
primDoubleGE :: Double -> Double -> Bool
primDoubleGE = primitive "fge"
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

primWordEQ  :: Word -> Word -> Bool
primWordEQ  = primitive "=="
primWordNE  :: Word -> Word -> Bool
primWordNE  = primitive "/="
primWordLT  :: Word -> Word -> Bool
primWordLT  = primitive "u<"
primWordLE   :: Word -> Word -> Bool
primWordLE   = primitive "<="
primWordGT   :: Word -> Word -> Bool
primWordGT   = primitive ">"
primWordGE   :: Word -> Word -> Bool
primWordGE   = primitive ">="

primWordToInt :: Word -> Int
primWordToInt = primitive "I"
primIntToWord :: Int -> Word
primIntToWord = primitive "I"

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
primCompare :: forall a . a -> a -> Ordering
primCompare  = primitive "compare"

primStringEQ  :: [Char] -> [Char] -> Bool
primStringEQ  = primitive "equal"

primChr :: Int -> Char
primChr = primitive "I"
primOrd :: Char -> Int
primOrd = primitive "I"

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
primDropArgs     :: Int -> IO ()
primDropArgs      = primitive "IO.dropArgs"
primPerformIO    :: forall a . IO a -> a
primPerformIO     = primitive "IO.performIO"

primWithDropArgs :: forall a . Int -> IO a -> IO a
primWithDropArgs i ioa = primThen (primDropArgs i) ioa

-- Use string for the exception until we can do better.
primCatch        :: forall a . IO a -> ([Char] -> IO a) -> IO a
primCatch         = primitive "IO.catch"

primRnfErr       :: forall a . a -> ()
primRnfErr        = primitive "rnf" (0::Int)

primRnfNoErr     :: forall a . a -> ()
primRnfNoErr      = primitive "rnf" (1::Int)

primNewCAString :: [Char] -> IO (Ptr Char)
primNewCAString = primitive "newCAString"

primPeekCAString :: Ptr Char -> IO [Char]
primPeekCAString = primitive "peekCAString"

primWordToPtr :: forall a . Word -> Ptr a
primWordToPtr = primitive "toPtr"

primPtrToWord :: forall a . Ptr a -> Word
primPtrToWord = primitive "toInt"
