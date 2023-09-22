-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Primitives(module Primitives) where
import Data.Bool_Type
import Data.Ordering_Type

infixr -1 ->

data Any
data Char
data Handle
data Int
data IO a
data Word

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

--primCompare  :: forall a . a -> a -> Int
primCompare :: [Char] -> [Char] -> Ordering
primCompare  = primitive "compare"

primEqString  :: [Char] -> [Char] -> Bool
primEqString  = primitive "equal"

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
primHPutChar     :: Handle -> Int -> IO ()
primHPutChar      = primitive "IO.putChar"
primHGetChar     :: Handle -> IO Int
primHGetChar      = primitive "IO.getChar"
primOpenFile     :: [Char] -> Int -> IO Handle
primOpenFile      = primitive "IO.open"
primIsNullHandle :: Handle -> Bool
primIsNullHandle  = primitive "IO.isNullHandle"
primHSerialize   :: forall a . Handle -> a -> IO ()
primHSerialize    = primitive "IO.serialize"
primHPrint       :: forall a . Handle -> a -> IO ()
primHPrint        = primitive "IO.print"
primHDeserialize :: forall a . Handle -> IO a
primHDeserialize  = primitive "IO.deserialize"
primHClose       :: Handle -> IO ()
primHClose        = primitive "IO.close"
primHFlush       :: Handle -> IO ()
primHFlush        = primitive "IO.flush"
primStdin        :: Handle
primStdin         = primitive "IO.stdin"
primStdout       :: Handle
primStdout        = primitive "IO.stdout"
primStderr       :: Handle
primStderr        = primitive "IO.stderr"
primGetArgs      :: IO [[Char]]
primGetArgs       = primitive "IO.getArgs"
primDropArgs     :: Int -> IO ()
primDropArgs      = primitive "IO.dropArgs"
primPerformIO    :: forall a . IO a -> a
primPerformIO     = primitive "IO.performIO"
primGetTimeMilli :: IO Int
primGetTimeMilli  = primitive "IO.getTimeMilli"
primGetRaw       :: IO Int
primGetRaw        = primitive "IO.getRaw"

primWithDropArgs :: forall a . Int -> IO a -> IO a
primWithDropArgs i ioa = primThen (primDropArgs i) ioa

-- Use string for the exception until we can do better.
primCatch        :: forall a . IO a -> ([Char] -> IO a) -> IO a
primCatch         = primitive "IO.catch"

primRnf          :: forall a . a -> ()
primRnf           = primitive "rnf"

-- Temporary until overloading
primIsInt        :: Any -> Bool
primIsInt         = primitive "isInt"
primIsIO         :: Any -> Bool
primIsIO          = primitive "isIO"
