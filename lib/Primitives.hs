-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Primitives(module Primitives) where
--import Data.Bool_Type

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

primChr :: Int -> Char
primChr = primitive "I"
primOrd :: Char -> Int
primOrd = primitive "I"

primUnsafeCoerce :: forall a b . a -> b
primUnsafeCoerce = primitive "I"

--data List a = Nil | (:) a (List a)

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
primOpenFile     :: String -> Int -> IO Handle
primOpenFile      = primitive "IO.open"
primIsNullHandle :: Handle -> Bool
primIsNullHandle  = primitive "IO.isNullHandle"
primHSerialize   :: forall a . Handle -> a -> IO ()
primHSerialize    = primitive "IO.serialize"
primHDeserialize :: forall a . Handle -> IO a
primHDeserialize  = primitive "IO.deserialize"
primHClose       :: Handle -> IO ()
primHClose        = primitive "IO.close"
primStdin        :: Handle
primStdin         = primitive "IO.stdin"
primStdout       :: Handle
primStdout        = primitive "IO.stdout"
primStderr       :: Handle
primStderr        = primitive "IO.stderr"
primGetArgs      :: IO [[Char]]
primGetArgs       = primitive "IO.getArgs"
primPerformIO    :: forall a . IO a -> a
primPerformIO     = primitive "IO.performIO"
primGetTimeMilli :: IO Int
primGetTimeMilli  = primitive "IO.getTimeMilli"
