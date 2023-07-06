module Primitives(
  module Primitives,
  Char,
  Int,
  IO,
  Handle,
  ) where
import System.IO
import System.Environment

primIntAdd :: Int -> Int -> Int
primIntAdd = (+)

primIntSub :: Int -> Int -> Int
primIntSub = (-)

primIntMul :: Int -> Int -> Int
primIntMul = (*)

primIntQuot :: Int -> Int -> Int
primIntQuot = quot

primIntRem :: Int -> Int -> Int
primIntRem = rem

primIntSubR :: Int -> Int -> Int
primIntSubR = subtract

primIntEQ :: Int -> Int -> Bool
primIntEQ = (==)

primIntNE :: Int -> Int -> Bool
primIntNE = (/=)

primIntLT :: Int -> Int -> Bool
primIntLT = (<)

primIntLE :: Int -> Int -> Bool
primIntLE = (<=)

primIntGT :: Int -> Int -> Bool
primIntGT = (>)

primIntGE :: Int -> Int -> Bool
primIntGE = (>=)

primOrd :: Char -> Int
primOrd = fromEnum

primChr :: Int -> Char
primChr = toEnum

primFix :: (a -> a) -> a
primFix f = let a = f a in a

primError :: String -> a
primError = error

------

primBind         :: IO a -> (a -> IO b) -> IO b
primBind          = (>>=)
primThen         :: IO a -> IO b -> IO b
primThen          = (>>)
primReturn       :: a -> IO a
primReturn        = return
primHPutChar     :: Handle -> Int -> IO ()
primHPutChar h c  = hPutChar h (toEnum c)
primHGetChar     :: Handle -> IO Int
primHGetChar h    = do eof <- hIsEOF h; if eof then pure (-1) else fromEnum <$> hGetChar h
primOpenFile     :: String -> Int -> IO Handle
primOpenFile s m  = openFile s (case m of 0->ReadMode; 1->WriteMode; 2->AppendMode; 3->ReadWriteMode)
primIsNullHandle  = const False
primHSerialize    = undefined
primHDeserialize  = undefined
primHClose        = hClose
primStdin         = stdin
primStdout        = stdout
primStderr        = stderr
primGetArgs       = getArgs

