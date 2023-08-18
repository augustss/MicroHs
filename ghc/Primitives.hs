module Primitives(
  module Primitives,
  Char,
  Int,
  IO,
  Handle,
  ) where
import Control.Exception(try)
import Data.Time
import Data.Time.Clock.POSIX
import System.IO
import System.IO.Unsafe
import System.Environment
import Unsafe.Coerce

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

primCharEQ :: Char -> Char -> Bool
primCharEQ = (==)

primCharNE :: Char -> Char -> Bool
primCharNE = (/=)

primCharLT :: Char -> Char -> Bool
primCharLT = (<)

primCharLE :: Char -> Char -> Bool
primCharLE = (<=)

primCharGT :: Char -> Char -> Bool
primCharGT = (>)

primCharGE :: Char -> Char -> Bool
primCharGE = (>=)

primOrd :: Char -> Int
primOrd = fromEnum

primChr :: Int -> Char
primChr = toEnum

primFix :: (a -> a) -> a
primFix f = let a = f a in a

primError :: String -> a
primError = error

primUnsafeCoerce :: a -> b
primUnsafeCoerce = unsafeCoerce

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
primOpenFile s m  = do
  r <- (try $ openFile s (case m of 0->ReadMode; 1->WriteMode; 2->AppendMode; 3->ReadWriteMode)) :: IO (Either IOError Handle)
  -- A gruesome hack to signal a failed as a Handle
  case r of
    Left _ -> return $ unsafeCoerce (0 :: Int)
    Right h -> return h
primIsNullHandle :: Handle -> Bool
primIsNullHandle h = unsafeCoerce h == (0 :: Int)
primHSerialize    = undefined
primHDeserialize  = undefined
primHPrint        = undefined
primHClose        = hClose
primStdin         = stdin
primStdout        = stdout
primStderr        = stderr
primGetArgs       = getArgs
primPerformIO    :: IO a -> a
primPerformIO     = unsafePerformIO
-- Current time (since 1970-01-01T00:00:00UTC) in ms
primGetTimeMilli :: IO Int
primGetTimeMilli  = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime
