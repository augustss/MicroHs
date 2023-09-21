module Primitives(
  module Primitives,
  Any,
  Char,
  Handle,
  Int,
  IO,
  Word,
  ) where
import Control.Exception(try)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import System.IO
import System.IO.Unsafe
import System.Environment
import Unsafe.Coerce
import GHC.Types(Any)

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

primEqString :: String -> String -> Bool
primEqString = (==)

primUnsafeCoerce :: a -> b
primUnsafeCoerce = unsafeCoerce

primSeq :: a -> b -> b
primSeq = seq

primWordEQ :: Word -> Word -> Bool
primWordEQ = (==)

primWordNE :: Word -> Word -> Bool
primWordNE = (/=)

primWordAdd :: Word -> Word -> Word
primWordAdd = (+)

primWordSub :: Word -> Word -> Word
primWordSub = (-)

primWordMul :: Word -> Word -> Word
primWordMul = (*)

primWordQuot :: Word -> Word -> Word
primWordQuot = quot

primWordRem :: Word -> Word -> Word
primWordRem = rem

primWordLT :: Word -> Word -> Bool
primWordLT = (<)

primWordLE :: Word -> Word -> Bool
primWordLE = (<=)

primWordGT :: Word -> Word -> Bool
primWordGT = (>)

primWordGE :: Word -> Word -> Bool
primWordGE = (>=)

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
primHFlush        = hFlush
primStdin         = stdin
primStdout        = stdout
primStderr        = stderr
primGetArgs      :: IO [[Char]]
primGetArgs       = getArgs
primWithDropArgs :: Int -> IO a -> IO a
primWithDropArgs i ioa = do
  as <- getArgs
  withArgs (drop i as) ioa
primPerformIO    :: IO a -> a
primPerformIO     = unsafePerformIO
-- Current time (since 1970-01-01T00:00:00UTC) in ms
primGetTimeMilli :: IO Int
primGetTimeMilli  = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime
primGetRaw       :: IO Int
primGetRaw        = return (-1) -- not implemented

primCatch        :: forall a . IO a -> (String -> IO a) -> IO a
primCatch         = error "primCatch"

-- Temporary until overloading
primIsInt        :: Any -> Bool
primIsInt         = error "primIsInt"
primIsIO         :: Any -> Bool
primIsIO          = error "primIsIO"

primCompare      :: String -> String -> Int
primCompare s t =
  case compare s t of
    LT -> -1
    EQ -> 0
    GT -> 1
    
