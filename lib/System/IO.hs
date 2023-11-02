-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO(
  module System.IO, Handle, IO,
  module Data.Functor,
  module Control.Applicative,
  module Control.Monad,
  ) where
import Primitives
import Control.Applicative
import Control.Error
import Control.Monad
import Data.Bool
import Data.Char
import Data.Eq
import Data.Int
import Data.List
import Data.Functor
import Data.Maybe
import Data.Tuple
import Text.Show

type FilePath = String

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

instance Functor IO where
  fmap f ioa   = ioa `primBind` \ a -> primReturn (f a)
instance Applicative IO where
  pure         = primReturn
  (<*>)        = ap
instance Monad IO where
  (>>=)        = primBind
  (>>)         = primThen
  return       = primReturn
instance MonadFail IO where
  fail         = error

{-
infixl 1 >>=
(>>=)       :: forall a b . IO a -> (a -> IO b) -> IO b
(>>=)        = primBind

infixl 1 >>
(>>)        :: forall a b . IO a -> IO b -> IO b
(>>)         = primThen

return      :: forall a . a -> IO a
return       = primReturn

fail        :: forall a . String -> IO a
fail s       = error s

fmap        :: forall a b . (a -> b) -> IO a -> IO b
fmap f ioa   = ioa >>= \ a -> return (f a)
-}

hSerialize   :: forall a . Handle -> a -> IO ()
hSerialize   = primHSerialize
hDeserialize :: forall a . Handle -> IO a
hDeserialize = primHDeserialize
hClose       :: Handle -> IO ()
hClose       = primHClose
hFlush       :: Handle -> IO ()
hFlush       = primHFlush
stdin        :: Handle
stdin        = primStdin
stdout       :: Handle
stdout       = primStdout
stderr       :: Handle
stderr       = primStderr

hGetChar :: Handle -> IO Char
hGetChar h = do
  c <- primHGetChar h
  if c == negate 1 then
    error "hGetChar: EOF"
   else
    return (chr c)

hPutChar :: Handle -> Char -> IO ()
hPutChar h c = primHPutChar h (ord c)

openFileM :: FilePath -> IOMode -> IO (Maybe Handle)
openFileM p m = do
  let
    n = case m of
          ReadMode -> 0
          WriteMode -> 1
          AppendMode -> 2
          ReadWriteMode -> 3
  hdl <- primOpenFile p n
  if primIsNullHandle hdl then
    return Nothing
   else
    return (Just hdl)

openFile :: String -> IOMode -> IO Handle
openFile p m = do
  mh <- openFileM p m
  case mh of
    Nothing -> error ("openFile: cannot open " ++ p)
    Just h -> return h

putChar :: Char -> IO ()
putChar = hPutChar stdout

getChar :: IO Char
getChar = hGetChar stdin

cprint :: forall a . a -> IO ()
cprint = primHPrint stdout

print :: forall a . (Show a) => a -> IO ()
print a = putStrLn (show a)

{-
mapM :: forall a b . (a -> IO b) -> [a] -> IO [b]
mapM f =
  let
    rec [] = return []
    rec (a : as) = do
      b <- f a
      bs <- rec as
      return (b : bs)
  in rec

mapM_ :: forall a b . (a -> IO b) -> [a] -> IO ()
mapM_ f =
  let
    rec [] = return ()
    rec (a : as) = do
      f a
      rec as
  in rec

when :: Bool -> IO () -> IO ()
when b io = if b then io else return ()
-}

putStr :: String -> IO ()
putStr = hPutStr stdout

hPutStr :: Handle -> String -> IO ()
hPutStr h = mapM_ (hPutChar h)

putStrLn :: String -> IO ()
putStrLn = hPutStrLn stdout

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

writeFile :: FilePath -> String -> IO ()
writeFile p s = do
  h <- openFile p WriteMode
  hPutStr h s
  hClose h

-- Lazy readFile
readFile :: FilePath -> IO String
readFile p = do
  h <- openFile p ReadMode
  cs <- hGetContents h
  --hClose h  can't close with lazy hGetContents
  return cs

-- Lazy hGetContents
hGetContents :: Handle -> IO String
hGetContents h = do
  c <- primHGetChar h
  if c == negate 1 then do
    hClose h   -- EOF, so close the handle
    return ""
   else do
    cs <- unsafeInterleaveIO (hGetContents h)
    return (chr c : cs)
  
writeSerialized :: forall a . FilePath -> a -> IO ()
writeSerialized p s = do
  h <- openFile p WriteMode
  hSerialize h s
  hClose h

readSerialized :: forall a . FilePath -> IO a
readSerialized p = do
  h <- openFile p ReadMode
  a <- hDeserialize h
  hClose h
  return a

getTimeMilli :: IO Int
getTimeMilli = primGetTimeMilli

unsafeInterleaveIO :: forall a . IO a -> IO a
unsafeInterleaveIO ioa = return (primPerformIO ioa)

seq :: forall a b . a -> b -> b
seq = primSeq

-- MicroHs is always in binary mode
hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode _ _ = return ()
