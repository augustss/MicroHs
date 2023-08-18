-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module System.IO(module System.IO) where
import qualified Primitives as P
import Control.Error
import Data.Bool
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Tuple

--Ytype IO = P.IO
--Ytype Handle = P.Handle

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

--Yinfixl 1 >>=
(>>=)       :: forall a b . IO a -> (a -> IO b) -> IO b
(>>=)        = P.primBind

--Yinfixl 1 >>
(>>)        :: forall a b . IO a -> IO b -> IO b
(>>)         = P.primThen

return      :: forall a . a -> IO a
return       = P.primReturn

hSerialize   :: forall a . Handle -> a -> IO ()
hSerialize   = P.primHSerialize
hDeserialize :: forall a . Handle -> IO a
hDeserialize = P.primHDeserialize
hClose       :: Handle -> IO ()
hClose       = P.primHClose
stdin        :: Handle
stdin        = P.primStdin
stdout       :: Handle
stdout       = P.primStdout
stderr       :: Handle
stderr       = P.primStderr

hGetChar :: Handle -> IO Char
hGetChar h = do
  c <- P.primHGetChar h
  case c == negate 1 of
    False -> return (chr c)
    True  -> error "hGetChar: EOF"

hPutChar :: Handle -> Char -> IO ()
hPutChar h c = P.primHPutChar h (ord c)

openFileM :: String -> IOMode -> IO (Maybe Handle)
openFileM p m = do
  let {
    n = case m of
          ReadMode -> 0
          WriteMode -> 1
          AppendMode -> 2
          ReadWriteMode -> 3
    }
  hdl <- P.primOpenFile p n
  case P.primIsNullHandle hdl of
    False -> return (Just hdl)
    True  -> return Nothing

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

print :: forall a . a -> IO ()
print = P.primHPrint stdout

mapM :: forall a b . (a -> IO b) -> [a] -> IO [b]
mapM f =
  let
    rec arg =
      case arg of
        [] -> return []
        a : as -> do
          b <- f a
          bs <- rec as
          return (b : bs)
  in rec

mapM_ :: forall a b . (a -> IO b) -> [a] -> IO ()
mapM_ f =
  let
    rec arg =
      case arg of
        [] -> return ()
        a : as -> do
          f a
          rec as
  in rec

when :: Bool -> IO () -> IO ()
when b io = if b then io else return ()

putStr :: String -> IO ()
putStr = hPutStr stdout

hPutStr :: Handle -> String -> IO ()
hPutStr h = mapM_ (hPutChar h)

putStrLn :: String -> IO ()
putStrLn = hPutStrLn stdout

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

writeFile :: String -> String -> IO ()
writeFile p s = do
  h <- openFile p WriteMode
  hPutStr h s
  hClose h

-- Strict readFile
readFile :: String -> IO String
readFile p = do
  h <- openFile p ReadMode
  cs <- hGetContents h
  hClose h
  return cs

-- Strict hGetContents
hGetContents :: Handle -> IO String
hGetContents h = do
  c <- P.primHGetChar h
  case c == negate 1 of
    False ->
      do { cs <- hGetContents h; return (chr c:cs) }
      -- This should use less stack, but it doesn't work. :(
      --return (chr c : P.primPerformIO (hGetContents h))
    True  -> return ""
  
writeSerialized :: forall a . String -> a -> IO ()
writeSerialized p s = do
  h <- openFile p WriteMode
  hSerialize h s
  hClose h

readSerialized :: forall a . String -> IO a
readSerialized p = do
  h <- openFile p ReadMode
  a <- hDeserialize h
  hClose h
  return a

getTimeMilli :: IO Int
getTimeMilli = P.primGetTimeMilli
