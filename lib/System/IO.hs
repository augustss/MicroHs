module System.IO(module System.IO) where
import qualified Primitives as P
import Control.Error
import Data.Bool
import Data.Char
import Data.Int
import Data.List

type IO = P.IO
type Handle = P.Handle

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

(>>=)        = P.primBind
(>>)         = P.primThen
return       = P.primReturn
hSerialize   = P.primHSerialize
hdeserialize = P.primHDeserialize
hClose       = P.primHClose
stdin        = P.primStdin
stdout       = P.primStdout
stderr       = P.primStderr

hGetChar :: Handle -> IO Char
hGetChar h = do
  c <- P.primHGetChar h
  case c == negate 1 of
    False -> return (chr c)
    True  -> error "hGetChar: EOF"

hPutChar h c = P.primHPutChar h (ord c)

openFile :: String -> IOMode -> IO Handle
openFile p m = do
  let {
    n = case m of
          ReadMode -> 0
          WriteMode -> 1
          AppendMode -> 2
          ReadWriteMode -> 3
    }
  hdl <- P.primOpenFile p n
  case P.primIsNullHandle hdl of
    False -> return hdl
    True  -> error ("openFile: cannot open " ++ p)

putChar :: Char -> IO ()
putChar = hPutChar stdout

getChar :: IO Char
getChar = hGetChar stdin

print :: a -> IO ()
print = hSerialize stdout

mapM :: (a -> IO b) -> [a] -> IO [b]
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

mapM_ :: (a -> IO b) -> [a] -> IO ()
mapM_ f =
  let
    rec arg =
      case arg of
        [] -> return ()
        a : as -> do
          f a
          rec as
  in rec

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
    False -> do { cs <- hGetContents h; return (chr c:cs) }
    True  -> return ""

writeSerialized :: String -> a -> IO ()
writeSerialized p s = do
  h <- openFile p WriteMode
  hSerialize h s
  hClose h

readSerialized :: String -> IO a
readSerialized p = do
  h <- openFile p ReadMode
  a <- hdeserialize h
  hClose h
  return a
