module System.IO(module System.IO) where
import Control.Error
import Data.Bool
import Data.Int
import Data.List

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

(>>=)        = primitive "IO.>>="
(>>)         = primitive "IO.>>"
return       = primitive "IO.return"
hPutChar     = primitive "IO.putChar"
hSerialize   = primitive "IO.serialize"
hdeserialize = primitive "IO.deserialize"
hClose       = primitive "IO.close"
stdin        = primitive "IO.stdin"
stdout       = primitive "IO.stdout"
stderr       = primitive "IO.stderr"

hGetChar h = do
  c <- (primitive "IO.getChar") h
  case ord c == negate 1 of
    False -> return c
    True  -> error "hGetChar: EOF"

openFile :: FilePath -> IOMode -> IO Handle
openFile p m = do
  let {
    n = case m of
          ReadMode -> 0
          WriteMode -> 1
          AppendMode -> 2
          ReadWriteMode -> 3
    }
  hdl <- (primitive "IO.open") p n
  case (primitive "IO.isNullHandle") hdl of
    False -> return hdl
    True  -> error ("openFile: cannot open " ++ p)

putChar :: Char -> IO ()
putChar = hPutChar stdout

getChar :: Char -> IO ()
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

writeFile :: FilePath -> String -> IO ()
writeFile p s = do
  h <- openFile p WriteMode
  hPutStr h s
  hClose h

-- Strict readFile
readFile :: FilePath -> IO Strict
readFile p = do
  h <- openFile p ReadMode
  cs <- hGetContents h
  hClose h
  return cs

-- Strict hGetContents
hGetContents :: Handle -> IO String
hGetContents h = do
  c <- (primitive "IO.getChar") h
  case ord c == negate 1 of
    False -> do { cs <- hGetContents h; return (c:cs) }
    True  -> return ""

writeSerialized :: FilePath -> a -> IO ()
writeSerialized p s = do
  h <- openFile p WriteMode
  hSerialize h s
  hClose h

readSerialized :: FilePath -> IO a
readSerialized p = do
  h <- openFile p ReadMode
  a <- hdeserialize h
  hClose h
  return a
