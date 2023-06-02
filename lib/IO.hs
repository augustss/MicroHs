module IO where
import Prelude
import List

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

(>>=)        = primitive "IO.>>="
(>>)         = primitive "IO.>>"
return       = primitive "IO.return"
hPutChar     = primitive "IO.putChar"
hGetChar     = primitive "IO.getChar"
hSerialize   = primitive "IO.serialize"
hdeserialize = primitive "IO.deserialize"
hClose       = primitive "IO.close"
stdin        = primitive "IO.stdin"
stdout       = primitive "IO.stdout"
stderr       = primitive "IO.stderr"

openFile :: FilePath -> IOMode -> IO Handle
openFile p m =
  let
    n = case m of
          ReadMode -> 0
          WriteMode -> 1
          AppendMode -> 2
          ReadWriteMode -> 3
  in  (primitive "IO.open") p n

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
        a : as -> IO.do
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
        a : as -> IO.do
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
writeFile p s = IO.do
  h <- openFile p WriteMode
  hPutStr h s
  hClose h

-- Strict readFile
readFile :: FilePath -> IO Strict
readFile p = IO.do
  h <- openFile p ReadMode
  cs <- hGetContents h
  hClose h
  return cs

-- Strict hGetContents
hGetContents :: Handle -> IO String
hGetContents h = IO.do
  c <- hGetChar h
  case ord c == negate 1 of
    False -> IO.do { cs <- hGetContents h; return (c:cs) }
    True  -> return ""

writeSerialized :: FilePath -> a -> IO ()
writeSerialized p s = IO.do
  h <- openFile p WriteMode
  hSerialize h s
  hClose h

readSerialized :: FilePath -> IO a
readSerialized p = IO.do
  h <- openFile p ReadMode
  a <- hdeserialize h
  hClose h
  return a
