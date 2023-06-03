module IOTest(module IOTest) where
import Prelude

f x = x*2+1

main = do
  putChar 'a'
  putChar 'b'
  c <- getChar
  putStrLn [c,c,c]
  let { p = putStrLn "hello" }
  p
  p
  p
  print (+)
  hout <- openFile "test.tmp" WriteMode
  hPutChar hout 'a'
  hPutChar hout 'z'
  hClose hout
  hin <- openFile "test.tmp" ReadMode
  c1 <- hGetChar hin
  c2 <- hGetChar hin
  putStrLn $ showPair showChar showChar (c1, c2)
  writeFile "test2.tmp" "more\n"
  s <- readFile "test2.tmp"
  putStrLn (showString s)
  writeSerialized "f.tmp" f
  g <- readSerialized "f.tmp"
  putStrLn $ showInt $ g 5
