module IOTest where
import Prelude
import IO
import String

f x = x*2+1

main = IO.do
  putChar 'a'
  putChar 'b'
  c <- getChar
  putStrLn [c,c,c]
  let p = putStrLn "hello"
  p
  p
  p
  print (+)
  hout <- openFile "test.txt" WriteMode
  hPutChar hout 'a'
  hPutChar hout 'z'
  hClose hout
  hin <- openFile "test.txt" ReadMode
  c1 <- hGetChar hin
  c2 <- hGetChar hin
  putStrLn $ showPair showChar showChar (c1, c2)
  writeFile "test2.txt" "more\n"
  s <- readFile "test2.txt"
  putStrLn (showString s)
  writeSerialized "f.comb" f
  g <- readSerialized "f.comb"
  putStrLn $ showInt $ g 5
