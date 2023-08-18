module IOTest(module IOTest) where
import Prelude
import System.IO as IO
import System.Environment
import Debug.Trace

f :: Int -> Int
f x = x*2+1

foo :: IO ()
foo = IO.do
  putStrLn "foo 1"
  putStrLn "foo 2"

main :: IO ()
main = do
  tstart <- getTimeMilli
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
  foo
  putStrLn $ showInt $ trace "tracing" 5
  as <- getArgs
  putStrLn $ showList showString as
  tend <- getTimeMilli
  putStrLn $ showInt (tend - tstart) ++ "ms execution time"
