module Serdes(main) where
import Prelude
import System.IO as IO
import System.Environment
import Debug.Trace

f :: Int -> Int
f x = x*2+1

main :: IO ()
main = do
  writeSerialized "f.tmp" f
  g <- readSerialized "f.tmp"
  putStrLn $ show $ (g (5::Int) :: Int)
