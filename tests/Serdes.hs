module Serdes(main) where

import System.IO.Serialize
import Mhs.Print

f :: Int -> Int
f x = x*2+1

main :: IO ()
main = do
  cprint ((+) :: Int->Int->Int)
  writeSerialized "f.tmp" f
  g <- readSerialized "f.tmp"
  print (g (5::Int) :: Int)
