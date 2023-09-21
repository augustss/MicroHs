import System.Environment
import qualified Data.Map as M
import Data.Function
import Data.List

main = do
  [fn] <- getArgs
  file <- readFile fn
  let res = loop M.empty file
      loop m "" = m
      loop m cs@(_:ds) | Just (x,y) <- getComb cs = loop (M.insertWith (+) x 1 m) y
                       | otherwise = loop m ds
      getComb ('(':'$':cs) = Just ('$':xs, tail ys) where (xs, ys) = span (/= ')') cs
      getComb _ = Nothing
      srt = sortBy (compare `on` snd) $ M.toList res
  mapM print srt
  
