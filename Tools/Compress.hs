module Compress(main) where
import Prelude
import Data.Map as M
import Data.Char
import System.IO
--import Debug.Trace

type Table = M.Map [Char] Int

toChar :: Int -> Char
toChar i = chr (i + 32)

(!) :: Table -> [Char] -> Int
(!) t s =
  case M.lookupBy compareString s t of
    Nothing -> undefined -- error $ "(!): " ++ showString s
    Just i -> i

compress :: Table -> [Char] -> [Char] -> [Int]
compress t [] p = [ t ! p ]
compress t (c:cs) p =
  let p' = p ++ [c]
      s = M.size t
      t' = if s < 4096 then M.insertBy compareString p' s t else t
  in
--      trace ("compress " ++ showString p') $
--      trace (showList (showPair showString showInt) (M.toList t)) $
      case M.lookupBy compareString p' t of
        Just _ ->
--          trace "found" $
          compress t cs p'
        Nothing ->
--          trace ("not found p=" ++ showString p ++ " " ++ showMaybe showInt (M.lookupBy compareString p t)) $
          (t ! p) : compress t' cs [c]

-- Initial table is ' ' .. '~', and '\n'
initTable :: Table
initTable = M.fromListBy compareString $ [([toChar c], c) | c <- [0..94] ] ++ [("\n", 95)]

toBytes :: [Int] -> [Int]
toBytes [] = []
toBytes [i] = [i `rem` 256, i `quot` 256, 0]
toBytes (i1:i2:is) =
  let i = i1 + 4096*i2
      b1 = i `rem` 256
      b2 = (i `quot` 256) `rem` 256
      b3 = i `quot` (256*256)
  in  b1 : b2 : b3 : toBytes is

main :: IO ()
main = do
  f <- hGetContents stdin
  let bs = compress initTable f []
  hSetBinaryMode stdout True
  putStr $ 'Z' : (map chr $ toBytes bs)
