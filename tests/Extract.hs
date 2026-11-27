module Extract(main) where
import Mhs.Extract
import Mhs.Reflect
import Primitives(primIntAdd, primIntSub, primIntEQ, primIntLT, primIntGT)
import System.Mem(performGCWithReduction)

-- The examples use primitives rather than class methods,
-- so the output does not depend on how the library is compiled.

-- Local recursion: the loop is a redex, since it has its first argument.
len :: [a] -> Int
len = go 0
  where go n [] = n
        go n (_:xs) = go (primIntAdd n 1) xs

-- Mutual recursion: odd' is copied into evn, and calls it back (self).
evn, odd' :: Int -> Bool
evn n = if primIntEQ n 0 then True else odd' (primIntSub n 1)
odd' n = if primIntEQ n 0 then False else evn (primIntSub n 1)

data Tree = Leaf | Node Tree Int Tree

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x t@(Node l y r) | primIntLT x y = Node (insert x l) y r
                        | primIntGT x y = Node l y (insert x r)
                        | otherwise = t

-- The strings are redexes, fromUTF8 applied to a ByteString.
answer :: Bool -> String
answer b = if b then "yes" else "no"

main :: IO ()
main = do
  let sh :: Int -> a -> IO ()
      sh limit a = do
        -- See the Reflect test.
        performGCWithReduction
        performGCWithReduction
        mx <- extract limit a
        case mx of
          Nothing -> putStrLn "not a partial application"
          Just x -> do
            putStrLn (extractionSummary x)
            putStr (ppProg (decompile (extractionProg x)))
        putStrLn ""
  sh defaultLimit (len :: [Int] -> Int)
  sh defaultLimit evn
  sh defaultLimit insert
  sh 5 insert                   -- the nodes furthest from the root are cut
  sh defaultLimit answer
  sh defaultLimit (42 :: Int)
