module Extract(main) where
import Control.Monad((>=>))
import Foreign.Marshal.Alloc(free, malloc)
import Foreign.Ptr(Ptr)
import Foreign.Storable
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

-- Skip the indirections.
skipInd :: NodePtr -> IO NodePtr
skipInd n@(CellPtr p) = do
  c <- peek p
  case c of
    IND n' -> skipInd n'
    _      -> return n
skipInd n = return n

-- What a value is, after the indirections.
nodeOf :: a -> IO NodePtr
nodeOf a = skipInd (decodeNodePtr (nodeAddr a))

-- The cell of a value.
cellOf :: a -> IO Cell
cellOf a = a `seq` do
  n <- nodeOf a
  case n of
    CellPtr p -> peek p
    _         -> error ("not a cell: " ++ show n)

-- Reading and writing cells.
cells :: IO ()
cells = do
  let i = 42 :: Int
      d = 1.5 :: Double
      j = Just i
  nodeOf True >>= print
  cellOf i >>= print
  cellOf d >>= print
  cj <- cellOf j
  case cj of
    AP f _ -> skipInd f >>= print
    _      -> putStrLn "not an application"
  -- encodeNodePtr is the inverse of decodeNodePtr
  let inverse x = encodeNodePtr (decodeNodePtr (nodeAddr x)) == nodeAddr x
  print (inverse True, inverse i, inverse j, inverse not)
  -- poke and peek, outside the heap
  p <- malloc :: IO (Ptr Cell)
  let roundTrip c = do
        poke p c
        c' <- peek p
        return (c' == c, c')
  mapM_ (roundTrip >=> print)
    [ AP (Comb (C 1 2)) (Comb (D 3)), IND (Comb (E 4)), INT (-7), INT64 (-3),
      DBL 2.5, FLT32 0.5, IO_CCALL 1, TICK 2, DATA 0 5 ]
  roundTrip (AP (CellPtr p) (CellPtr p)) >>= print . fst
  free p
  putStrLn ""

main :: IO ()
main = do
  cells
  let sh :: Int -> a -> IO ()
      sh limit a = do
        -- See the Reflect test.
        performGCWithReduction
        performGCWithReduction
        mx <- extract limit a
        case mx of
          Nothing -> putStrLn "not a partial application"
          Just x -> do
            let (prog, refs) = extractionProg x
            putStrLn (extractionSummary x)
            putStrLn (ppProg (decompile prog))
            -- The free variables, and the kinds of their cells.
            kinds <- mapM (\ (v, p) -> do c <- peek p; return (v ++ "=" ++ takeWhile (/= ' ') (show c))) refs
            putStrLn (unwords ("free:" : kinds))
        putStrLn ""
  sh defaultLimit (len :: [Int] -> Int)
  sh defaultLimit evn
  sh defaultLimit insert
  sh 5 insert                   -- the nodes furthest from the root are cut
  sh defaultLimit answer
  sh defaultLimit (42 :: Int)
