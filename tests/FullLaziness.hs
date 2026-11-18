module FullLaziness(main) where
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- Check that the combinator translation is fully lazy, i.e., that an
-- expression in a function body that does not depend on the function's
-- arguments is evaluated once, however many times the function is called.
-- Each check calls functions several times and reports how many times the
-- counted expressions were evaluated.

evals :: IORef Int
evals = unsafePerformIO (newIORef 0)

-- Count each evaluation of this expression.
counted :: a -> a
counted a = unsafePerformIO $ do
  modifyIORef evals (+ 1)
  return a

check :: String -> IO () -> IO ()
check name act = do
  writeIORef evals 0
  act
  n <- readIORef evals
  putStrLn (name ++ ": " ++ show n)

data Tree a = Leaf | Node (Tree a) a (Tree a)

-- Counted each time it is applied to a dictionary.
-- Each check below uses a different occurrence of insert, so they do not
-- share their specialisations.
insert :: Ord a => a -> Tree a -> Tree a
insert = counted ins
  where ins x Leaf = Node Leaf x Leaf
        ins x t@(Node l y r) | x < y = Node (ins x l) y r
                             | x > y = Node l y (ins x r)
                             | otherwise = t

size :: Tree a -> Int
size Leaf = 0
size (Node l _ r) = size l + 1 + size r

-- A dictionary application in a monomorphic function.
build :: [Int] -> Tree Int
build xs = foldr insert Leaf xs

-- The same, under a case and in a local recursive function.
build2 :: Int -> [Int] -> Int
build2 k xs =
  case k of
    0 -> 0
    _ -> let go [] t = t
             go (y:ys) t = go ys (insert y t)
         in  size (go xs Leaf)

-- A polymorphic function, called from one place.
buildP :: Ord a => [a] -> Tree a
buildP xs = foldr insert Leaf xs

g :: Int -> Int
g x = size (buildP [x, 1, 2])

-- A dictionary that is built from the dictionary argument, Ord [a] from Ord a.
buildL :: Ord a => [[a]] -> Tree [a]
buildL xss = foldr insert Leaf xss

h :: Int -> Int
h n = size (buildL [[n], [1]])

-- An expression that is not a dictionary application.
expensive :: Int -> Int -> Int
expensive k = counted (\ x -> x + k)

addAll :: [Int] -> [Int]
addAll xs = map (expensive 5) xs

-- This depends on the argument k, so it is evaluated once per call of addAll2,
-- and shared by the calls in map.
addAll2 :: Int -> [Int] -> [Int]
addAll2 k xs = map (expensive k) xs

main :: IO ()
main = do
  check "monomorphic" $ do print (size (build [3,1,2])); print (size (build [5,4]))
  check "local function" $ do print (build2 1 [3,1,2]); print (build2 1 [5,4])
  check "polymorphic" $ mapM_ (print . g) [3, 4, 5]
  check "built dictionary" $ mapM_ (print . h) [1, 2, 3]
  check "constant" $ do print (addAll [1,2]); print (addAll [3])
  check "argument" $ do print (addAll2 10 [1,2,3]); print (addAll2 20 [4])
