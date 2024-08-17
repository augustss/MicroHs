-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.List where
import Data.List

-- Various useful list functions.
-- These are not really MicroHs specific.

------- List --------

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq a = any (eq a)

-- A simple "quicksort" for now.
sortLE :: forall a . (a -> a -> Bool) -> [a] -> [a]
sortLE _  [] = []
sortLE le (x:xs) = sortLE le lt ++ (x : sortLE le ge)
  where (ge, lt) = partition (le x) xs

showListS :: (a -> String) -> [a] -> String
showListS sa arg =
  let
    showRest as =
      case as of
        [] -> "]"
        x : xs -> "," ++ sa x ++ showRest xs
  in
    case arg of
      [] -> "[]"
      a : as -> "[" ++ sa a ++ showRest as

anySame :: (Eq a) => [a] -> Bool
anySame = anySameBy (==)

anySameBy :: (a -> a -> Bool) -> [a] -> Bool
anySameBy _ [] = False
anySameBy eq (x:xs) = elemBy eq x xs || anySameBy eq xs

deleteAllBy :: forall a . (a -> a -> Bool) -> a -> [a] -> [a]
deleteAllBy _ _ [] = []
deleteAllBy eq x (y:ys) = if eq x y then deleteAllBy eq x ys else y : deleteAllBy eq x ys

deleteAllsBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteAllsBy eq = foldl (flip (deleteAllBy eq))

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
  b <- p x
  (ts,fs) <- partitionM p xs
  return $ if b then (x:ts, fs) else (ts, x:fs)

substString :: forall a . Eq a => [a] -> [a] -> [a] -> [a]
substString _ _ [] = []
substString from to xs@(c:cs) | Just rs <- stripPrefix from xs = to ++ substString from to rs
                              | otherwise = c : substString from to cs

showPairS :: forall a b . (a -> String) -> (b -> String) -> (a, b) -> String
showPairS sa sb (a, b) = "(" ++ sa a ++ "," ++ sb b ++ ")"

findCommonPrefix :: Eq a => [[a]] -> [a]
findCommonPrefix [] = []
findCommonPrefix ([] : _) = []
findCommonPrefix ((x:xs) : ys) | Just ys' <- mapM (f x) ys = x : findCommonPrefix (xs:ys')
                               | otherwise = []
  where f a (b:bs) | a == b = Just bs
        f _ _ = Nothing

dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse
