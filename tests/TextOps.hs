-- Data.Text against String reference implementations, on generated
-- strings with ASCII, multi-byte characters, NUL (two bytes in modified
-- UTF-8), and every kind of space.
module TextOps where
import Control.Concurrent (forkIO)
import qualified Data.ByteString as B
import Data.Bifunctor (first)
import qualified Data.Char as C
import qualified Data.Char.Unicode as U
import qualified Data.List as L
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

pieces :: [String]
pieces =
  [ "a", "b", "ab", "ba", "aa", "A", "Z", " ", "  ", "\t", "\n", "\r\n", "x\n"
  , "\0", "\160", "\x1680", "\x180E", "\x2000", "\x200A", "\x200B", "\x2028"
  , "\x202F", "\x3000", "\xFEFF", "\233", "\8364", "\931", "\128512", "::"
  ]

-- A deterministic generator: a list of seeds from a linear congruential
-- generator.  The arithmetic is on Integer, since Int may have 32 bits
-- (emscripten) and MicroHs checks Int arithmetic for overflow; the seeds
-- are below 2^31, so they fit in any Int.
seeds :: [Int]
seeds = L.map fromInteger (L.tail (L.iterate (\ x -> (x * 1103515245 + 12345) `mod` 2147483648) (42 :: Integer)))

-- | @n@ strings of up to @k@ pieces each.
strings :: Int -> Int -> [String]
strings n k = L.take n (go seeds)
  where
    go (x:xs) = let m = x `mod` (k + 1)
                    (ys, rest) = L.splitAt m xs
                in L.concatMap (\ y -> pieces L.!! (y `mod` L.length pieces)) ys : go rest
    go [] = []

inputs :: [String]
inputs = "" : strings 300 12

-- | Long enough (up to about 500 bytes) for the chunked paths.
longInputs :: [String]
longInputs = strings 40 150

needles :: [String]
needles = L.nub (L.filter (not . L.null) (pieces ++ strings 30 2 ++ ["aa", "a\n", "\128512\128512", "zzz"]))

preds :: [Char -> Bool]
preds = [C.isSpace, (== 'a'), (> 'z'), C.isAlpha, const True, const False]

-- | Report the first input where the two sides differ.
check :: (Show a, Eq b, Show b) => String -> [a] -> (a -> b) -> (a -> b) -> IO ()
check name xs new ref =
  case [ (x, new x, ref x) | x <- xs, new x /= ref x ] of
    []              -> putStrLn (name ++ " ok")
    ((x, a, b) : _) -> putStrLn (name ++ " FAILED on " ++ show x ++ ": " ++ show a ++ " /= " ++ show b)

u :: Text -> String
u = unpack

us :: [Text] -> [String]
us = L.map unpack

refBreakOn :: String -> String -> (String, String)
refBreakOn p = go []
  where go acc s@(c:cs) | p `L.isPrefixOf` s = (L.reverse acc, s)
                        | otherwise = go (c:acc) cs
        go acc [] = (L.reverse acc, [])

refSplitOn :: String -> String -> [String]
refSplitOn sep = loop []
  where
    loop r [] = [L.reverse r]
    loop r s@(c:cs) | Just t <- L.stripPrefix sep s = L.reverse r : loop [] t
                    | otherwise = loop (c:r) cs

refSplit :: (Char -> Bool) -> String -> [String]
refSplit p s = case L.break p s of
                 (a, [])       -> [a]
                 (a, _ : rest) -> a : refSplit p rest

refChunksOf :: Int -> String -> [String]
refChunksOf n s | n <= 0 || L.null s = []
                | otherwise = L.take n s : refChunksOf n (L.drop n s)

main :: IO ()
main = do
  let ints = [-1 .. 14]
      withInt = [ (n, s) | n <- ints, s <- inputs ]
      withNeedle = [ (p, s) | p <- needles, s <- inputs ]
      withPred = [ (i, s) | i <- [0 .. L.length preds - 1], s <- inputs ++ longInputs ]
      nonEmpty = L.filter (not . L.null) inputs
      lists = L.take 200 (L.map (\ k -> L.take (k `mod` 6) (L.drop k inputs)) [0 ..])
      chars = L.nub (L.concat pieces) ++ ['\xD800', 'q']
      p i = preds L.!! i

  check "length" inputs (T.length . pack) L.length
  check "compareLength" withInt (\ (n, s) -> T.compareLength (pack s) n) (\ (n, s) -> compare (L.length s) n)
  check "take" withInt (\ (n, s) -> u (T.take n (pack s))) (uncurry L.take)
  check "drop" withInt (\ (n, s) -> u (T.drop n (pack s))) (uncurry L.drop)
  check "splitAt" withInt (\ (n, s) -> case T.splitAt n (pack s) of (a, b) -> (u a, u b)) (uncurry L.splitAt)
  check "takeEnd" withInt (\ (n, s) -> u (T.takeEnd n (pack s))) (\ (n, s) -> L.reverse (L.take n (L.reverse s)))
  check "dropEnd" withInt (\ (n, s) -> u (T.dropEnd n (pack s))) (\ (n, s) -> L.reverse (L.drop n (L.reverse s)))
  check "index" [ (i, s) | s <- inputs, i <- [0 .. L.length s - 1] ] (\ (i, s) -> T.index (pack s) i) (\ (i, s) -> s L.!! i)
  check "last" nonEmpty (T.last . pack) L.last
  check "init" nonEmpty (u . T.init . pack) L.init
  check "unsnoc" inputs (fmap (first u) . T.unsnoc . pack)
                        (\ s -> if L.null s then Nothing else Just (L.init s, L.last s))
  check "chunksOf" withInt (\ (n, s) -> us (T.chunksOf n (pack s))) (uncurry refChunksOf)
  check "inits" inputs (us . T.inits . pack) L.inits
  check "tails" inputs (us . T.tails . pack) L.tails
  check "reverse" inputs (u . T.reverse . pack) L.reverse

  check "isPrefixOf" withNeedle (\ (q, s) -> T.isPrefixOf (pack q) (pack s)) (uncurry L.isPrefixOf)
  check "isSuffixOf" withNeedle (\ (q, s) -> T.isSuffixOf (pack q) (pack s)) (uncurry L.isSuffixOf)
  check "isInfixOf" withNeedle (\ (q, s) -> T.isInfixOf (pack q) (pack s)) (uncurry L.isInfixOf)
  check "isInfixOf empty" inputs (T.isInfixOf T.empty . pack) (const True)
  check "stripPrefix" withNeedle (\ (q, s) -> fmap u (T.stripPrefix (pack q) (pack s))) (uncurry L.stripPrefix)
  check "stripSuffix" withNeedle (\ (q, s) -> fmap u (T.stripSuffix (pack q) (pack s))) (uncurry L.stripSuffix)
  check "breakOn" withNeedle (\ (q, s) -> case T.breakOn (pack q) (pack s) of (a, b) -> (u a, u b))
                                             (uncurry refBreakOn)
  check "breakOnEnd" withNeedle
    (\ (q, s) -> case T.breakOnEnd (pack q) (pack s) of (a, b) -> (u a, u b))
    (\ (q, s) -> case refBreakOn (L.reverse q) (L.reverse s) of (a, b) -> (L.reverse b, L.reverse a))
  check "splitOn" withNeedle (\ (q, s) -> us (T.splitOn (pack q) (pack s))) (uncurry refSplitOn)
  check "count" withNeedle (\ (q, s) -> T.count (pack q) (pack s)) (\ (q, s) -> L.length (refSplitOn q s) - 1)
  check "replace" withNeedle (\ (q, s) -> u (T.replace (pack q) (pack "<\128512>") (pack s)))
                             (\ (q, s) -> L.intercalate "<\128512>" (refSplitOn q s))
  check "elem" [ (c, s) | c <- chars, s <- inputs ] (\ (c, s) -> T.elem c (pack s)) (uncurry L.elem)

  check "lines" inputs (us . T.lines . pack) L.lines
  check "words" inputs (us . T.words . pack) L.words
  check "unlines" lists (u . T.unlines . L.map pack) L.unlines
  check "unwords" lists (u . T.unwords . L.map pack) L.unwords
  check "concat" lists (u . T.concat . L.map pack) L.concat
  check "intercalate" lists (u . T.intercalate (pack ", ") . L.map pack) (L.intercalate ", ")
  check "concatMap" inputs (u . T.concatMap (\ c -> pack (if c == 'a' then "<a>" else [c])) . pack)
                           (L.concatMap (\ c -> if c == 'a' then "<a>" else [c]))
  check "replicate" [ (n, s) | n <- [-1 .. 5], s <- L.take 50 inputs ] (\ (n, s) -> u (T.replicate n (pack s)))
                                                                         (\ (n, s) -> L.concat (L.replicate n s))
  check "strip" inputs (u . T.strip . pack) (L.dropWhileEnd C.isSpace . L.dropWhile C.isSpace)
  check "stripStart" inputs (u . T.stripStart . pack) (L.dropWhile C.isSpace)
  check "stripEnd" inputs (u . T.stripEnd . pack) (L.dropWhileEnd C.isSpace)
  check "isSpace" ['\0' .. '\x3100'] (T.null . T.strip . T.singleton) C.isSpace

  check "takeWhile" withPred (\ (i, s) -> u (T.takeWhile (p i) (pack s))) (\ (i, s) -> L.takeWhile (p i) s)
  check "dropWhile" withPred (\ (i, s) -> u (T.dropWhile (p i) (pack s))) (\ (i, s) -> L.dropWhile (p i) s)
  check "takeWhileEnd" withPred (\ (i, s) -> u (T.takeWhileEnd (p i) (pack s)))
                                (\ (i, s) -> L.reverse (L.takeWhile (p i) (L.reverse s)))
  check "dropWhileEnd" withPred (\ (i, s) -> u (T.dropWhileEnd (p i) (pack s))) (\ (i, s) -> L.dropWhileEnd (p i) s)
  check "dropAround" withPred (\ (i, s) -> u (T.dropAround (p i) (pack s)))
                              (\ (i, s) -> L.dropWhile (p i) (L.dropWhileEnd (p i) s))
  check "span" withPred (\ (i, s) -> case T.span (p i) (pack s) of (a, b) -> (u a, u b)) (\ (i, s) -> L.span (p i) s)
  check "break" withPred (\ (i, s) -> case T.break (p i) (pack s) of (a, b) -> (u a, u b)) (\ (i, s) -> L.break (p i) s)
  check "all" withPred (\ (i, s) -> T.all (p i) (pack s)) (\ (i, s) -> L.all (p i) s)
  check "any" withPred (\ (i, s) -> T.any (p i) (pack s)) (\ (i, s) -> L.any (p i) s)
  check "find" withPred (\ (i, s) -> T.find (p i) (pack s)) (\ (i, s) -> L.find (p i) s)
  check "findIndex" withPred (\ (i, s) -> T.findIndex (p i) (pack s)) (\ (i, s) -> L.findIndex (p i) s)
  check "split" withPred (\ (i, s) -> us (T.split (p i) (pack s))) (\ (i, s) -> refSplit (p i) s)
  check "group" (inputs ++ longInputs) (us . T.group . pack) L.group
  check "groupBy" (inputs ++ longInputs) (us . T.groupBy (<=) . pack) (L.groupBy (<=))
  check "words (long)" longInputs (us . T.words . pack) L.words
  check "lines (long)" longInputs (us . T.lines . pack) L.lines

  check "toLower" inputs (u . T.toLower . pack) (L.concatMap U._toLowers)
  check "toUpper" inputs (u . T.toUpper . pack) (L.concatMap U._toUppers)
  check "toCaseFold" inputs (u . T.toCaseFold . pack) (L.concatMap U._toFolds)
  check "toLower ASCII" ["Hello, World! 123", "ABCXYZ@[`{", ""] (u . T.toLower . pack) (L.map C.toLower)
  check "toUpper ASCII" ["Hello, World! 123", "abcxyz@[`{", ""] (u . T.toUpper . pack) (L.map C.toUpper)
  check "justifyLeft" withInt (\ (n, s) -> u (T.justifyLeft n '\128512' (pack s)))
                              (\ (n, s) -> s ++ L.replicate (n - L.length s) '\128512')
  check "justifyRight" withInt (\ (n, s) -> u (T.justifyRight n '.' (pack s)))
                               (\ (n, s) -> L.replicate (n - L.length s) '.' ++ s)

  -- A large text, to make sure nothing is quadratic.
  let big = T.replicate 20000 (pack "word \128512 line\n")
  print (T.length big, L.length (T.lines big), L.length (T.words big), T.count (pack "\128512") big)
  print (T.length (T.concat (T.splitOn (pack "line") big)), T.length (T.reverse big))

  -- NUL is C0 80 in a Text, also when it comes from decodeUtf8 (00).
  let z = decodeUtf8 (B.pack [0x61, 0, 0x41])
  print (z == pack "a\0A", T.elem '\0' z, T.count (pack "\0") z, T.toLower z == pack "a\0a")
  print (B.unpack (encodeUtf8 z), B.unpack (encodeUtf8 (pack "\0\128512")))

  -- Joining must finish while another thread is runnable (the runtime
  -- primitive is only given evaluated lists, so it cannot be preempted).
  _ <- forkIO (mapM_ return (L.repeat ()))
  print (T.length (T.concat (L.replicate 100000 (pack "ab"))))
  print (L.length (T.lines (T.unlines (L.replicate 60000 (pack "x")))))
