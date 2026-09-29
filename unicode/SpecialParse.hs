--module UniParse where
import Data.Bits
import Data.Char
import Data.Maybe
import Numeric
import System.Environment
import System.IO

specialData, foldData :: FilePath
specialData = "SpecialCasing.txt"
foldData = "caseFolding.txt"

main :: IO ()
main = do
  args <- getArgs
  let (specfilename, foldfilename) =
        case args of
          [] -> (specialData, foldData)
          [s1, s2] -> (s1, s2)
          _ -> error "usage: SpecialParse [FILE FILE]"
  specfile <- readFile specfilename
  let info = catMaybes $ map parseSpec $ dropEmpty $ map dropComment $ lines specfile
      dropEmpty = filter (any (not . isSpace))
      dropComment = takeWhile (/= '#')
      (lowers, titles, uppers) = bucket info
      lowers' = filter (keep toLower) (reverse lowers)
      titles' = filter (keep toTitle) (reverse titles)
      uppers' = filter (keep toUpper) (reverse uppers)

  putStrLn $ printTable "Lower" lowers'
  putStrLn $ printTable "Title" titles'
  putStrLn $ printTable "Upper" uppers'

  foldFile <- readFile foldfilename
  let foldInfo = catMaybes $ map parseFold $ dropEmpty $ map dropComment $ lines foldFile
      folds = filter keepFold foldInfo
  putStrLn $ printFold folds

parseSpec :: String -> Maybe (Char, [Char], [Char], [Char])
parseSpec = decode . splitBy ';'
  where decode [ code, lower, title, upper, spc ] | all isSpace spc = Just
          (readHex' code, map readHex' (words lower), map readHex' (words title), map readHex' (words upper))
        decode _ = Nothing

type Table = [(Char, [Char])]

bucket :: [(Char, [Char], [Char], [Char])] -> (Table, Table, Table)
bucket = foldr f ([], [], [])
  where f (c, l, t, u) (ls, ts, us) = (add c l ls, add c t ts, add c u us)
        add c cs tbl | cs == [c] = tbl
                     | otherwise = (c, cs) : tbl

keep :: (Char -> Char) -> (Char, [Char]) -> Bool
keep f (c, cs) = [f c] /= cs

printTable :: String -> Table -> String
printTable s tbl = unlines $
 [ "_to" ++ s ++ "s :: Char -> [Char]",
   "_to" ++ s ++ "s c = case c of "] ++
 map ch tbl ++
 [ "  _ -> [ to" ++ s ++ " c]"]
 where ch (c, cs) = "  " ++ show c ++ " -> " ++ show cs

parseFold :: String -> Maybe (Char, [Char])
parseFold = decode . splitBy ';'
  where decode [ code, _status, mapping, spc ] | all isSpace spc = Just
          (readHex' code, map readHex' (words mapping))
        decode _ = Nothing

keepFold :: (Char, [Char]) -> Bool
keepFold (c, cs) = [toLower c] /= cs

printFold :: [(Char, [Char])] -> String
printFold fs = unlines $
  [ "_toFolds :: Char -> [Char]",
    "_toFolds c = case c of" ] ++
  map ch fs ++
  [ "  _ -> [toLower c]" ]
 where ch (c, cs) = "  " ++ show c ++ " -> " ++ show cs

readHex' :: String -> Char
readHex' s =
  case readHex s of
    [(i, "")] -> toEnum i
    _ -> error $ "readHex': " ++ show s

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep = loop []
  where loop r [] = [reverse r]
        loop r (c:cs) | c == sep  = reverse r : loop [] cs
                      | otherwise = loop (c:r) cs
