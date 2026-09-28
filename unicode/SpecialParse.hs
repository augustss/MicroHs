--module UniParse where
import Data.Bits
import Data.Char
import Data.Maybe
import Numeric
import System.Environment
import System.IO

specialData :: FilePath
specialData = "SpecialCasing.txt"

main :: IO ()
main = do
  args <- getArgs
  let unifilename =
        case args of
          [] -> specialData
          [s] -> s
          _ -> error "usage: SpecialParse [File]"
  specfile <- readFile unifilename
  let info = catMaybes $ map parseOne $ dropEmpty $ map dropComment $ lines specfile
      dropEmpty = filter (any (not . isSpace))
      dropComment = takeWhile (/= '#')
      (lowers, titles, uppers) = bucket info
      lowers' = filter (keep toLower) (reverse lowers)
      titles' = filter (keep toTitle) (reverse titles)
      uppers' = filter (keep toUpper) (reverse uppers)
  putStrLn $ printTable "Lower" lowers'
  putStrLn $ printTable "Title" titles'
  putStrLn $ printTable "Upper" uppers'

parseOne :: String -> Maybe (Char, [Char], [Char], [Char])
parseOne = decode . splitBy ';'
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

