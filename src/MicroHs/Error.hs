module MicroHs.Error(
  mhsError,
  reportErrors
  ) where
import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

newtype MhsError = MhsError String
instance Show MhsError where
  show (MhsError s) = s
instance Exception MhsError

mhsError :: String -> a
mhsError s = throw (MhsError s)

-- Catch errors in the program being compiled, print them nicely, and exit with failure.
reportErrors :: IO () -> IO ()
reportErrors io = catch io $ \ (MhsError msg) -> do
  -- Colors when stderr is a terminal, unless NO_COLOR is set.
  tty <- hIsTerminalDevice stderr
  noColor <- lookupEnv "NO_COLOR"
  let color = tty && isNothing noColor
      sgr code str = if color then "\ESC[" ++ code ++ "m" ++ str ++ "\ESC[0m" else str
      bold = sgr "1"
      red  = sgr "1;31"
      blue = sgr "34"
      green = sgr "32"
      -- "file:line:col: error: headline" with the parts highlighted
      headline locS hd =
        case stripPrefix locS hd of
          Just (' ' : 'e' : 'r' : 'r' : 'o' : 'r' : ':' : rest) -> bold locS ++ " " ++ red "error:" ++ rest
          _                                                    -> hd
      detail ln | "    expected:" `isPrefixOf` ln = "    expected:" ++ green (drop 13 ln)
                | "    actual:" `isPrefixOf` ln   = "    actual:" ++ red (drop 11 ln)
                | otherwise = ln
  case parseErrorLoc msg of
    Nothing -> hPutStrLn stderr $ "mhs: " ++ msg
    Just (fn, l, c) -> do
      case lines msg of
        [] -> return ()
        (hd : rest) -> do
          hPutStrLn stderr $ headline (fn ++ ":" ++ show l ++ ":" ++ show c ++ ":") hd
          mapM_ (hPutStrLn stderr . detail) rest
      mls <- catch (Just . lines <$> readFile fn) (\ (_ :: SomeException) -> return Nothing)
      case mls of
        Just ls | l >= 1 && l <= length ls -> do
          let src = ls !! (l - 1)
              num = show l
              pad = map (const ' ') num
          hPutStrLn stderr $ blue (pad ++ " |")
          hPutStrLn stderr $ blue (num ++ " | ") ++ src
          hPutStrLn stderr $ blue (pad ++ " | ") ++ caretPad src c ++ red "^"
        _ -> return ()
  exitWith (ExitFailure 1)

-- Whitespace that puts the caret under column c of the source line.
-- Tabs are kept, so they expand the same way as in the line above;
-- the lexer counts a tab as advancing to the next multiple of 8.
caretPad :: String -> Int -> String
caretPad src c = go 1 src
  where
    go col _ | col >= c = ""
    go col ('\t' : r) = '\t' : go (((col + 7) `quot` 8) * 8 + 1) r
    go col (_ : r)    = ' ' : go (col + 1) r
    go col []         = replicate (c - col) ' '

-- Recognize a leading "file:line:col:" in an error message.
-- The file name may itself contain colons, e.g., C:\src\M.hs.
parseErrorLoc :: String -> Maybe (FilePath, Int, Int)
parseErrorLoc = go []
  where
    go acc@(_:_) (':' : r1) | (ds1@(_:_), ':' : r2) <- span isDigit r1
                            , (ds2@(_:_), ':' : _ ) <- span isDigit r2 = Just (reverse acc, read ds1, read ds2)
    go acc (c : r) | c /= '\n' = go (c : acc) r
    go _ _ = Nothing

