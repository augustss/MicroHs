{-# OPTIONS_GHC -Wno-orphans #-}
module MicroHs.Error(
  MhsError(..),
  mhsError,
  mhsErrorLoc,
  reportErrors,
  formatMhsError,
  errLoc,
  ) where
import qualified Prelude(); import MHSPrelude
import Data.Maybe
import MicroHs.Ident({-SLoc(..),-} showSLoc, isNoSLoc)
import System.Environment
import System.Exit
import System.IO

-- This belongs in MicroHs.Error, but Hugs cannot derive Tyoeable
-- and GHC doesn't allow explicit Typeable instances.
-- Instead it lives in {ghc,hugs,mhs}/MHSPrelude.hs
{-
data MhsError
  = MhsSyntaxError SLoc String String
  | MhsTypeError   SLoc String String String  -- header, expected, actual
  | MhsOtherError  SLoc String
  | MhsError            String                -- no location
-}

instance Show MhsError where
  show = formatMhsError False

instance Exception MhsError

errLoc :: MhsError -> Maybe SLoc
errLoc (MhsSyntaxError l _ _) = Just l
errLoc (MhsTypeError l _ _ _) = Just l
errLoc (MhsUndefError l _ _) = Just l
errLoc (MhsAmbigError l _ _ _) = Just l
errLoc (MhsOtherError l _) = Just l
errLoc (MhsError _) = Nothing

mhsError :: String -> a
mhsError s = throw (MhsError s)

mhsErrorLoc :: SLoc -> String -> a
mhsErrorLoc loc s = throw (MhsOtherError loc s)

formatMhsError :: Bool -> MhsError -> String
formatMhsError col err =
  case err of
    MhsError msg -> "mhs: " ++ redError ++ msg
    MhsOtherError loc msg -> locErr loc ++ msg
    MhsTypeError loc mode a e ->
      locErr loc ++ mode ++ " mismatch" ++
      "\n    expected: " ++ green col e ++
      "\n    actual:   " ++ red   col a
    MhsSyntaxError loc a e ->
      locErr loc ++ "syntax" ++
      "\n    found:    " ++ a ++
      "\n    expected: " ++ e
    MhsUndefError loc k i ->
      locErr loc ++ "undefined " ++ k ++ ": " ++ i
    MhsAmbigError loc k i alts ->
      locErr loc ++ "ambiguous " ++ k ++ ": " ++ i ++ " " ++ alts
  where
    locErr loc | isNoSLoc loc = redError
               | otherwise    = bold col (showSLoc loc) ++ ": " ++ redError
    redError = red col "error: "

sgr :: String -> Bool -> String -> String
sgr code col str = if col then "\ESC[" ++ code ++ "m" ++ str ++ "\ESC[0m" else str

bold, red, green, blue :: Bool -> String -> String
bold  = sgr "1"
red   = sgr "1;31"
blue  = sgr "34"
green = sgr "32"

fileSnippet :: Bool -> SLoc -> IO (Maybe String)
fileSnippet col (SLoc fn l c) = do
  mls <- catch (Just . lines <$> readFile fn) (\ (_ :: SomeException) -> return Nothing)
  case mls of
    Just ls | l >= 1 && l <= length ls -> do
      let src = ls !! (l - 1)
          num = show l
          pad = map (const ' ') num
      return $ Just $ unlines [ blue col (pad ++ " |")
                              , blue col (num ++ " | ") ++ src
                              , blue col (pad ++ " | ") ++ caretPad src c ++ red col "^"
                              ]
    _ -> return Nothing

-- Catch errors in the program being compiled, print them nicely, and exit with failure.
reportErrors :: IO () -> IO ()
reportErrors io = catch io $ \ err -> do
  -- Colors when stderr is a terminal, unless NO_COLOR is set.
  tty <- hIsTerminalDevice stderr
  noColor <- lookupEnv "NO_COLOR"
  let color = tty && isNothing noColor
      errMsg = formatMhsError color err
  msnippet <- maybe (return Nothing) (fileSnippet color) (errLoc err)
  hPutStrLn stderr errMsg
  maybe (return ()) (hPutStrLn stderr) msnippet

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
