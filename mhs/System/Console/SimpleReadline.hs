-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
--
-- Simple readline with line editing and history.
-- Only assumes the terminal is capable of (sane) backspace.
module System.Console.SimpleReadline(
  getInputLine,
  getInputLineHist,
  getInputLineHistComp,
  ) where
import qualified Prelude(); import MiniPrelude
import Control.Monad
import Data.Char
import Data.List(isPrefixOf, tails)
import System.IO

foreign import ccall "GETRAW" c_getRaw :: IO Int


-- Get an input line with editing.
-- Return Nothing if the input is ^D, otherwise the typed string.
getInputLine :: String -> IO (Maybe String)
getInputLine prompt = do
  (_, r) <- loop (\ _ -> return []) emptyHist "" ""
  return r

getInputLineHist :: FilePath -> String -> IO (Maybe String)
getInputLineHist = getInputLineHistComp (\ _ -> return [])

type CompleteFn = (String, String) -> IO [String]

-- Get an input line with editing.
-- Return Nothing if the input is ^D, otherwise the typed string.
-- The FilePath gives the name of a file that stores the history.
getInputLineHistComp :: CompleteFn -> FilePath -> String -> IO (Maybe String)
getInputLineHistComp comp hfn prompt = do
  mhdl <- openFileM hfn ReadMode
  hist <-
    case mhdl of
      Nothing -> return []
      Just hdl -> do
        file <- hGetContents hdl
        let h = lines file
        seq (length h) (return h)   -- force file to be read
  putStr prompt
  (hist', r) <- loop comp (fromLines hist) "" ""
--  putStrLn $ "done: " ++ hfn ++ "\n" ++ unlines hist'
  writeFile hfn $ unlines hist'
  return r   -- XXX no type error

getRaw :: IO Char
getRaw = do
  i <- c_getRaw
  when (i < 0) $
    error "getRaw failed"
  return (chr i)

-- The history.
-- before: the inputs before the current input
-- after: the inputs after the current input, including the current input (if non-empty)
-- original: the original input before using the history, if the history was already used
data Hist = Hist {- before -} [String] {- after -} [String] {- original -} (Maybe String)

emptyHist :: Hist
emptyHist = Hist [] [] Nothing

fromLines :: [String] -> Hist
fromLines hist = Hist (reverse hist) [] Nothing

toLines :: Hist -> [String]
toLines (Hist before after _) = reverse before ++ after

-- Move one step back in the history, given the current line.
-- Returns the new history and the line to show.
histPrev :: String -> Hist -> Maybe (Hist, String)
histPrev _   (Hist [] _ _) = Nothing
histPrev cur (Hist (l:p) [] Nothing) = Just (Hist p [l] (Just cur), l)
histPrev _   (Hist (l:p) n orig) = Just (Hist p (l:n) orig, l)

-- Search backwards in the history, starting with the current line, for a line
-- that contains the pattern and is not equal to the line to skip.
-- Returns the new history, the found line, and the position of the match.
histSearch :: Maybe String -> String -> String -> Hist -> Maybe (Hist, String, Int)
histSearch skip pat line hist =
  case [ i | (i, t) <- zip [0..] (tails line), pat `isPrefixOf` t ] of
    is@(_:_) | Just line /= skip -> Just (hist, line, last is)
    _ -> histPrev line hist >>= \ (hist', line') -> histSearch skip pat line' hist'

back :: Int -> IO ()
back n = putStr (replicate n '\b')

bsSpBs :: Int -> String
bsSpBs n = concat $ replicate n "\b \b"

-- Decode a UTF-8 encoded character, given the first byte.
utf8 :: Int -> IO (Maybe Char)
utf8 i | i `quot` 32 ==  6 = utf8' 1 (i `rem` 32)
       | i `quot` 16 == 14 = utf8' 2 (i `rem` 16)
       | i `quot`  8 == 30 = utf8' 3 (i `rem`  8)
       | otherwise         = return Nothing
  where
    utf8' :: Int -> Int -> IO (Maybe Char)
    utf8' 0 c = return (Just (chr c))
    utf8' n c = do
      d <- getRaw
      utf8' (n - 1) (c * 64 + ord d `rem` 64)

loop :: CompleteFn -> Hist -> String -> String -> IO ([String], Maybe String)
loop comp hist before after = do
  hFlush stdout
  i <- getRaw
  loop' comp hist before after i

loop' :: CompleteFn -> Hist -> String -> String -> Char -> IO ([String], Maybe String)
loop' comp hist before after cmd = do
  let
    cur = reverse before ++ after

    ins c = do
      putChar c
      putStr after
      back (length after)
    add c = do
      ins c
      loop comp hist (c:before) after
    backward =
      case before of
        [] -> noop
        c:cs -> do
          back 1
          loop comp hist cs (c:after)
    forward =
      case after of
        [] -> noop
        c:cs -> do
          putChar c
          loop comp hist (c:before) cs
    bol = do
      back (length before)
      loop comp hist [] cur
    eol = do
      putStr after
      loop comp hist (reverse after ++ before) []
    bs = do
      case before of
        [] -> noop
        _:cs -> do
          back 1
          putStr after
          putChar ' '
          back (length after + 1)
          loop comp hist cs after
    del = do
      case after of
        [] -> noop
        _:cs -> do
          putStr cs
          putChar ' '
          back (length cs + 1)
          loop comp hist before cs
    send =
      ret (Just cur)
    ret ms = do
      putChar '\n'
      hFlush stdout
      let
        o = toLines hist
        l =
          case ms of
            Nothing -> []
            Just [] -> []
            Just s  | not (null o) && s == last o -> []
                    | otherwise -> [s]
        h = o ++ l
      return (h, ms)
    erase = do
      eraseLine
      loop comp hist [] []
    noop = loop comp hist before after
    kill = do
      putStr after
      putStr $ bsSpBs $ length after
      loop comp hist before []

    next =
      case hist of
        Hist _ [] Nothing -> noop
        Hist p [] (Just orig) -> setLine (Hist p [] Nothing) orig
        Hist _ [_] Nothing -> noop
        Hist p [l] (Just orig) -> setLine (Hist (l:p) [] Nothing) orig
        Hist p (c:l:n) orig -> setLine (Hist (c:p) (l:n) orig) l
    previous =
      case histPrev cur hist of
        Nothing -> noop
        Just (h, l) -> setLine h l
    setLine h s = do
      eraseLine
      putStr s
      loop comp h (reverse s) ""

    eraseLine = do
      putStr after
      putStr $ bsSpBs $ length before + length after

    complete = do
      alts <- comp (before, after)
      case alts of
        []  -> loop comp hist before after
        [s] -> do mapM_ ins s; loop comp hist (reverse s ++ before) after
        ss  -> tabLoop ss

    tabLoop (s:ss) = do
      mapM_ ins s           -- show first alternative
      hFlush stdout
      c <- getRaw
      if c /= '\t' then
        loop' comp hist (reverse s ++ before) after c
       else do
        let n = length s
        back n                    -- back up this alternative
        putStr after              -- put back old text
        putStr $ replicate n ' '  -- erase extra
        back (n + length after)   -- put cursor back
        tabLoop (ss ++ [s])       -- try next alternative

    search = do
      eraseLine
      let s = Search "" False hist cur (length before)
      drawSearch s
      isearch comp (hist, before, after) [s]

    exec i =
      case i of
        '\^D' ->                     -- CTL-D, EOF
          if null before && null after then
            ret Nothing
          else
            del
        '\^B'  -> backward           -- CTL-B, backwards
        '\^F'  -> forward            -- CTL-F, forwards
        '\^A'  -> bol                -- CTL-A, beginning of line
        '\^E'  -> eol                -- CTL-E, end of line
        '\b'   -> bs                 -- BS, backspace
        '\DEL' -> bs                 -- DEL, backspace
        '\r'   -> send               -- CR, return
        '\n'   -> send               -- LF, return
        '\^N'  -> next               -- CTL-N, next line
        '\^P'  -> previous           -- CTL-P, previous line
        '\^U'  -> erase              -- CTL-U, erase line
        '\^K'  -> kill               -- CTL-K, kill to eol
        '\t'   -> complete           -- TAB, complete word
        '\^R'  -> search             -- CTL-R, reverse incremental search
        '\ESC' -> do                 -- ESC
          b <- getRaw
          if b /= '[' then
            noop
           else do
            c <- getRaw
            case c of
              'A' -> previous
              'B' -> next
              'C' -> forward
              'D' -> backward
              'F' -> eol
              'H' -> bol
              _ | isDigit c -> do
                    d <- getRaw
                    case (c,d) of
                      ('1','~') -> bol
                      ('3','~') -> del
                      ('4','~') -> eol
                      _         -> noop
              _   -> noop
        _ | ord i >= 128 -> utf8 (ord i) >>= maybe noop add
        _ -> if i >= ' ' && i < '\DEL' then add i else noop

  exec cmd

-- The state of an incremental search:
-- the pattern, if the search failed, the history, the found line, and the position of the match.
data Search = Search String Bool Hist String Int

-- The text shown for a search, and the cursor position within it.
showSearch :: Search -> (String, Int)
showSearch (Search pat failed _ line pos) = (s, length s - length line + pos)
  where s = (if failed then "(failed " else "(") ++ "reverse-i-search)`" ++ pat ++ "': " ++ line

drawSearch :: Search -> IO ()
drawSearch s = do
  let (str, k) = showSearch s
  putStr str
  back (length str - k)

eraseSearch :: Search -> IO ()
eraseSearch s = do
  let (str, k) = showSearch s
  putStr (drop k str)
  putStr $ bsSpBs $ length str

-- Incremental reverse search in the history.
-- The second argument is the state before the search started, used if the search is aborted.
-- The third argument is a stack of search states, the head is the current one.
-- Backspace pops the stack, i.e., undoes the last step.
isearch :: CompleteFn -> (Hist, String, String) -> [Search] -> IO ([String], Maybe String)
isearch comp o@(ohist, obefore, oafter) ss@(s@(Search pat failed hist line pos) : ps) = do
  let
    noop = isearch comp o ss
    redraw s' ss' = do
      eraseSearch s
      drawSearch s'
      isearch comp o ss'
    push s' = redraw s' (s' : ss)
    found p (Just (h, l, i)) = Search p False h l i
    found p Nothing = Search p True hist line pos
    add c
      | failed    = push (Search pat' True hist line pos)
      | otherwise = push (found pat' (histSearch Nothing pat' line hist))
      where pat' = pat ++ [c]
    again = push (found pat (histSearch (Just line) pat line hist))
    pop =
      case ps of
        [] -> noop
        s' : _ -> redraw s' ps
    abort = do
      eraseSearch s
      putStr (reverse obefore ++ oafter)
      back (length oafter)
      loop comp ohist obefore oafter
    accept c = do
      eraseSearch s
      putStr line
      back (length line - pos)
      loop' comp hist (reverse (take pos line)) (drop pos line) c
  hFlush stdout
  i <- getRaw
  case i of
    '\^R'  -> again                  -- CTL-R, search for next match
    '\^G'  -> abort                  -- CTL-G, abort search
    '\b'   -> pop                    -- BS, undo last step
    '\DEL' -> pop                    -- DEL, undo last step
    _ | ord i >= 128 -> utf8 (ord i) >>= maybe noop add
      | i >= ' ' && i < '\DEL' -> add i
      | otherwise -> accept i        -- other keys end the search and are then executed
