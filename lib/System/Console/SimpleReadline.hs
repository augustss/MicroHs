-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
--
-- Simple readline with line editing and history.
-- Only assumes the terminal is capable of (sane) backspace.
module System.Console.SimpleReadline(
  getInputLine,
  getInputLineHist
  ) where
import Prelude
import Control.Monad
import Data.Char
import System.IO
--Ximport Compat

foreign import ccall "getRaw" c_getRaw :: IO Int


-- Get an input line with editing.
-- Return Nothing if the input is ^D, otherwise the typed string.
getInputLine :: String -> IO (Maybe String)
getInputLine prompt = do
  (_, r) <- loop ([],[]) "" ""
  return r


-- Get an input line with editing.
-- Return Nothing if the input is ^D, otherwise the typed string.
-- The FilePath gives the name of a file that stores the history.
getInputLineHist :: FilePath -> String -> IO (Maybe String)
getInputLineHist hfn prompt = do
  mhdl <- openFileM hfn ReadMode
  hist <-
    case mhdl of
      Nothing -> return []
      Just hdl -> do
        file <- hGetContents hdl
        let h = lines file
        seq (length h) (return h)   -- force file to be read
  putStr prompt
  (hist', r) <- loop (reverse hist, []) "" ""
--  putStrLn $ "done: " ++ hfn ++ "\n" ++ unlines hist'
  writeFile hfn $ unlines hist'
  return r   -- XXX no type error

getRaw :: IO Int
getRaw = do
  i <- c_getRaw
  when (i < 0) $
    error "getRaw failed"
  return i

type Hist = ([String], [String])

loop :: Hist -> String -> String -> IO ([String], Maybe String)
loop hist before after = do
  hFlush stdout
  i <- getRaw
  let
    cur = reverse before ++ after
    back n = putStr (replicate n '\b')

    add c = do
      putChar c
      putStr after
      back (length after)
      loop hist (c:before) after
    backward =
      case before of
        [] -> noop
        c:cs -> do
          back 1
          loop hist cs (c:after)
    forward =
      case after of
        [] -> noop
        c:cs -> do
          putChar c
          loop hist (c:before) cs
    bol = do
      back (length before)
      loop hist "" (reverse before ++ after)
    eol = do
      putStr after
      loop hist (reverse after ++ before) ""
    bs = do
      case before of
        [] -> noop
        _:cs -> do
          back 1
          putStr after
          putChar ' '
          back (length after + 1)
          loop hist cs after
    del = do
      case after of
        [] -> noop
        _:cs -> do
          putStr cs
          putChar ' '
          back (length cs + 1)
          loop hist before cs
    send =
      ret (Just cur)
    ret ms = do
      putChar '\n'
      hFlush stdout
      let
        o = reverse (fst hist) ++ snd hist
        l =
          case ms of
            Nothing -> []
            Just "" -> []
            Just s  | not (null o) && s == last o -> []
                    | otherwise -> [s]
        h = o ++ l
      return (h, ms)
    erase = do
      eraseLine
      loop hist "" ""
    noop = loop hist before after
    kill = do
      putStr after
      putStr $ concat $ replicate (length after) "\b \b"
      loop hist before ""

    next =
      case hist of
        (_, []) -> noop
        (p, l:n) -> setLine (l:p, n) l
    previous =
      case hist of
        ([], _) -> noop
        (l:p, n) -> setLine (p, l:n) l
    setLine h s = do
      eraseLine
      putStr s
      loop h (reverse s) ""

    eraseLine = do
      putStr after
      putStr $ concat $ replicate (length before + length after) "\b \b"

  case i of
    4 ->                     -- CTL-D, EOF
      if null before && null after then
        ret Nothing
      else
        del
    2  -> backward           -- CTL-B, backwards
    6  -> forward            -- CTL-F, forwards
    1  -> bol                -- CTL-A, beginning of line
    5  -> eol                -- CTL-E, end of line
    8  -> bs                 -- BS, backspace
    127 -> bs                -- DEL, backspace
    13 -> send               -- CR, return
    10 -> send               -- LF, return
    14 -> next               -- CTL-N, next line
    15 -> previous           -- CTL-P, previous line
    21 -> erase              -- CTL-U, erase line
    11 -> kill               -- CTL-K, kill to eol
    27 -> do                 -- ESC
      b <- getRaw
      if b /= ord '[' then
        noop
       else do
        c <- getRaw
        case chr c of
          'A' -> previous
          'B' -> next
          'C' -> forward
          'D' -> backward
          _ -> noop
    _ -> if i >= 32 && i < 127 then add (chr i) else noop
