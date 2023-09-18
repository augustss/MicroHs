module System.Console.SimpleReadline(getInputLine) where
import Primitives
import Prelude

getInputLine :: String -> IO (Maybe String)
getInputLine prompt = do
  putStr prompt
  loop "" ""

getRaw :: IO Int
getRaw = do
  i <- primGetRaw
  when (i < 0) $
    error "getRaw failed"
  return i

loop :: String -> String -> IO (Maybe String)
loop before after = do
  hFlush stdout
  i <- getRaw
  let
    back n = putStr (replicate n '\b')

    add c = do
      putChar c
      putStr after
      back (length after)
      loop (c:before) after
    backward =
      case before of
        [] -> noop
        c:cs -> do
          back 1
          loop cs (c:after)
    forward =
      case after of
        [] -> noop
        c:cs -> do
          putChar c
          loop (c:before) cs
    bol = do
      back (length before)
      loop "" (reverse before ++ after)
    eol = do
      putStr after
      loop (before ++ reverse after) ""
    bs = do
      case before of
        [] -> noop
        _:cs -> do
          back 1
          putStr after
          putChar ' '
          back (length after + 1)
          loop cs after
    send = do
      putChar '\n'
      hFlush stdout
      return (Just (reverse before ++ after))
    noop = loop before after

  case i of
    4 -> do                  -- CTL-D, EOF
      let r = before ++ after
      return $ if null r then Nothing else Just r
    2 -> backward            -- CTL-B, backwards
    6 -> forward             -- CTL-F, forwards
    1 -> bol                 -- CTL-A, beginning of line
    5 -> eol                 -- CTL-E, end of line
    8 -> bs                  -- BS, backspace
    127 -> bs                -- DEL, backspace
    13 -> send               -- CR, return
    10 -> send               -- LF, return
    27 -> do                 -- ESC
      b <- getRaw
      if b /= ord '[' then
        noop
       else do
        c <- getRaw
        case chr c of
          'C' -> forward
          'D' -> backward
          _ -> noop
    _ -> if i >= 32 && i < 127 then add (chr i) else noop
