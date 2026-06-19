module Interpolate where

-- These are used to test qualified interpolation
interpolateRaw :: String -> String
interpolateRaw = id
interpolateValue :: Show a => a -> String
interpolateValue = show
interpolateAppend :: String -> String -> String
interpolateAppend = (++)
interpolateEmpty :: String
interpolateEmpty = ""
interpolateFinalize :: String -> String
interpolateFinalize = id
--

x :: Int
x = 33

y :: String
y = "Hello"

main :: IO ()
main = do
  putStrLn s"x=${x}"
  putStrLn s"y=${y}"
  putStrLn s"x+1=${x+1}"
  putStrLn s"y+!=${y ++ "!"}-end"
  putStrLn s"y+{=${y ++ "{"}-end"
  putStrLn s"y+\"=${y ++ "\""}-end"
  putStrLn s"x=${x}, y=${y} end"
  putStrLn s"a${ 1 + {- some {-nested-} comment -} 2}b"
  putStrLn s"${ let { x = 21 } in x+x }"
  putStr   s"""
    This is a multiline string.
    x=${x}
    Third line.
    """
  putStrLn s"a '${ y ++ s" x=${x}" ++ " end" }' b"
  putStrLn Interpolate.s"x=${x}"
  putStrLn Interpolate.s"y=${y}"
