module QualDoModule where

data Result a e = Ok a | Err e deriving (Show)

(>>) :: Result a e -> Result b e -> Result b e
Ok _ >> r = r
Err e >> _ = Err e

(>>=) :: Result a e -> (a -> Result b e) -> Result b e
Ok a >>= f = f a
Err e >>= _ = Err e

fail :: String -> Result a String
fail msg = Err msg
