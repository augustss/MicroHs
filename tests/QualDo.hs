module QualDo where

import QualDoModule as Result

res1 :: Result Int String
res1 = Result.do
    a <- Ok 3
    b <- Ok 5
    Ok (a + b)

res2 :: Result Int String
res2 = Result.do
    12 <- Ok 42
    Ok 0

res3 :: Result Int String
res3 = Result.do
    Ok 1
    Err "2"
    Ok 3

main :: IO ()
main = do
    print res1
    print res2
    print res3
