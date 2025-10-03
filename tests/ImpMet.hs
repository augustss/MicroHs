module ImpMet where
import Prelude(IO, putStrLn)

-- check that these symbols are available without import
ls :: [] ()
ls = () : []

main :: IO ()
main = putStrLn "ok"
