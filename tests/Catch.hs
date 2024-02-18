module Catch(main) where
import Prelude
import Control.Exception

f :: [Int] -> Int
f (_:_) = 99

class C a where
  m :: a -> Int

instance C ()

main :: IO ()
main = do
  let sshow :: String -> String
      sshow = show
  x <- catch (return ("o" ++ "k")) (\ _ -> return "what?")
  putStrLn $ sshow x
  y <- catch (do { error "bang!"; return "yyy" }) (\ (Exn s) -> return s)
  putStrLn $ sshow y
  z <- catch (do { print (f []); return "zzz" })  (\ (Exn s) -> return s)
  putStrLn $ sshow z
  w <- catch (do { print (m ()); return "www" })  (\ (Exn s) -> return s)
  putStrLn $ sshow w
