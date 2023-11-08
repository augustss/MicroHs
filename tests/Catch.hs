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
  x <- catch (return ("o" ++ "k")) (\ _ -> return "what?")
  putStrLn $ show x
  y <- catch (do { error "bang!"; return "yyy" }) (\ (Exn s) -> return s)
  putStrLn $ show y
  z <- catch (do { print (f []); return "zzz" })  (\ (Exn s) -> return s)
  putStrLn $ show z
  w <- catch (do { print (m ()); return "www" })  (\ (Exn s) -> return s)
  putStrLn $ show w
