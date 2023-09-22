module Catch(main) where
import Prelude
import Control.Exception

main :: IO ()
main = do
  x <- catch (return ("o" ++ "k")) (\ _ -> return "what?")
  putStrLn $ showString x
  y <- catch (do { error "bang!"; return "huh?" }) (\ (Exn s) -> return s)
  putStrLn $ showString y
