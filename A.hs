module A(module A) where
import Prelude

main = do
  putStrLn $ showList showInt $ map (+ 1) [2,3]
  putStrLn $ showList showInt $ map (1 +) [2,3]
  putStrLn $ showList showInt $ map (- 1) [2,3]
  putStrLn $ showList showInt $ map (1 -) [2,3]
  
