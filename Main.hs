import System.Environment(getArgs)
import Parse
import Desugar
import Exp

main :: IO ()
main = do
  [fn] <- getArgs
  mdl <- parseDie pTop fn <$> readFile fn
  let ds = desugar mdl
      ds' = [ (i, improve $ compile e) | (i, e) <- ds ]
  mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toString e) ds
  mapM_ (\ (i, e) -> putStrLn $  i ++ " = " ++ toString e) ds'

