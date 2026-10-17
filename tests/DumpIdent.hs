module DumpIdent(main) where
-- -ddump-desugar quotes identifiers that contain parentheses, quotes or
-- white space, such as instance names, so that the dump can be parsed back.

data T a = T { fieldX :: a }

instance Show (T Int) where
  show (T x) = "T " ++ show x

main :: IO ()
main = do
  print (T (1 :: Int))
  print (fieldX (T 'a'))
