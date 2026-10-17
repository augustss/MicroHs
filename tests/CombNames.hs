module CombNames(main) where
-- --comb-names writes the name of every label of the program to
-- out.comb.names, one "label name" per line, in label order.

fac :: Integer -> Integer
fac n = if n <= 1 then 1 else n * fac (n - 1)

main :: IO ()
main = print (fac 20)
