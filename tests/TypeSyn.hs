module TypeSyn where

type Foo a = a -> a -> Bool

foo :: Foo (forall b. b->b)
foo f g = f 'a' == g 'a' && f () == g ()

type Generic i o = forall x. i x -> o x
type Id x = x

bar :: Generic Id []
bar x = [x]

type S a = Int

a :: S ()
a = 1
b :: S Bool
b = 2

main :: IO ()
main = do
  print [a, b]
  print (foo id id)
