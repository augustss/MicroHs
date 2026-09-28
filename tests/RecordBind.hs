-- A record pattern that cannot fail in a do bind does not need MonadFail.
module RecordBind(main) where

-- A monad without a MonadFail instance
newtype M a = M a
instance Functor M where
  fmap f (M a) = M (f a)
instance Applicative M where
  pure = M
  M f <*> M a = M (f a)
instance Monad M where
  M a >>= f = f a

runM :: M a -> a
runM (M a) = a

data R = R { x :: Int, y :: Int }

sumR :: M R -> M Int
sumR mr = do
  R { x = a, y = b } <- mr
  return (a + b)

sumPun :: M R -> M Int
sumPun mr = do
  R { x, y } <- mr
  return (x + y)

sumWild :: M R -> M Int
sumWild mr = do
  R { .. } <- mr
  return (x + y)

main :: IO ()
main = do
  print (runM (sumR (M (R 1 2))))
  print (runM (sumPun (M (R 3 4))))
  print (runM (sumWild (M (R 5 6))))
