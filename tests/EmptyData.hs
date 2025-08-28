module EmptyData where

data Void

absurd1 :: Void -> a
absurd1 x = case x of {}

absurd2 :: Void -> a
absurd2 = \case {}

data Empty deriving (Eq, Ord, Read, Show)

main :: IO ()
main = do
    print $ (undefined :: Empty) == (undefined :: Empty)
    print $ compare (undefined :: Empty) (undefined :: Empty)
