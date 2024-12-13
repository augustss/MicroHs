module PatSynE(pattern Sing, pattern Sings, pattern Swap) where

pattern Sing :: a -> [a]
pattern Sing a = [a]

pattern Sings a as <- as@[a]

pattern Swap :: a -> a -> [a]
pattern Swap a b = [b, a]

{-
pattern Dup :: Eq a => a -> [a]
pattern Dup a <- [(dup -> (Just a))]
  where Dup a = [a, a]

dup :: (Eq a) => [a] -> Maybe a
dup [x,x'] | x==x' = Just x
dup _ = Nothing
-}
