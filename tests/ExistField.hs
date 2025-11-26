{-# LANGUAGE RecordWildCards #-}
module ExistField(main) where

data T = forall a . (Show a, Eq a) =>
         T { a :: a
           , b :: a }

instance Show T where
  show (T a b) = "T " ++ show a ++ " " ++ show b

myTs :: [T]
myTs = [ T  42 42
       , T (Just 42) Nothing
       , T 'c' 'd'
       , T  13 42
       ]

patTest :: [T] -> IO ()
patTest [] = pure ()
patTest (x:xs)
   | T {..} <- x
   , a == b = do
       putStrLn $ "These are equal: " ++ show (a,b)
       patTest xs
   | T {a, b } <- x
   , a /= b = do
       putStrLn $ "These are not: " ++ show (a,b)
       patTest xs

selTest :: [T] -> IO ()
selTest [] = pure ()
selTest ((T y z):xs) = do
  print (y,z)
  selTest xs

myT :: T
myT = T (Just 12) Nothing

updateT :: T -> T
updateT (T a' b') = myT {a = Nothing, b = Just 14}

data TA = forall a b . (Show a, Eq a, Show b, Eq b) =>
         T1 { a1 :: a
            , b1 :: b
            , c1 :: String }
        | forall a . Show a =>
          T2 { a2 :: a
             , b2 :: String }
        | T3 String

myT1 :: TA
myT1 = T1 (Just 1) 11 "my string"

main :: IO ()
main = do
  patTest myTs
  selTest myTs
  print myT
  print $ updateT myT
  case (myT1 {c1 = "updated"}) of
    T1 a b c -> print (a,b,c)
  case myT1 {a1 = "updated"} of
    T1 a b c -> print (a,b,c)
  case (myT1 {b1 = "updated"}) of
    T1 a b c -> print (a,b,c)
  let a = case myT1 of
            T1 a b c -> T1 "updated" "updated" c
  case a of
    T1 a' b' c' -> print (a',b',c')

  case T2 11 "a string" of
    t -> case t {a2 = "updated"} of
           T2 a' b' -> print (a',b')
