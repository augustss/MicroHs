module Error14() where
infixl 5 +++
infixr 5 ***
(+++) :: Int -> Int -> Int
(+++) = (+)
(***) :: Int -> Int -> Int
(***) = (*)
x :: Int
x = 1 +++ 2 *** 3
