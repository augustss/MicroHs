-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Double(module Data.Double, Double) where
import Primitives
import Data.Bool_Type
import Data.Eq
import Text.Show

infixl 6 +,-
infixl 7 *

-- Arithmetic
(+) :: Double -> Double -> Double
(+)  = primDoubleAdd
(-) :: Double -> Double -> Double
(-)  = primDoubleSub
(*) :: Double -> Double -> Double
(*)  = primDoubleMul
(/) :: Double -> Double -> Double
(/) = primDoubleDiv

negate :: Double -> Double
negate x = 0.0 - x

addDouble :: Double -> Double -> Double
addDouble = (+)
subDouble :: Double -> Double -> Double
subDouble = (-)
mulDouble :: Double -> Double -> Double
mulDouble = (*)
divDouble :: Double -> Double -> Double
divDouble = (/)

--------------------------------

--infix 4 ==,/=
infix 4 <,<=,>,>=

{-
-- Comparison
(==) :: Double -> Double -> Bool
(==) = primDoubleEQ
(/=) :: Double -> Double -> Bool
(/=) = primDoubleNE
-}

instance Eq Double where
  (==) = primDoubleEQ
  (/=) = primDoubleNE

eqDouble :: Double -> Double -> Bool
eqDouble = (==)
neqDouble :: Double -> Double -> Bool
neqDouble = (/=)

(<)  :: Double -> Double -> Bool
(<)  = primDoubleLT
(<=) :: Double -> Double -> Bool
(<=) = primDoubleLE
(>)  :: Double -> Double -> Bool
(>)  = primDoubleGT
(>=) :: Double -> Double -> Bool
(>=) = primDoubleGE

ltDouble :: Double -> Double -> Bool
ltDouble = (<)

leDouble :: Double -> Double -> Bool
leDouble = (<=)

gtDouble :: Double -> Double -> Bool
gtDouble = (>)

geDouble :: Double -> Double -> Bool
geDouble = (>=)

-- | this primitive will print doubles with up to 6 decimal points
-- it turns out that doubles are extremely tricky, and just printing them is a
-- herculean task of its own...
instance Show Double where
  show = primDoubleShow

--------------------------------
