-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Double(module Data.Double) where
import Primitives
import Data.Bool_Type

--Yinfixl 6 +,-
--Yinfixl 7 *

-- Arithmetic
(+) :: Double -> Double -> Double
(+)  = primDoubleAdd
(-) :: Double -> Double -> Double
(-)  = primDoubleSub
(*) :: Double -> Double -> Double
(*)  = primDoubleMul

negate :: Double -> Double
negate x = 0.0 - x

addDouble :: Double -> Double -> Double
addDouble = (+)
subDouble :: Double -> Double -> Double
subDouble = (-)
mulDouble :: Double -> Double -> Double
mulDouble = (*)

--------------------------------

--Yinfix 4 ==,/=,<,<=,>,>=

-- Comparison
(==) :: Double -> Double -> Bool
(==) = primDoubleEQ
(/=) :: Double -> Double -> Bool
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

showDouble :: Double -> String
showDouble = primDoubleShow

--------------------------------
