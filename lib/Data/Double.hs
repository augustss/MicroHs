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

--------------------------------

--Yinfix 4 ==,/=,<,<=,>,>=

-- Comparison
(==) :: Double -> Double -> Bool
(==) = primDoubleEQ
(/=) :: Double -> Double -> Bool
(/=) = primDoubleNE

(<)  :: Double -> Double -> Bool
(<)  = primDoubleLT
(<=) :: Double -> Double -> Bool
(<=) = primDoubleLE
(>)  :: Double -> Double -> Bool
(>)  = primDoubleGT
(>=) :: Double -> Double -> Bool
(>=) = primDoubleGE

eqDouble :: Double -> Double -> Bool
eqDouble = (==)

ltDouble :: Double -> Double -> Bool
ltDouble = (<)

showDouble :: Double -> String
showDouble = primDoubleShow

--------------------------------
