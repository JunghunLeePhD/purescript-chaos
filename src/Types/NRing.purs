module Types.NRing where

import Prelude
import Data.Number (sqrt)

class Ring a <= NormedRing a where
  norm :: a -> Number

type Real = Number
data Complex = Complex Real Real

derive instance eqComplex :: Eq Complex
instance showComplex :: Show Complex where
  show (Complex r i) = show r <> " + " <> show i <> "i"

instance semiringComplex :: Semiring Complex where
  add (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
  zero = Complex 0.0 0.0
  mul (Complex r1 i1) (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)
  one = Complex 1.0 0.0

instance ringComplex :: Ring Complex where
  sub (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)

instance normedRingComplex :: NormedRing Complex where
  norm (Complex r i) = sqrt (r * r + i * i)
