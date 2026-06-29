module Test.Main
  ( main
  ) where

import Prelude

import Data.Array ((..), foldl)
import Data.Foldable (traverse_)
import Data.List.Lazy (List, findIndex, fromFoldable, repeat, scanl, take, takeWhile)
import Data.Maybe (Maybe)
import Data.Monoid.Endo (Endo)
import Data.Newtype (unwrap)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Console (log)

class Ring a <= NormedRing a where
  norm :: a -> Number

class PolymorphicAction actor target result | actor target -> result where
  act :: actor -> target -> result

instance actEndoAny :: PolymorphicAction (Endo (->) a) a a where
  act = unwrap

else instance actListEndoAny :: PolymorphicAction (List (Endo (->) a)) a (List a) where
  act fs z = scanl (\acc f -> act f acc) z fs

else instance actListTarget ::
  ( PolymorphicAction actor target result
  ) =>
  PolymorphicAction actor (List target) (List result) where
  act = map <<< act

class PolymorphicActionE actor target result element | actor target -> result element where
  actE :: (element -> Boolean) -> actor -> target -> result

instance actEListEndoNormed :: NormedRing a => PolymorphicActionE (List (Endo (->) a)) a (List a) a where
  actE isOk fs z = takeWhile isOk (act fs z)

else instance actEListTarget ::
  ( PolymorphicActionE actor target result element
  ) =>
  PolymorphicActionE actor (List target) (List result) element where
  actE isOk fs zs = (actE isOk fs) <$> zs

escapeTime
  :: forall actor target
   . (PolymorphicAction (List actor) target (List target))
  => (target -> Boolean)
  -> List actor
  -> target
  -> Maybe Int
escapeTime isOk fs z =
  findIndex (not <<< isOk) (act fs z)

data Complex = Complex Number Number

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

type EndoComplex = Endo (->) Complex

-- [Pixel] ->(w/ Screen and Lenz) [Real] ->(w/ actions) [EscapeTime] -> [Color]
type Pixel = Int
type Real = Number
type EndoReal = Endo (->) Real
type EscapeTime = Int
type Color =
  { r :: Int
  , g :: Int
  , b :: Int
  }

main :: Effect Unit
main = do
  let
    pixels :: List Pixel
    pixels = fromFoldable (0 .. 100)
  traverse_ (log <<< show) pixels