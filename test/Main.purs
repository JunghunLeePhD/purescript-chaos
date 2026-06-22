module Test.Main
  ( main
  ) where

import Prelude

import Data.List.Lazy
  ( List
  , take
  , findIndex
  , repeat
  , scanl
  , fromFoldable
  , takeWhile
  )
import Data.Maybe (Maybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Console (log)

class Ring a <= NormedRing a where
  norm :: a -> Number

cyclic :: forall a. Endo (->) a -> List (Endo (->) a)
cyclic = repeat

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

quadratic :: Complex -> EndoComplex
quadratic c = Endo $ \z -> z * z + c

-- colorPixel :: Maybe Int -> String
-- colorPixel Nothing = "#000000"
-- colorPixel (Just n) = "hsl(" <> show (n * 5) <> ", 100%, 50%)"

main :: Effect Unit
main = do
  let
    base = quadratic (Complex 0.0 0.0)
    -- endos = base
    endos = take 10 (cyclic base)
    -- pts = Complex 0.0 1.02
    pts = fromFoldable [ Complex 0.0 0.9, Complex 0.0 1.1 ]
  log $ show $ act endos pts
  log $ show $ escapeTime isInside endos <$> pts
  log $ show $ actE isInside endos pts
  where
  isInside :: forall a. NormedRing a => a -> Boolean
  isInside = \z -> norm z < 4.0