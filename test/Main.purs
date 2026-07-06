module Test.Main
  ( main
  ) where

import Prelude

import Data.Array ((..))
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.List.Lazy (List, findIndex, fromFoldable, length, repeat, scanl, take, takeWhile, zipWith, (!!))
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Number (sqrt)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (logShow)

class Ring a <= NormedRing a where
  norm :: a -> Number

-- Type: complex number as normed ring
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

instance normedRingReal :: NormedRing Real where
  norm x
    | x < 0.0 = -x
    | otherwise = x

instance normedRingComplex :: NormedRing Complex where
  norm (Complex r i) = sqrt (norm r + norm i)

class PolymorphicAction actor target result | actor target -> result where
  act :: actor -> target -> result

instance actEndoAny :: PolymorphicAction (Endo (->) a) a a where
  act = unwrap

else instance actListEndoAny ::
  PolymorphicAction (List (Endo (->) a)) a (List a) where
  act fs z = scanl (\acc f -> act f acc) z fs

else instance actListEndoListAny ::
  ( PolymorphicAction (List (Endo (->) a)) a (List a)
  ) =>
  PolymorphicAction (List (Endo (->) a)) (List a) (List (List a)) where
  act fs zs = act fs <$> zs

else instance actFunction :: PolymorphicAction (a -> b) a b where
  act f x = f x

-- [Pixel] ->(w/ Screen and Lenz) [Real] ->(w/ actions) [EscapeTime] -> [Color]
type Screen = List Int
type Pixel = List Int
type EndoReal = Endo (->) Real
type EscapeTime = Maybe Int

main :: Effect Unit
main = do
  -- Screen -> [Pixel]
  let
    generatePixel :: Screen -> List Pixel
    generatePixel = traverse (\d -> fromFoldable (0 .. d))
    width = 20
    height = 10
    screen = fromFoldable [ width, height ]

    pixels :: List Pixel
    pixels = generatePixel screen

  -- [Pixel] -> [[Real]]
  let
    affine :: Real -> Real -> EndoReal
    affine a b = Endo $ \x -> a * x + b

    magnitude = 1.0

    normalizer :: List EndoReal
    normalizer = fromFoldable
      [ affine (1.0 / (magnitude * toNumber width)) (-0.5 / magnitude)
      , affine (1.0 / (magnitude * toNumber height)) (-0.5 / magnitude)
      ]

    fromLazyListToComplex :: List Real -> Maybe Complex
    fromLazyListToComplex lazyList =
      Complex <$> (lazyList !! 0) <*> (lazyList !! 1)

    cpxs =
      (fromLazyListToComplex <<< (zipWith act normalizer) <<< map toNumber) <$> pixels
  traverse_ (logShow) cpxs

-- let
--   isBounded :: Real -> Boolean
--   isBounded x = norm x < 2.0

--   getEscapeTime :: List EndoComplex -> Real -> Maybe Int
--   getEscapeTime fs x = findIndex (not <<< isBounded) (act fs x)

--   endos :: List EndoReal
--   endos = take 200 $ repeat $ Endo $ \x -> x * x + 0.0

--   et :: List EscapeTime
--   et =
--     getEscapeTime endos <$> cpxs

-- traverse_ (logShow) et
-- type HSLColor =
--   { h :: Int
--   , s :: Int
--   , l :: Int
--   }
-- -- [EscapeTime] -> [HSLColor]
-- let
--   etToHSLColor :: EscapeTime -> HSLColor
--   etToHSLColor Nothing =
--     { h: 0
--     , s: 0
--     , l: 0
--     }
--   etToHSLColor (Just n) =
--     { h: n * 5
--     , s: 100
--     , l: 50
--     }

--   hslcolors :: List HSLColor
--   hslcolors = etToHSLColor <$> es
-- traverse_ (log <<< show) $ hslcolors