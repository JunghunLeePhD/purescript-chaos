module Test.Main
  ( main
  ) where

import Prelude

import Data.Array ((..))
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.List.Lazy (List, findIndex, fromFoldable, length, repeat, scanl, take, takeWhile)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Number (sqrt)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)

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

-- [Pixel] ->(w/ Screen and Lenz) [Real] ->(w/ actions) [EscapeTime] -> [Color]
type Screen = List Int
type Pixel = List Int

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

  traverse_ (log <<< show) pixels

-- type EndoReal = Endo (->) Real
-- type EscapeTime = Maybe Int
-- type HSLColor =
--   { h :: Int
--   , s :: Int
--   , l :: Int
--   }

-- affine :: Real -> Real -> EndoReal
-- affine a b = Endo \x -> a * x + b

-- -- [Pixel] -> [Real]
-- let
--   magnitude = 0.1
--   size = toNumber $ length pixels

--   normalizer :: EndoReal
--   normalizer =
--     affine (1.0 / (magnitude * size)) (-0.5 / magnitude)

--   coord :: List Real
--   coord = act normalizer $ toNumber <$> pixels
-- -- traverse_ (log <<< show) coord
-- let
--   isBounded :: Real -> Boolean
--   isBounded x = norm x < 2.0

--   getEscapeTime :: List EndoReal -> Real -> Maybe Int
--   getEscapeTime fs x = findIndex (not <<< isBounded) (act fs x)

--   endos :: List EndoReal
--   endos = take 200 $ repeat $ Endo $ \x -> x * x + 0.0

--   es :: List EscapeTime
--   es = getEscapeTime endos <$> coord
-- -- traverse_ (log <<< show) $ es

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