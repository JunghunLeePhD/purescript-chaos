module Test.Main
  ( main
  ) where

import Prelude

import Data.Array ((..), foldl)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.List.Lazy (List, findIndex, fromFoldable, repeat, scanl, take, takeWhile, length)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Console (log)
import JuliaSet.Algorithm (getEscapeTime)

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

-- [Pixel] ->(w/ Screen and Lenz) [Real] ->(w/ actions) [EscapeTime] -> [Color]
type Pixel = Int
type Real = Number

instance normedRingReal :: NormedRing Real where
  norm x
    | x < 0.0 = -x
    | otherwise = x

type EndoReal = Endo (->) Real
type EscapeTime = Maybe Int
type HSLColor =
  { h :: Int
  , s :: Int
  , l :: Int
  }

affine :: Real -> Real -> EndoReal
affine a b = Endo \x -> a * x + b

main :: Effect Unit
main = do
  let
    pixels :: List Pixel
    pixels = fromFoldable (0 .. 999)

  -- traverse_ (log <<< show) pixels
  -- [Pixel] -> [Real]
  let
    magnitude = 0.1
    size = toNumber $ length pixels

    normalizer :: EndoReal
    normalizer =
      affine (1.0 / (magnitude * size)) (-0.5 / magnitude)

    coord :: List Real
    coord = act normalizer $ toNumber <$> pixels
  -- traverse_ (log <<< show) coord
  let
    isBounded :: Real -> Boolean
    isBounded x = norm x < 2.0

    getEscapeTime :: List EndoReal -> Real -> Maybe Int
    getEscapeTime fs x = findIndex (not <<< isBounded) (act fs x)

    endos :: List EndoReal
    endos = take 200 $ repeat $ Endo $ \x -> x * x + 0.0

    es :: List EscapeTime
    es = getEscapeTime endos <$> coord
  -- traverse_ (log <<< show) $ es

  -- [EscapeTime] -> [HSLColor]
  let
    etToHSLColor :: EscapeTime -> HSLColor
    etToHSLColor Nothing =
      { h: 0
      , s: 0
      , l: 0
      }
    etToHSLColor (Just n) =
      { h: n * 5
      , s: 100
      , l: 50
      }

    hslcolors :: List HSLColor
    hslcolors = etToHSLColor <$> es
  traverse_ (log <<< show) $ hslcolors