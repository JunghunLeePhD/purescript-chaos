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

-- [Pixel] ->(w/ Screen and Lenz) [Real] ->(w/ actions) [EscapeTime] -> [Color]
type Pixel = Int
type Real = Number

instance normedRingReal :: NormedRing Real where
  norm x
    | x < 0.0 = -x
    | otherwise = x

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