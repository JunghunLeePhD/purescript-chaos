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
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console (log)

type EndoReal = Endo (->) Number

quadratic :: Number -> EndoReal
quadratic c = Endo (\z -> z * z + c)

cyclic :: EndoReal -> List EndoReal
cyclic f = repeat f

class PolymorphicAction actor target result | actor target -> result where
  act :: actor -> target -> result

instance actEndoNum :: PolymorphicAction EndoReal Number Number where
  act = unwrap

instance actListEndoNum :: PolymorphicAction (List EndoReal) Number (List Number) where
  act fs z = scanl (\acc f -> act f acc) z fs

instance actListTarget ::
  ( PolymorphicAction actor target result
  ) =>
  PolymorphicAction actor (List target) (List result) where
  act = map <<< act

escapeTime :: (Number -> Boolean) -> List EndoReal -> Number -> Maybe Int
escapeTime isOk fs z =
  findIndex (not <<< isOk) (act fs z)

class (PolymorphicAction actor target result) <= PolymorphicActionE actor target result | actor target -> result where
  actE :: (Number -> Boolean) -> actor -> target -> result

instance actEListEndoNum :: PolymorphicActionE (List EndoReal) Number (List Number) where
  actE isOk fs z = takeWhile isOk (act fs z)

instance actEListEndoListNum :: PolymorphicActionE (List EndoReal) (List Number) (List (List (Number))) where
  actE isOk fs zs = (actE isOk fs) <$> zs

colorPixel :: Maybe Int -> String
colorPixel Nothing = "#000000"
colorPixel (Just n) = "hsl(" <> show (n * 5) <> ", 100%, 50%)"

main :: Effect Unit
main = do
  let
    base = quadratic 0.0
    -- endos = base
    endos = take 100 (cyclic base)
    -- pts = 1.1
    pts = fromFoldable [ 1.02, 1.03 ]
  -- log $ show $ act endos pts
  log $ show $ escapeTime isInside endos <$> pts
  log $ show $ actE isInside endos pts
  where
  isInside :: Number -> Boolean
  isInside = \z -> z * z < 4.0