module JuliaSet.Algorithm where

import Prelude

import Data.List.Lazy
  ( List
  , findIndex
  , repeat
  , take
  )
import Data.Maybe (Maybe)
import Data.Monoid.Endo (Endo(..))
import Types.Action
  ( class PolymorphicAction
  , act
  , EndoComplex
  )
import Types.NRing (Complex, norm)

quadraticF :: Complex -> List EndoComplex
quadraticF c = repeat $ quadratic c
  where
  quadratic :: Complex -> EndoComplex
  quadratic c' = Endo $ \z -> z * z + c'

quadraticFF :: Int -> Complex -> List EndoComplex
quadraticFF n c = take n $ quadraticF c

escapeTime
  :: forall actor target
   . (PolymorphicAction (List actor) target (List target))
  => (target -> Boolean)
  -> List actor
  -> target
  -> Maybe Int
escapeTime isOk fs z =
  findIndex (not <<< isOk) (act fs z)

isBounded :: Complex -> Boolean
isBounded z = norm z < 4.0