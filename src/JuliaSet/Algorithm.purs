module JuliaSet.Algorithm where

import Prelude

import Data.List.Lazy (List, findIndex)
import Data.Maybe (Maybe)
import Types.Action (class PolymorphicAction, act)

escapeTime
  :: forall actor target
   . (PolymorphicAction (List actor) target (List target))
  => (target -> Boolean)
  -> List actor
  -> target
  -> Maybe Int
escapeTime isOk fs z =
  findIndex (not <<< isOk) (act fs z)