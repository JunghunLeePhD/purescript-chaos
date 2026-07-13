module JuliaSet.Endomorphism where

import Prelude
import Data.Maybe (Maybe)
import Data.Monoid.Endo (Endo)
import Data.Newtype (unwrap)
import Data.List.Lazy (List, scanl)

class PolymorphicAction actor target result | actor target -> result where
  act :: actor -> target -> result

instance actEndoAny :: PolymorphicAction (Endo (->) a) a a where
  act = unwrap

else instance actMaybeTarget ::
  ( PolymorphicAction actor target result
  ) =>
  PolymorphicAction actor (Maybe target) (Maybe result) where
  act actor maybeTarget = act actor <$> maybeTarget

else instance actListEndoAny ::
  PolymorphicAction (List (Endo (->) a)) a (List a) where
  act fs z = scanl (\acc f -> act f acc) z fs

else instance actArrayEndoAny ::
  PolymorphicAction (Array (Endo (->) a)) a (Array a) where
  act fs z = scanl (\acc f -> act f acc) z fs