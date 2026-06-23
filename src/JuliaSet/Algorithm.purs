module JuliaSet.Algorithm where

import Prelude

import Data.List.Lazy
  ( List
  , findIndex
  )
import Data.Maybe (Maybe(..))
import Types.Action
  ( act
  , EndoComplex
  )
import Types.NRing (Complex, norm)

getEscapeTime :: List EndoComplex -> Complex -> Maybe Int
getEscapeTime fs z = findIndex (not <<< isBounded) (act fs z)
  where
  isBounded :: Complex -> Boolean
  isBounded z' = norm z' < 4.0

getColor :: List EndoComplex -> Complex -> String
getColor fs z = orbitToColor $ getEscapeTime fs z
  where
  orbitToColor :: Maybe Int -> String
  orbitToColor Nothing = "#000000"
  orbitToColor (Just n') = "hsl(" <> show (n' * 5) <> ", 100%, 50%)"
