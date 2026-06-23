module JuliaSet.Screen where

import Prelude

import Control.Monad.Reader (Reader, ask)
import Data.List.Lazy (List)
import Data.Int (toNumber)
import JuliaSet.Algorithm (getColor)
import Types.Action (EndoComplex)
import Types.NRing (Complex(..))

data Screen = Screen Int Int
data Plane = Plane Number Number Number Number
data Pixel = Pixel Int Int
data PixelWithColor = PixelWithColor Int Int String

type RenderEnv =
  { screen :: Screen
  , plane :: Plane
  }

pixelToComplex :: Pixel -> Reader RenderEnv Complex
pixelToComplex (Pixel px py) = do
  env <- ask
  let
    (Screen w h) = env.screen
    (Plane xMin xMax yMin yMax) = env.plane
    xRatio = toNumber px / toNumber w
    yRatio = toNumber py / toNumber h
    zx = xMin + xRatio * (xMax - xMin)
    zy = yMin + yRatio * (yMax - yMin)
  pure $ Complex zx zy
