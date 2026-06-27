module JuliaSet.Figure where

import Prelude

import Control.Monad.Reader (Reader, ask)
import Data.Int (toNumber)

import Types.NRing (Complex(..))

type Screen = { width :: Int, height :: Int }
type Plane = { xMin :: Number, xMax :: Number, yMin :: Number, yMax :: Number }
type Pixel = { px :: Int, py :: Int }

type RenderEnv =
  { screen :: Screen
  , plane :: Plane
  }

pixelToComplex :: Pixel -> Reader RenderEnv Complex
pixelToComplex { px, py } = do
  { screen, plane } <- ask
  let
    xRatio = toNumber px / toNumber screen.width
    yRatio = toNumber py / toNumber screen.height
    zx = plane.xMin + xRatio * (plane.xMax - plane.xMin)
    zy = plane.yMin + yRatio * (plane.yMax - plane.yMin)
  pure $ Complex zx zy
