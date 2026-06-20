module Main where

import Prelude

import Data.Array ((..))
import Data.Int (floor)
import Data.Either (Either(..), note)
import Data.Foldable (foldM, traverse_)
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Control.Monad.Except (runExceptT, except)
import Control.Monad.Trans.Class (lift)
import Data.Time.Duration (Milliseconds(..))
import Graphics.Canvas (getCanvasElementById, getContext2D, setFillStyle, fillRect, getCanvasWidth, getCanvasHeight)

import Fractal.Types (Complex(..), Screen(..))
import Fractal.Algorithm (JuliaSet(..), calculatePixel)

main :: Effect Unit
main = launchAff_ do
  finalResult <- runExceptT do
    mCanvas <-
      lift $ liftEffect $ getCanvasElementById "juliaCanvas"
    canvas <-
      except $ note "Canvas element 'juliaCanvas' not found!" mCanvas

    ctx <- lift $ liftEffect $ getContext2D canvas
    wNum <- lift $ liftEffect $ getCanvasWidth canvas
    hNum <- lift $ liftEffect $ getCanvasHeight canvas

    let
      width = floor wNum
      height = floor hNum
      screen = Screen width height
      viewPlane =
        { xMin: -1.5
        , xMax: 1.5
        , yMin: -1.5
        , yMax: 1.5
        }
      maxIter = 100
      myFractal = JuliaSet (Complex (-0.8) 0.156)
      processCoord = calculatePixel screen viewPlane maxIter myFractal

    lift $ liftEffect $ log "Painting fractal row-by-row functionally..."

    lift $ foldM
      ( \_ px -> do
          let rowPixels = processCoord px <$> (0 .. width)
          liftEffect $ traverse_
            ( \{ x, y, color } -> do
                setFillStyle ctx color
                fillRect ctx
                  { x
                  , y
                  , width: 1.0
                  , height: 1.0
                  }
            )
            rowPixels
          delay $ Milliseconds 0.0
      )
      unit
      (0 .. height)

  case finalResult of
    Left errorMsg -> liftEffect $ log errorMsg
    Right _ -> liftEffect $ log "Julia set rendered successfully!"