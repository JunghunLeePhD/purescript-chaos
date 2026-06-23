module Main where

import Prelude

import Control.Monad.Except (runExceptT, except)
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Class (lift)
import Data.Array ((..))
import Data.Either (Either(..), note)
import Data.Foldable (foldM, traverse_)
import Data.Int (floor, toNumber)
import Data.List.Lazy (take, repeat)
import Data.Monoid.Endo (Endo(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Graphics.Canvas (getCanvasElementById, getContext2D, setFillStyle, fillRect, getCanvasWidth, getCanvasHeight)
import JuliaSet.Figure (Pixel(..), PixelWithColor(..), Plane(..), Screen(..), getPixelWithColor)
import Types.NRing (Complex(..))

main :: Effect Unit
main = launchAff_ do
  finalResult <- runExceptT do
    mCanvas <-
      lift $ liftEffect $ getCanvasElementById "juliaCanvas"
    canvas <-
      except $ note "Canvas element 'juliaCanvas' not found!" mCanvas

    ctx <-
      lift $ liftEffect $ getContext2D canvas
    wNum <-
      lift $ liftEffect $ getCanvasWidth canvas
    hNum <-
      lift $ liftEffect $ getCanvasHeight canvas

    let
      width = floor wNum
      height = floor hNum

      renderEnv =
        { screen:
            Screen width height
        , plane:
            Plane (-1.5) 1.5 (-1.5) 1.5
        }
      fs =
        take 400
          $ repeat
          $
            Endo (\z -> z * z + (Complex (-0.8) 0.156))

    lift $ liftEffect $ log "Painting fractal row-by-row functionally..."

    lift $ foldM
      ( \_ py -> do
          let
            rowPixels =
              (\px -> runReader (getPixelWithColor fs (Pixel px py)) renderEnv) <$> (0 .. width)

          liftEffect $ traverse_
            ( \(PixelWithColor curPx curPy color) -> do
                setFillStyle ctx color
                fillRect ctx
                  { x: toNumber curPx
                  , y: toNumber curPy
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