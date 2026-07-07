module JuliaSet.Main where

import Prelude

import Control.Monad.Except (runExceptT, except)
import Data.Either (Either(..), note)
import Data.Int (floor)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Graphics.Canvas
  ( getCanvasElementById
  , getCanvasHeight
  , getCanvasWidth
  , getContext2D
  )

import Data.Array ((..))
import Data.List.Lazy (List, fromFoldable)
import Data.Traversable (traverse)

type Screen = List Int
type Pixel = List Int

generatePixel :: Screen -> List Pixel
generatePixel = traverse (\d -> fromFoldable (0 .. d))

main :: Effect Unit
main = launchAff_ do
  finalResult <- runExceptT do
    mCanvas <- liftEffect $ getCanvasElementById "juliaCanvas"
    canvas <- except $ note "Canvas element 'juliaCanvas' not found!" mCanvas

    liftEffect $ do
      ctx <- getContext2D canvas
      width <- floor <$> getCanvasWidth canvas
      height <- floor <$> getCanvasHeight canvas

      let
        -- Screen -> [Pixel]
        screen = fromFoldable [ width, height ]
        pixels = generatePixel screen
      pure $ unit
    pure $ unit

  case finalResult of
    Left errorMsg -> liftEffect $ log errorMsg
    Right _ -> pure unit
