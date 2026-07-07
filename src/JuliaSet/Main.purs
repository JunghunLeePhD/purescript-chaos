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

main :: Effect Unit
main = launchAff_ do
  finalResult <- runExceptT do
    mCanvas <- liftEffect $ getCanvasElementById "juliaCanvas"
    canvas <- except $ note "Canvas element 'juliaCanvas' not found!" mCanvas

    liftEffect $ do
      ctx <- getContext2D canvas
      width <- floor <$> getCanvasWidth canvas
      height <- floor <$> getCanvasHeight canvas

      pure $ unit
    pure $ unit

  case finalResult of
    Left errorMsg -> liftEffect $ log errorMsg
    Right _ -> pure unit
