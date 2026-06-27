module JuliaSet.Main where

import Prelude

import Control.Monad.Except (runExceptT, except)
import Data.Either (Either(..), note)
import Data.Foldable (traverse_)
import Data.Int (floor, toNumber)
import Data.Array (replicate)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref -- NEW: Import Ref for our mutable task counter
import Graphics.Canvas
  ( Context2D
  , getCanvasElementById
  , getCanvasHeight
  , getCanvasWidth
  , getContext2D
  , setFillStyle
  , fillRect
  )

import Types.Worker (Request, Response)

foreign import data Worker :: Type
foreign import getHardwareConcurrency :: Effect Int
foreign import createWorker :: Effect Worker
foreign import sendRequest :: Worker -> Request -> Effect Unit
foreign import getResponse :: Worker -> (Response -> Effect Unit) -> Effect Unit

main :: Effect Unit
main = launchAff_ do
  finalResult <- runExceptT do
    mCanvas <- liftEffect $ getCanvasElementById "juliaCanvas"
    canvas <- except $ note "Canvas element 'juliaCanvas' not found!" mCanvas

    liftEffect $ do
      ctx <- getContext2D canvas
      width <- floor <$> getCanvasWidth canvas
      height <- floor <$> getCanvasHeight canvas

      cores <- getHardwareConcurrency
      log $ "Booting up " <> show cores <> " workers purely..."

      pool <- sequence $ replicate cores createWorker

      let
        totalPixels = width * height
      nextPixelIndex <- Ref.new 0

      let
        dispatchNext :: Worker -> Effect Unit
        dispatchNext worker = do
          currentIndex <- Ref.read nextPixelIndex

          if currentIndex < totalPixels then do
            Ref.write (currentIndex + 1) nextPixelIndex

            let
              px = currentIndex `mod` width
              py = currentIndex / width

            sendRequest worker { pixel: { px, py } }
          else
            pure unit

      traverse_
        ( \worker ->
            getResponse worker
              ( \res -> do
                  fillColor ctx res
                  dispatchNext worker
              )
        )
        pool

      log "Dispatching math to all cores..."
      log "Starting continuous pull-based render..."

      traverse_ dispatchNext pool

  case finalResult of
    Left errorMsg -> liftEffect $ log errorMsg
    Right _ -> pure unit

fillColor :: Context2D -> Response -> Effect Unit
fillColor ctx { pixel: { px, py }, color } = do
  setFillStyle ctx color
  fillRect ctx
    { x: toNumber px
    , y: toNumber py
    , width: 1.0
    , height: 1.0
    }