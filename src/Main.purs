module Main where

import Prelude

import Control.Monad.Except (runExceptT, except)
import Data.Either (Either(..), note)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Int (floor, toNumber)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Graphics.Canvas (getCanvasElementById, getContext2D, setFillStyle, fillRect, getCanvasWidth, getCanvasHeight)

import Types (StartMsg, RowMsg)

foreign import data Worker :: Type
foreign import createWorker :: Effect Worker
foreign import postStartMessage :: Worker -> StartMsg -> Effect Unit
foreign import onRowMessage :: Worker -> (RowMsg -> Effect Unit) -> Effect Unit

main :: Effect Unit
main = launchAff_ do
  finalResult <- runExceptT do
    mCanvas <-
      liftEffect $ getCanvasElementById "juliaCanvas"
    canvas <-
      except $ note "Canvas element 'juliaCanvas' not found!" mCanvas

    ctx <-
      liftEffect $ getContext2D canvas
    wNum <-
      liftEffect $ getCanvasWidth canvas
    hNum <-
      liftEffect $ getCanvasHeight canvas

    let
      width = floor wNum
      height = floor hNum

    liftEffect $ do
      log "Starting Web Worker..."
      worker <- createWorker

      onRowMessage worker \msg -> do
        traverseWithIndex_
          ( \px color -> do
              setFillStyle ctx color
              fillRect ctx
                { x: toNumber px
                , y: toNumber msg.y
                , width: 1.0
                , height: 1.0
                }
          )
          msg.colors

      log "Telling worker to calculate the Julia Set..."
      postStartMessage worker { width, height }

  case finalResult of
    Left errorMsg -> liftEffect $ log errorMsg
    Right _ -> pure unit
