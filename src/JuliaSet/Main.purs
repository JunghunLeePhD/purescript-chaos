module JuliaSet.Main where

import Prelude

import Control.Monad.Except (runExceptT, except)
import Data.Array (replicate, (..))
import Data.Either (Either(..), note)
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Int (floor, toNumber)
import Data.List.Lazy as Lazy
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D, setFillStyle, fillRect, getCanvasWidth, getCanvasHeight)

import Types.Worker (TaskMsg, ResultMsg)

foreign import data Worker :: Type
foreign import getHardwareConcurrency :: Effect Int
foreign import createWorker :: Effect Worker
foreign import postToWorker :: Worker -> TaskMsg -> Effect Unit
foreign import onWorkerMessage :: Worker -> (ResultMsg -> Effect Unit) -> Effect Unit

main :: Effect Unit
main = launchAff_ do
  finalResult <- runExceptT do
    mCanvas <-
      liftEffect $ getCanvasElementById "juliaCanvas"
    canvas <-
      except $ note "Canvas element 'juliaCanvas' not found!" mCanvas

    liftEffect $ do
      ctx <- getContext2D canvas
      width <- floor <$> getCanvasWidth canvas
      height <- floor <$> getCanvasHeight canvas

      cores <- getHardwareConcurrency
      log $ "Booting up " <> show cores <> " workers purely..."

      pool <- sequence $ replicate cores createWorker
      traverse_ (\worker -> onWorkerMessage worker (paintRow ctx)) pool

      let
        workerStream = Lazy.cycle (Lazy.fromFoldable $ pool)
        taskStream = Lazy.fromFoldable (0 .. height)
        assignments = Lazy.zip workerStream taskStream

      log "Dispatching math to all cores..."
      traverse_
        (\(Tuple worker y) -> postToWorker worker { y, width, height })
        assignments

  case finalResult of
    Left errorMsg -> liftEffect $ log errorMsg
    Right _ -> pure unit

paintRow :: Context2D -> ResultMsg -> Effect Unit
paintRow ctx { y, colors } = do
  traverseWithIndex_
    ( \px color -> do
        setFillStyle ctx color
        fillRect ctx
          { x: toNumber px
          , y: toNumber y
          , width: 1.0
          , height: 1.0
          }
    )
    colors