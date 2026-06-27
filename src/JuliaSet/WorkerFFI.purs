module JuliaSet.WorkerFFI where

import Prelude

import Control.Monad.Reader (runReader)
import Data.List.Lazy (take, repeat)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Effect (Effect)

import JuliaSet.Algorithm (getEscapeTime)
import JuliaSet.Figure (pixelToComplex)

import Types.Action (EndoComplex)
import Types.NRing (Complex(..))
import Types.Worker (Request, Response)

foreign import getRequest :: (Request -> Effect Unit) -> Effect Unit
foreign import sendResponse :: Response -> Effect Unit

quadratic :: Complex -> EndoComplex
quadratic c = Endo $ \z -> z * z + c

main :: Effect Unit
main = do
  getRequest \{ pixel } -> do
    let
      renderEnv =
        { screen:
            { width: 800
            , height: 800
            }
        , plane:
            { xMin: -1.5
            , xMax: 1.5
            , yMin: -1.5
            , yMax: 1.5
            }
        }
      z =
        runReader (pixelToComplex pixel) renderEnv
      fs =
        take 200 $ repeat $ quadratic (Complex (-0.8) 0.156)
      et =
        getEscapeTime fs z
      color = etToColor et

    sendResponse { pixel, color }
  where
  etToColor :: Maybe Int -> String
  etToColor Nothing = "#000000"
  etToColor (Just n) = "hsl(" <> show (n * 5) <> ", 100%, 50%)"