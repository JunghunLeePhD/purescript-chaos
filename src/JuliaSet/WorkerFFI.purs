module JuliaSet.WorkerFFI where

import Prelude

import Control.Monad.Reader (runReader)
import Data.Array ((..))
import Data.List.Lazy (take, repeat)
import Data.Monoid.Endo (Endo(..))
import Effect (Effect)

import JuliaSet.Figure (Pixel(..), PixelWithColor(..), Plane(..), Screen(..), getPixelWithColor)
import Types.NRing (Complex(..))
import Types.Worker (TaskMsg, ResultMsg)

foreign import onTaskMessage :: (TaskMsg -> Effect Unit) -> Effect Unit
foreign import postResultToMain :: ResultMsg -> Effect Unit

main :: Effect Unit
main = do
  onTaskMessage \{ y, width, height } -> do
    let
      renderEnv =
        { screen: Screen width height
        , plane: Plane (-1.5) 1.5 (-1.5) 1.5
        , fs:
            take 200 $ repeat $ Endo (\z -> z * z + (Complex (-0.8) 0.156))
        }
      rowColors =
        ( \px ->
            let
              (PixelWithColor _ _ color) =
                runReader (getPixelWithColor (Pixel px y)) renderEnv
            in
              color
        ) <$>
          (0 .. width)

    postResultToMain { y, colors: rowColors }