module WorkerMain where

import Prelude

import Control.Monad.Reader (runReader)
import Data.Array ((..))
import Data.Foldable (foldM)
import Data.List.Lazy (take, repeat)
import Data.Monoid.Endo (Endo(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import JuliaSet.Figure (Pixel(..), PixelWithColor(..), Plane(..), Screen(..), getPixelWithColor)
import Types.NRing (Complex(..))

import Types (StartMsg, RowMsg)

foreign import onStartMessage :: (StartMsg -> Effect Unit) -> Effect Unit
foreign import postRowToMain :: RowMsg -> Effect Unit

main :: Effect Unit
main = do
  onStartMessage \{ width, height } -> do
    let
      renderEnv =
        { screen: Screen width height
        , plane: Plane (-1.5) 1.5 (-1.5) 1.5
        }

      fs = take 400 $ repeat $ Endo (\z -> z * z + (Complex (-0.8) 0.156))

    launchAff_ do
      foldM
        ( \_ py -> do
            let
              rowColors =
                ( \px ->
                    let
                      (PixelWithColor _ _ color) =
                        runReader (getPixelWithColor fs (Pixel px py)) renderEnv
                    in
                      color
                ) <$>
                  (0 .. width)
            liftEffect $ postRowToMain { y: py, colors: rowColors }
            delay $ Milliseconds 0.0

        )
        unit
        (0 .. height)
