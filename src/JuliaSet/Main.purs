module JuliaSet.Main where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, except)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..), note)
import Data.Int (floor, toNumber)
import Effect (Effect)
import Effect.Aff (launchAff_, delay, Milliseconds(..), Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Graphics.Canvas
  ( Context2D
  , getCanvasElementById
  , getCanvasHeight
  , getCanvasWidth
  , getContext2D
  , setFillStyle
  , fillRect
  )

import Data.Array
  ( (..)
  , (!!)
  , zipWith
  , findIndex
  , fromFoldable
  , replicate
  )
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Endo (Endo(..))
import Data.Foldable (sequence_)
import Data.Traversable (traverse)

import JuliaSet.Space (Real, Complex(..), norm)
import JuliaSet.Endomorphism (act)

type Screen = Array Int
type Pixel = Array Int
type EndoReal = Endo (->) Real
type EndoComplex = Endo (->) Complex
type EscapeTime = Maybe Int
type HSLColor =
  { h :: Int
  , s :: Int
  , l :: Int
  }

generatePixel :: Screen -> Array Pixel
generatePixel = traverse (\d -> fromFoldable (0 .. d))

affine :: Real -> Real -> EndoReal
affine a b = Endo $ \x -> a * x + b

getComplex :: Array Real -> Maybe Complex
getComplex lazyList =
  Complex <$> (lazyList !! 0) <*> (lazyList !! 1)

isNotBounded :: Complex -> Boolean
isNotBounded z = norm z > 2.0

etToHSLColor :: EscapeTime -> HSLColor
etToHSLColor Nothing =
  { h: 0
  , s: 0
  , l: 0
  }
etToHSLColor (Just n) =
  { h: n * 5
  , s: 100
  , l: 50
  }

fillColor :: Context2D -> Pixel -> HSLColor -> Effect Unit
fillColor ctx pixel hslcolor = do
  setFillStyle ctx $ hslColorText hslcolor
  let
    xPos = toNumber (fromMaybe 0 (pixel !! 0))
    yPos = toNumber (fromMaybe 0 (pixel !! 1))
  fillRect ctx
    { x: xPos
    , y: yPos
    , width: 1.0
    , height: 1.0
    }
  where
  hslColorText :: HSLColor -> String
  hslColorText { h, s, l } =
    "hsl(" <> show h <> ", " <> show s <> "%, " <> show l <> "%)"

main :: Effect Unit
main = launchAff_ do
  finalResult <- runExceptT do
    mCanvas <- liftEffect $ getCanvasElementById "juliaCanvas"
    canvas <- except $ note "Canvas element 'juliaCanvas' not found!" mCanvas

    ctx <- liftEffect $ getContext2D canvas
    width <- liftEffect $ floor <$> getCanvasWidth canvas
    height <- liftEffect $ floor <$> getCanvasHeight canvas

    let
      magnitude = 0.3

      endos :: Array EndoComplex
      endos = replicate 50 $ Endo $ \z -> z * z + (Complex (-0.835) (-0.2321))

      renderRow :: Int -> ExceptT String Aff Unit
      renderRow y | y >= height = pure unit
      renderRow y = do
        let
          rowPixels = (\x -> [ x, y ]) <$> (0 .. (width - 1))

          normalizer =
            [ affine (1.0 / (magnitude * toNumber width)) (-0.5 / magnitude)
            , affine (1.0 / (magnitude * toNumber height)) (-0.5 / magnitude)
            ]

          cpxs = (getComplex <<< (zipWith act normalizer) <<< map toNumber) <$> rowPixels
          et = (\mCpx -> mCpx >>= \cpx -> findIndex isNotBounded (act endos cpx)) <$> cpxs
          hslcolors = etToHSLColor <$> et

        liftEffect $ sequence_ (zipWith (fillColor ctx) rowPixels hslcolors)

        lift $ delay (Milliseconds 0.0)

        renderRow (y + 1)

    renderRow 0

  case finalResult of
    Left errorMsg -> liftEffect $ log errorMsg
    Right _ -> pure unit