module JuliaSet.Main where

import Prelude

import Control.Monad.Except (runExceptT, except)
import Data.Either (Either(..), note)
import Data.Int (floor, toNumber)
import Effect (Effect)
import Effect.Aff (launchAff_)
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

import Data.Array ((..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Endo (Endo(..))
import Data.Foldable (sequence_)
import Data.List.Lazy
  ( List
  , fromFoldable
  , (!!)
  , zipWith
  , take
  , repeat
  , findIndex
  )
import Data.Traversable (traverse)

import JuliaSet.Space (Real, Complex(..), norm)
import JuliaSet.Endomorphism (act)

type Screen = List Int
type Pixel = List Int
type EndoReal = Endo (->) Real
type EndoComplex = Endo (->) Complex
type EscapeTime = Maybe Int
type HSLColor =
  { h :: Int
  , s :: Int
  , l :: Int
  }

generatePixel :: Screen -> List Pixel
generatePixel = traverse (\d -> fromFoldable (0 .. d))

affine :: Real -> Real -> EndoReal
affine a b = Endo $ \x -> a * x + b

getComplex :: List Real -> Maybe Complex
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

    liftEffect $ do
      ctx <- getContext2D canvas
      width <- floor <$> getCanvasWidth canvas
      height <- floor <$> getCanvasHeight canvas

      let
        -- Screen -> [Pixel]
        screen = fromFoldable [ width, height ]
        pixels = generatePixel screen
        magnitude = 0.01

        normalizer :: List EndoReal
        normalizer = fromFoldable
          [ affine (1.0 / (magnitude * toNumber width)) (-0.5 / magnitude)
          , affine (1.0 / (magnitude * toNumber height)) (-0.5 / magnitude)
          ]

        cpxs =
          (getComplex <<< (zipWith act normalizer) <<< map toNumber) <$> pixels

        endos :: List EndoComplex
        endos = take 3 $ repeat $ Endo $ \z -> z * z + (Complex 0.0 0.0)

        et :: List EscapeTime
        et = do
          -- mOrbit :: List (Maybe (List Complex))
          mOrbit <- (act endos <$> cpxs)
          pure $ mOrbit >>= findIndex (isNotBounded)

        hslcolors :: List HSLColor
        hslcolors = etToHSLColor <$> et

      sequence_ $ zipWith (fillColor ctx) pixels hslcolors
      pure $ unit
    pure $ unit

  case finalResult of
    Left errorMsg -> liftEffect $ log errorMsg
    Right _ -> pure unit
