module Fractal.Algorithm where

import Prelude
import Data.Int (toNumber)
import Data.List.Lazy (iterate, takeWhile, take)
import Data.Foldable (length)

import Fractal.Types (Complex(..), Screen(..), Plane, CanvasPixel, norm)

class Fractal a where
  escapeTime :: Int -> a -> Complex -> Int

data JuliaSet = JuliaSet Complex

instance fractalJuliaSet :: Fractal JuliaSet where
  escapeTime maxIter (JuliaSet c) z0 =
    let
      orbit = iterate (\z -> z * z + c) z0
      boundedOrbit = takeWhile (\z -> norm z <= 2.0) orbit
    in
      length $ take maxIter boundedOrbit

pixelToComplex :: Screen -> Plane -> Int -> Int -> Complex
pixelToComplex (Screen w h) plane px py =
  let
    xRatio = toNumber px / toNumber w
    yRatio = toNumber py / toNumber h
    zx = plane.xMin + xRatio * (plane.xMax - plane.xMin)
    zy = plane.yMin + yRatio * (plane.yMax - plane.yMin)
  in
    Complex zx zy

toCanvasPixel :: Int -> Int -> Int -> Int -> CanvasPixel
toCanvasPixel px py iter maxIter =
  let
    color =
      if iter == maxIter then "#000000"
      else "hsl(" <> show (iter * 8) <> ", 100%, 50%)"
  in
    { x: toNumber px, y: toNumber py, color }

calculatePixel :: forall a. Fractal a => Screen -> Plane -> Int -> a -> Int -> Int -> CanvasPixel
calculatePixel screen plane maxIter fractal px py =
  let
    z0 = pixelToComplex screen plane px py
    iter = escapeTime maxIter fractal z0
  in
    toCanvasPixel px py iter maxIter