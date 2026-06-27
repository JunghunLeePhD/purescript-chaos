module Types.Worker where

import JuliaSet.Figure (Pixel)

type Request =
  { pixel :: Pixel
  }

type Response =
  { pixel :: Pixel
  , color :: String
  }