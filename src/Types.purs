module Types where

type StartMsg =
  { width :: Int
  , height :: Int
  }

type RowMsg =
  { y :: Int
  , colors :: Array String
  }