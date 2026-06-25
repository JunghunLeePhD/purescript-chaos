module Types.Worker where

type TaskMsg = { y :: Int, width :: Int, height :: Int }
type ResultMsg = { y :: Int, colors :: Array String }