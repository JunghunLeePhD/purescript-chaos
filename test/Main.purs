module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual)

-- Imagine this function comes from your src/ code
add :: Int -> Int -> Int
add a b = a + b

main :: Effect Unit
main = do
  log "Running local tests..."

  assertEqual { expected: 4, actual: add 2 2 }
  assertEqual { expected: 10, actual: add 5 5 }

  log "All tests passed successfully!"