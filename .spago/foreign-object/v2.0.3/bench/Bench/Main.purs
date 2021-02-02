module Bench.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Bench.Foreign.Object (benchForeignObject)

main :: Effect Unit
main = do
  log "Foreign.Object"
  log "======"
  benchForeignObject
