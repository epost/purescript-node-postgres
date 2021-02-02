module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Data.CatList (testCatList)
import Test.Data.CatQueue (testCatQueue)

main :: Effect Unit
main = do
  log "CatQueue"
  testCatQueue

  log ""

  log "CatList"
  testCatList
