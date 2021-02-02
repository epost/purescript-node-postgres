module Bench.Foreign.Object where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (benchWith)

import Data.Tuple (Tuple(..))
import Data.List as L
import Foreign.Object as Object

benchForeignObject :: Effect Unit
benchForeignObject = do
  log "fromFoldable"
  benchFromFoldable

  where
  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> Object.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> Object.fromFoldable natPairs
