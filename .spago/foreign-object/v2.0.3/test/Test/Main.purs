module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO
import Test.Assert as Assert
import Test.Foreign.Object (objectTests)

example :: Effect Unit
example = do
  let
    -- make an empty Object
    empty = FO.empty

    -- insert to an empty Object
    inserted = FO.insert "a" 1 empty

    -- or: use the singleton function
    -- singleton FO.singleton "a" 1

  -- lookup values for existing in the Object as a result of Maybe
  let lookup = FO.lookup "a" inserted
  Assert.assertEqual { actual: lookup, expected: Just 1 }

  -- delete a value from an Object
  let deleted = FO.delete "a" inserted
  Assert.assertEqual { actual: deleted, expected: FO.empty }

  let
    -- convert homogeneous records to Object
    converted = FO.fromHomogeneous { a: 1, b: 2, c: 3}
    -- check that the converted is equal to a regularly built Object
    built
      = FO.empty
      # FO.insert "a" 1
      # FO.insert "b" 2
      # FO.insert "c" 3

  Assert.assertEqual { actual: converted, expected: built }

main :: Effect Unit
main = do
  log "Running StrMap tests"
  example
  objectTests
