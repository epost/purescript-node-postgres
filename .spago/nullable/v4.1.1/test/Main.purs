module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable, toMaybe)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  assertEqual
    { actual: toMaybe $ toNullable (Nothing :: Maybe Number)
    , expected: Nothing
    }
  assertEqual
    { actual: toMaybe $ toNullable (Just 42)
    , expected: Just 42
    }
  assertEqual
    { actual: toNullable Nothing == toNullable (Just 42)
    , expected: false
    }
  assertEqual
    { actual: toNullable Nothing `compare` toNullable (Just 42)
    , expected: LT
    }
