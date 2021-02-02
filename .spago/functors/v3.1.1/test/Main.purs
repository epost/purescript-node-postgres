module Test.Main where

import Prelude

import Data.Const (Const)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Functor.Coproduct.Inject (inj)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testComposeOrdering
  testInjections

testComposeOrdering :: Effect Unit
testComposeOrdering = do
  assertEqual
    { expected: Compose (Identity (Just true))
    , actual: Compose (Identity (Just true))
    }
  assertEqual
    { expected: Compose (Identity (Just true)) > Compose (Identity (Just false))
    , actual: true
    }

testInjections :: Effect Unit
testInjections = do
  assertEqual
    { expected: Identity unit
    , actual: inj (Identity unit)
    }
  assertEqual
    { expected: left (Identity unit) :: Coproduct Identity (Const Void) Unit
    , actual: inj (Identity unit)
    }
  assertEqual
    { expected: right (Identity unit) :: Coproduct (Const Void) Identity Unit
    , actual: inj (Identity unit)
    }
