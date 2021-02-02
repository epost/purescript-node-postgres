module Example where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Pipes (yield, for)
import Pipes.Core (runEffect, Producer_)

pipedRandomInt
  :: Int
  -> Int
  -> Producer_ String Effect Int
pipedRandomInt x y = do
  yield $ "Generating an Int between " <> show x <> " and " <> show y <> "..."
  r <- liftEffect $ randomInt x y
  yield $ "Generated " <> show r
  pure r

main :: Effect Unit
main =
  let go = pipedRandomInt 1 10
   in do
    r <- runEffect $ for go (log <<< ("Log: " <> _))
    log $ "Result: " <> show r
