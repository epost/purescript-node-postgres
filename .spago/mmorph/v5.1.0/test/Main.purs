-- | This test is a port from the [mmorph tutorial](http://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html#g:6) section on mixing diverse transformers.
module Test.Main where

import Prelude

import Control.Monad.Morph (generalize, hoist)
import Control.Monad.State.Trans (StateT, runStateT, get, modify)
import Control.Monad.Writer.Trans (WriterT, execWriterT, lift, tell)
import Data.Identity (Identity)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Console (log)

tick :: StateT Int Identity Int
tick = modify (_ +1)

type MyEnv = WriterT (Array Int) Identity
type MyState = StateT Int MyEnv

save :: MyState Unit
save = do
  n <- get
  lift $ tell [n :: Int]

tock :: StateT Int Effect Unit
tock = do
  _ <- hoist generalize tick
  lift $ log "Tock!"

program :: StateT Int (WriterT (Array Int) Effect) (Array Unit)
program = replicateA 4 $ do
  hoist lift tock
  hoist (hoist generalize) save

main :: Effect (Array Int)
main = execWriterT (runStateT (void program) 0)
