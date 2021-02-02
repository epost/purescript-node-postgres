module Test.Data.CatQueue (testCatQueue) where

import Prelude

import Data.CatQueue (CatQueue, cons, empty, fromFoldable, length, null, singleton, snoc, uncons, unsnoc)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala)
import Data.Traversable (foldMap, foldl, traverse)
import Data.Tuple (fst, snd)
import Data.Unfoldable (range, replicate)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)

testCatQueue :: Effect Unit
testCatQueue = unsafePartial do
  log "null should be true for the empty queue"
  assert $ null empty

  log "singleton should create a queue with one element"
  assert $ (fst <$> uncons (singleton 1)) == Just 1
  assert $ (null <<< snd <$> uncons (singleton 1)) == Just true

  log "length should return the length of the queue"
  assert $ length empty == 0
  assert $ length (snoc empty 1) == 1
  assert $ length (snoc (snoc empty 1) 2) == 2

  log "cons should add an item to the beginning of the queue"
  assert $ fst (fromJust (uncons (20 `cons` (10 `cons` empty)))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (20 `cons` (10 `cons` empty))))))) == 10

  log "snoc should add an item to the end of the queue"
  assert $ fst (fromJust (uncons ((empty `snoc` 10) `snoc` 20))) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons ((empty `snoc` 10) `snoc` 20)))))) == 20

  log "uncons of the empty queue should be Nothing"
  assert $ isNothing (uncons empty)

  log "uncons of a queue with left and right lists should remove items properly"
  let queue1 = ((empty `snoc` 10) `snoc` 20) `snoc` 30
  assert $ fst (fromJust (uncons queue1)) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons queue1))))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons queue1)))))))) == 30

  log "unsnoc of the empty queue should be Nothing"
  assert $ isNothing (unsnoc empty)

  log "unsnoc of a queue with left and right queues should remove items properly"
  assert $ fst (fromJust (unsnoc queue1)) == 30
  assert $ fst (fromJust (unsnoc (snd (fromJust (unsnoc queue1))))) == 20
  assert $ fst (fromJust (unsnoc (snd (fromJust (unsnoc (snd (fromJust (unsnoc queue1)))))))) == 10

  log "fromFoldable should convert an array into a CatList with the same values"
  let queue3 = fromFoldable ["a", "b", "c"]
  assert $ fst (fromJust (uncons queue3)) == "a"
  assert $ fst (fromJust (uncons (snd (fromJust (uncons queue3))))) == "b"
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons queue3)))))))) == "c"
  assert $ null (snd (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons queue3)))))))))

  log "appending two empty lists should be empty"
  assert $ null (empty <> empty)

  log "foldMap over a queue of monoids should produce the concatenation of the monoids"
  let queue2 = ((empty `snoc` "a") `snoc` "b") `snoc` "c"
  assert $ foldMap identity queue2 == "abc"

  log "foldMap is stack safe"
  let longList :: CatQueue Int
      longList = range 0 10000
  assert $ ala Additive foldMap longList == 50005000

  log "foldl is stack-safe"
  assert $ foldl (+) 0 longList == 50005000

  log "sequence is stack-safe"
  assert $ traverse Just longList == Just longList

  log "functor should correctly map a function over the contents of a CatList"
  let queue4 = (_ + 3) <$> fromFoldable [1, 2, 3]
  assert $ foldMap (\v -> [v]) queue4 == [4, 5, 6]

  log "replicate should produce a CatList with a value repeated"
  let queue5 = (replicate 3 "foo") :: CatQueue String
  assert $ foldMap (\v -> [v]) queue5 == ["foo", "foo", "foo"]
