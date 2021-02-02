module Test.Data.CatList (testCatList) where

import Data.CatList

import Data.Foldable (foldMap, foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (fst, snd)
import Data.Unfoldable (range, replicate)
import Effect (Effect)
import Effect.Console (log, logShow)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, discard, identity, ($), (+), (<$>), (<<<), (==))
import Test.Assert (assert)

testCatList :: Effect Unit
testCatList = unsafePartial do
  log "null should be true for the empty list"
  assert $ null empty

  log "singleton should create a queue with one element"
  assert $ (fst <$> uncons (singleton 1)) == Just 1
  assert $ (null <<< snd <$> uncons (singleton 1)) == Just true

  log "length should return the length of the list"
  assert $ length empty == 0
  assert $ length (snoc empty 1) == 1
  assert $ length (snoc (snoc empty 1) 2) == 2

  log "cons should add an item to the beginning of the list"
  assert $ fst (fromJust (uncons (20 `cons` (10 `cons` empty)))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (20 `cons` (10 `cons` empty))))))) == 10

  log "snoc should add an item to the end of the list"
  assert $ fst (fromJust (uncons ((empty `snoc` 10) `snoc` 20))) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons ((empty `snoc` 10) `snoc` 20)))))) == 20

  log "appending two empty lists should be empty"
  assert $ null (append empty empty)

  log "uncons of a list with left and right lists should remove items properly"
  let list1 = ((10 `cons` empty) `snoc` 20) `snoc` 30
  assert $ fst (fromJust (uncons list1)) == 10
  assert $ fst (fromJust (uncons (snd (fromJust (uncons list1))))) == 20
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons list1)))))))) == 30
  assert $ null (snd (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons list1)))))))))

  log "foldMap over a list of monoids should produce the concatenation of the monoids"
  let list2 = (("a" `cons` empty) `snoc` "b") `snoc` "c"
  assert $ foldMap identity list2 == "abc"

  log "foldMap is stack safe"
  let longList :: CatList Int
      longList = range 0 10000
  logShow $ foldMap Additive longList

  log "foldl is stack-safe"
  logShow $ foldl (+) 0 longList

  log "fromFoldable should convert an array into a CatList with the same values"
  let list3 = fromFoldable ["a", "b", "c"]
  assert $ fst (fromJust (uncons list3)) == "a"
  assert $ fst (fromJust (uncons (snd (fromJust (uncons list3))))) == "b"
  assert $ fst (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons list3)))))))) == "c"
  assert $ null (snd (fromJust (uncons (snd (fromJust (uncons (snd (fromJust (uncons list3)))))))))

  log "functor should correctly map a function over the contents of a CatList"
  let list4 = (_ + 3) <$> fromFoldable [1, 2, 3]
  assert $ foldMap (\v -> [v]) list4 == [4, 5, 6]

  log "replicate should produce a CatList with a value repeated"
  let list5 = (replicate 3 "foo") :: CatList String
  assert $ foldMap (\v -> [v]) list5 == ["foo", "foo", "foo"]
