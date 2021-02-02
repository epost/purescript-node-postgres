module Test.Foreign.Object where

import Prelude

import Control.Monad.Writer (runWriter, tell)
import Data.Array as A
import Data.Foldable (foldl, foldr)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex, foldMapWithIndex)
import Data.Function (on)
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as O
import Foreign.Object.Gen (genForeignObject)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assertEqual)
import Test.QuickCheck ((<?>), quickCheck, quickCheck', (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen

newtype TestObject v = TestObject (O.Object v)

instance arbTestObject :: (Arbitrary v) => Arbitrary (TestObject v) where
  arbitrary = TestObject <$> genForeignObject arbitrary arbitrary

newtype SmallArray v = SmallArray (Array v)

instance arbSmallArray :: (Arbitrary v) => Arbitrary (SmallArray v) where
  arbitrary = SmallArray <$> Gen.resize 3 arbitrary

data Instruction k v = Insert k v | Delete k

instance showInstruction :: (Show k, Show v) => Show (Instruction k v) where
  show (Insert k v) = "Insert (" <> show k <> ") (" <> show v <> ")"
  show (Delete k) = "Delete (" <> show k <> ")"

instance arbInstruction :: (Arbitrary v) => Arbitrary (Instruction String v) where
  arbitrary = do
    b <- arbitrary
    k <- Gen.frequency $ Tuple 10.0 (pure "hasOwnProperty") :| pure (Tuple 50.0 arbitrary)
    case b of
      true -> do
        v <- arbitrary
        pure (Insert k v)
      false -> do
        pure (Delete k)

runInstructions :: forall v. L.List (Instruction String v) -> O.Object v -> O.Object v
runInstructions instrs t0 = foldl step t0 instrs
  where
  step tree (Insert k v) = O.insert k v tree
  step tree (Delete k) = O.delete k tree

number :: Int -> Int
number n = n

toAscArray :: forall a. O.Object a -> Array (Tuple String a)
toAscArray = O.toAscUnfoldable

objectTests :: Effect Unit
objectTests = do
  log "Test inserting into empty tree"
  quickCheck $ \k v -> O.lookup k (O.insert k v O.empty) == Just (number v)
    <?> ("k: " <> show k <> ", v: " <> show v)

  log "Test inserting two values with same key"
  quickCheck $ \k v1 v2 ->
    O.lookup k (O.insert k v2 (O.insert k v1 O.empty)) == Just (number v2)

  log "Test delete after inserting"
  quickCheck $ \k v -> O.isEmpty (O.delete k (O.insert k (number v) O.empty))
    <?> ("k: " <> show k <> ", v: " <> show v)

  log "Test pop after inserting"
  quickCheck $ \k v -> O.pop k (O.insert k (number v) O.empty) == Just (Tuple v O.empty)
    <?> ("k: " <> show k <> ", v: " <> show v)

  log "Pop non-existent key"
  quickCheck $ \k1 k2 v -> ((k1 == k2) || (O.pop k2 (O.insert k1 (number v) O.empty) == Nothing))
    <?> ("k1: " <> show k1 <> ", k2: " <> show k2 <> ", v: " <> show v)

  log "Insert two, lookup first"
  quickCheck $ \k1 v1 k2 v2 -> ((k1 == k2) || (O.lookup k1 (O.insert k2 (number v2) (O.insert k1 (number v1) O.empty)) == Just v1))
    <?> ("k1: " <> show k1 <> ", v1: " <> show v1 <> ", k2: " <> show k2 <> ", v2: " <> show v2)

  log "Insert two, lookup second"
  quickCheck $ \k1 v1 k2 v2 -> O.lookup k2 (O.insert k2 (number v2) (O.insert k1 (number v1) O.empty)) == Just v2
    <?> ("k1: " <> show k1 <> ", v1: " <> show v1 <> ", k2: " <> show k2 <> ", v2: " <> show v2)

  log "Insert two, delete one"
  quickCheck $ \k1 v1 k2 v2 -> ((k1 == k2) || (O.lookup k2 (O.delete k1 (O.insert k2 (number v2) (O.insert k1 (number v1) O.empty))) == Just v2))
    <?> ("k1: " <> show k1 <> ", v1: " <> show v1 <> ", k2: " <> show k2 <> ", v2: " <> show v2)

  log "Lookup from empty"
  quickCheck $ \k -> O.lookup k (O.empty :: O.Object Int) == Nothing

  log "Lookup from singleton"
  quickCheck $ \k v -> O.lookup k (O.singleton k (v :: Int)) == Just v

  log "Random lookup"
  quickCheck' 1000 $ \instrs k v ->
    let
      tree :: O.Object Int
      tree = O.insert k v (runInstructions instrs O.empty)
    in O.lookup k tree == Just v <?> ("instrs:\n  " <> show instrs <> "\nk:\n  " <> show k <> "\nv:\n  " <> show v)

  log "Singleton to list"
  quickCheck $ \k v -> O.toUnfoldable (O.singleton k v :: O.Object Int) == L.singleton (Tuple k v)

  log "filterWithKey gives submap"
  quickCheck $ \(TestObject (s :: O.Object Int)) p ->
                 O.isSubmap (O.filterWithKey p s) s

  log "filterWithKey keeps those keys for which predicate is true"
  quickCheck $ \(TestObject (s :: O.Object Int)) p ->
                 A.all (uncurry p) (O.toAscUnfoldable (O.filterWithKey p s) :: Array (Tuple String Int))

  log "filterKeys gives submap"
  quickCheck $ \(TestObject (s :: O.Object Int)) p ->
                 O.isSubmap (O.filterKeys p s) s

  log "filterKeys keeps those keys for which predicate is true"
  quickCheck $ \(TestObject (s :: O.Object Int)) p ->
                 A.all p (O.keys (O.filterKeys p s))

  log "filter gives submap"
  quickCheck $ \(TestObject (s :: O.Object Int)) p ->
                 O.isSubmap (O.filter p s) s

  log "filter keeps those values for which predicate is true"
  quickCheck $ \(TestObject (s :: O.Object Int)) p ->
                 A.all p (O.values (O.filter p s))

  log "fromFoldable [] = empty"
  quickCheck (O.fromFoldable [] == (O.empty :: O.Object Unit)
    <?> "was not empty")

  log "fromFoldable & key collision"
  do
    let nums = O.fromFoldable [Tuple "0" "zero", Tuple "1" "what", Tuple "1" "one"]
    quickCheck (O.lookup "0" nums == Just "zero" <?> "invalid lookup - 0")
    quickCheck (O.lookup "1" nums == Just "one"  <?> "invalid lookup - 1")
    quickCheck (O.lookup "2" nums == Nothing     <?> "invalid lookup - 2")

  log "fromFoldableWith const [] = empty"
  quickCheck (O.fromFoldableWith const [] == (O.empty :: O.Object Unit)
    <?> "was not empty")

  log "fromFoldableWith (+) & key collision"
  do
    let nums = O.fromFoldableWith (+) [Tuple "0" 1, Tuple "1" 1, Tuple "1" 1]
    quickCheck (O.lookup "0" nums == Just 1  <?> "invalid lookup - 0")
    quickCheck (O.lookup "1" nums == Just 2  <?> "invalid lookup - 1")
    quickCheck (O.lookup "2" nums == Nothing <?> "invalid lookup - 2")

  log "toUnfoldable . fromFoldable = id"
  quickCheck $ \arr -> let f x = O.toUnfoldable (O.fromFoldable x)
                       in f (f arr) == f (arr :: L.List (Tuple String Int)) <?> show arr

  log "fromFoldable . toUnfoldable = id"
  quickCheck $ \(TestObject m) ->
    let f m1 = O.fromFoldable ((O.toUnfoldable m1) :: L.List (Tuple String Int)) in
    O.toUnfoldable (f m) == (O.toUnfoldable m :: L.List (Tuple String Int)) <?> show m

  log "fromFoldableWith const = fromFoldable"
  quickCheck $ \arr -> O.fromFoldableWith const arr ==
                       O.fromFoldable (arr :: L.List (Tuple String Int)) <?> show arr

  log "fromFoldableWith (<>) = fromFoldable . collapse with (<>) . group on fst"
  quickCheck $ \arr ->
    let combine (Tuple s a) (Tuple t b) = (Tuple s $ b <> a)
        foldl1 g = unsafePartial \(L.Cons x xs) -> foldl g x xs
        f = O.fromFoldable <<< map (foldl1 combine <<< NEL.toList) <<<
            L.groupBy ((==) `on` fst) <<< L.sortBy (compare `on` fst) in
    O.fromFoldableWith (<>) arr == f (arr :: L.List (Tuple String String)) <?> show arr

  log "Lookup from union"
  quickCheck $ \(TestObject m1) (TestObject m2) k ->
    O.lookup k (O.union m1 m2) == (case O.lookup k m1 of
      Nothing -> O.lookup k m2
      Just v -> Just (number v)) <?> ("m1: " <> show m1 <> ", m2: " <> show m2 <> ", k: " <> show k <> ", v1: " <> show (O.lookup k m1) <> ", v2: " <> show (O.lookup k m2) <> ", union: " <> show (O.union m1 m2))

  log "Union is idempotent"
  quickCheck $ \(TestObject m1) (TestObject m2) ->
    (m1 `O.union` m2) == ((m1 `O.union` m2) `O.union` (m2 :: O.Object Int)) <?> (show (O.size (m1 `O.union` m2)) <> " != " <> show (O.size ((m1 `O.union` m2) `O.union` m2)))

  log "fromFoldable = zip keys values"
  quickCheck $ \(TestObject m) -> O.toUnfoldable m == A.zipWith Tuple (O.keys m) (O.values m :: Array Int)

  log "mapWithKey is correct"
  quickCheck $ \(TestObject m :: TestObject Int) -> let
    f k v = k <> show v
    resultViaMapWithKey = m # O.mapWithKey f
    resultViaLists = m # O.toUnfoldable # map (\(Tuple k v) → Tuple k (f k v)) # (O.fromFoldable :: forall a. L.List (Tuple String a) -> O.Object a)
    in resultViaMapWithKey === resultViaLists

  log "foldl = foldlWithIndex <<< const"
  quickCheck \(TestObject m :: TestObject String) ->
    let f z v = z <> "," <> v
    in foldl f "" m === foldlWithIndex (const f) "" m

  log "foldr = foldrWithIndex <<< const"
  quickCheck \(TestObject m :: TestObject String) ->
    let f v z = v <> "," <> z
    in foldr f "" m === foldrWithIndex (const f) "" m

  log "foldlWithIndex = foldrWithIndex with flipped operation"
  quickCheck \(TestObject m :: TestObject String) ->
    let f k z v = z <> "," <> k <> ":" <> v
        g k v z = k <> ":" <> v <> "," <> z
    in foldlWithIndex f "" m <> "," === "," <> foldrWithIndex g "" m

  log "foldMapWithIndex f ~ traverseWithIndex (\\k v -> tell (f k v))"
  quickCheck \(TestObject m :: TestObject Int) ->
    let f k v = "(" <> "k" <> "," <> show v <> ")"
        resultA = foldMapWithIndex f m
        resultB = snd (runWriter (traverseWithIndex (\k v -> tell (f k v)) m))
    in resultA === resultB

  log "traverse = traverseWithIndex <<< const (for m = Writer)"
  quickCheck \(TestObject m :: TestObject String) ->
    runWriter (traverse tell m) ===
    runWriter (traverseWithIndex (const tell) m)

  log "sequence works (for m = Array)"
  quickCheck \(TestObject mOfSmallArrays :: TestObject (SmallArray Int)) ->
    let m                 = (\(SmallArray a) -> a) <$> mOfSmallArrays
        Tuple keys values = A.unzip (toAscArray m)
        resultViaArrays   = (O.fromFoldable <<< A.zip keys) <$> sequence values
    in  A.sort (sequence m) === A.sort (resultViaArrays)

  log "sequence works (for m = Maybe)"
  quickCheck \(TestObject m :: TestObject (Maybe Int)) ->
    let Tuple keys values = A.unzip (toAscArray m)
        resultViaArrays   = (O.fromFoldable <<< A.zip keys) <$> sequence values
    in  sequence m === resultViaArrays

  log "Bug #63: accidental observable mutation in foldMap"
  quickCheck \(TestObject m) ->
    let lhs = go m
        rhs = go m
        go :: O.Object (Array Ordering) -> Array Ordering
        go = O.foldMap (const identity)
    in lhs == rhs <?> ("lhs: " <> show lhs <> ", rhs: " <> show rhs)

  log "fromFoldable stack safety"
  do
    let entries = 100000
    assertEqual
      { expected: entries
      , actual: O.size (O.fromFoldable (map (\x -> Tuple (show x) x) (A.range 1 entries)))
      }
