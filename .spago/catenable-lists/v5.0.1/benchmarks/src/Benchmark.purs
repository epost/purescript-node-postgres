module Test.Benchmark (runBenchmarks) where

import Prelude

import Control.Monad.Eff

import Data.Foldable
import Data.Maybe
import Data.Tuple

import Benchotron.Core
import Benchotron.UI.Console

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Data.Array as A
import qualified Data.CatQueue as Q
import qualified Data.CatList as C
import qualified Data.List as L
import qualified Data.Sequence as S

(..) = A.(..)

consBenchmark :: Benchmark
consBenchmark = mkBenchmark
  { slug: "cons"
  , title: "Add elements to the beginning of the lists"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be added"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "CatList" (foldr C.cons C.empty)
               , benchFn "List" (foldr L.(:) L.Nil)
               , benchFn "Sequence" (foldr S.cons S.empty)
               ]
  }

snocBenchmark :: Benchmark
snocBenchmark = mkBenchmark
  { slug: "snoc"
  , title: "Add elements to the end of the lists"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be added"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "CatQueue" (foldl Q.snoc Q.empty)
               , benchFn "CatList" (foldl C.snoc C.empty)
               , benchFn "List" (foldl L.snoc L.Nil)
               , benchFn "Sequence" (foldl S.snoc S.empty)
               ]
  }

unconsBenchmark :: Benchmark
unconsBenchmark = mkBenchmark
  { slug: "uncons"
  , title: "Remove elements from the front of the list"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be removed"
  , inputsPerSize: 1
  , gen: \n -> { queue: _
               , cat: _
               , list: _
               , sequence: _
               } <$> (randomCatQueue n)
                 <*> (randomCatList n)
                 <*> (randomList n)
                 <*> (randomSequence n)
  , functions: [ benchFn "CatQueue" \a -> whileUncons isJust (\(Just (Tuple _ x)) -> x) Q.uncons a.queue
               , benchFn "CatList" \a -> whileUncons isJust (\(Just (Tuple _ x)) -> x) C.uncons a.cat
               , benchFn "List" \a -> whileUncons isJust (\(Just x) -> x.tail) L.uncons a.list
               , benchFn "Sequence" \a -> whileUncons isJust (\(Just (Tuple _ x)) -> x) S.uncons a.sequence
               ]
  }

appendBenchmark :: Benchmark
appendBenchmark = mkBenchmark
  { slug: "append"
  , title: "Add all elements from one list to the end of another list"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements in the list"
  , inputsPerSize: 1
  , gen: \n -> { cat: _
               , list: _
               , sequence: _
               } <$> (randomCatList n)
                 <*> (randomList n)
                 <*> (randomSequence n)
  , functions: [ benchFn "CatList" \a -> C.append a.cat a.cat
               , benchFn "List" \a -> a.list <> a.list
               , benchFn "Sequence" \a -> S.append a.sequence a.sequence
               ]
  }

snocUnconsBenchmark :: Benchmark
snocUnconsBenchmark = mkBenchmark
  { slug: "snoc-uncons"
  , title: "Add elements to the end of the lists and then remove them from the front"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be added and removed"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "CatQueue" (foldl (\b a -> fromMaybe Q.empty (snd <$> Q.uncons (Q.snoc b a))) Q.empty)
               , benchFn "CatList" (foldl (\b a -> fromMaybe C.empty (snd <$> C.uncons (C.snoc b a))) C.empty)
               , benchFn "List" (foldl (\b a -> fromMaybe L.Nil ((\x -> x.tail) <$> L.uncons (L.snoc b a))) L.Nil)
               , benchFn "Sequence" (foldl (\b a -> fromMaybe S.empty (snd <$> S.uncons (S.snoc b a))) S.empty)
               ]
  }

consUnconsNBenchmark :: Benchmark
consUnconsNBenchmark = mkBenchmark
  { slug: "cons-uncons-n"
  , title: "Add N elements to the front then remove N elements from the front of the list"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be added and removed"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "CatList" \as -> foldl (\b _ -> fromMaybe C.empty (snd <$> C.uncons b)) (foldr C.cons C.empty as) as
               , benchFn "List" \as -> foldl (\b _ -> fromMaybe L.Nil ((\x -> x.tail) <$> L.uncons b)) (foldr L.(:) L.Nil as) as
               , benchFn "Sequence" \as -> foldl (\b _ -> fromMaybe S.empty (snd <$> S.uncons b)) (foldr S.cons S.empty as) as
               ]
  }

snocUnconsNBenchmark :: Benchmark
snocUnconsNBenchmark = mkBenchmark
  { slug: "snoc-uncons-n"
  , title: "Add N elements to the end then remove N elements from the front of the list"
  , sizes: (1..10) <#> (*100)
  , sizeInterpretation: "Number of elements to be added and removed"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "CatQueue" \as -> foldl (\b _ -> fromMaybe Q.empty (snd <$> Q.uncons b)) (foldl Q.snoc Q.empty as) as
               , benchFn "CatList" \as -> foldl (\b _ -> fromMaybe C.empty (snd <$> C.uncons b)) (foldl C.snoc C.empty as) as
               , benchFn "List" \as -> foldl (\b _ -> fromMaybe L.Nil ((\x -> x.tail) <$> L.uncons b)) (foldl L.snoc L.Nil as) as
               , benchFn "Sequence" \as -> foldl (\b _ -> fromMaybe S.empty (snd <$> S.uncons b)) (foldl S.snoc S.empty as) as
               ]
  }

randomCatQueue :: forall eff. Int -> Gen (Q.CatQueue Number)
randomCatQueue n = (foldl Q.snoc Q.empty) <$> (randomArray n)

randomCatList :: forall eff. Int -> Gen (C.CatList Number)
randomCatList n = (foldl C.snoc C.empty) <$> (randomArray n)

randomList :: forall eff. Int -> Gen (L.List Number)
randomList n = (foldl L.snoc L.Nil) <$> (randomArray n)

randomSequence :: forall eff. Int -> Gen (S.Seq Number)
randomSequence n = (foldl S.snoc S.empty) <$> (randomArray n)

randomArray :: forall eff. Int -> Gen (Array Number)
randomArray = flip vectorOf arbitrary

foreign import whileUncons :: forall f a uncons. (uncons -> Boolean) -> (uncons -> f a) -> (f a -> uncons) -> f a -> Unit

runBenchmarks = runSuite [ consBenchmark
                         , snocBenchmark
                         , unconsBenchmark
                         , appendBenchmark
                         , snocUnconsBenchmark
                         , consUnconsNBenchmark
                         , snocUnconsNBenchmark
                         ]
