module Data.Functor.Coproduct where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)

-- | `Coproduct f g` is the coproduct of two functors `f` and `g`
newtype Coproduct f g a = Coproduct (Either (f a) (g a))

-- | Left injection
left :: forall f g a. f a -> Coproduct f g a
left fa = Coproduct (Left fa)

-- | Right injection
right :: forall f g a. g a -> Coproduct f g a
right ga = Coproduct (Right ga)

-- | Eliminate a coproduct by providing eliminators for the left and
-- | right components
coproduct :: forall f g a b. (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f _ (Coproduct (Left a)) = f a
coproduct _ g (Coproduct (Right b)) = g b

-- | Change the underlying functors in a coproduct
bihoistCoproduct
  :: forall f g h i
   . (f ~> h)
  -> (g ~> i)
  -> Coproduct f g
  ~> Coproduct h i
bihoistCoproduct natF natG (Coproduct e) = Coproduct (bimap natF natG e)

derive instance newtypeCoproduct :: Newtype (Coproduct f g a) _

instance eqCoproduct :: (Eq1 f, Eq1 g, Eq a) => Eq (Coproduct f g a) where
  eq = eq1

instance eq1Coproduct :: (Eq1 f, Eq1 g) => Eq1 (Coproduct f g) where
  eq1 (Coproduct x) (Coproduct y) =
    case x, y of
      Left fa, Left ga -> eq1 fa ga
      Right fa, Right ga -> eq1 fa ga
      _, _ -> false

instance ordCoproduct :: (Ord1 f, Ord1 g, Ord a) => Ord (Coproduct f g a) where
  compare = compare1

instance ord1Coproduct :: (Ord1 f, Ord1 g) => Ord1 (Coproduct f g) where
  compare1 (Coproduct x) (Coproduct y) =
    case x, y of
      Left fa, Left ga -> compare1 fa ga
      Left _, _ -> LT
      _, Left _ -> GT
      Right fa, Right ga -> compare1 fa ga

instance showCoproduct :: (Show (f a), Show (g a)) => Show (Coproduct f g a) where
  show (Coproduct (Left fa)) = "(left " <> show fa <> ")"
  show (Coproduct (Right ga)) = "(right " <> show ga <> ")"

instance functorCoproduct :: (Functor f, Functor g) => Functor (Coproduct f g) where
  map f (Coproduct e) = Coproduct (bimap (map f) (map f) e)

instance functorWithIndexCoproduct :: (FunctorWithIndex a f, FunctorWithIndex b g) => FunctorWithIndex (Either a b) (Coproduct f g) where
  mapWithIndex f (Coproduct e) = Coproduct (bimap (mapWithIndex (f <<< Left)) (mapWithIndex (f <<< Right)) e)

instance extendCoproduct :: (Extend f, Extend g) => Extend (Coproduct f g) where
  extend f = Coproduct <<< coproduct
    (Left <<< extend (f <<< Coproduct <<< Left))
    (Right <<< extend (f <<< Coproduct <<< Right))

instance comonadCoproduct :: (Comonad f, Comonad g) => Comonad (Coproduct f g) where
  extract = coproduct extract extract

instance foldableCoproduct :: (Foldable f, Foldable g) => Foldable (Coproduct f g) where
  foldr f z = coproduct (foldr f z) (foldr f z)
  foldl f z = coproduct (foldl f z) (foldl f z)
  foldMap f = coproduct (foldMap f) (foldMap f)

instance foldableWithIndexCoproduct :: (FoldableWithIndex a f, FoldableWithIndex b g) => FoldableWithIndex (Either a b) (Coproduct f g) where
  foldrWithIndex f z = coproduct (foldrWithIndex (f <<< Left) z) (foldrWithIndex (f <<< Right) z)
  foldlWithIndex f z = coproduct (foldlWithIndex (f <<< Left) z) (foldlWithIndex (f <<< Right) z)
  foldMapWithIndex f = coproduct (foldMapWithIndex (f <<< Left)) (foldMapWithIndex (f <<< Right))

instance traversableCoproduct :: (Traversable f, Traversable g) => Traversable (Coproduct f g) where
  traverse f = coproduct
    (map (Coproduct <<< Left) <<< traverse f)
    (map (Coproduct <<< Right) <<< traverse f)
  sequence = coproduct
    (map (Coproduct <<< Left) <<< sequence)
    (map (Coproduct <<< Right) <<< sequence)

instance traversableWithIndexCoproduct :: (TraversableWithIndex a f, TraversableWithIndex b g) => TraversableWithIndex (Either a b) (Coproduct f g) where
  traverseWithIndex f = coproduct
    (map (Coproduct <<< Left) <<< traverseWithIndex (f <<< Left))
    (map (Coproduct <<< Right) <<< traverseWithIndex (f <<< Right))
