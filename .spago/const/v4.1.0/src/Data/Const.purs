module Data.Const where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Functor.Contravariant (class Contravariant)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)

-- | The `Const` type constructor, which wraps its first type argument
-- | and ignores its second. That is, `Const a b` is isomorphic to `a`
-- | for any `b`.
-- |
-- | `Const` has some useful instances. For example, the `Applicative`
-- | instance allows us to collect results using a `Monoid` while
-- | ignoring return values.
newtype Const a b = Const a

derive instance newtypeConst :: Newtype (Const a b) _

derive newtype instance eqConst :: Eq a => Eq (Const a b)

derive instance eq1Const :: Eq a => Eq1 (Const a)

derive newtype instance ordConst :: Ord a => Ord (Const a b)

derive instance ord1Const :: Ord a => Ord1 (Const a)

derive newtype instance boundedConst :: Bounded a => Bounded (Const a b)

instance showConst :: Show a => Show (Const a b) where
  show (Const x) = "(Const " <> show x <> ")"

instance semigroupoidConst :: Semigroupoid Const where
  compose _ (Const x) = Const x

derive newtype instance semigroupConst :: Semigroup a => Semigroup (Const a b)

derive newtype instance monoidConst :: Monoid a => Monoid (Const a b)

derive newtype instance semiringConst :: Semiring a => Semiring (Const a b)

derive newtype instance ringConst :: Ring a => Ring (Const a b)

derive newtype instance euclideanRingConst :: EuclideanRing a => EuclideanRing (Const a b)

derive newtype instance commutativeRingConst :: CommutativeRing a => CommutativeRing (Const a b)

derive newtype instance heytingAlgebraConst :: HeytingAlgebra a => HeytingAlgebra (Const a b)

derive newtype instance booleanAlgebraConst :: BooleanAlgebra a => BooleanAlgebra (Const a b)

derive instance functorConst :: Functor (Const a)

instance bifunctorConst :: Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

instance functorWithIndexConst :: FunctorWithIndex Void (Const a) where
  mapWithIndex _ (Const x) = Const x

instance invariantConst :: Invariant (Const a) where
  imap = imapF

instance contravariantConst :: Contravariant (Const a) where
  cmap _ (Const x) = Const x

instance applyConst :: Semigroup a => Apply (Const a) where
  apply (Const x) (Const y) = Const (x <> y)

instance applicativeConst :: Monoid a => Applicative (Const a) where
  pure _ = Const mempty

instance foldableConst :: Foldable (Const a) where
  foldr _ z _ = z
  foldl _ z _ = z
  foldMap _ _ = mempty

instance foldableWithIndexConst :: FoldableWithIndex Void (Const a) where
  foldrWithIndex _ z _ = z
  foldlWithIndex _ z _ = z
  foldMapWithIndex _ _ = mempty

instance bifoldableConst :: Bifoldable Const where
  bifoldr f _ z (Const a) = f a z
  bifoldl f _ z (Const a) = f z a
  bifoldMap f _ (Const a) = f a

instance traversableConst :: Traversable (Const a) where
  traverse _ (Const x) = pure (Const x)
  sequence (Const x) = pure (Const x)

instance traversableWithIndexConst :: TraversableWithIndex Void (Const a) where
  traverseWithIndex _ (Const x) = pure (Const x)

instance bitraversableConst :: Bitraversable Const where
  bitraverse f _ (Const a) = Const <$> f a
  bisequence (Const a) = Const <$> a
