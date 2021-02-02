## Module Control.Monad.Morph

A port of Haskell's [mmorph library](http://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html)

#### `MFunctor`

``` purescript
class MFunctor t where
  hoist :: forall m n. Monad m => m ~> n -> (t m) ~> (t n)
```

##### Instances
``` purescript
MFunctor (ExceptT e)
MFunctor MaybeT
MFunctor (ReaderT r)
MFunctor (WriterT w)
MFunctor (StateT s)
MFunctor (RWST r w s)
(Functor f) => MFunctor (Compose f)
MFunctor (Product f)
```

#### `generalize`

``` purescript
generalize :: forall m a. Monad m => Identity a -> m a
```

#### `MMonad`

``` purescript
class (MFunctor t, MonadTrans t) <= MMonad t where
  embed :: forall n m b. Monad n => (forall a. m a -> t n a) -> t m b -> t n b
```

##### Instances
``` purescript
MMonad (ExceptT e)
MMonad MaybeT
MMonad (ReaderT r)
(Monoid w) => MMonad (WriterT w)
```

#### `squash`

``` purescript
squash :: forall m t. (Monad m, MMonad t) => (t (t m)) ~> (t m)
```

#### `(>|>)`

``` purescript
infixr 2 composeKleisliRight as >|>
```

#### `(<|<)`

``` purescript
infixl 2 composeKleisliLeft as <|<
```

#### `(=<|)`

``` purescript
infixr 2 embed as =<|
```

#### `(|>=)`

``` purescript
infixl 2 flipEmbed as |>=
```

#### `composeKleisliRight`

``` purescript
composeKleisliRight :: forall m1 m2 m3 t. (MMonad t, Monad m3) => m1 ~> (t m2) -> m2 ~> (t m3) -> m1 ~> (t m3)
```

#### `composeKleisliLeft`

``` purescript
composeKleisliLeft :: forall m1 m2 m3 t. (MMonad t, Monad m3) => m2 ~> (t m3) -> m1 ~> (t m2) -> m1 ~> (t m3)
```

#### `flipEmbed`

``` purescript
flipEmbed :: forall t m n a. (MMonad t, Monad n) => t m a -> m ~> (t n) -> t n a
```


