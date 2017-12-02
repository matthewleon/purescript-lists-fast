-- | Faster replacements for common methods on linked lists,
-- | which exploit mutation under the hood.

module Data.List.Fast
  ( FastList(..)
  , map
  , filter
  ) where

import Prelude hiding (map)

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Either (Either, fromLeft, fromRight, isLeft)
import Data.Eq (class Eq1)
import Data.Filterable (class Filterable)
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2, Fn4, Fn5, runFn4, runFn5)
import Data.List (List)
import Data.Maybe (Maybe, fromJust, isJust)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.Unfoldable (class Unfoldable)
import Data.Witherable (class Witherable)
import Partial.Unsafe (unsafePartial)

newtype FastList a = FastList (List a)
derive instance newtypeFastList :: Newtype (FastList a) _
derive newtype instance showFastList :: Show a => Show (FastList a)
derive newtype instance eqFastList :: Eq a => Eq (FastList a)
derive newtype instance eq1FastList :: Eq1 FastList
derive newtype instance ordFastList :: Ord a => Ord (FastList a)
derive newtype instance ord1FastList :: Ord1 FastList
derive newtype instance semigroupFastList :: Semigroup (FastList a)
derive newtype instance monoidFastList :: Monoid (FastList a)
derive newtype instance foldableFastList :: Foldable FastList
derive newtype instance unfoldableFastList :: Unfoldable FastList
derive newtype instance traversableFastList :: Traversable FastList
derive newtype instance applyFastList :: Apply FastList
derive newtype instance applicativeFastList :: Applicative FastList
derive newtype instance bindFastList :: Bind FastList
derive newtype instance monadFastList :: Monad FastList
derive newtype instance altFastList :: Alt FastList
derive newtype instance plusFastList :: Plus FastList
derive newtype instance alternativeFastList :: Alternative FastList
derive newtype instance monadZeroFastList :: MonadZero FastList
derive newtype instance monadPlusFastList :: MonadPlus FastList
derive newtype instance extendFastList :: Extend FastList
derive newtype instance witherableFastList :: Witherable FastList

instance functorFastList :: Functor FastList where
  map f (FastList xs) = FastList (map f xs)

instance filterableFastList :: Filterable FastList where
  filter f (FastList xs) = FastList (filter f xs)
  filterMap = mapMaybe
  partition = partitionImpl
  partitionMap
    = runFn5 partitionMapImpl
             isLeft
             (unsafePartial fromLeft)
             (unsafePartial fromRight)

foreign import map :: forall a b. (a -> b) -> List a -> List b

foreign import filter :: forall a. (a -> Boolean) -> List a -> List a

mapMaybe :: forall a b. (a -> Maybe b) -> FastList a -> FastList b
mapMaybe = runFn4 mapMaybeImpl isJust (unsafePartial fromJust)

foreign import mapMaybeImpl
  :: forall a b
   . Fn4 (Maybe b -> Boolean)
         (Maybe b -> b)
         (a -> Maybe b)
         (FastList a)
         (FastList b)

foreign import partitionImpl
  :: forall a
   . (a -> Boolean)
  -> FastList a
  -> { no :: FastList a, yes :: FastList a }
   

foreign import partitionMapImpl
  :: forall a l r
   . Fn5 (Either l r -> Boolean)
         (Either l r -> l)
         (Either l r -> r)
         (a -> Either l r)
         (FastList a)
         { left :: FastList l, right :: FastList r}

foreign import zipWith :: forall a b c. Fn2 a b c -> FastList a -> FastList b -> FastList c
