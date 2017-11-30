module Data.List.Fast.Types
  ( FastList(..)
  , (:)
  ) where

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Eq (class Eq1)
import Data.Filterable (class Filterable)
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2)
import Data.List (List)
import Data.Maybe (Maybe, isJust)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.Unfoldable (class Unfoldable)
import Data.Witherable (class Witherable)

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
  filter = filter
  filterMap = mapMaybe
  partition = partitionImpl
  partitionMap = partitionMapImpl isLeft fromLeft fromRight

foreign import map :: forall a b. (a -> b) -> List a -> List b

foreign import filter :: forall a. (a -> Boolean) -> List a -> List a

mapMaybe :: forall a b. (a -> Maybe b) -> FastList a -> FastList b
mapMaybe = runFn3 filterMapImpl isJust fromJust

foreign import mapMaybeImpl
  :: forall a b
   . Fn4 (Maybe b -> Boolean)
         (Maybe b -> b)
         (a -> Maybe b)
         FastList a
         FastList b

foreign import partitionImpl
  :: forall a
   . (a -> Boolean)
  -> FastList a
  -> { no :: FastList a, yes :: FastList b }
   

foreign import partitionMapImpl
  :: forall a l r
   . Fn5 (Either a b -> Boolean)
         (Either a b -> a)
         (Either a b -> b)
         FastList a
         { left :: FastList l, right :: FastList r}

