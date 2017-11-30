-- | Faster replacements for common methods on linked lists,
-- | which exploit mutation under the hood.

module Data.List.Fast
  ( module Data.List.Types
  , toUnfoldable
  , fromFoldable

  , singleton
  , (..), range
  , some
  , someRec
  , many
  , manyRec

  , null
  , length

  , snoc
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc

  , (!!), index
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt
  , alterAt

  , reverse
  , concat
  , concatMap
  , filter
  , filterM
  , mapMaybe
  , catMaybes
  , mapWithIndex

  , sort
  , sortBy

  , Pattern(..)
  , stripPrefix
  , slice
  , take
  , takeEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhile
  , span
  , group
  , group'
  , groupBy
  , partition

  , nub
  , nubBy
  , union
  , unionBy
  , delete
  , deleteBy
  , (\\), difference
  , intersect
  , intersectBy

  , zipWith
  , zipWithA
  , zip
  , unzip

  , transpose

  , foldM

  , module Exports
  ) where

import Prelude hiding (map)

import Data.List as L
import Data.Newtype (unwrap)

toUnfoldable :: forall f. Unfoldable f => FastList ~> f
toUnfoldable = L.toUnfoldable <<< unwrap

fromFoldable :: forall f. Foldable f => f ~> FastList
fromFoldable = wrap <<< L.fromFoldable

singleton :: forall a. a -> FastList a
singleton = wrap <<< L.singleton

infix 8 range as ..

range :: Int -> Int -> List Int
range = wrap <<< L.range

--TODO
--some :: forall f a. Alternative f => Lazy (f (FastList a)) => f a -> f (FastList a)

--TODO
--someRec :: forall f a. MonadRec f => Alternative f => f a -> f (FastList a)

--TODO
--many :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)

--TODO
--manyRec :: forall f a. MonadRec f => Alternative f => f a -> f (List a)

null :: forall a. FastList a -> Boolean
null = L.null <<< unwrap

length :: forall a. FastList a -> Int
length = L.length <<< unwrap

snoc :: forall a. FastList a -> a -> FastList a
snoc (FastList xs) x = FastList (L.snoc xs x)

insert :: forall a. Ord a => a -> FastList a -> FastList a
insert x (FastList xs) = FastList (L.insert x xs)

insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
insertBy cmp x (FastList xs) = FastList (L.insertBy cmp x xs)

head :: FastList ~> Maybe
head = L.head <<< unwrap

last :: FastList ~> Maybe
last = L.last <<< unwrap

tail :: forall a. FastList a -> Maybe (FastList a)
tail (FastList xs) = FastList <$> L.tail xs

foreign import zipWith :: forall a b c. Fn2 a b c -> FastList a -> FastList b -> FastList c
