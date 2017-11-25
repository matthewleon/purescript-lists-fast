-- | Faster replacements for common methods on linked lists,
-- | which exploit mutation under the hood.

module Data.List.Fast where

import Data.Function.Uncurried (Fn2, Fn4, runFn4)
import Data.List (List(..))

map :: forall a b. (a -> b) -> List a -> List b
map = runFn4 mapImpl Nil Cons

foreign import mapImpl
  :: forall a b
   . Fn4 (List b)
         (b -> List b -> List b)
         (a -> b)
         (List a)
         (List b)

foreign import filter :: forall a. (a -> Boolean) -> List a -> List a

foreign import zipWith :: forall a b c. Fn2 a b c -> List a -> List b -> List c
