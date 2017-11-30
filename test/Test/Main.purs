module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Assert (ASSERT)
import Test.Data.List.Fast (testList)

main :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
main = testFastList
