module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Test.Assert (ASSERT)
import Test.Diff (assertDiffAlgorithm)
import Test.TestHelper (enableJSDOM)



main :: forall e. Eff (dom :: DOM, assert :: ASSERT | e) Unit
main = do
  enableJSDOM
  assertDiffAlgorithm
