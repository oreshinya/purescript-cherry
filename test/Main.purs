module Test.Main where

import Prelude

import Effect (Effect)
import Test.Diff (assertDiffAlgorithm)
import Test.Router.Parser (testRouteParser)
import Test.TestHelper (enableJSDOM)



main :: Effect Unit
main = do
  enableJSDOM
  assertDiffAlgorithm
  testRouteParser
