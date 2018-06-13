module Test.Main where

import Prelude

import Effect (Effect)
import Test.Diff (assertDiffAlgorithm)
import Test.Router.Parser (assertRouteParser)
import Test.Style (assertStyle)
import Test.TestHelper (enableJSDOM)



main :: Effect Unit
main = do
  enableJSDOM
  assertDiffAlgorithm
  assertRouteParser
  assertStyle
