module Test.TestHelper where

import Prelude

import Effect (Effect)

foreign import enableJSDOM :: Effect Unit

foreign import clearBody :: Effect Unit
