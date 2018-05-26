module Test.TestHelper where

import Prelude

import Control.Monad.Eff (Eff)

foreign import enableJSDOM :: forall e. Eff e Unit

foreign import clearBody :: forall e. Eff e Unit
