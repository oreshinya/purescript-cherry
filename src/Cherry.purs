module Cherry where

import Prelude

import Cherry.Renderer (Renderer, runRenderer)
import Cherry.Store (Store, subscribe)
import Data.Foldable (sequence_)
import Effect (Effect)



mount
  :: forall s
   . Store s
  -> Renderer s
  -> Array (Effect Unit)
  -> Effect Unit
mount store renderer subscriptions = do
  sequence_ subscriptions
  subscribe (runRenderer store renderer) store
  runRenderer store renderer
