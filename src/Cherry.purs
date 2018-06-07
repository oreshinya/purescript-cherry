module Cherry where

import Prelude
import Effect (Effect)
import Cherry.Store (Store, subscribe)
import Cherry.Renderer (Renderer, runRenderer)
import Data.Foldable (sequence_)



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
