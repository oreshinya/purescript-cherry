module Cherry where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Cherry.Store (Store, subscribe)
import Cherry.Renderer (Renderer, runRenderer)
import DOM (DOM)
import Data.Foldable (sequence_)



mount :: forall e s.
         Store (dom :: DOM, console :: CONSOLE, ref :: REF | e) s ->
         Renderer (dom :: DOM, console :: CONSOLE, ref :: REF | e) s ->
         Array (Eff (dom :: DOM, console :: CONSOLE, ref :: REF | e) Unit) ->
         Eff (dom :: DOM, console :: CONSOLE, ref :: REF | e) Unit
mount store renderer subscriptions = do
  sequence_ subscriptions
  subscribe (runRenderer store renderer) store
  runRenderer store renderer
