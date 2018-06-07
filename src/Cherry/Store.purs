module Cherry.Store
  ( Store
  , createStore
  , subscribe
  , unsubscribe
  , select
  , reduce
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref, modify_, new, read)
import Effect.Unsafe (unsafePerformEffect)
import SimpleEmitter as S



data Event = Emit

derive instance eqEvent :: Eq Event

derive instance ordEvent :: Ord Event



newtype Store s = Store
  { emitter :: S.Emitter Event
  , stateRef :: Ref s
  }



createStore :: forall s. s -> Store s
createStore state = unsafePerformEffect do
  emitter <- S.createEmitter
  stateRef <- new state
  pure $ Store { emitter, stateRef }



subscribe
  :: forall s
   . Effect Unit
  -> Store s
  -> Effect Unit
subscribe f (Store s) = S.subscribe Emit f s.emitter



unsubscribe
  :: forall s
   . Store s
  -> Effect Unit
unsubscribe (Store s) = S.unsubscribe Emit s.emitter



select
  :: forall s a
   . Store s
  -> (s -> a)
  -> Effect a
select (Store s) f = map f $ read s.stateRef



reduce
  :: forall s
   . Store s
  -> (s -> s)
  -> Effect Unit
reduce (Store s) f = do
  modify_ f s.stateRef
  S.emit Emit s.emitter
