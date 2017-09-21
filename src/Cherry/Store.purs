module Cherry.Store
  ( Store
  , createStore
  , subscribe
  , unsubscribe
  , select
  , reduce
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import SimpleEmitter as S



data Event = Emit

derive instance eqEvent :: Eq Event

derive instance ordEvent :: Ord Event



newtype Store e s = Store
  { emitter :: S.Emitter e Event
  , stateRef :: Ref s
  }



createStore :: forall e s. s -> Store (ref :: REF | e) s
createStore state = unsafePerformEff do
  emitter <- S.createEmitter
  stateRef <- newRef state
  pure $ Store { emitter, stateRef }



subscribe
  :: forall e s
   . Eff (ref :: REF | e) Unit
  -> Store (ref :: REF | e) s
  -> Eff (ref :: REF | e) Unit
subscribe f (Store s) = S.subscribe Emit f s.emitter



unsubscribe
  :: forall e s
   . Store (ref :: REF | e) s
  -> Eff (ref :: REF | e) Unit
unsubscribe (Store s) = S.unsubscribe Emit s.emitter



select
  :: forall e s a
   . Store (ref :: REF | e) s
  -> (s -> a)
  -> Eff (ref :: REF | e) a
select (Store s) f = map f $ readRef s.stateRef



reduce
  :: forall e s
   . Store (ref :: REF | e) s
  -> (s -> s)
  -> Eff (ref :: REF | e) Unit
reduce (Store s) f = do
  modifyRef s.stateRef f
  S.emit Emit s.emitter
