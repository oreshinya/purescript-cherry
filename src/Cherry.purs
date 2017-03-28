module Cherry
  ( CHERRY
  , Config(..)
  , AppEffects
  , View
  , Subscription
  , app
  , select
  , reduce
  , router
  , navigateTo
  , redirectTo
  , goForward
  , goBack
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (newRef, readRef, modifyRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Data.Foldable (sequence_)
import Data.Foreign.Null (writeNull)
import Data.List (List(..), (!!), (:))
import Data.Maybe (maybe)
import Data.Nullable (toMaybe)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType, EventTarget)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (popstate)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState, replaceState, forward, back)
import DOM.HTML.Location (pathname, search)
import DOM.HTML.Types (Window, HISTORY, windowToEventTarget, htmlDocumentToParentNode)
import DOM.HTML.Window (document, location, history)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (elementToNode)
import VOM (VNode, patch)


foreign import data CHERRY :: !

type AppEffects e = (console :: CONSOLE, cherry :: CHERRY, dom :: DOM, history :: HISTORY | e)

type View e s = s -> VNode e

type Subscription e = Eff e Unit

newtype Config e s = Config
  { selector :: String
  , state :: s
  , view :: View e s
  , subscriptions :: Array (Subscription e)
  }



foreign import select :: forall e s. (s -> s) -> Eff (cherry :: CHERRY | e) s

foreign import reduce :: forall e s. (s -> s) -> Eff (cherry :: CHERRY | e) Unit

foreign import requestAnimationFrame :: forall e. Eff (dom :: DOM | e) Unit ->
                                        Window ->
                                        Eff (dom :: DOM | e) Unit

foreign import dispatchEvent :: forall e. EventType ->
                                Window ->
                                Eff (dom :: DOM | e) Unit



app :: forall e s. Config (AppEffects e) s -> Eff (AppEffects e) Unit
app (Config { selector, state, view, subscriptions }) = do
  reduce (\_ -> state)
  historyRef <- unsafeRunRef $ newRef Nil
  sequence_ subscriptions
  draw $ render historyRef
    where
      container =
        let doc = htmlDocumentToParentNode <$> (window >>= document)
        in map elementToNode <<< toMaybe <$> (doc >>= querySelector selector)

      warn = log $ selector <> "is not found."

      addHistory historyRef currentState = do
        modifyRef historyRef (\h -> (view currentState) : h)
        readRef historyRef

      render historyRef = do
        target <- container
        currentState <- select (\s -> s)
        history <- unsafeRunRef $ addHistory historyRef currentState
        maybe warn (patch (history !! 1) (history !! 0)) target

      draw renderer = do
        renderer
        window >>= requestAnimationFrame (draw renderer)



router :: forall e. (String -> Eff (dom :: DOM | e) Unit) ->
          Eff (dom :: DOM | e) Unit
router matcher = do
  handler
  eventWindow >>= addEventListener popstate listener false
    where
      path = do
        l <- window >>= location
        (<>) <$> pathname l <*> search l
      handler = path >>= matcher
      listener = eventListener (\_ -> handler)



navigateTo :: forall e. String ->
              Eff (dom :: DOM, history :: HISTORY | e) Unit
navigateTo url = do
  window >>= history >>= pushState writeNull (DocumentTitle "") (URL url)
  window >>= dispatchEvent popstate



redirectTo :: forall e. String ->
              Eff (dom :: DOM, history :: HISTORY | e) Unit
redirectTo url = do
  window >>= history >>= replaceState writeNull (DocumentTitle "") (URL url)
  window >>= dispatchEvent popstate



goForward :: forall e. Eff (dom :: DOM, history :: HISTORY | e) Unit
goForward = window >>= history >>= forward



goBack :: forall e. Eff (dom :: DOM, history :: HISTORY | e) Unit
goBack = window >>= history >>= back



eventWindow :: forall e. Eff (dom :: DOM | e) EventTarget
eventWindow = windowToEventTarget <$> window
