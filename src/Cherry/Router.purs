module Cherry.Router
  ( router
  , navigateTo
  , redirectTo
  , goForward
  , goBack
  , module Exports
  ) where

import Prelude

import Cherry.Router.Parser as Exports
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventTarget, EventType)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (popstate)
import DOM.HTML.History (DocumentTitle(..), URL(..), back, forward, pushState, replaceState)
import DOM.HTML.Location (pathname, search)
import DOM.HTML.Types (HISTORY, Window, windowToEventTarget)
import DOM.HTML.Window (history, location)
import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..))



router
  :: forall e
   . (String -> Eff (dom :: DOM | e) Unit)
  -> Eff (dom :: DOM | e) Unit
router matcher = do
  handler
  eventWindow >>= addEventListener popstate listener false
    where
      handler = do
        l <- window >>= location
        path <- (<>) <$> pathname l <*> search l
        matcher path
      listener = eventListener (\_ -> handler)



navigateTo
  :: forall e
   . String
  -> Eff (dom :: DOM, history :: HISTORY | e) Unit
navigateTo url = do
  window >>= history >>= pushState null (DocumentTitle "") (URL url)
  window >>= dispatchEvent popstate



redirectTo
  :: forall e
   . String
  -> Eff (dom :: DOM, history :: HISTORY | e) Unit
redirectTo url = do
  window >>= history >>= replaceState null (DocumentTitle "") (URL url)
  window >>= dispatchEvent popstate



goForward :: forall e. Eff (dom :: DOM, history :: HISTORY | e) Unit
goForward = window >>= history >>= forward



goBack :: forall e. Eff (dom :: DOM, history :: HISTORY | e) Unit
goBack = window >>= history >>= back



eventWindow :: forall e. Eff (dom :: DOM | e) EventTarget
eventWindow = windowToEventTarget <$> window



null :: Foreign
null = toForeign Nothing



foreign import dispatchEvent
  :: forall e
   . EventType
  -> Window
  -> Eff (dom :: DOM | e) Unit
