module Cherry.Router
  ( router
  , navigateTo
  , redirectTo
  , goForward
  , goBack
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Web.Event.Event (EventType)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.History (DocumentTitle(..), URL(..), back, forward, pushState, replaceState)
import Web.HTML.Location (pathname, search)
import Web.HTML.Window (Window, toEventTarget, history, location)



router
  :: (String -> Effect Unit)
  -> Effect Unit
router matcher = do
  handler
  listener <- eventListener (\_ -> handler)
  eventWindow >>= addEventListener popstate listener false
    where
      handler = do
        l <- window >>= location
        path <- (<>) <$> pathname l <*> search l
        matcher path



navigateTo
  :: String
  -> Effect Unit
navigateTo url = do
  window >>= history >>= pushState null (DocumentTitle "") (URL url)
  window >>= dispatchEvent popstate



redirectTo
  :: String
  -> Effect Unit
redirectTo url = do
  window >>= history >>= replaceState null (DocumentTitle "") (URL url)
  window >>= dispatchEvent popstate



goForward ::Effect Unit
goForward = window >>= history >>= forward



goBack :: Effect Unit
goBack = window >>= history >>= back



eventWindow :: Effect EventTarget
eventWindow = toEventTarget <$> window



null :: Foreign
null = unsafeToForeign Nothing



foreign import dispatchEvent
  :: EventType
  -> Window
  -> Effect Unit
