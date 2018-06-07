module Cherry.Renderer
  ( Renderer
  , createRenderer
  , runRenderer
  ) where

import Prelude

import Cherry.Store (Store, select)
import Cherry.VDOM (VNode, patch)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, modify_, new, read, write)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.DOM.Internal.Types (Node)
import Web.DOM.Element (toNode)
import Data.List (List(..), (!!), (:), take)
import Data.Maybe (Maybe(..))



newtype Renderer s = Renderer
  { container :: Maybe Node
  , view :: s -> VNode
  , historyRef :: Ref (List VNode)
  , renderFlagRef :: Ref Boolean
  }



createRenderer
  :: forall s
   . String
  -> (s -> VNode)
  -> Effect (Renderer s)
createRenderer selector view = do
  doc <- toParentNode <$> (window >>= document)
  container <- map toNode <$> querySelector (QuerySelector selector) doc
  historyRef <- new Nil
  renderFlagRef <- new false
  pure $ Renderer { container, view, historyRef, renderFlagRef }



runRenderer
  :: forall s
   . Store s
  -> Renderer s
  -> Effect Unit
runRenderer store (Renderer r) =
  case r.container of
    Nothing -> log "Container is not found"
    Just t -> do
      renderFlag <- read r.renderFlagRef
      if renderFlag
        then pure unit
        else do
          write true r.renderFlagRef
          void $ window >>= requestAnimationFrame do
            write false r.renderFlagRef
            currentState <- select store (\s -> s)
            flip modify_ r.historyRef \h -> take 2 $ r.view currentState : h
            history <- read r.historyRef
            patch
              { current: history !! 1
              , next: history !! 0
              , parent: t
              , i: 0
              }
