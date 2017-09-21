module Cherry.Renderer
  ( Renderer
  , createRenderer
  , runRenderer
  ) where

import Prelude

import Cherry.Store (Store, select)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document, requestAnimationFrame)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (Node, elementToNode)
import Data.List (List(..), (!!), (:), take)
import Data.Maybe (Maybe(..))
import VOM (VNode, patch)



newtype Renderer e s = Renderer
  { container :: Maybe Node
  , view :: s -> VNode e
  , historyRef :: Ref (List (VNode e))
  , renderFlagRef :: Ref Boolean
  }



createRenderer
  :: forall e s
   . String
  -> (s -> VNode (dom :: DOM, ref :: REF | e))
  -> Eff (dom :: DOM, ref :: REF | e) (Renderer (dom :: DOM, ref :: REF | e) s)
createRenderer selector view = do
  doc <- htmlDocumentToParentNode <$> (window >>= document)
  container <- map elementToNode <$> querySelector (QuerySelector selector) doc
  historyRef <- newRef Nil
  renderFlagRef <- newRef false
  pure $ Renderer { container, view, historyRef, renderFlagRef }



runRenderer
  :: forall e s
   . Store (console :: CONSOLE, dom :: DOM, ref :: REF | e) s
  -> Renderer (console :: CONSOLE, dom :: DOM, ref :: REF | e) s
  -> Eff (console :: CONSOLE, dom :: DOM, ref :: REF | e) Unit
runRenderer store (Renderer r) =
  case r.container of
    Nothing -> log "Container is not found"
    Just t -> do
      renderFlag <- readRef r.renderFlagRef
      if renderFlag
        then pure unit
        else do
          writeRef r.renderFlagRef true
          void $ window >>= requestAnimationFrame do
            writeRef r.renderFlagRef false
            currentState <- select store (\s -> s)
            modifyRef r.historyRef \h -> take 2 $ r.view currentState : h
            history <- readRef r.historyRef
            patch (history !! 1) (history !! 0) t
