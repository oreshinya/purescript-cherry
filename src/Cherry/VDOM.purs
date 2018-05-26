module Cherry.VDOM
  ( VProp
  , VNode
  , h
  , t
  , attribute, (:=)
  , handler, (~>)
  , targetValue
  , patch
  ) where


import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Safely as Safe
import DOM (DOM)
import DOM.Event.Event (target)
import DOM.Event.EventTarget (eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.HTMLInputElement (value)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createTextNode, createElement, createElementNS)
import DOM.Node.Element (removeAttribute, setAttribute)
import DOM.Node.Node (appendChild, childNodes, insertBefore, removeChild, replaceChild)
import DOM.Node.NodeList (item)
import DOM.Node.Types (Document, Element, Node, elementToNode, textToNode)
import Data.Array (union, length, (!!))
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, lookup, curry)
import Unsafe.Coerce (unsafeCoerce)



data VProp e
  = Attribute String
  | Handler (Event -> Eff e Unit)



data VInfo e
  = Element
    { tag :: String
    , props :: Array (Tuple String (VProp e))
    , children :: Array (VNode e)
    }
  | Text String


data VNode e = VNode String (VInfo e)



foreign import setForeign :: forall e. String -> Foreign -> Element -> Eff (dom :: DOM | e) Unit

foreign import removeForeign :: forall e. String -> Element -> Eff (dom :: DOM | e) Unit



vPropToKey :: forall e. VProp e -> String
vPropToKey (Attribute x) = "attribute(" <> x <> ")"
vPropToKey (Handler x) = "handler()"



vInfoToKey :: forall e. VInfo e -> String
vInfoToKey (Text x) = "text(" <> x <> ")"
vInfoToKey (Element { tag, props, children }) =
  "element(" <> tagKey <> propsKey <> childrenKey <> ")"
  where
    tagKey = "tag(" <> tag <> ")"

    propsKey = foldMap tupleToKey props

    tupleToKey (Tuple a b) = "prop(" <> a <> ":" <> vPropToKey b <> ")"

    childrenKey = foldMap childToKey children

    childToKey (VNode key _) = key



h :: forall e
   . String
  -> Array (Tuple String (VProp e))
  -> Array (VNode e)
  -> VNode e
h tag props children = VNode key el
  where
    el = Element { tag, props, children }
    key = vInfoToKey el



t :: forall e. String -> VNode e
t x = VNode key txt
  where
    txt = Text x
    key = vInfoToKey txt



attribute :: forall e. String -> String -> Tuple String (VProp e)
attribute key value' = Tuple key (Attribute value')

infixr 1 attribute as :=



handler :: forall e. String -> (Event -> Eff e Unit) -> Tuple String (VProp e)
handler key fn = Tuple key (Handler fn)

infixr 1 handler as ~>



doc :: forall e. Eff (dom :: DOM | e) Document
doc = window >>= document >>= htmlDocumentToDocument >>> pure



svgNameSpace :: Maybe String
svgNameSpace = Just "http://www.w3.org/2000/svg"



setProp
  :: forall e
   . Element
  -> Tuple String (VProp (dom :: DOM | e))
  -> Eff (dom :: DOM | e) Unit
setProp el (Tuple k v) =
  case v of
    Attribute val -> do
      setForeign k (toForeign val) el
      setAttribute k val el
    Handler val ->
      setForeign k (toForeign $ eventListener val) el



removeProp
  :: forall e
   . { el :: Element, key :: String }
  -> Eff (dom :: DOM | e) Unit
removeProp { el, key } = do
  removeForeign key el
  removeAttribute key el



createNode :: forall e. VInfo (dom :: DOM | e) -> Eff (dom :: DOM | e) Node
createNode (Text text) =
  doc >>= createTextNode text >>= textToNode >>> pure

createNode (Element { tag, props, children }) = do
  el <- case tag of
    "svg" ->
      doc >>= createElementNS svgNameSpace tag
    _ ->
      doc >>= createElement tag
  Safe.for_ props $ setProp el
  let node = elementToNode el
  Safe.for_ children
    \(VNode _ vc) -> do
      child <- createNode vc
      void $ appendChild child node
  pure node



updateProps
  :: forall e
   . { currentProps :: Array (Tuple String (VProp (dom :: DOM | e)))
     , nextProps :: Array (Tuple String (VProp (dom :: DOM | e)))
     , el :: Element
     }
  -> Eff (dom :: DOM | e) Unit
updateProps { currentProps, nextProps, el } =
  Safe.for_ keys update
  where
    keys = union (map fst currentProps) (map fst nextProps)
    update key =
      case lookup key currentProps, lookup key nextProps of
        Nothing, Nothing -> pure unit
        Just _, Nothing -> removeProp { el, key }
        Just (Attribute c), Just (Attribute n) | c == n -> pure unit
        _, Just nextProp -> curry (setProp el) key nextProp



changed
  :: forall e
   . { currentInfo :: VInfo e
     , nextInfo :: VInfo e
     }
   -> Boolean
changed { currentInfo: Element current, nextInfo: Element next } = current.tag /= next.tag
changed { currentInfo: Text current, nextInfo: Text next } = current /= next
changed _ = true



targetValue :: forall e. Event -> Eff (dom :: DOM | e) String
targetValue = target >>> unsafeCoerce >>> value



patch
  :: forall e
   . { current :: Maybe (VNode (dom :: DOM | e))
     , next :: Maybe (VNode (dom :: DOM | e))
     , parent :: Node
     , i :: Int
     }
  -> Eff (dom :: DOM | e) Unit
patch { current: Nothing, next: Nothing } = pure unit
patch { current: Nothing, next: Just (VNode _ next), parent, i } = do
  newNode <- createNode next
  maybeNode <- childNodes parent >>= item i
  void $ case maybeNode of
    Nothing -> appendChild newNode parent
    Just node -> insertBefore newNode node parent
patch { current: Just _, next: Nothing, parent, i } = do
  maybeNode <- childNodes parent >>= item i
  case maybeNode of
    Nothing -> pure unit
    Just node -> void $ removeChild node parent
patch { current: Just (VNode _ current), next: Just (VNode _ next), parent, i } = do
  maybeNode <- childNodes parent >>= item i
  case maybeNode of
    Nothing -> pure unit
    Just node ->
      if changed { currentInfo: current, nextInfo: next } then do
        newNode <- createNode next
        void $ replaceChild newNode node parent
      else
        case current, next of
          Element c, Element n -> do
            updateProps
              { currentProps: c.props
              , nextProps: n.props
              , el: unsafeCoerce node
              }
            tailRecM takeDiff
              { currentChildren: c.children
              , nextChildren: n.children
              , target: node
              , currentStart: 0
              , currentEnd: length c.children - 1
              , nextStart: 0
              , nextEnd: length n.children - 1
              }
          _, _ -> pure unit
  where
    takeDiff x@({ currentChildren, nextChildren, target, currentStart, currentEnd, nextStart, nextEnd }) =
      if equalKey (currentChildren !! currentStart) (nextChildren !! nextStart) then do
        patch
          { current: currentChildren !! currentStart
          , next: nextChildren !! nextStart
          , parent: target
          , i: currentStart
          }
        pure $ Loop $ downStart x
      else if equalKey (currentChildren !! currentEnd) (nextChildren !! nextEnd) then do
        patch
          { current: currentChildren !! currentEnd
          , next: nextChildren !! nextEnd
          , parent: target
          , i: currentEnd
          }
        pure $ Loop $ upEnd x
      else if currentStart <= currentEnd && nextStart <= nextEnd then do
        patch
          { current: currentChildren !! currentStart
          , next: nextChildren !! nextStart
          , parent: target
          , i: currentStart
          }
        pure $ Loop $ downStart x
      else if currentStart <= currentEnd then do
        patch
          { current: currentChildren !! currentEnd
          , next: Nothing
          , parent: target
          , i: currentEnd
          }
        pure $ Loop $ upEnd x
      else if nextStart <= nextEnd then do
        patch
          { current: Nothing
          , next: nextChildren !! nextStart
          , parent: target
          , i: nextStart
          }
        pure $ Loop $ downStart x
      else
        pure $ Done unit

    equalKey (Just (VNode ck _)) (Just (VNode nk _)) = ck == nk
    equalKey _ _ = false

    downStart x = x { currentStart = x.currentStart + 1, nextStart = x.nextStart + 1 }

    upEnd x = x { currentEnd = x.currentEnd - 1, nextEnd = x.nextEnd - 1 }
