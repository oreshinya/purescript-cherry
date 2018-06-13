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

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (union, length, (!!))
import Data.Foldable (for_, foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), curry, fst, lookup)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, createTextNode, createElement, createElementNS)
import Web.DOM.Element (Element, removeAttribute, setAttribute)
import Web.DOM.Element as E
import Web.DOM.Node (Node, appendChild, childNodes, insertBefore, removeChild, replaceChild)
import Web.DOM.NodeList (item)
import Web.DOM.Text as T
import Web.Event.Event (target)
import Web.Event.EventTarget (eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLInputElement (value)
import Web.HTML.Window (document)



data VProp
  = Attribute String
  | Handler (Event -> Effect Unit)



data VInfo
  = Element
    { tag :: String
    , props :: Array (Tuple String VProp)
    , children :: Array VNode
    }
  | Text String


data VNode = VNode String VInfo



foreign import setForeign :: String -> Foreign -> Element -> Effect Unit

foreign import removeForeign :: String -> Element -> Effect Unit



vPropToKey :: VProp -> String
vPropToKey (Attribute x) = "attribute(" <> x <> ")"
vPropToKey (Handler x) = "handler()"



vInfoToKey :: VInfo -> String
vInfoToKey (Text x) = "text(" <> x <> ")"
vInfoToKey (Element { tag, props, children }) =
  "element(" <> tagKey <> propsKey <> childrenKey <> ")"
  where
    tagKey = "tag(" <> tag <> ")"

    propsKey = foldMap tupleToKey props

    tupleToKey (Tuple a b) = "prop(" <> a <> ":" <> vPropToKey b <> ")"

    childrenKey = foldMap childToKey children

    childToKey (VNode key _) = key



h :: String
  -> Array (Tuple String VProp)
  -> Array VNode
  -> VNode
h tag props children = VNode key el
  where
    el = Element { tag, props, children }
    key = vInfoToKey el



t :: String -> VNode
t x = VNode key txt
  where
    txt = Text x
    key = vInfoToKey txt



attribute :: String -> String -> Tuple String VProp
attribute key value' = Tuple key (Attribute value')

infixr 1 attribute as :=



handler :: String -> (Event -> Effect Unit) -> Tuple String VProp
handler key fn = Tuple key (Handler fn)

infixr 1 handler as ~>



doc :: Effect Document
doc = window >>= document >>= toDocument >>> pure



svgNameSpace :: Maybe String
svgNameSpace = Just "http://www.w3.org/2000/svg"



setProp
  :: Element
  -> Tuple String VProp
  -> Effect Unit
setProp el (Tuple k v) =
  case v of
    Attribute val -> do
      setForeign k (unsafeToForeign val) el
      setAttribute k val el
    Handler val ->
      setForeign k (unsafeToForeign $ eventListener val) el



removeProp
  :: { el :: Element, key :: String }
  -> Effect Unit
removeProp { el, key } = do
  removeForeign key el
  removeAttribute key el



createNode :: VInfo -> Effect Node
createNode (Text text) =
  doc >>= createTextNode text >>= T.toNode >>> pure

createNode (Element { tag, props, children }) = do
  el <- case tag of
    "svg" ->
      doc >>= createElementNS svgNameSpace tag
    _ ->
      doc >>= createElement tag
  for_ props $ setProp el
  let node = E.toNode el
  for_ children
    \(VNode _ vc) -> do
      child <- createNode vc
      void $ appendChild child node
  pure node



updateProps
  :: { currentProps :: Array (Tuple String VProp)
     , nextProps :: Array (Tuple String VProp)
     , el :: Element
     }
  -> Effect Unit
updateProps { currentProps, nextProps, el } =
  for_ keys update
  where
    keys = union (map fst currentProps) (map fst nextProps)
    update key =
      case lookup key currentProps, lookup key nextProps of
        Nothing, Nothing -> pure unit
        Just _, Nothing -> removeProp { el, key }
        Just (Attribute c), Just (Attribute n) | c == n -> pure unit
        _, Just nextProp -> curry (setProp el) key nextProp



changed
  :: { currentInfo :: VInfo
     , nextInfo :: VInfo
     }
   -> Boolean
changed { currentInfo: Element current, nextInfo: Element next } = current.tag /= next.tag
changed { currentInfo: Text current, nextInfo: Text next } = current /= next
changed _ = true



targetValue :: Event -> Effect String
targetValue = target >>> unsafeCoerce >>> value



patch
  :: { current :: Maybe VNode
     , next :: Maybe VNode
     , parent :: Node
     , i :: Int
     }
  -> Effect Unit
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
