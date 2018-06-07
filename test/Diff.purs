module Test.Diff (assertDiffAlgorithm) where

import Prelude

import Cherry.VDOM (h, patch, t)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)
import Web.DOM.Node (childNodes, firstChild, textContent)
import Web.DOM.NodeList (item)
import Data.Array (length, range, (!!))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Test.Assert (assert)
import Test.TestHelper (clearBody)



assertDiffAlgorithm :: Effect Unit
assertDiffAlgorithm = do
  assertDiffAlgorithmForText
  assertDiffAlgorithmForElement



assertDiffAlgorithmForText :: Effect Unit
assertDiffAlgorithmForText =
  for_ afterCases assertDiff
  where
    root testCase = h "div" [] $ map t testCase

    assertDiff testCase = do
      clearBody
      mBody <- window >>= document >>= body
      case mBody of
        Nothing -> assert false
        Just body' -> do
          patch
            { current: Nothing
            , next: Just $ root beforeCase
            , parent: toNode body'
            , i: 0
            }
          (firstChild $ toNode body') >>= assertText beforeCase
          patch
            { current: Just $ root beforeCase
            , next: Just $ root testCase
            , parent: toNode body'
            , i: 0
            }
          (firstChild $ toNode body') >>= assertText testCase

    assertText _ Nothing = assert false
    assertText testCase (Just parent) =
      for_ (range 0 $ length testCase - 1) \i -> do
        nodes <- childNodes parent
        node <- item i nodes
        case node of
          Nothing -> assert false
          Just n -> do
             text <- textContent n
             case testCase !! i of
               Nothing -> assert false
               Just text' -> assert $ text == text'



assertDiffAlgorithmForElement :: Effect Unit
assertDiffAlgorithmForElement =
  for_ afterCases assertDiff
  where
    root testCase = h "div" [] $ flip map testCase \x -> h "div" [] [ t x ]

    assertDiff testCase = do
      clearBody
      mBody <- window >>= document >>= body
      case mBody of
        Nothing -> assert false
        Just body' -> do
          patch
            { current: Nothing
            , next: Just $ root beforeCase
            , parent: toNode body'
            , i: 0
            }
          (firstChild $ toNode body') >>= assertElement beforeCase
          patch
            { current: Just $ root beforeCase
            , next: Just $ root testCase
            , parent: toNode body'
            , i: 0
            }
          (firstChild $ toNode body') >>= assertElement testCase

    assertElement _ Nothing = assert false
    assertElement testCase (Just parent) =
      for_ (range 0 $ length testCase - 1) \i -> do
        nodes <- childNodes parent
        node <- item i nodes
        case node of
          Nothing -> assert false
          Just node' -> do
            mChild <- firstChild node'
            case mChild of
              Nothing -> assert false
              Just n -> do
                 text <- textContent n
                 case testCase !! i of
                   Nothing -> assert false
                   Just text' -> assert $ text == text'



beforeCase :: Array String
beforeCase =
  [ "text 1"
  , "text 2"
  , "text 3"
  , "text 4"
  , "text 5"
  ]



afterCases :: Array (Array String)
afterCases =
  [ beforeCase
  , [ "text 5"
    , "text 4"
    , "text 3"
    , "text 2"
    , "text 1"
    ]
  , [ "text 8"
    , "text 0"
    , "text 1"
    , "text 2"
    , "text 3"
    , "text 4"
    , "text 5"
    ]
  , [ "text 1"
    , "text 2"
    , "text 8"
    , "text 0"
    , "text 3"
    , "text 4"
    , "text 5"
    ]
  , [ "text 1"
    , "text 2"
    , "text 3"
    , "text 4"
    , "text 5"
    , "text 6"
    , "text 7"
    ]
  , [ "text 2"
    , "text 3"
    , "text 4"
    , "text 5"
    ]
  , [ "text 1"
    , "text 2"
    , "text 4"
    , "text 5"
    ]
  , [ "text 1"
    , "text 2"
    , "text 3"
    , "text 4"
    ]
  , [ "text 8"
    , "text 0"
    , "text 5"
    , "text 4"
    , "text 3"
    , "text 2"
    , "text 1"
    ]
  , [ "text 5"
    , "text 4"
    , "text 3"
    , "text 8"
    , "text 0"
    , "text 2"
    , "text 1"
    ]
  , [ "text 5"
    , "text 4"
    , "text 3"
    , "text 2"
    , "text 1"
    , "text 0"
    , "text 8"
    ]
  , [ "text 4"
    , "text 3"
    , "text 2"
    , "text 1"
    ]
  , [ "text 5"
    , "text 4"
    , "text 2"
    , "text 1"
    ]
  , [ "text 5"
    , "text 4"
    , "text 3"
    , "text 2"
    ]
  , [ "text 1"
    , "text 2"
    , "text 5"
    , "text 100"
    , "text 3"
    , "text 99"
    , "text 4"
    ]
  ]
