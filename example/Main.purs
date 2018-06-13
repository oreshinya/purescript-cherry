module Main where

import Prelude

import Cherry (mount)
import Cherry.Renderer as R
import Cherry.Router (router, navigateTo, goBack)
import Cherry.Router.Parser (match, lit, int, end)
import Cherry.Store as S
import Cherry.Style (getStyle)
import Cherry.VDOM (VNode, h, t, targetValue, (:=), (~>))
import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Style (sheet, link)
import Web.Event.Event (Event)


-- Initialize app
main :: Effect Unit
main = do
  renderer <- R.createRenderer "#app" view
  mount store renderer [ route ]



-- Initialize core

store :: S.Store State
store = S.createStore initialState



select :: forall a. (State -> a) -> Effect a
select = S.select store



reduce :: (State -> State) -> Effect Unit
reduce = S.reduce store



-- Router

data Route
  = Home
  | Item Int
  | NotFound



detectRoute :: String -> Route
detectRoute url = fromMaybe NotFound $ match url $
  Home <$ end
  <|>
  Item <$> (lit "items" *> int) <* end



route :: Effect Unit
route = router (\url -> reduce (\s -> s { route = detectRoute url }))



-- View

view :: State -> VNode
view state =
  h "div" []
    [ scene
    , style
    ]
  where
    scene =
      case state.route of
        Home -> home state.message
        Item id -> item id state.count
        NotFound -> notFound

style :: VNode
style = h "style" [] [ t styleStr ]
  where
    styleStr = getStyle sheet

home :: String -> VNode
home message =
  h "div" []
    [ h "h1" [] [ t "Home" ]
    , h "div" [] [ t message ]
    , h "input"
        [ "type" := "text"
        , "value" := message
        , "onInput" ~> changeMessage
        ]
        []
    , h "a" [ "class" := link, "onClick" ~> (const $ navigateTo "/items/1") ] [ t "Item 1 " ]
    , h "a" [ "class" := link, "onClick" ~> (const $ navigateTo "/items/2") ] [ t "Item 2 " ]
    , h "a" [ "class" := link, "onClick" ~> (const $ navigateTo "/not_found") ] [ t "404 Not Found" ]
    ]

item :: Int -> Int -> VNode
item id count =
  h "div" []
    [ h "h1" [] [ t $ "Item " <> show id ]
    , h "div" [ "onClick" ~> const increment ] [ t $ show (count :: Int) ]
    , h "a" [ "onClick" ~> const goBack ] [ t "Back" ]
    ]

notFound :: VNode
notFound =
  h "div" []
    [ h "h1" [ "style" := "color: red;" ] [ t "404" ]
    , h "a" [ "href" := "https://github.com/oreshinya/purescript-cherry", "target" := "_blank" ] [ t "Github" ]
    ]



-- Action

increment :: Effect Unit
increment = do
  _ <- launchAff $ do
    delay $ Milliseconds 1500.0
    liftEffect $ reduce incr
  pure unit



changeMessage :: Event -> Effect Unit
changeMessage ev = do
  content <- targetValue ev
  reduce $ updateMsg content
  cnt <- select _.count
  reduce \s -> s { count = cnt + 1 }
  log "bar"



-- Reducer

incr :: State -> State
incr s = s { count = s.count + 1 }



updateMsg :: String -> State -> State
updateMsg content s = s { message = content }



-- State

type State =
  { route :: Route
  , count :: Int
  , message :: String
  }

initialState :: State
initialState =
  { route: Home
  , count: 0
  , message: ""
  }
