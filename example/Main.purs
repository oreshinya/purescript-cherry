module Main where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (liftEff', launchAff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Rout (match, lit, int, end)
import VOM (VNode, h, t, (:=), (:|), (~>), stringTo, noneTo)
import Cherry (mount)
import Cherry.Store as S
import Cherry.Renderer as R
import Cherry.Router (router, navigateTo, goBack)
import Style (whole, link)


-- Initialize app
main :: Eff (dom :: DOM, console :: CONSOLE, ref :: REF, exception :: EXCEPTION, history :: HISTORY) Unit
main = do
  renderer <- R.createRenderer "#app" view
  mount store renderer [ route ]



-- Initialize core

store :: forall e. S.Store (ref :: REF | e) State
store = S.createStore initialState



select :: forall e a. (State -> a) -> Eff (ref :: REF | e) a
select = S.select store



reduce :: forall e. (State -> State) -> Eff (ref :: REF | e) Unit
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



route :: forall e. Eff (dom :: DOM, ref :: REF | e) Unit
route = router (\url -> reduce (\s -> s { route = detectRoute url }))



-- View

view :: forall e. State -> VNode (exception :: EXCEPTION, dom :: DOM, history :: HISTORY, ref :: REF, console :: CONSOLE | e)
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

style :: forall e. VNode e
style = h "style" [] [ t styleStr ]
  where
    styleStr = fold [ whole, link.output ]

home :: forall e. String -> VNode (dom :: DOM, console :: CONSOLE, ref :: REF, history :: HISTORY | e)
home message =
  h "div" []
    [ h "h1" [] [ t "Home" ]
    , h "div" [] [ t message ]
    , h "input"
        [ "type" := "text"
        , "value" := message
        , "onInput" ~> stringTo changeMessage
        ]
        []
    , h "a" [ "class" := link.name, "onClick" ~> (noneTo $ navigateTo "/items/1") ] [ t "Item 1 " ]
    , h "a" [ "class" := link.name, "onClick" ~> (noneTo $ navigateTo "/items/2") ] [ t "Item 2 " ]
    , h "a" [ "class" := link.name, "onClick" ~> (noneTo $ navigateTo "/not_found") ] [ t "404 Not Found" ]
    ]

item :: forall e. Int -> Int -> VNode (dom :: DOM, exception :: EXCEPTION, ref :: REF, history :: HISTORY | e)
item id count =
  h "div" []
    [ h "h1" [] [ t $ "Item " <> show id ]
    , h "div" [ "onClick" ~> noneTo increment ] [ t $ show (count :: Int) ]
    , h "a" [ "onClick" ~> noneTo goBack ] [ t "Back" ]
    ]

notFound :: forall e. VNode e
notFound =
  h "div" []
    [ h "h1" [ "style" :| [ "color" /\ "red" ] ] [ t "404" ]
    , h "a" [ "href" := "https://github.com/oreshinya/purescript-cherry", "target" := "_blank" ] [ t "Github" ]
    ]



-- Action

increment :: forall e. Eff (ref :: REF, exception :: EXCEPTION | e) Unit
increment = do
  _ <- launchAff $ do
    delay $ Milliseconds 1500.0
    liftEff' $ reduce incr
  pure unit



changeMessage :: forall e. String -> Eff (console :: CONSOLE, ref :: REF | e) Unit
changeMessage content = do
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
