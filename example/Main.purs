module Main where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (liftEff', launchAff, later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Rout (match, lit, int, end)
import VOM (VNode, h, t, (:=), (:|), (~>), stringTo, noneTo)
import Cherry (Config(..), AppEffects, Subscription, router, navigateTo, goBack, reduce, app)
import Style (whole, link)

data Route
  = Home
  | Item Int
  | NotFound

detectRoute :: String -> Route
detectRoute url = fromMaybe NotFound $ match url $
  Home <$ end
  <|>
  Item <$> (lit "items" *> int) <* end

route :: forall e. Subscription (AppEffects e)
route = router (\url -> reduce (\s -> s { route = detectRoute url }))

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

-- Reducer

incr :: State -> State
incr s = s { count = s.count + 1 }

updateMsg :: String -> State -> State
updateMsg content s = s { message = content }

-- Action

increment :: forall e. Eff (AppEffects (err :: EXCEPTION | e)) Unit
increment = do
  launchAff $ later' 1500 $ liftEff' $ reduce incr
  pure unit

changeMessage :: forall e. String -> Eff (AppEffects e) Unit
changeMessage content = do
  reduce $ updateMsg content
  log "bar"

-- View

view :: forall e. State -> VNode (AppEffects (err :: EXCEPTION | e))
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

style :: forall e. VNode (AppEffects e)
style = h "style" [] [ t styleStr ]
  where
    styleStr = fold [ whole, link.output ]

home :: forall e. String -> VNode (AppEffects e)
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

item :: forall e. Int -> Int -> VNode (AppEffects (err :: EXCEPTION | e))
item id count =
  h "div" []
    [ h "h1" [] [ t $ "Item " <> show id ]
    , h "div" [ "onClick" ~> noneTo increment ] [ t $ show (count :: Int) ]
    , h "a" [ "onClick" ~> noneTo goBack ] [ t "Back" ]
    ]

notFound :: forall e. VNode (AppEffects e)
notFound =
  h "div" []
    [ h "h1" [ "style" :| [ "color" /\ "red" ] ] [ t "404" ]
    , h "a" [ "href" := "https://github.com/oreshinya/purescript-cherry" ] [ t "Github" ]
    ]


-- Subscription

subscriptions :: forall e. Array (Subscription (AppEffects e))
subscriptions = [ route ]

-- Init

config :: forall e. Config (AppEffects (err :: EXCEPTION | e)) State
config = Config
  { selector: "#app"
  , state: initialState
  , view: view
  , subscriptions
  }

main :: forall e. Eff (AppEffects (err :: EXCEPTION | e)) Unit
main = app config
