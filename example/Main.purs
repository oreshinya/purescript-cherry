module Main where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Rout (match, lit, int, end)
import VOM (VNode, h, t, attr, handler, style)
import Cherry (Config(..), AppEffects, Subscription, router, navigateTo, goBack, reduce, app)

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
  }

initialState :: State
initialState =
  { route: Home
  , count: 0
  }

-- Action

increment :: forall e. Eff (AppEffects e) Unit
increment = do
  reduce (\s -> s { count = s.count + 1 })
  log "foo"

-- View
view :: forall e. State -> VNode (AppEffects e)
view { route: Home } = home
view { route: Item id, count } = item id count
view { route: NotFound } = notFound


home :: forall e. VNode (AppEffects e)
home =
  h "div" []
    [ h "h1" [] [ t "Home" ]
    , h "a" [ Tuple "onClick" $ handler (\_ -> navigateTo "/items/1") ] [ t "Item 1 " ]
    , h "a" [ Tuple "onClick" $ handler (\_ -> navigateTo "/items/2") ] [ t "Item 2 " ]
    , h "a" [ Tuple "onClick" $ handler (\_ -> navigateTo "/not_found") ] [ t "404" ]
    ]

item :: forall e. Int -> Int -> VNode (AppEffects e)
item id count =
  h "div" []
    [ h "h1" [] [ t $ "Item " <> show id ]
    , h "div" [ Tuple "onClick" $ handler (\_ -> increment) ] [ t $ show (count :: Int) ]
    , h "a" [ Tuple "onClick" $ handler (\_ -> goBack) ] [ t "Back" ]
    ]

notFound :: forall e. VNode (AppEffects e)
notFound =
  h "div" []
    [ h "h1" [ Tuple "style" $ style [ Tuple "color" "red" ] ] [ t "404" ]
    , h "a" [ Tuple "href" $ attr "https://github.com/oreshinya/purescript-cherry" ] [ t "Github" ]
    ]


-- Subscription

subscriptions :: forall e. Array (Subscription (AppEffects e))
subscriptions = [ route ]

-- Init
config :: forall e. Config (AppEffects e) State
config = Config
  { selector: "#app"
  , state: initialState
  , view: view
  , subscriptions
  }

main :: forall e. Eff (AppEffects e) Unit
main = app config
