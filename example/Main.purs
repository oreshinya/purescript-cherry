module Main where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromMaybe)
import Data.VirtualDOM (text, prop, h, with, EventListener(..))
import Rout (match, lit, int, end)
import Cherry (Config(..), AppEffects, Tree, Subscription, router, navigateTo, goBack, reduce, app)

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
view :: forall e. State -> Tree (AppEffects e)
view { route: Home } = home
view { route: Item id, count } = item id count
view { route: NotFound } = notFound


home :: forall e. Tree (AppEffects e)
home = h "div" (prop [])
  [ h "h1" (prop []) [ text "Home" ]
  , with (h "a" (prop []) [ text "Item 1 " ])
    [ On "click" (\_ -> navigateTo "/items/1") ]
  , with (h "a" (prop []) [ text "Item 2 " ])
    [ On "click" (\_ -> navigateTo "/items/2") ]
  , with (h "a" (prop []) [ text "404" ])
    [ On "click" (\_ -> navigateTo "/not_found") ]
  ]

item :: forall e. Int -> Int -> Tree (AppEffects e)
item id count =
  h "div" (prop [])
  [ h "h1" (prop []) [ text $ "Item " <> show id ]
  , with (h "div" (prop []) [ text $ show (count :: Int) ])
    [ On "click" (\_ -> increment) ]
  , with (h "a" (prop []) [ text "Back" ])
    [ On "click" (\_ -> goBack) ]
  ]

notFound :: forall e. Tree (AppEffects e)
notFound = h "h1" (prop []) [ text "404" ]

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
