module Test.Router.Parser (assertRouteParser) where

import Prelude

import Effect (Effect)
import Test.Assert (assert)
import Cherry.Router.Parser (match, lit, int, param, end)
import Data.Maybe (fromMaybe)
import Control.Alt ((<|>))

data Route
  = Home
  | User Int
  | Users String
  | NotFound

derive instance routeEq :: Eq Route

route :: String -> Route
route url = fromMaybe NotFound $ match url $
  Home <$ end
  <|>
  Users <$> (lit "users" *> param "name") <* end
  <|>
  User <$> (lit "users" *> int) <* end

assertRouteParser :: Effect Unit
assertRouteParser = do
  assert $ route "" == Home
  assert $ route "/" == Home
  assert $ route "/users?name=oreshinya" == Users "oreshinya"
  assert $ route "/users/42" == User 42
  assert $ route "/projects" == NotFound
