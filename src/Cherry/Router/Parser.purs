module Cherry.Router.Parser
  ( Parser
  , match
  , lit
  , str
  , num
  , int
  , bool
  , param
  , params
  , any
  , end
  ) where

import Prelude
import Data.Array as A
import Data.Map as M
import Data.String as S
import Control.Alt (class Alt)
import Control.MonadPlus (guard)
import Control.Plus (class Plus)
import Data.Foldable (foldr)
import Data.Int (fromString)
import Data.List (catMaybes, List(Nil, Cons), fromFoldable, drop)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Profunctor (lmap)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Global (readFloat, isNaN)



data Part = Path String | Query (M.Map String String)

type Route = List Part

newtype Parser a = Parser (Route -> Maybe (Tuple Route a))



instance parserFunctor :: Functor Parser where
  map f (Parser r2t) = Parser $ \r ->
    maybe Nothing (\t -> Just $ Tuple (fst t) (f (snd t))) $ r2t r

instance parserAlt :: Alt Parser where
  alt (Parser a) (Parser b) = Parser $ \r ->
    case a r of
      Nothing -> b r
      Just x  -> Just x

instance parserApply :: Apply Parser where
  apply (Parser r2a2b) (Parser r2a) = Parser $ \r1 ->
    case (r2a2b r1) of
      Nothing -> Nothing
      Just (Tuple r2 f) -> case (r2a r2) of
        Nothing -> Nothing
        Just (Tuple r3 b) -> Just $ Tuple r3 (f b)

instance parserPlus :: Plus Parser where
  empty = Parser \r -> Nothing

instance parserApplicative :: Applicative Parser where
  pure a = Parser \r -> pure $ Tuple r a



end :: Parser Unit
end = Parser $ \r ->
  case r of
    Cons (Query m) Nil -> Just $ Tuple Nil unit
    Nil -> Just $ Tuple Nil unit
    _ -> Nothing



lit :: String -> Parser Unit
lit part = Parser $ \r ->
  case r of
    Cons (Path p) ps | p == part -> Just $ Tuple ps unit
    _ -> Nothing



num :: Parser Number
num = Parser $ \r ->
  case r of
    Cons (Path p) ps ->
      let res = readFloat p in
      if isNaN res then
        Nothing
      else
        Just $ Tuple ps res
    _ -> Nothing



int :: Parser Int
int = Parser $ \r ->
  case r of
    Cons (Path p) ps -> maybe Nothing (Just <<< Tuple ps) $ fromString p
    _ -> Nothing



bool :: Parser Boolean
bool = Parser $ \r ->
  case r of
    Cons (Path p) ps | p == "true" -> Just $ Tuple ps true
    Cons (Path p) ps | p == "false" -> Just $ Tuple ps false
    _ -> Nothing



str :: Parser String
str = Parser $ \r ->
  case r of
    Cons (Path p) ps -> Just $ Tuple ps p
    _ -> Nothing



param :: String -> Parser String
param key = Parser $ \r ->
  case r of
    Cons (Query map) ps ->
      case M.lookup key map of
        Nothing -> Nothing
        Just s -> Just $ Tuple (Cons (Query <<< M.delete key $ map) ps) s
    _ ->  Nothing



params :: Parser (M.Map String String)
params = Parser $ \r ->
  case r of
    Cons (Query map) ps -> Just $ Tuple ps map
    _ -> Nothing



any :: Parser Unit
any = Parser $ \r ->
  case r of
    Cons p ps -> Just $ Tuple ps unit
    _ -> Nothing



routeFromUrl :: String -> Route
routeFromUrl "/" = Nil
routeFromUrl url =
  case S.indexOf (S.Pattern "?") url of
    Nothing -> parsePath Nil url
    Just queryPos ->
      let queryPart = parseQuery <<< S.drop queryPos $ url
      in parsePath (Cons queryPart Nil) <<< S.take queryPos $ url



parsePath :: Route -> String -> Route
parsePath query = drop 1 <<< foldr prependPath query <<< S.split (S.Pattern "/")
  where prependPath = lmap Path Cons



parseQuery :: String -> Part
parseQuery s = Query <<< M.fromFoldable <<< catMaybes <<< map part2tuple $ parts
  where
    parts :: List String
    parts = fromFoldable $ S.split (S.Pattern "&") $ S.drop 1 s

    part2tuple :: String -> Maybe (Tuple String String)
    part2tuple part = do
      let param' = S.split (S.Pattern "=") part
      guard $ A.length param' == 2
      Tuple <$> (A.head param') <*> (param' A.!! 1)



match :: forall a. String -> Parser a -> Maybe a
match url (Parser parser) = maybe Nothing (Just <<< snd) result
  where result = parser $ routeFromUrl url
