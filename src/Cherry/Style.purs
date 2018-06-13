module Cherry.Style
  ( StyleSheet
  , createStyleSheet
  , getStyle
  , registerStyle
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldr)
import Data.Int (base36, toStringAs)
import Data.Int.Bits (xor, zshr)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodeUnits (toCharArray)
import Effect.Ref (Ref, modify_, new, read)
import Effect.Unsafe (unsafePerformEffect)




newtype StyleSheet = StyleSheet (Ref String)



createStyleSheet :: StyleSheet
createStyleSheet = StyleSheet $ unsafePerformEffect $ new ""



getStyle :: StyleSheet -> String
getStyle (StyleSheet ref) = unsafePerformEffect $ read ref



registerStyle :: StyleSheet -> String -> String
registerStyle (StyleSheet ref) style = unsafePerformEffect do
  modify_ (flip append output) ref
  pure name
  where
    name = "p" <> generateHash style
    output = replaceToken name style



generateHash :: String -> String
generateHash str = toStringAs base36 $ zshr seed 0
  where
    culc char value = xor (value * 33) (toCharCode char)
    seed = foldr culc 5381 $ toCharArray str



replaceToken :: String -> String -> String
replaceToken instead target =
  replaceAll (Pattern "&") (Replacement instead) target
