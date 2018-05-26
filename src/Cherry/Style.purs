module Cherry.Style
  ( StyleSheet
  , createStyleSheet
  , getStyle
  , registerStyle
  ) where

import Prelude

import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Char (toCharCode)
import Data.Foldable (foldr)
import Data.Int (base36, toStringAs)
import Data.Int.Bits (xor, zshr)
import Data.String (Pattern(..), Replacement(..), replaceAll, toCharArray)




newtype StyleSheet = StyleSheet (Ref String)



createStyleSheet :: StyleSheet
createStyleSheet = StyleSheet $ unsafePerformEff $ newRef ""



getStyle :: StyleSheet -> String
getStyle (StyleSheet ref) = unsafePerformEff $ readRef ref



registerStyle :: StyleSheet -> String -> String
registerStyle (StyleSheet ref) style = unsafePerformEff do
  modifyRef ref $ flip append output
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
