module Style where

import Prelude

import Cherry.Style (StyleSheet, createStyleSheet, registerStyle)



sheet :: StyleSheet
sheet = const sheet' $ registerStyle sheet'
  """
  html, body {
    height: 100%;
    background-color: yellow;
  }
  """
  where
    sheet' = createStyleSheet



link :: String
link = registerStyle sheet
  """
  .& {
    font-size: 12px;
  }
  .&:hover {
    font-size: 14px;
    color: gray;
  }
  """
