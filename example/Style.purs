module Style where

import PureStyle (Result, (:), (:-), className, css)

whole :: String
whole = css "html, body"
  [ "height" : "100%"
  , "background-color" : "yellow"
  ]

link :: Result
link = className
  [ "font-size" : "12px"
  , "&:hover" :-
      [ "font-size" : "14px"
      , "color" : "gray"
      ]
  ]
