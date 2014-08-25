module Unison.Styles where

body : Style
body =
  { typeface = [ "Lato", "latin" ]
  , height   = Just 16
  , color    = black
  , bold     = False
  , italic   = False
  , line     = Nothing }

code : Style
code =
  { typeface = [ "Inconsolata", "monospace", "latin" ]
  , height   = Just 16
  , color    = black
  , bold     = False
  , italic   = False
  , line     = Nothing }
