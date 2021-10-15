--
-- This is a small Html combinator library for building up an Html document.
-- There exists a few Haskell libraries like this, but we had a preference of
-- an Elm-like API that did not exist.
--
-- Example:
--
--   toText $ (
--     article
--       []
--       [ h1 [] [ text "Hello World" ]
--       , p [] [ text "Really cool example!" ]
--       ]
--   )
--
--  Results in:
--
--    "<article><h1>Hello World</h1><p>Really cool example!</p></article>"
--
--
-- It does not try to do anything fancy with which element is allowed as a
-- child of which.
--
{-# LANGUAGE OverloadedStrings #-}

module Unison.Server.Html where

import qualified Data.List as List
import Data.Text (Text)
import Unison.Server.Html.Attribute (Attribute)
import qualified Unison.Server.Html.Attribute as Attribute

data Html = Html Text [Attribute] Element

data Element
  = -- The "hidden" textNode that exists within an Element
    -- https://developer.mozilla.org/en-US/docs/Web/API/Text
    TextElement Text
  | -- Elements that can have children
    -- https://developer.mozilla.org/en-US/docs/Glossary/Element
    -- Examples: <div>hello world</div>
    Element [Html]
  | -- Elements without children. Sometimes called Empty Elements
    -- https://developer.mozilla.org/en-US/docs/Glossary/Empty_element
    -- Examples: <hr />, <br />, <img />
    VoidElement

-- Elements -------------------------------------------------------------------

element :: Text -> [Attribute] -> [Html] -> Html
element tagName attrs inner = Html tagName attrs (Element inner)

voidElement :: Text -> [Attribute] -> Html
voidElement tagName attrs = Html tagName attrs VoidElement

text :: Text -> Html
text t = Html "text" [] (TextElement t)

blank :: Html
blank =
  text ""

div_ :: [Attribute] -> [Html] -> Html
div_ = element "div"

span_ :: [Attribute] -> [Html] -> Html
span_ = element "span"

p :: [Attribute] -> [Html] -> Html
p = element "p"

a :: [Attribute] -> [Html] -> Html
a = element "a"

small :: [Attribute] -> [Html] -> Html
small = element "small"

strong :: [Attribute] -> [Html] -> Html
strong = element "strong"

em :: [Attribute] -> [Html] -> Html
em = element "em"

label :: [Attribute] -> [Html] -> Html
label = element "label"

img :: [Attribute] -> Html
img = voidElement "img"

article :: [Attribute] -> [Html] -> Html
article = element "article"

header :: [Attribute] -> [Html] -> Html
header = element "header"

section :: [Attribute] -> [Html] -> Html
section = element "section"

footer :: [Attribute] -> [Html] -> Html
footer = element "footer"

h1 :: [Attribute] -> [Html] -> Html
h1 = element "h1"

h2 :: [Attribute] -> [Html] -> Html
h2 = element "h2"

h3 :: [Attribute] -> [Html] -> Html
h3 = element "h3"

h4 :: [Attribute] -> [Html] -> Html
h4 = element "h4"

h5 :: [Attribute] -> [Html] -> Html
h5 = element "h5"

h6 :: [Attribute] -> [Html] -> Html
h6 = element "h6"

ol :: [Attribute] -> [Html] -> Html
ol = element "ol"

ul :: [Attribute] -> [Html] -> Html
ul = element "ul"

li :: [Attribute] -> [Html] -> Html
li = element "li"

details :: [Attribute] -> [Html] -> Html
details = element "details"

summary :: [Attribute] -> [Html] -> Html
summary = element "summary"

pre :: [Attribute] -> [Html] -> Html
pre = element "pre"

code :: [Attribute] -> [Html] -> Html
code = element "code"

table :: [Attribute] -> [Html] -> Html
table = element "table"

thead :: [Attribute] -> [Html] -> Html
thead = element "thead"

tbody :: [Attribute] -> [Html] -> Html
tbody = element "tbody"

tfoot :: [Attribute] -> [Html] -> Html
tfoot = element "tfoot"

tr :: [Attribute] -> [Html] -> Html
tr = element "tr"

th :: [Attribute] -> [Html] -> Html
th = element "th"

td :: [Attribute] -> [Html] -> Html
td = element "td"

aside :: [Attribute] -> [Html] -> Html
aside = element "aside"

blockquote :: [Attribute] -> [Html] -> Html
blockquote = element "blockquote"

hr :: [Attribute] -> Html
hr = voidElement "hr"

br :: [Attribute] -> Html
br = voidElement "br"

-- Rendering ------------------------------------------------------------------

toText :: Html -> Text
toText html =
  let openTag name attrs =
        let renderedAttrs =
              List.foldl (\acc attr -> acc <> " " <> Attribute.toText attr) "" attrs
         in "<" <> name <> renderedAttrs <> ">"

      selfClosingTag name attrs =
        let renderedAttrs =
              List.foldl (\acc attr -> acc <> " " <> Attribute.toText attr) "" attrs
         in "<" <> name <> renderedAttrs <> " />"

      closeTag name =
        "</" <> name <> ">"

      renderChildren children =
        List.foldl (\acc c -> acc <> toText c) "" children
   in case html of
        Html _ _ (TextElement t) ->
          t
        Html tagName attrs VoidElement ->
          selfClosingTag tagName attrs
        Html tagName attrs (Element children) ->
          openTag tagName attrs <> renderChildren children <> closeTag tagName
