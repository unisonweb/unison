{-# LANGUAGE OverloadedStrings #-}

module Unison.Server.Html.Attribute where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

type Nat = Word64

data Attribute = Attribute Text Text

-- Attributes -----------------------------------------------------------------

data_ :: Text -> Text -> Attribute
data_ name = Attribute ("data-" <> name)

class_ :: Text -> Attribute
class_ = Attribute "class"

style :: Text -> Attribute
style = Attribute "style"

id_ :: Text -> Attribute
id_ = Attribute "id"

title :: Text -> Attribute
title = Attribute "title"

src :: Text -> Attribute
src = Attribute "src"

href :: Text -> Attribute
href = Attribute "href"

rel :: Text -> Attribute
rel = Attribute "rel"

start :: Nat -> Attribute
start n = Attribute "start" (Text.pack $ show n)

target :: Text -> Attribute
target = Attribute "target"

alt :: Text -> Attribute
alt = Attribute "alt"

open :: Attribute
open = Attribute "open" "open"

-- Rendering ------------------------------------------------------------------

toText :: Attribute -> Text
toText (Attribute attrName attrValue) =
  attrName <> "='" <> attrValue <> "'"
