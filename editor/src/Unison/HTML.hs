{-# LANGUAGE OverloadedStrings #-}

module Unison.HTML where

import Unison.Dom

hbox :: [Dom Node] -> Dom Node
hbox ds =
  el "div" [("class", "hbox")] $ map item ds
  where
  item d = el "div" [("class", "hitem")] [d]

vbox :: [Dom Node] -> Dom Node
vbox ds =
  el "div" [("class", "vbox")] $ map item ds
  where
  item d = el "div" [("class", "vitem")] [d]
