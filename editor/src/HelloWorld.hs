{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex
import Reflex.Dom
import Unison.Term
import Unison.Type (defaultSymbol)
import Unison.Dimensions (Width(..),X(..),Y(..))
import qualified Unison.DocView as DocView
import Unison.UI (mouseMove')

term = var' "foo" `app`
       vector (map num [0..4]) `app`
       var' "bar" `app`
       (var' "baz" `app` num 42)

termDoc = view defaultSymbol term

main :: IO ()
main = mainWidget $ do
  el "pre" $ text "Layout of a single Unison term, with path resolution"
  (e,d) <- DocView.widget (Width 200) termDoc
  mouse <- (mouseMove' e >>= holdDyn (X 0, Y 0))
  el "pre" $ do
    text "mouse: "
    display mouse
  el "pre" $ do
    text "path: "
    display =<< mapDyn (concat . DocView.at d) mouse
    pure ()
