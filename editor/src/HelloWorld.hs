{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad
import Reflex
import Reflex.Dom
import Unison.Term
import Unison.Type (defaultSymbol)
import Unison.Dimensions (Width(..),X(..),Y(..))
import qualified Unison.Doc as Doc
import qualified Unison.DocView as DocView
import qualified Reflex.Dynamic as Dynamic
import Unison.UI (mouseMove')

term = var' "foo" `app`
       vector (map num [0..5]) `app`
       var' "bar" `app`
       (var' "baz" `app` num 42)

termDoc = view defaultSymbol term

main :: IO ()
main = mainWidget $ do
  _ <- el' "div" $ DocView.widget (Width 300) termDoc
  return ()

