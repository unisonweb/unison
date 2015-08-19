{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom
import Unison.Term
import Unison.Type (defaultSymbol)
import Unison.Dimensions (Width(..))
import qualified Unison.DocView as DocView

term = var' "foo" `app`
         vector [num 1, num 2, num 3, num 4] `app`
         var' "bar" `app`
         (var' "baz" `app` num 42)

termDoc = view defaultSymbol term

main :: IO ()
main = mainWidget $ do
  (e,d) <- DocView.widget (Width 10000) termDoc
  el "pre" $ text (toString term)
  pure ()
