{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex
import Reflex.Dom
import Unison.Term
import Unison.Type (defaultSymbol)
import Unison.Dimensions (Width(..))
import qualified Unison.DocView as DocView
import Unison.UI (mouseMove')

term = var' "foo" `app`
         vector [num 1, num 2, num 3, num 4] `app`
         var' "bar" `app`
         (var' "baz" `app` num 42)

termDoc = view defaultSymbol term

main :: IO ()
main = mainWidget $ do
  (e,d) <- DocView.widget (Width 200) termDoc
  el "pre" $ text (toString term)
  mouse <- mouseMove' e
  path <- holdDyn "None" (fmap (show . DocView.at d) mouse)
  text "path: "
  dynText path
  pure ()
