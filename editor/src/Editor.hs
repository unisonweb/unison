{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Reflex
import Reflex.Dom
import Unison.Dimensions (Width(..),X(..),Y(..))
import Unison.Term
import Unison.UI (mouseMove')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Reflex.Dynamic as Dynamic
import qualified Unison.DocView as DocView
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.MemNode as MemNode
import qualified Unison.Note as Note
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.TermExplorer as TermExplorer
import qualified Unison.Type as Type
import qualified Unison.UI as UI
import qualified Unison.Views as Views

term = builtin "Vector.concatenate" `app`
         (vector (map num [11..15])) `app`
         (vector ([builtin "Number.plus" `app` num 1 `app` num 1, num 2, num 9]))

termDoc = Views.term Views.defaultSymbol term

main :: IO ()
main = mainWidget $ do
  node <- liftIO MemNode.make
  symbols <- (liftIO . Note.run . Node.metadatas node . Set.toList . Term.dependencies') term
  keydown <- UI.windowKeydown
  let firstName (Metadata.Names (n:_)) = n
  let lookupSymbol ref = maybe (Views.defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref symbols)
  let termDoc = Views.term lookupSymbol term
  (e, dims, path) <- elClass "div" "root" $ DocView.widget keydown (Width 300) termDoc
  highlightedType <- holdDyn (Type.v' "..") =<< dyn =<< mapDyn (liftIO . Note.run . Node.typeAt node term) path
  el "div" $ do
     text "type: "
     el "pre" $ display highlightedType
  return ()

