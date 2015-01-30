module Unison.Panel where

import Elmz.Layout (Containment,Layout,Pt,Region)
import Elmz.Layout as Layout
import Elmz.Movement as Movement
import Elmz.Trie (Trie)
import Elmz.Trie as Trie
import Graphics.Element (Element)
import Graphics.Element as Element
import Signal
import Unison.Hash (Hash)
import Unison.Metadata as Metadata
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Scope (Scope)
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term (Term)
import Unison.Term as Term
import Unison.View as View

type alias Model =
  { term : Term
  , scope : Scope.Model
  , dependents : Trie Path.E (List Path)
  , overrides : Trie Path.E (Layout View.L)
  , hashes : Trie Path.E Hash }

type alias Action = Model -> Model

view : Int -> Model -> (Layout View.L, Maybe Region)
view availableWidth model =
  let termLayout = View.layout model.term
                    { rootMetadata = Metadata.anonymousTerm
                    , availableWidth = availableWidth
                    , metadata h = Metadata.anonymousTerm
                    , overrides x = Nothing }
  in case model.scope of
       Nothing -> (termLayout, Nothing)
       Just scope -> Scope.view { layout = termLayout, term = model.term } scope

movement : Movement.D2 -> Action
movement d2 model = { model | scope <- Scope.movement model.term d2 model.scope }

reset : (Int,Int) -> Layout { a | path : Path } -> Action
reset xy layout model = { model | scope <- Scope.reset xy layout model.scope }

setTerm : Term -> Action
setTerm term model = { model | term <- term }
