module Unison.EditableTerm where

import Elmz.Layout as Layout
import Elmz.Layout (Layout)
import Elmz.Movement as Movement
import Elmz.Trie as Trie
import Elmz.Trie (Trie)
import Graphics.Element as Element
import Graphics.Element (Element)
import Maybe
import Unison.Path as Path
import Unison.Path (Path)
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term as Term
import Unison.Term (Term)
import Unison.View as View

type alias Model =
  { term : Term
  , layout : Layout View.L
  , scope : Scope.Model }

type alias Action = Model -> Model

view : Model -> Element
view model = case Maybe.andThen model.scope (Scope.view model.layout) of
  Nothing -> Layout.element model.layout
  Just region ->
    Element.layers [ Layout.element model.layout
                   , Styles.selection region ]

mouse : (Int,Int) -> Action
mouse xy model =
  { model | scope <- Scope.mouse xy model.layout model.scope }

movement : Movement.D2 -> Action
movement d2 model =
  { model | scope <- Scope.movement model.term d2 model.scope }

-- Returns the new model, and a set of paths that now need reevaluation
modify : View.Env -> Path -> (Term -> Term) -> Model -> (Model, Trie Path.E ())
modify env path f model = case Term.modify path f model.term of
  Nothing -> (model, Trie.empty)
  Just term ->
    let
      model' = { model | term <- term
                       , layout <- View.layout term env
                       , scope <- Just (Scope.scope path) }
      -- todo: be less pessimistic here
      dirty = View.reactivePaths term
    in
      (model', dirty)
