module Unison.EditableTerm where

import Elmz.Layout as Layout
import Elmz.Layout (Layout)
import Elmz.Moore (Moore(..))
import Elmz.Moore as Moore
import Elmz.Movement as Movement
import Elmz.Trie as Trie
import Elmz.Trie (Trie)
import Graphics.Element as Element
import Graphics.Element (Element)
import List
import Maybe
import Unison.Path as Path
import Unison.Path (Path)
import Unison.Reference (Reference)
import Unison.Metadata (Metadata)
import Unison.Metadata as Metadata
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term as Term
import Unison.Term (Term)
import Unison.View as View

-- todo: make EditableTerm responsible for maintaining local names
-- and just have it receive the Reference -> Metadata externally

type Event
  = Mouse (Int,Int)
  | Movement Movement.D2
  | Modify (Term -> Term)
  | AvailableWidth Int
  | Metadata (Reference -> Metadata)
  | Evaluations (List { path : Path, old : Term, new : Term })
  | ToggleRaw

type alias Out =
  { term : Term
  , layout : Layout View.L
  , scope : Scope.Model
  , dirtyPaths : Trie Path.E () }

out : Term -> Layout View.L -> Scope.Model -> Out
out term layout scope = Out term layout scope Trie.empty

type alias Model = Moore Event Out

model : Term -> Model
model term =
  let
    refresh : Maybe Int -> (Reference -> Metadata) -> Bool -> Trie Path.E Term -> Out -> Out
    refresh w md raw evals s = case w of
      Nothing -> { s | layout <- Layout.empty { path = [], selectable = False } }
      Just w ->
        let l = View.layout s.term
                  { availableWidth = w
                  , metadata = md
                  , overrides p = Trie.lookup p evals
                  , raw = if raw then Maybe.map .focus s.scope else Nothing }
        in { s | layout <- l }

    next : Maybe Int -> (Reference -> Metadata) -> Bool -> Trie Path.E Term -> Out -> Event -> Maybe Model
    next w md raw evals s e = case e of
      Metadata md ->
        let s' = refresh w md raw evals s
        in Just <| Moore s' (next w md raw evals s')
      AvailableWidth w ->
        let s' = refresh (Just w) md raw evals s
        in Just <| Moore s' (next (Just w) md raw evals s')
      ToggleRaw ->
        let s' = refresh w md (not raw) evals s
        in Just <| Moore s' (next w md (not raw) evals s')
      Evaluations es ->
        let
          f e = if Term.at e.path s.term == Just e.old then Just (e.path, e.new) else Nothing
          evals' = Trie.fromList (List.filterMap f es)
          s' = refresh w md raw evals' s
        in
          if Trie.isEmpty evals' then Nothing
          else Just <| Moore s' (next w md raw evals' s')
      Mouse xy -> case Scope.mouse xy s.layout Nothing of
        scope -> let s' = refresh w md raw evals { s | scope <- scope }
                 in Just <| Moore s' (next w md raw evals s')
      Movement d2 -> case Scope.movement s.term d2 s.scope of
        scope -> let s' = refresh w md raw evals { s | scope <- scope }
                 in Just <| Moore s' (next w md raw evals s')
      Modify f -> s.scope `Maybe.andThen`
        \scope -> Term.modify scope.focus f s.term `Maybe.andThen`
        \term ->
          -- todo: can be less pessimistic here
          -- okay to keep overrides that don't depend on the value
          -- for this need to build dependency graph
          let
             evals' = Trie.deleteSubtree scope.focus evals
             s' = refresh w md raw evals' { s | term <- term
                                             , dirtyPaths <- View.reactivePaths term
                                             , scope <- Just (Scope.scope scope.focus) }
          in
            Just <| Moore s' (next w md raw evals' { s' | dirtyPaths <- Trie.empty })
    highlight o = Maybe.withDefault o <| o.scope `Maybe.andThen`
      \scope -> Scope.view o.layout scope `Maybe.andThen`
      \region ->
        let f e = Element.layers [e, Styles.selection region]
        in Just { o | layout <- Layout.transform f o.layout }
  in
    let s0 = out term (Layout.empty { path = [], selectable = False }) (Just (Scope.scope []))
    in Moore s0 (next Nothing Metadata.defaultMetadata False Trie.empty s0) |> Moore.map highlight
