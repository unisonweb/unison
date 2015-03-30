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
import Maybe
import Unison.Path as Path
import Unison.Path (Path)
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term as Term
import Unison.Term (Term)
import Unison.View as View

type Event
  = Mouse (Int,Int)
  | Movement Movement.D2
  | Modify (Term -> Term)
  | Env View.Env

type alias Out =
  { term : Term
  , layout : Layout View.L
  , scope : Scope.Model
  , dirtyPaths : Trie Path.E () }

out : Term -> Layout View.L -> Scope.Model -> Out
out term layout scope = Out term layout scope Trie.empty

type alias Model = Moore Event Out

model : View.Env -> Term -> Model
model env term =
  let
    next env s e = case e of
      Mouse xy -> case Scope.mouse xy s.layout Nothing of
        scope -> let s' = { s | scope <- scope }
                 in Just <| Moore s' (next env s')
      Movement d2 -> case Scope.movement s.term d2 s.scope of
        scope -> let s' = { s | scope <- scope }
                 in Just <| Moore s' (next env s')
      Modify f -> s.scope `Maybe.andThen`
        \scope -> Term.modify scope.focus f s.term `Maybe.andThen`
        \term ->
          let o = { term = term
                  , layout = View.layout s.term env
                  , scope = Just (Scope.scope scope.focus)
                  -- todo, can be less pessimistic here
                  , dirtyPaths = View.reactivePaths term }
          in Just (Moore o (next env { o | dirtyPaths <- Trie.empty }))
      Env env -> Just (Moore s (next env s))
    highlight o = Maybe.withDefault o <| o.scope `Maybe.andThen`
      \scope -> Scope.view o.layout scope `Maybe.andThen`
      \region ->
        let f e = Element.layers [e, Styles.selection region]
        in Just { o | layout <- Layout.transform f o.layout }
  in
    let s0 = out term (View.layout term env) (Just (Scope.scope []))
    in Moore s0 (next env s0) |> Moore.map highlight
