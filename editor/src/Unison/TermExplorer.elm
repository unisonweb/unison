module Unison.TermExplorer where

import Debug
import Elmz.Layout as Layout
import Elmz.Layout (Layout)
import Elmz.Movement as Movement
import Graphics.Element as Element
import Graphics.Element (Element)
import Graphics.Input.Field as Field
import Maybe
import Result
import Signal
import Unison.Metadata as Metadata
import Unison.Metadata (Query)
import Unison.Node as Node
import Unison.Path as Path
import Unison.Path (Path)
import Unison.SearchboxParser as SearchboxParser
import Unison.Styles as Styles
import Unison.Term as Term
import Unison.Term (Term)
import Unison.Type as Type
import Unison.Type (Type)
import Unison.View as View

type alias S0 =
  { isKeyboardOpen : Bool
  , prompt : String
  , content : Field.Content
  -- does not include the layout of the input field
  , layout : Layout (Maybe Int) }

s00 : S0
s00 =
  { isKeyboardOpen = True
  , prompt = ""
  , content = Field.noContent
  , layout = Layout.embed Nothing Element.empty }

type alias S1 =
  { info : Node.LocalInfo
  -- includes invalid completions (which have `Nothing` as edit function)
  , keyedCompletions : List (String, Element, Maybe (Term -> Term))
  , s0 : S0 }

type Model
  = Initializing S0
  | Ready S1
  | Searching S1
  | Closed

type Request
  = Open (Term,Path)
  | Search (Term, Path, Int, Metadata.Query, Maybe Type)
  | Noop

type Response
  = LocalInfo Node.LocalInfo
  | SearchResults Node.SearchResults

inputString model = case model of
  Closed -> ""
  Initializing s -> s.content.string
  Ready s -> s.s0.content.string
  Searching s -> s.s0.content.string

s1 : Model -> Maybe S1
s1 model = case model of
  Ready s1 -> Just s1
  Searching s1 -> Just s1
  _ -> Nothing

admissible : Model -> Maybe Type
admissible model = Maybe.map (\s1 -> s1.info.admissible) (s1 model)

parseSearchbox : Model -> List (String, Element, Maybe (Term -> Term))
parseSearchbox model = let s = inputString model
  in case SearchboxParser.parseTerm s of
    Result.Err _   -> [(s, Styles.codeText s, Nothing )]
    Result.Ok term -> case View.literalKey term of
      Just k ->
        let
          edit = admissible model `Maybe.andThen`
            \typ -> if Term.checkLiteral term typ then Just (always term)
                    else Nothing
        in [(inputString model, Styles.codeText k, edit)]
      Nothing -> Debug.crash "unpossible"

keyedCompletions : View.Env -> Model -> List (String, Element, Maybe (Term -> Term))
keyedCompletions env model = parseSearchbox model ++ case s1 model of
  Nothing -> []
  Just s1 -> s1.keyedCompletions

view : (Field.Content -> Signal.Message) -> Model -> Layout (Maybe Int)
view searchbox model =
  let
    fld s ok w =
      let c = Styles.statusColor ok
      in Element.flow Element.down
           [ Element.spacer 1 15
           , Element.spacer 9 1 `Element.beside` Styles.carotUp 6 c
           , Maybe.withDefault identity (Maybe.map Element.width w) <|
               Field.field (Styles.autocomplete False)
                           searchbox
                           s.prompt
                           s.content
           ]
  in
    case model of
      Initializing s -> todo

todo : a
todo = todo

-- Open the explorer at the given location
openAt : Term -> Path -> Model -> (Model, Request)
openAt term path _ =
  (Initializing s00, Open (term,path))

-- Provide a `LocalInfo` at the given location
localInfo : Term -> Path -> Node.LocalInfo -> Model -> (Model, Request)
localInfo term path info model = case model of
  Initializing s0 ->
    let search = Search (term, path, 7, Metadata.Query s0.content.string, Just info.admissible)
    -- todo - actually populate keyedCompletions
    in (Searching { info = info, keyedCompletions = [], s0 = s0 }, search)
  _ -> (model, Noop)
