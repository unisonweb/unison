module Unison.TermExplorer where

import Debug
import Elmz.Moore (Moore)
import Elmz.Layout as Layout
import Elmz.Layout (Layout)
import Elmz.Movement as Movement
import Graphics.Element as Element
import Graphics.Element (Element)
import Graphics.Input.Field as Field
import List
import Maybe
import Result
import Signal
import String
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

{-
type alias LocalFocus =
  { rootTerm : Term
  , pathToClosedSubterm : Path
  , closedSubterm : Term
  , pathFromClosedSubterm : Path }

type Event
  = Open LocalFocus
  | Move Movement.D1
  | Mouse (Int,Int)
  | Click (Int,Int)
  | SearchResults Node.SearchResults
  | LocalInfo Node.LocalInfo
  | FieldContent Field.Content

type alias Model' =
  Moore Event { selection : Maybe Term
              , view : Element
              , request : Maybe Request }
-}

type alias S0 =
  { path : Path
  , term : Term
  , isKeyboardOpen : Bool
  , prompt : String
  , content : Field.Content
  -- does not include the layout of the input field
  , layout : Layout (Maybe Int) }

s00 : S0
s00 =
  { path = []
  , term = Term.Blank
  , isKeyboardOpen = True
  , prompt = ""
  , content = Field.noContent
  , layout = Layout.embed Nothing Element.empty }

type alias S1 =
  { info : Node.LocalInfo
  -- includes invalid completions (which have `Nothing` as edit function)
  , localCompletions : List (String, Element, Maybe Term)
  -- Moore (Either SearchResults String) (List (String,Element,Maybe Term), Maybe Request)
  , searchCompletions : List (String, Element, Maybe Term)
  , s0 : S0 }

type Model
  = Initializing S0
  | Ready S1
  | Searching S1
  | Closed

type Request
  = Open (Term, Path)
  | Search (Term, Path, Int, Metadata.Query, Maybe Type)
  | Noop

type Response
  = LocalInfo Node.LocalInfo
  | SearchResults Node.SearchResults

-- Open the explorer at the given location
openAt : Term -> Path -> Model -> (Model, Request)
openAt term path _ =
  (Initializing { s00 | path <- path, term <- term }, Open (term,path))

-- Provide `LocalInfo` to the explorer
localInfo : View.Env -> Node.LocalInfo -> Model -> (Model, Request)
localInfo viewEnv info model = case model of
  Initializing s0 ->
    let search = Search (s0.term, s0.path, 7, Metadata.Query s0.content.string, Just info.admissible)
        la cur n = (String.padLeft (n+1) '.' "", showAppBlanks viewEnv s0.path n, Just (appBlanks n cur))
        currentApps = case Term.at s0.path s0.term of
          Nothing -> []
          Just cur -> List.map (la cur) info.localApplications
        completions = currentApps ++ List.map (searchEntry True viewEnv s0.path) info.wellTypedLocals
    in (Searching { info = info, localCompletions = completions, searchCompletions = [], s0 = s0 }, search)
  _ -> (model, Noop)

-- searchResults : Node.SearchResults ->
-- mouse : (Int,Int)
-- content : Field.Content ->

layout : View.Env -> (Field.Content -> Signal.Message) -> Model -> Layout (Maybe Int)
layout viewEnv searchbox model =
  let
    valids = validCompletions model
    invalids = invalidCompletions model
    ok = not (List.isEmpty valids)
    fld s w = Layout.embed Nothing <| Element.flow Element.down
        [ Element.spacer 1 15
        , Element.spacer 9 1 `Element.beside` Styles.carotUp 6 (Styles.statusColor ok)
        , Maybe.withDefault identity (Maybe.map Element.width w) <|
             Field.field (Styles.autocomplete ok)
                         searchbox
                         s.prompt
                         s.content
        , Element.spacer 1 10 ]
    bottom : S1 -> Layout (Maybe Int)
    bottom s =
      let
        above : Element
        above = Element.flow Element.down <|
          [ Element.spacer 1 10
          , pad << Styles.boldCodeText <|
              Type.key { metadata = viewEnv.metadata } s.info.admissible
          , Element.spacer 1 12
          , pad <| Styles.currentSymbol `Element.beside`
                   Styles.codeText (" : " ++ Type.key { metadata = viewEnv.metadata } s.info.current)
          ]
          ++ List.map (renderTerm viewEnv (path model)) s.info.locals
          ++ [ Element.spacer 1 10 ]
        fit e = Element.width ((Element.widthOf above - 12) `max` (Element.widthOf e)) e
        renderedValids = List.indexedMap (\i ((_,e),_) -> Layout.embed (Just i) (fit e)) valids
        renderedInvalids = List.map (\(_,e) -> Layout.embed Nothing (fit e)) invalids
        sep = Layout.embed Nothing (Styles.menuSeparator (Element.widthOf above `max` Layout.widthOf below))
        below : Layout (Maybe Int)
        below =
          let cells = if List.isEmpty valids && not (List.isEmpty invalids)
                      then Styles.explorerCells Nothing renderedInvalids
                      else Styles.explorerCells Nothing renderedValids
          in if List.isEmpty valids && List.isEmpty invalids then Layout.empty Nothing
             else Layout.vertical Nothing [sep, Layout.embed Nothing (Element.spacer 1 5), cells]
      in
        Styles.explorerOutline (Styles.statusColor ok) <|
        Layout.above
          Nothing
          (Layout.embed Nothing above)
          below
  in
    case model of
      Initializing s -> fld s Nothing
      Ready s1 ->
        let b = bottom s1
        in Layout.above Nothing (fld s1.s0 (Just (Layout.widthOf b))) b
      Searching s1 -> -- todo: spinner or color change to indicate loading
        let b = bottom s1
        in Layout.above Nothing (fld s1.s0 (Just (Layout.widthOf b))) b
      Closed -> Layout.empty Nothing

inputString model = case model of
  Closed -> ""
  Initializing s -> s.content.string
  Ready s -> s.s0.content.string
  Searching s -> s.s0.content.string

path : Model -> Path
path model = case model of
  Closed -> []
  Initializing s -> s.path
  Ready s -> s.s0.path
  Searching s -> s.s0.path

s1 : Model -> Maybe S1
s1 model = case model of
  Ready s1 -> Just s1
  Searching s1 -> Just s1
  _ -> Nothing

admissible : Model -> Maybe Type
admissible model = Maybe.map (\s1 -> s1.info.admissible) (s1 model)

parseSearchbox : Model -> List (String, Element, Maybe Term)
parseSearchbox model = let s = inputString model
  in case SearchboxParser.parseTerm s of
    Result.Err _   -> [(s, Styles.codeText s, Nothing )]
    Result.Ok term -> case View.literalKey term of
      Just k ->
        let
          edit = admissible model `Maybe.andThen`
            \typ -> if Term.checkLiteral term typ then Just term
                    else Nothing
        in [(inputString model, Styles.codeText k, edit)]
      Nothing -> Debug.crash "unpossible"

keyedCompletions : Model -> List (String, Element, Maybe Term)
keyedCompletions model = parseSearchbox model ++ case s1 model of
  Nothing -> []
  Just s1 -> s1.localCompletions ++ s1.searchCompletions

validCompletions' : List (x, y, Maybe z) -> List ((x,y),z)
validCompletions' entries =
  List.filterMap (\(x,y,z) -> Maybe.map (\z -> ((x, y),z)) z) entries

validCompletions : Model -> List ((String, Element), Term)
validCompletions model = validCompletions' (keyedCompletions model)

invalidCompletions : Model -> List (String, Element)
invalidCompletions model =
  let f (x,y,f) = case f of
    Nothing -> Just (x,y)
    Just _ -> Nothing
  in List.filterMap f (keyedCompletions model)

box = Term.Embed (Layout.embed { path = [], selectable = False } Styles.currentSymbol)
appBlanks n e = List.foldl (\_ cur -> Term.App cur Term.Blank) e [1 .. n]

showAppBlanks viewEnv path n = renderTerm viewEnv path (appBlanks n box)

searchKey : View.Env -> Path -> Term -> String
searchKey viewEnv path e =
  View.key viewEnv { path = path, term = e, boundAt = Path.boundAt }

renderExplorerEntry : View.Env -> Path -> Term -> Element
renderExplorerEntry viewEnv path e = Layout.element <|
  View.layout' viewEnv { path = path, term = e, boundAt = Path.boundAt }

searchEntry : Bool -> View.Env -> Path -> Term -> (String,Element,Maybe Term)
searchEntry valid viewEnv path e =
  (searchKey viewEnv path e, renderExplorerEntry viewEnv path e, if valid then Just e else Nothing)

localCompletions : View.Env -> Node.LocalInfo -> List (String,Element,Maybe (Term -> Term))
localCompletions viewEnv info = Debug.crash "todo"

pad e = let s = Element.spacer 10 1 in Element.flow Element.right [s, e, s]
renderTerm viewEnv path expr = pad << Layout.element <|
  View.layout' viewEnv { term = expr, path = path, boundAt = Path.boundAt }
