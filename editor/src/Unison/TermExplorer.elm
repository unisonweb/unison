module Unison.TermExplorer where

import Debug
import Dict (Dict)
import Dict as Dict
import Elmz.Moore (Moore(..))
import Elmz.Moore as M
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

type alias LocalFocus =
  { rootTerm : Term
  , pathToClosedSubterm : Path
  , closedSubterm : Term
  , pathFromClosedSubterm : Path }

path : LocalFocus -> Path
path focus = focus.pathToClosedSubterm ++ focus.pathFromClosedSubterm

type Event
  = Open LocalFocus Field.Content View.Env
  | Move Movement.D1
  | Mouse (Int,Int)
  | Click (Int,Int)
  | SearchResults Node.SearchResults
  | LocalInfoResults Node.LocalInfo
  | FieldContent Field.Content
  | Env View.Env

type alias Model =
  Moore Event { selection : Maybe (LocalFocus, Term)
              , view : Element
              , request : Maybe Request }

type Request
  = LocalInfo LocalFocus
  | Search (Term, Path, Int, Metadata.Query, Maybe Type)

model : (Field.Content -> Signal.Message) -> Model
model searchbox =
  let
    closed e = case e of
      Open focus content env -> Just <|
        let f = viewField searchbox False content Nothing
        in Moore { selection = Nothing, request = Just (LocalInfo focus), view = f } (initialize focus content env)
      _ -> Nothing

    initialize focus content env e = case e of
      LocalInfoResults info -> Just <|
        let req = Search ( focus.closedSubterm
                         , focus.pathFromClosedSubterm
                         , 7
                         , Metadata.Query content.string
                         , Just info.admissible )
            la cur n = (String.padLeft (n+1) '.' "", showAppBlanks env (path focus) n, Just (appBlanks n cur))
            currentApps = case Term.at focus.pathFromClosedSubterm focus.closedSubterm of
              Nothing -> []
              Just cur -> List.map (la cur) info.localApplications
            locals = currentApps ++ List.map (searchEntry True env (path focus)) info.wellTypedLocals
            lits = parseSearchbox info.admissible content.string
            layout' = layout env (path focus) searchbox info (lits ++ locals) content
            vw = Layout.element layout'
        in Moore { selection = Nothing, request = Just req, view = vw }
                 (search focus content env info lits locals [] layout')
      _ -> Nothing

    search focus content env info lits localCompletions searchCompletions layout' e = case e of
      SearchResults results -> Just <|
        let
          searchCompletions' = processSearchResults results
          layout'' = layout env (path focus) searchbox info (lits ++ localCompletions ++ searchCompletions') content
        in Moore { selection = Nothing, request = Nothing, view = Layout.element layout'' } <|
           search focus content env info lits localCompletions searchCompletions' layout''
      _ -> Nothing

  in Moore { selection = Nothing, request = Nothing, view = Element.empty } closed

processSearchResults : Node.SearchResults -> List (String, Element, Maybe Term)
processSearchResults results = [] -- todo

parseSearchbox : Type -> String -> List (String, Element, Maybe Term)
parseSearchbox admissible s =
  case SearchboxParser.parseTerm s of
    Result.Err _   -> [(s, Styles.codeText s, Nothing )]
    Result.Ok term -> case View.literalKey term of
      Just k ->
        let edit = if Term.checkLiteral term admissible then Just term else Nothing
        in [(s, Styles.codeText k, edit)]
      Nothing -> Debug.crash "unpossible"

layout : View.Env -> Path -> (Field.Content -> Signal.Message) -> Node.LocalInfo
      -> List (String,Element,Maybe Term)
      -> Field.Content
      -> Layout (Maybe Int)
layout viewEnv path searchbox info keyedCompletions content =
  let
    valids = validCompletions keyedCompletions
    invalids = invalidCompletions keyedCompletions
    ok = not (List.isEmpty valids)
    above : Element
    above = Element.flow Element.down <|
      [ Element.spacer 1 10
      , pad << Styles.boldCodeText <| Type.key { metadata = viewEnv.metadata } info.admissible
      , Element.spacer 1 12
      , pad <| Styles.currentSymbol `Element.beside`
               Styles.codeText (" : " ++ Type.key { metadata = viewEnv.metadata } info.current)
      ]
      ++ List.map (renderTerm viewEnv path) info.locals
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
    resultsBox = Styles.explorerOutline (Styles.statusColor ok) <|
      Layout.above Nothing (Layout.embed Nothing above) below
    inputBox = Layout.vertical Nothing
      [ Layout.embed Nothing (viewField searchbox ok content (Just (Layout.widthOf resultsBox)))
      , Layout.embed Nothing (Element.spacer 1 10) ]
  in
    Layout.above Nothing inputBox resultsBox

viewField : (Field.Content -> Signal.Message) -> Bool -> Field.Content -> Maybe Int -> Element
viewField searchbox ok content w =
  Element.flow Element.down
    [ Element.spacer 1 15
    , Element.spacer 9 1 `Element.beside` Styles.carotUp 6 (Styles.statusColor ok)
    , Maybe.withDefault identity (Maybe.map Element.width w) <|
        Field.field (Styles.autocomplete ok)
                     searchbox
                     ""
                     content
    , Element.spacer 1 10 ]

validCompletions : List (String,Element,Maybe Term) -> List ((String, Element), Term)
validCompletions entries =
  List.filterMap (\(x,y,z) -> Maybe.map (\z -> ((x,y),z)) z) entries

invalidCompletions : List (String,Element,Maybe Term) -> List (String, Element)
invalidCompletions entries =
  let f (x,y,f) = case f of
    Nothing -> Just (x,y)
    Just _ -> Nothing
  in List.filterMap f entries

{-
type Model
  = Initializing S0
  | Ready S1
  | Searching S1
  | Closed

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

keyedCompletions : Model -> List (String, Element, Maybe Term)
keyedCompletions model = parseSearchbox model ++ case s1 model of
  Nothing -> []
  Just s1 -> s1.localCompletions ++ s1.searchCompletions

validCompletions' : List (x, y, Maybe z) -> List ((x,y),z)
validCompletions' entries =
  List.filterMap (\(x,y,z) -> Maybe.map (\z -> ((x, y),z)) z) entries

validCompletions : Model -> List ((String, Element), Term)
validCompletions model = validCompletions' (keyedCompletions model)

-}

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
