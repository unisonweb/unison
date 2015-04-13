module Unison.TermExplorer where

import Debug
import Dict (Dict)
import Dict as Dict
import Elmz.Moore (Moore(..))
import Elmz.Moore as Moore
import Elmz.Layout as Layout
import Elmz.Layout (Layout)
import Elmz.Movement as Movement
import Elmz.Selection1D as Selection1D
import Elmz.Matcher as Matcher
import Graphics.Element as Element
import Graphics.Element (Element)
import Graphics.Input.Field as Field
import List
import Maybe
import Result
import Signal
import String
import Unison.Metadata as Metadata
import Unison.Metadata (Metadata,Query)
import Unison.Node as Node
import Unison.Path as Path
import Unison.Path (Path)
import Unison.Reference as Reference
import Unison.Reference (Reference)
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

localFocus : Path -> Term -> LocalFocus
localFocus path rootTerm =
  -- todo: tighten path to closed subterm
  { rootTerm = rootTerm
  , pathToClosedSubterm = []
  , closedSubterm = rootTerm
  , pathFromClosedSubterm = path }

path : LocalFocus -> Path
path focus = focus.pathToClosedSubterm ++ focus.pathFromClosedSubterm

type Event
  = Open View.Env LocalFocus Field.Content
  | Navigate (Selection1D.Event Term)
  | Enter
  | Click (Int,Int)
  | SearchResults Node.SearchResults
  | LocalInfoResults Node.LocalInfo
  | FieldContent Field.Content

type alias Model =
  Moore Event { selection : Maybe (LocalFocus, Term)
              , view : Element
              , request : Maybe Request }

type Request
  = LocalInfo LocalFocus
  | Search (Term, Path, Int, Metadata.Query, Maybe Type)

type alias Completions =
  { literals : List (String,Element,Maybe Term)
  , locals : List (String,Element,Maybe Term)
  , results : Matcher.Model (String,Element,Maybe Term) }

allCompletions : String -> Completions -> List (String,Element,Maybe Term)
allCompletions q c =
  let u = 42-- Debug.log "completions" c
  in c.results `Moore.feed` (Matcher.Query { string = q, values = c.literals ++ c.locals })
     |> Moore.extract |> .matches

model : (Field.Content -> Signal.Message) -> Model
model searchbox =
  let
    closed e = case e of
      Open env focus content -> Just <|
        let f = viewField searchbox False content Nothing
        in Moore { selection = Nothing, request = Just (LocalInfo focus), view = f } (initialize env focus content)
      _ -> Nothing

    initialize env focus content e = case e of
      LocalInfoResults info -> Just <|
        let req = Search ( focus.closedSubterm
                         , focus.pathFromClosedSubterm
                         , 7
                         , content.string
                         , Just info.admissible )
            la cur n = (String.padLeft (n+1) '.' "", showAppBlanks env (path focus) n, Just (appBlanks n cur))
            currentApps = case Term.at focus.pathFromClosedSubterm focus.closedSubterm of
              Nothing -> []
              Just cur -> List.map (la cur) info.localApplications
            completions =
              { locals = currentApps ++ List.map (searchEntry True env (path focus)) info.wellTypedLocals
              , literals = parseSearchbox info.admissible content.string
              , results = Matcher.model match }
            infoLayout' = infoLayout env (path focus) info
            (sel, layout') = layout env.metadata
                                     (path focus)
                                     searchbox
                                     (allCompletions content.string completions)
                                     Selection1D.model
                                     content
                                     infoLayout'
            vw = Layout.element layout'
        in Moore { selection = Nothing, request = Just req, view = vw }
                 (search info.admissible env focus completions sel content infoLayout' layout')
      _ -> Nothing

    search admissible env focus completions sel content infoLayout layout' e = case e of
      SearchResults results -> Just <|
        let
          completions' = processSearchResults env results completions content.string
          dict = Dict.fromList results.references
          metadata' r = case Dict.get (Reference.toKey r) dict of
            Nothing -> env.metadata r
            Just md -> md
          matches = Moore.extract completions'.results |> .matches
          (sel', layout'') = layout metadata' (path focus) searchbox matches sel content infoLayout
        in Moore { selection = Nothing, request = Nothing, view = Layout.element layout'' } <|
           search admissible { env | metadata <- metadata' } focus completions' sel' content infoLayout layout''
      Navigate nav -> Moore.step sel { event = Just nav, layout = layout' } `Maybe.andThen`
        \sel -> Just <|
          let (sel'', layout'') = layout env.metadata (path focus) searchbox
                                         (allCompletions content.string completions)
                                         sel content infoLayout
          in Moore { selection = Nothing, request = Nothing, view = Layout.element layout'' } <|
             search admissible env focus completions sel'' content infoLayout layout''
      Enter ->
        let valids = validCompletions (.matches << Moore.extract <| completions.results)
        in Maybe.withDefault (Just state0) ( (Moore.extract sel |> .index) `Maybe.andThen`
            \i -> Selection1D.index i valids `Maybe.andThen`
            \(_,term) -> (Just << Just) (
              Moore { selection = Just (focus, term), request = Nothing, view = Element.empty }
              closed
           ))
      Click xy -> case Moore.feed sel { event = Just (Selection1D.Mouse xy), layout = layout' } of
        sel -> case Moore.extract sel |> .index of
          -- click on unselectable region is a noop
          Nothing -> Just <|
            Moore { selection = Nothing, request = Nothing, view = Layout.element layout' }
                  (search admissible env focus completions sel content infoLayout layout')
          -- if click is on a selectable region, we accept it
          Just _ -> Layout.leafAtPoint layout' (Layout.ptFromPair xy) `Maybe.andThen` \i -> i `Maybe.andThen` \_ ->
            search admissible env focus completions sel content infoLayout layout' Enter
      FieldContent content -> Just <| case { completions | literals <- parseSearchbox admissible content.string } of
        completions ->
        let
          q = Matcher.Query { string = content.string, values = completions.literals ++ completions.locals }
          results = Moore.feed completions.results q
          matches = Moore.extract results |> .matches
          completions' = { completions | results <- results }
          (sel', layout'') = layout env.metadata (path focus) searchbox matches sel content infoLayout
          req = Maybe.map mkquery (Moore.extract results |> .query)
          mkquery q = Search ( focus.closedSubterm
                             , focus.pathFromClosedSubterm
                             , 7
                             , q
                             , Just admissible )
        in
          Moore { selection = Nothing, request = req, view = Layout.element layout'' }
          (search admissible env focus completions' sel' content infoLayout layout'')
      _ -> Nothing

    match s (k,_,_) =
      String.startsWith (String.toLower s) (String.toLower k)
    state0 = Moore { selection = Nothing, request = Nothing, view = Element.empty } closed
  in state0

processSearchResults : View.Env -> Node.SearchResults -> Completions -> String -> Completions
processSearchResults env results cs query =
  let valids = List.map (searchEntry True env []) (fst results.matches)
      invalids = List.map (searchEntry False env []) (fst results.illTypedMatches)
      msg = Matcher.Results
        { query = results.query, positionsExamined = results.positionsExamined
        , additionalResults = snd results.matches + snd results.illTypedMatches
        , values = valids ++ invalids }
      msg2 = Matcher.Query { string = query, values = cs.literals ++ cs.locals }
  in { cs | results <- Moore.feeds cs.results [ msg, msg2 ]  }

parseSearchbox : Type -> String -> List (String, Element, Maybe Term)
parseSearchbox admissible s =
  case SearchboxParser.parseTerm s of
    Result.Err _   -> [(s, Styles.codeText s, Nothing )]
    Result.Ok term -> case View.literalKey term of
      Just k ->
        let edit = if Term.checkLiteral term admissible then Just term else Nothing
        in [(s, Styles.codeText k, edit)]
      Nothing -> Debug.crash "unpossible"

infoLayout : View.Env -> Path -> Node.LocalInfo -> Element
infoLayout viewEnv path info = Element.flow Element.down <|
  [ Element.spacer 1 10
  , pad << Styles.boldCodeText <| Type.key { metadata = viewEnv.metadata } info.admissible
  , Element.spacer 1 12
  , pad <| Styles.currentSymbol `Element.beside`
           Styles.codeText (" : " ++ Type.key { metadata = viewEnv.metadata } info.current)
  ]
  ++ List.map (renderTerm viewEnv path) info.locals
  ++ [ Element.spacer 1 10 ]

layout : (Reference -> Metadata)
      -> Path
      -> (Field.Content -> Signal.Message)
      -> List (String,Element,Maybe Term)
      -> Selection1D.Model Term
      -> Field.Content
      -> Element
      -> (Selection1D.Model Term, Layout (Maybe Int))
layout md path searchbox keyedCompletions sel content infoLayout =
  let
    u = Debug.log "keyedCompletions" (List.map (\(k,_,_) -> k) keyedCompletions)
    above = infoLayout
    valids = validCompletions keyedCompletions
    invalids = invalidCompletions keyedCompletions
    ok = not (List.isEmpty valids)
    fit e = Element.width ((Element.widthOf above - 12) `max` (Element.widthOf e)) e
    renderedValids = List.indexedMap (\i ((_,e),_) -> Layout.embed (Just i) (fit e)) valids
    renderedInvalids = List.map (\(_,e) -> Layout.embed Nothing (fit e)) invalids
    cells = if List.isEmpty valids && not (List.isEmpty invalids)
            then Styles.explorerCells Nothing renderedInvalids
            else Styles.explorerCells Nothing renderedValids
    sep = Layout.embed Nothing (Styles.menuSeparator (Element.widthOf above `max` Layout.widthOf cells))
    below : Layout (Maybe Int)
    below =
      if List.isEmpty valids && List.isEmpty invalids then Layout.empty Nothing
      else Layout.vertical Nothing [sep, Layout.embed Nothing (Element.spacer 1 5), cells]
    resultsBox =
      Layout.above Nothing (Layout.embed Nothing above) below
      |> Styles.explorerOutline (Styles.statusColor ok)
    inputBox = Layout.embed Nothing (viewField searchbox ok content (Just (Layout.widthOf resultsBox)))
    sel' = Moore.feed sel { layout = resultsBox, event = Just (Selection1D.Values (List.map snd valids)) }
           `Moore.feed` { layout = resultsBox, event = Nothing }
    -- NB: careful to apply the highlight just to the results box, otherwise focus is stolen
    -- whenever the results box updates, see https://github.com/elm-lang/core/issues/3
    hl e = case Moore.extract sel' |> .region of
      Nothing -> e
      Just region -> Element.layers [e, Styles.explorerSelection region]
    result = Layout.above Nothing inputBox (Layout.transform hl resultsBox)
  in
    (sel', result)

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
    , Element.spacer 1 5 ]

validCompletions : List (String,Element,Maybe Term) -> List ((String, Element), Term)
validCompletions entries =
  List.filterMap (\(x,y,z) -> Maybe.map (\z -> ((x,y),z)) z) entries

invalidCompletions : List (String,Element,Maybe Term) -> List (String, Element)
invalidCompletions entries =
  let f (x,y,f) = case f of
    Nothing -> Just (x,y)
    Just _ -> Nothing
  in List.filterMap f entries

box p = Term.Embed (Layout.embed { path = p, selectable = False } Styles.currentSymbol)
appBlanks n e = List.foldl (\_ cur -> Term.App cur Term.Blank) e [1 .. n]

showAppBlanks viewEnv path n = renderTerm viewEnv path (appBlanks n (box path))

searchKey : View.Env -> Path -> Term -> String
searchKey viewEnv path e =
  View.key viewEnv { path = path, term = e }

renderExplorerEntry : View.Env -> Path -> Term -> Element
renderExplorerEntry viewEnv path e = Layout.element <|
  View.layout' viewEnv { path = path, term = e }

searchEntry : Bool -> View.Env -> Path -> Term -> (String,Element,Maybe Term)
searchEntry valid viewEnv path e =
  (searchKey viewEnv path e, renderExplorerEntry viewEnv path e, if valid then Just e else Nothing)

localCompletions : View.Env -> Node.LocalInfo -> List (String,Element,Maybe (Term -> Term))
localCompletions viewEnv info = Debug.crash "todo"

pad e = let s = Element.spacer 10 1 in Element.flow Element.right [s, e, s]
renderTerm viewEnv path expr = pad << Layout.element <|
  View.layout' viewEnv { term = expr, path = path }
