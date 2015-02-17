module Unison.Editor where

import Debug
import Dict
import Dict (Dict)
import Elmz.Json.Request as JR
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Maybe
import Elmz.Movement as Movement
import Elmz.Result
import Elmz.Selection1D as Selection1D
import Elmz.Signal as Signals
import Elmz.Trie (Trie)
import Elmz.Trie as Trie
import Graphics.Element (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Http
import Keyboard
import List
import Maybe
import Mouse
import Result
import Signal
import String
import Time
import Touch
import Unison.Action as Action
import Unison.Explorer as Explorer
import Unison.Hash (Hash)
import Unison.Reference (Reference)
import Unison.Reference as Reference
import Unison.Metadata (Metadata)
import Unison.Metadata as Metadata
import Unison.Node as Node
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term (Term)
import Unison.Term as Term
import Unison.Terms as Terms
import Unison.Type (Type)
import Unison.Type as Type
import Unison.View as View
import Window

type alias Model =
  { term : Term
  , scope : Scope.Model
  , localInfo : Maybe Node.LocalInfo
  , globalMatches : String -> Result (List Term) (List Term)
  , rootMetadata : Metadata
  , metadata : Reference -> Metadata
  , availableWidth : Maybe Int
  , dependents : Trie Path.E (List Path)
  , overrides : Trie Path.E (Layout View.L)
  , hashes : Trie Path.E Hash
  , explorer : Explorer.Model
  , explorerSelection : Selection1D.Model
  , layouts : { panel : Layout View.L
              , explorer : Layout (Result Containment Int) }
  , errors : List String }

viewEnv : Model -> View.Env
viewEnv model =
  let explorerTopLeft : Pt
      explorerTopLeft = case panelHighlight model of
        Nothing -> Pt 0 0
        Just region -> { x = region.topLeft.x - 6, y = region.topLeft.y + region.height + 6 }
  in { rootMetadata = model.rootMetadata
     , metadata = model.metadata
     , availableWidth = (Maybe.withDefault 1000 model.availableWidth - explorerTopLeft.x - 12) `max` 40
     , overrides path = Trie.lookup path model.overrides
     , overall = model.term }

keyedCompletions : Model -> List (String,Term,Element)
keyedCompletions model =
  let f e i scope =
    let search = e.input.string
        env = viewEnv model
        render term = Layout.element (View.layout term (viewEnv model))
        regulars = i.wellTypedLocals ++ Elmz.Result.merge (model.globalMatches search)
        key e = View.key { model | overall = model.term } { path = scope.focus, term = model.term }
        format e = (key e, e, render e)
        box = Term.Embed (Layout.embed { path = [], selectable = False } Styles.currentSymbol)
        appBlanks n e = if n <= 0 then e else appBlanks (n-1) (Term.App e Term.Blank)
        showAppBlanks n e =
          let go n e = if n <= 0 then e else go (n-1) (Term.App e box)
          in render (go n e)
        la cur n = (toString i, appBlanks n cur, showAppBlanks n cur)
        currentApps = case Term.at scope.focus model.term of
          Nothing -> []
          Just cur -> List.map (la cur) i.localApplications
    in List.map format regulars
  in Maybe.withDefault [] (Elmz.Maybe.map3 f model.explorer model.localInfo model.scope)

explorerValues : Model -> List Term
explorerValues model =
  let search = Maybe.withDefault "" (Maybe.map (.input >> .string) (model.explorer))
  in List.filter (\(k,_,_) -> String.contains search k) (keyedCompletions model)
     |> List.map (\(_,e,_) -> e)

explorerInput : Model -> String
explorerInput model =
  Elmz.Maybe.maybe "" (.input >> .string) model.explorer

type Request
  = Open Term Path -- obtain the current and admissible type and local completions
  | Search Type String -- global search for a given type
  | Declare Term
  | Edit Path Action.Action Term
  | Metadatas (List Hash)

type alias Action = Model -> (Maybe Request, Model)

combine : Action -> Action -> Action
combine f g model =
  let (req, m2) = f model
      (req', m3) = g m2
  in (Maybe.oneOf [req', req], m3)

pin : (Int,Int) -> Layout View.L -> Layout View.L
pin origin l =
  let t = Layout.tag l
  in Layout.pin origin { t | selectable <- False } l

model0 : Model
model0 =
  { term = Term.Blank
  , scope = Nothing
  , localInfo = Nothing
  , globalMatches s = Result.Err []
  , rootMetadata = Metadata.anonymousTerm
  , metadata r = Metadata.anonymousTerm
  , availableWidth = Nothing
  , dependents = Trie.empty
  , overrides = Trie.empty
  , hashes = Trie.empty
  , explorer = Nothing
  , explorerSelection = 0
  , errors = []
  , layouts = { panel = layout0
              , explorer = explorerLayout0  } }

layout0 : Layout View.L
layout0 = Layout.empty { path = [], selectable = True }

explorerLayout0 : Layout (Result Containment a)
explorerLayout0 = Layout.empty (Result.Err Outside)

panelHighlight : Model -> Maybe Region
panelHighlight model =
  Maybe.andThen model.scope (Scope.view model.layouts.panel)

openRequest : Model -> (Maybe Request, Model)
openRequest model = case model.scope of
  Nothing -> (Nothing, model)
  Just scope -> (Just (Open model.term scope.focus), model)

norequest : Model -> (Maybe Request, Model)
norequest model = (Nothing, model)

type alias Sink a = a -> Signal.Message

click : Sink Field.Content -> (Int,Int) -> (Int,Int) -> Action
click searchbox origin (x,y) model = case model.explorer of
  Nothing -> case Layout.leafAtPoint model.layouts.panel (Pt x y) of
    Nothing -> norequest model -- noop, user didn't click on anything!
    Just node -> openExplorer searchbox model
  Just _ -> case Layout.leafAtPoint model.layouts.explorer (Pt x y) of
    Nothing -> norequest (closeExplorer model) -- treat this as a close event
    Just (Result.Ok i) -> norequest (close origin { model | explorerSelection <- i }) -- close w/ selection
    Just (Result.Err Inside) -> norequest model -- noop click inside explorer
    Just (Result.Err Outside) -> norequest (closeExplorer model) -- treat this as a close event

moveMouse : (Int,Int) -> Action
moveMouse xy model = case model.explorer of
  Nothing ->
    let scope = Scope.reset xy model.layouts.panel model.scope
    in norequest <| { model | scope <- scope }
  Just _ -> let e = Selection1D.reset xy model.layouts.explorer model.explorerSelection
            in norequest <| { model | explorerSelection <- e }

{-
updateExplorerValues : Sink Field.Content -> List Term -> Model -> Model
updateExplorerValues searchbox cur model =
  refreshExplorer searchbox
    { model | explorerValues <- cur
            , explorerSelection <- Selection1D.selection model.explorerInfo.matches
                                                         cur
                                                         model.explorerSelection }
-}

movement : Movement.D2 -> Model -> Model
movement d2 model = case model.explorer of
  Nothing ->
    let scope = Scope.movement model.term d2 model.scope
    in { model | scope <- scope }
  Just ex -> let d1 = Movement.negateD1 (Movement.xy_y d2)
                 search = Elmz.Maybe.maybe "" (\e -> e.input.string) model.explorer
                 limit = List.length ex.completions
                 sel = model.explorerSelection
             in { model | explorerSelection <- Selection1D.movement d1 limit sel }

delete : (Int,Int) -> Model -> Model
delete origin model = case model.explorer of
  Nothing -> case model.scope of
    Nothing -> model
    Just scope -> case Term.delete scope.focus model.term of
      Nothing -> model
      Just term -> refreshPanel Nothing origin
                     { model | term <- term
                             , scope <- Just { scope | focus <- List.drop 1 scope.focus } }
  Just _ -> model

closeExplorer : Model -> Model
closeExplorer model =
  let layouts = model.layouts
  in { model | explorer <- Nothing, layouts <- { layouts | explorer <- explorerLayout0 }}

close : (Int,Int) -> Model -> Model
close origin model =
  refreshPanel Nothing origin << Maybe.withDefault (closeExplorer model) <|
  Selection1D.index model.explorerSelection (explorerValues model) `Maybe.andThen`
    \term -> model.scope `Maybe.andThen`
    \scope -> Term.set scope.focus model.term term `Maybe.andThen`
    \t2 -> (Just << closeExplorer) { model | term <- t2 }

openExplorer : Sink Field.Content -> Action
openExplorer searchbox model =
  let (req, m2) = openRequest { model | explorer <- Explorer.zero
                                      , localInfo <- Nothing
                                      , explorerSelection <- 0 }
  in (req, refreshExplorer searchbox m2)

pushError : String -> Model -> Model
pushError msg model =
  { model | errors <- msg :: List.take 5 model.errors }

-- todo: invalidate dependents and overrides if under the edit path

setSearchbox : Sink Field.Content -> (Int,Int) -> Bool -> Field.Content -> Action
setSearchbox sink origin modifier content model =
  let model' = { model | explorer <- Explorer.setInput content model.explorer }
  in if String.endsWith " " content.string && (not (List.isEmpty (explorerValues model)))
     then model |> close origin
                |> (if modifier then apply origin
                    else movement (Movement.D2 Movement.Positive Movement.Zero))
                |> refreshPanel Nothing origin
                |> openExplorer sink
     else case model.localInfo of
            Nothing -> norequest <| refreshExplorer sink model'
            Just info -> case model.globalMatches (explorerInput model) of
              Result.Err terms -> (Just (Search info.admissible content.string),
                                   refreshExplorer sink model')
              Result.Ok terms -> norequest model'

apply : (Int,Int) -> Model -> Model
apply origin model = case model.scope of
  Nothing -> model
  Just scope -> Maybe.withDefault model <|
    Term.at scope.focus model.term `Maybe.andThen`
      \focus -> Term.set scope.focus model.term (Term.App focus Term.Blank) `Maybe.andThen`
      \term -> let scope' = Scope.scope (scope.focus `Path.snoc` Path.Arg)
               in Just { model | term <- term, scope <- Just scope' }

{-| Updates `layouts.panel` and `layouts.panelHighlight` based on a change. -}
refreshPanel : Maybe (Sink Field.Content) -> (Int,Int) -> Model -> Model
refreshPanel searchbox origin model =
  let layout = pin origin <| case model.availableWidth of
        Nothing -> layout0
        Just availableWidth -> View.layout model.term <|
          { rootMetadata = Metadata.anonymousTerm
          , availableWidth = availableWidth - fst origin
          , metadata h = Metadata.anonymousTerm
          , overrides x = Nothing
          , overall = model.term }
      layouts = model.layouts
      explorerRefresh = case searchbox of
        Nothing -> identity
        Just searchbox -> refreshExplorer searchbox
  in explorerRefresh <| case model.scope of
       Nothing -> { model | layouts <- { layouts | panel <- layout }}
       Just scope -> { model | layouts <- { layouts | panel <- layout }}

refreshExplorer : Sink Field.Content -> Model -> Model
refreshExplorer searchbox model = case model.localInfo of
  Nothing -> model
  Just localInfo ->
    let explorerTopLeft : Pt
        explorerTopLeft = case panelHighlight model of
          Nothing -> Pt 0 0
          Just region -> { x = region.topLeft.x - 6, y = region.topLeft.y + region.height + 6 }

        availableWidth = (Maybe.withDefault 1000 model.availableWidth - explorerTopLeft.x - 12)
                         `max` 40

        rootMetadata = Metadata.anonymousTerm
        metadata h = Metadata.anonymousTerm

        completions : List Element
        completions = -- todo: real metadata
          let show term = Layout.element << View.layout term <|
            { rootMetadata = Metadata.anonymousTerm
            , availableWidth = availableWidth
            , metadata = metadata
            , overrides x = Nothing
            , overall = term }
          in List.map show (explorerValues model)

        aboveMsg = "Allowed: " ++ toString localInfo.admissible ++ "\n" ++
                   "Current: " ++ toString localInfo.current
        explorer' : Explorer.Model
        explorer' = model.explorer |> Maybe.map (\e ->
          { e | completions <- completions
              , above <- Styles.codeText aboveMsg })

        explorerLayout : Layout (Result Containment Int)
        explorerLayout = Explorer.view explorerTopLeft searchbox explorer'

    in let layouts = model.layouts
       in { model | explorer <- explorer'
                  , layouts <- { layouts | explorer <- explorerLayout } }

resize : Maybe (Sink Field.Content) -> (Int,Int) -> Int -> Model -> Model
resize sink origin width model =
  refreshPanel sink origin { model | availableWidth <- Just width }

enter : Sink Field.Content -> (Int,Int) -> Action
enter snk origin model = case model.explorer of
  Nothing ->
    let (req, m2) = openRequest { model | explorer <- Explorer.zero
                                        , localInfo <- Nothing
                                        , globalMatches <- \s -> Result.Err []
                                        , explorerSelection <- 0 }
    in (req, refreshExplorer snk m2)
  Just _ -> norequest (close origin model)

type alias Inputs =
  { origin : (Int,Int)
  , clicks : Signal ()
  , mouse : Signal (Int,Int)
  , enters : Signal ()
  , deletes : Signal ()
  , modifier : Signal Bool
  , movements : Signal Movement.D2
  , searchbox : Signal.Channel Field.Content
  , width : Signal Int }

actions : Inputs -> Signal Action
actions ctx =
  let content = ignoreUpDown (Signal.subscribe ctx.searchbox)
      -- delay seems to be needed because otherwise the text field
      -- swallows our arrow key events; the delay ensures they don't co-occur
      movementsRepeated = Time.delay 0 (Movement.repeatD2 ctx.movements)
      merge = Signals.mergeWith combine
      clickPositions = Signal.sampleOn ctx.clicks ctx.mouse
      searchbox = Signal.send ctx.searchbox
      steadyWidth = Signals.sampleOnMerge Signals.start
                                          (Signals.steady (100 * Time.millisecond) ctx.width)
      movementf : Movement.D2 -> Action
      movementf d model = movement d model |> norequest
      resizef : Int -> Action
      resizef w model = resize (Just searchbox) ctx.origin w model |> norequest
      deletef origin _ model = delete origin model |> norequest
  in Signal.map movementf movementsRepeated `merge`
     Signal.map resizef steadyWidth `merge`
     Signal.map (deletef ctx.origin) ctx.deletes `merge`
     Signal.map (always (enter searchbox ctx.origin)) ctx.enters `merge`
     Signal.map moveMouse ctx.mouse `merge`
     Signals.map2r (setSearchbox searchbox ctx.origin) ctx.modifier content `merge`
     Signal.map (click searchbox ctx.origin) clickPositions

models : Inputs -> (Signal Request -> Signal Action) -> Model -> Signal Model
models ctx search model0 =
  Signals.asyncUpdate
    search
    (actions ctx)
    (Open model0.term [])
    model0

view : Model -> Element
view model =
  let highlight = case panelHighlight model of
        Nothing -> Element.empty
        Just region -> Styles.selection region

      explorerHighlight : Element
      explorerHighlight =
        Selection1D.view Styles.explorerSelection model.layouts.explorer model.explorerSelection

      highlightedExplorerLayout : Layout (Result Containment Int)
      highlightedExplorerLayout =
        Layout.transform (\e -> Element.layers [e, explorerHighlight])
                         model.layouts.explorer

  in Element.layers [ Layout.element model.layouts.panel
                    , highlight
                    , Layout.element highlightedExplorerLayout ]

ignoreUpDown : Signal Field.Content -> Signal Field.Content
ignoreUpDown s =
  let f arrows c prevC = if arrows.y /= 0 && c.string == prevC.string then prevC else c
  in Signal.dropRepeats <|
     Signal.map3 f (Signal.keepIf (\a -> a.y /= 0) {x = 0, y = 0} Keyboard.arrows)
                   s
                   (Signals.delay Field.noContent s)

host = "http://localhost:8080"

search2 : Sink Field.Content -> Signal Request -> Signal Action
search2 searchbox reqs =
  let openEdit r = case r of
        Open term path -> Just (term,path)
        _ -> Nothing
      openEdit' =
        let go oe model = List.head []
        in Signal.map openEdit reqs |> JR.send (Node.openEdit host `JR.to` go)
      search r = case r of
        Search typ query -> Just (typ,query)
        _ -> Nothing
      declare r = case r of
        Declare term -> Just term
        _ -> Nothing
      edit r = case r of
        Edit path action term -> Just (path,action,term)
        _ -> Nothing
      metadatas r = case r of
        Metadatas hs -> Just hs
        _ -> Nothing

  in List.head []

main =
  let origin = (15,15)
      inputs = { origin = origin
               , clicks = Mouse.clicks `Signal.merge` (Signals.doubleWithin Time.second Touch.taps)
               , mouse = Mouse.position `Signal.merge` (Signal.map (\{x,y} -> (x,y)) Touch.taps)
               , enters = Signal.map (always ()) (Signals.ups (Keyboard.enter))
               , modifier = Keyboard.shift
               , deletes = Signal.map (always ()) (Signals.ups (Keyboard.isDown 68))
               , movements = Movement.d2' Keyboard.arrows
               , searchbox = Signal.channel Field.noContent
               , width = Window.width }
      -- queries = Signal.map (always ()) (ignoreUpDown (Signal.subscribe inputs.searchbox))
      ignoreReqs actions =
        let ignore action model = snd (action model)
        in Signal.map ignore actions
      ms = models inputs
                  (search2 (Signal.send inputs.searchbox))
                  { model0 | term <- Terms.int 42 }
      debug model =
        let summary model = model.explorer
        in Debug.watchSummary "model" summary model
      ms' = Signal.map debug ms
  in Signal.map view ms'
