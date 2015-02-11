module Unison.Editor where

import Debug
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Movement as Movement
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
import Unison.Explorer as Explorer
import Unison.Hash (Hash)
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
  , admissibleType : Type
  , currentType : Type
  , availableWidth : Maybe Int
  , dependents : Trie Path.E (List Path)
  , overrides : Trie Path.E (Layout View.L)
  , hashes : Trie Path.E Hash
  , explorer : Explorer.Model
  , explorerValues : List Term
  , explorerSelection : Selection1D.Model
  , errors : List String
  , layouts : { panel : Layout View.L
              , explorer : Layout (Result Containment Int) } }

type alias Request = { term : Term, path : Path, query : Maybe String }

type Req
  = Edit Term Path
  | Accept Term Path Term
  | Search Type String
  | Act

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
  , admissibleType = Type.all
  , currentType = Type.all
  , availableWidth = Nothing
  , dependents = Trie.empty
  , overrides = Trie.empty
  , hashes = Trie.empty
  , explorer = Nothing
  , explorerValues = []
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

request : Model -> (Maybe Request, Model)
request model = case model.scope of
  Nothing -> (Nothing, model)
  Just scope ->
    let query = Maybe.map (.input >> .string) model.explorer
    in (Just { term = model.term, path = scope.focus, query = query }, model)

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

updateExplorerValues : Sink Field.Content -> List Term -> Model -> Model
updateExplorerValues searchbox cur model =
  refreshExplorer searchbox
    { model | explorerValues <- cur
            , explorerSelection <- Selection1D.selection model.explorerValues
                                                         cur
                                                         model.explorerSelection }

movement : Movement.D2 -> Model -> Model
movement d2 model = case model.explorer of
  Nothing ->
    let scope = Scope.movement model.term d2 model.scope
    in { model | scope <- scope }
  Just _ -> let d1 = Movement.negateD1 (Movement.xy_y d2)
                limit = List.length model.explorerValues
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
  Selection1D.index model.explorerSelection model.explorerValues `Maybe.andThen`
    \term -> model.scope `Maybe.andThen`
    \scope -> Term.set scope.focus model.term term `Maybe.andThen`
    \t2 -> (Just << closeExplorer) { model | term <- t2 }

openExplorer : Sink Field.Content -> Action
openExplorer searchbox model =
  let (req, m2) = request { model | explorer <- Explorer.zero
                                  , explorerValues <- []
                                  , explorerSelection <- 0 }
  in (req, refreshExplorer searchbox m2)

pushError : String -> Model -> Model
pushError msg model =
  { model | errors <- msg :: List.take 5 model.errors }

-- todo: invalidate dependents and overrides if under the edit path

setSearchbox : Sink Field.Content -> (Int,Int) -> Bool -> Field.Content -> Action
setSearchbox sink origin modifier content model =
  let ex = model.explorer
  in if String.endsWith " " content.string && (not (List.isEmpty model.explorerValues))
     then model |> close origin
                |> (if modifier then apply origin
                    else movement (Movement.D2 Movement.Positive Movement.Zero))
                |> refreshPanel Nothing origin
                |> openExplorer sink
     else request <| refreshExplorer sink { model | explorer <- Explorer.setInput content ex }

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
          , overrides x = Nothing }
      layouts = model.layouts
      explorerRefresh = case searchbox of
        Nothing -> identity
        Just searchbox -> refreshExplorer searchbox
  in explorerRefresh <| case model.scope of
       Nothing -> { model | layouts <- { layouts | panel <- layout }}
       Just scope -> { model | layouts <- { layouts | panel <- layout }}

refreshExplorer : Sink Field.Content -> Model -> Model
refreshExplorer searchbox model =
  let explorerTopLeft : Pt
      explorerTopLeft = case panelHighlight model of
        Nothing -> Pt 0 0
        Just region -> { x = region.topLeft.x - 6, y = region.topLeft.y + region.height + 6 }

      availableWidth = (Maybe.withDefault 1000 model.availableWidth - explorerTopLeft.x - 12)
                       `max` 40

      completions : List Element
      completions = -- todo: real metadata
        let show term = Layout.element << View.layout term <|
          { rootMetadata = Metadata.anonymousTerm
          , availableWidth = availableWidth
          , metadata h = Metadata.anonymousTerm
          , overrides x = Nothing }
        in List.map show model.explorerValues

      aboveMsg = "Allowed: " ++ toString model.admissibleType ++ "\n" ++
                 "Current: " ++ toString model.currentType
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
    let (req, m2) = request { model | explorer <- Explorer.zero
                                    , explorerValues <- []
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
    { term = model0.term, path = [], query = Nothing }
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

search : Sink Field.Content -> Signal Request -> Signal Action
search searchbox reqs =
  let containsNocase sub overall = String.contains (String.toLower sub) (String.toLower overall)
      possible = ["Alice", "Alicia", "Bob", "Burt", "Carol", "Carolina", "Dave", "Don", "Eve"]
      matches query = List.filter (containsNocase query) possible
      go _ model = -- our logic is pure, ignore the request
        let possible = matches (Explorer.getInputOr Field.noContent model.explorer).string
        in norequest (updateExplorerValues searchbox (List.map Terms.str possible) model)
  in Time.delay (200 * Time.millisecond) (Signal.map go reqs)

host : Signal Node.Host
host = Signal.constant "http://localhost:8080"

-- type alias Request = { term : Term, path : Path, query : Maybe String }
search2 : Sink Field.Content -> Signal Request -> Signal Action
search2 searchbox reqs =
  let req r = (r.term, r.path)
      admissible resp model = norequest << refreshExplorer searchbox <| case resp of
        Http.Success t -> { model | admissibleType <- t }
        Http.Waiting -> model
        Http.Failure code msg -> pushError msg model
      current resp model = norequest << refreshExplorer searchbox <| case resp of
        Http.Success t -> { model | currentType <- t }
        Http.Waiting -> model
        Http.Failure code msg -> pushError msg model
      admissibleTypes = Node.admissibleTypeOf host (Signal.map req reqs) |> Signal.map admissible
      currentTypes = Node.typeOf host (Signal.map req reqs) |> Signal.map current
  in Signal.merge admissibleTypes currentTypes

-- need to hook into the Signal Field.Content associated with the model

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
