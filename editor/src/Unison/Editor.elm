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
import Set
import Signal
import String
import Time
import Touch
import Unison.Action as Action
import Unison.Explorer as Explorer
import Unison.SearchboxParser as SearchboxParser
import Unison.Hash (Hash)
import Unison.Metadata (Metadata)
import Unison.Metadata as Metadata
import Unison.Node as Node
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Reference (Reference)
import Unison.Reference as Reference
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term (Term)
import Unison.Term as Term
import Unison.Terms as Terms
import Unison.Type (Type)
import Unison.Type as Type
import Unison.Var as Var
import Unison.View as View
import Window

type alias Model =
  { term : Term
  , scope : Scope.Model
  , localInfo : Maybe Node.LocalInfo
  , globalMatches : String -> Result (List Term) (List Term)
  , rootMetadata : Metadata
  , metadata : Dict Reference.Key Metadata
  , availableWidth : Maybe Int
  , dependents : Trie Path.E (List Path)
  , overrides : Trie Path.E (Layout View.L)
  , hashes : Trie Path.E Hash
  , explorer : Explorer.Model
  , explorerSelection : Selection1D.Model
  , literal : Maybe Term.Literal -- a literal parsed from the current searchbox
  , layouts : { panel : Layout View.L
              , explorer : Layout (Result Containment Int) }
  , status : List (JR.Status String) }

model0 : Model
model0 =
  { term = Term.Blank
  , scope = Nothing
  , localInfo = Nothing
  , globalMatches s = Result.Err []
  , rootMetadata = Metadata.anonymousTerm
  , metadata = Dict.empty
  , availableWidth = Nothing
  , dependents = Trie.empty
  , overrides = Trie.empty
  , hashes = Trie.empty
  , explorer = Nothing
  , explorerSelection = 0
  , literal = Nothing
  , status = [JR.Inactive]
  , layouts = { panel = layout0
              , explorer = explorerLayout0  } }

layout0 : Layout View.L
layout0 = Layout.empty { path = [], selectable = True }

explorerLayout0 : Layout (Result Containment a)
explorerLayout0 = Layout.empty (Result.Err Outside)

metadata : Model -> Reference -> Metadata
metadata model r =
  Maybe.withDefault (Metadata.defaultMetadata r)
                    (Dict.get (Reference.toKey r) model.metadata)

explorerViewEnv : Model -> View.Env
explorerViewEnv model =
  let explorerTopLeft = case panelHighlight model of
        Nothing -> Pt 0 0
        Just region -> { x = region.topLeft.x - 6, y = region.topLeft.y + region.height + 6 }
  in { rootMetadata = model.rootMetadata
     , metadata = metadata model
     , availableWidth = (Maybe.withDefault 1000 model.availableWidth - explorerTopLeft.x - 12) `max` 40
     , overrides path = Trie.lookup path model.overrides
     , overall = model.term }

focus : Model -> Maybe Term
focus model = model.scope `Maybe.andThen` \scope -> Term.at scope.focus model.term

focusOr : Term -> Model -> Term
focusOr e model = Maybe.withDefault e (focus model)

keyedCompletions : Model -> List (String,Term,Element)
keyedCompletions model =
  let f e i scope =
    let search = e.input.string
        env = explorerViewEnv model
        render expr = Layout.element <|
          View.layout' (explorerViewEnv model)
                       { path = scope.focus, term = expr, boundAt = Path.boundAt }
        lits = case (Debug.log "model.literal" model.literal) of
          Nothing -> []
          Just lit -> if Term.checkLiteral lit i.admissible
                      then [(e.input.string, Term.Lit lit, render (Term.Lit lit))]
                      else []
        regulars = Debug.log "regulars" <|
          i.wellTypedLocals ++ Elmz.Result.merge (model.globalMatches search)
        key e =
          let ctx = Term.trySet scope.focus e model.term
          in View.key
            { metadata = metadata model, rootMetadata = model.rootMetadata, overall = ctx }
            { path = scope.focus, term = e, boundAt = Path.boundAt }
        format e = (key e, e, render e)
        box = Term.Embed (Layout.embed { path = [], selectable = False } Styles.currentSymbol)
        appBlanks n e = List.foldl (\_ cur -> Term.App cur Term.Blank) e [0 .. n]
        showAppBlanks n = render (List.foldl (\_ box -> Term.App box Term.Blank) box [0 .. n])
        la cur n = (String.padLeft (n+1) '.' "", appBlanks n cur, showAppBlanks n)
        currentApps = Debug.log "currentApps" <| case Term.at scope.focus model.term of
          Nothing -> []
          Just cur -> (".", cur, Styles.currentSymbol) :: List.map (la cur) i.localApplications
        ks = Debug.log "keys" (List.map (\(k,_,_) -> k) results)
        results = currentApps
               ++ List.map format regulars
               ++ lits
    in results
  in Maybe.withDefault [] (Elmz.Maybe.map3 f model.explorer model.localInfo model.scope)

filteredCompletions : Model -> List (Term,Element)
filteredCompletions model =
  let search = Maybe.withDefault "" (Maybe.map (.input >> .string) (model.explorer))
  in List.filter (\(k,_,_) -> String.contains (String.trim search) k) (keyedCompletions model)
     |> List.map (\(_,e,l) -> (e,l))

explorerInput : Model -> String
explorerInput model =
  Elmz.Maybe.maybe "" (.input >> .string) model.explorer

type Request
  = Open Term Path -- obtain the current and admissible type and local completions
  | Search Type String -- global search for a given type
  | Declare Term
  | Edit Path Path Action.Action Term
  | Metadatas (List Reference)

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

panelHighlight : Model -> Maybe Region
panelHighlight model =
  Maybe.andThen model.scope (Scope.view model.layouts.panel)

openRequest : Model -> (Maybe Request, Model)
openRequest model = case model.scope of
  Nothing -> (Nothing, model)
  -- todo: can trim path to be with respect to nearest closed term
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
  in { model | explorer <- Nothing
             , layouts <- { layouts | explorer <- explorerLayout0 }
             , literal <- Nothing }

close : (Int,Int) -> Model -> Model
close origin model =
  refreshPanel Nothing origin << Maybe.withDefault (closeExplorer model) <|
  Selection1D.index model.explorerSelection (List.map fst (filteredCompletions model)) `Maybe.andThen`
    \term -> model.scope `Maybe.andThen`
    \scope -> Term.set scope.focus model.term term `Maybe.andThen`
    \t2 -> (Just << closeExplorer) { model | term <- t2 }

openExplorer : Sink Field.Content -> Action
openExplorer = openExplorerWith Field.noContent

openExplorerWith : Field.Content -> Sink Field.Content -> Action
openExplorerWith content searchbox model =
  let zero = Maybe.map (\z -> { z | input <- content }) Explorer.zero
      (req, m2) = openRequest { model | explorer <- zero
                                      , localInfo <- Nothing
                                      , explorerSelection <- 0
                                      , literal <- Nothing }
  in (req, refreshExplorer searchbox m2)

-- todo: invalidate dependents and overrides if under the edit path

ops = Set.fromList (String.toList "!@#$%^&*-+|\\;.></`~")

modifyFocus : (Term -> Term) -> Model -> Model
modifyFocus f model = Maybe.withDefault model <|
  model.scope `Maybe.andThen`
    \scope -> Term.modify scope.focus f model.term `Maybe.andThen`
    \term -> Just { model | term <- term }

clearScopeHistory : Model -> Model
clearScopeHistory model = case model.scope of
  Nothing -> model
  Just scope -> { model | scope <- Just (Scope.scope scope.focus) }

setSearchbox : Sink Field.Content -> (Int,Int) -> Bool -> Field.Content -> Action
setSearchbox sink origin modifier content model =
  let model' = { model | explorer <- Explorer.setInput content model.explorer }
      trimArg scope model =
        { model | scope <- Debug.log "trimArg" (Just (Scope.scope (Path.trimArg scope.focus))) }

      leftover s =
        let z = Field.noContent
        in { z | string <- s }

      seq : Action -> Char -> Action
      seq action op model =
        if List.isEmpty (filteredCompletions model) then
          let u = Debug.log "no valid completions" ()
          in action model
        else case model.scope of
          Nothing -> action model
          Just scope ->
            if | Set.member op ops ->
               snd (action model)
                 |> close origin
                 -- something screwy in here, appears path is invalid after trimming
                 |> (if (Debug.log "rightmost?" <| Path.isRightmostArg scope.focus)
                     then trimArg scope
                     else identity)
                 |> modifyFocus (\e -> Term.App (Term.App Term.Blank e) Term.Blank)
                 |> clearScopeHistory
                 |> movement (Movement.D2 Movement.Zero Movement.Negative)
                 |> refreshPanel Nothing origin
                 |> openExplorerWith (leftover (String.fromChar op)) sink
               | op == ' ' -> action model
               | otherwise -> action model

      literal e model =
        norequest (refreshExplorer sink { model | literal <- Just e })
      query string model = case model.localInfo of
        Nothing -> norequest <| refreshExplorer sink model
        Just info -> case model.globalMatches (explorerInput model) of
          Result.Err terms -> (Just (Search info.admissible content.string),
                               refreshExplorer sink model)
          Result.Ok terms -> norequest (refreshExplorer sink model)
      env = { literal = literal, query = query, combine = seq }
  in case SearchboxParser.parse env content.string of
       Result.Err msg -> norequest model'
       Result.Ok action ->
         let r = action model'
             x = Debug.log "parse succeeded" (snd r).literal
         in r

  --if String.endsWith " " content.string && (not (List.isEmpty (filteredCompletions model)))
  --   then model |> close origin
  --              |> (if modifier then apply origin
  --                  else movement (Movement.D2 Movement.Positive Movement.Zero))
  --              |> refreshPanel Nothing origin
  --              |> openExplorer sink
  --   else

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
          { rootMetadata = model.rootMetadata
          , availableWidth = availableWidth - fst origin
          , metadata = metadata model
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

        completions : List Element
        completions = List.map snd (filteredCompletions model)
        viewEnv = explorerViewEnv model
        path = Maybe.withDefault [] (Maybe.map .focus model.scope)
        pad e = let s = Element.spacer 10 1
                in Element.flow Element.right [s, e, s]
        render expr = View.layout'
                        viewEnv
                        { term = expr, path = path, boundAt = Path.boundAt }
                   |> Layout.element >> pad
        currentType = Element.flow Element.right
          [ Styles.currentSymbol
          , Styles.codeText (" : " ++ Type.key { metadata = metadata model } localInfo.current) ]
        above0 = Element.flow Element.down <|
          [ Element.spacer 1 10
          , pad <| Styles.boldCodeText (Type.key { metadata = metadata model } localInfo.admissible)
          , Element.spacer 1 12
          , pad currentType ] ++
          List.map render localInfo.locals
        above = Element.flow Element.down
          [ above0
          , Element.spacer 1 10
          , Styles.menuSeparator (Element.widthOf above0)
          , Element.spacer 1 10 ]

        explorer' : Explorer.Model
        explorer' = model.explorer |> Maybe.map (\e ->
          { e | completions <- completions, above <- above })

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

withStatus : Result (JR.Status String) Action -> Action
withStatus r = case r of
  Result.Err status -> \model ->
    let f s = case status of
                JR.Inactive -> List.drop 1 s
                JR.Waiting -> status :: s
                JR.Failed e -> let l = Debug.log e () in status :: s
    in norequest { model | status <- f model.status }
  Result.Ok action -> action

search2 : Sink Field.Content -> (Int,Int) -> Signal Request -> Signal Action
search2 searchbox origin reqs =
  let openEdit r = case r of
        Open term path -> Just (term,path)
        _ -> Nothing
      openEdit' =
        let go oe model =
          norequest (refreshExplorer searchbox { model | localInfo <- Just (Debug.log "info" oe) })
        in Signal.map openEdit reqs
           |> JR.send (Node.localInfo host `JR.to` go) (model0.term, [])
           |> Signal.map withStatus
      search r = case r of
        Search typ query -> Just (typ,query)
        _ -> Nothing
      declare r = case r of
        Declare term -> Just term
        _ -> Nothing
      edit r = case r of
        -- todo: reroot the request to point to tightest bound term
        Edit rootPath relPath action term -> Just (rootPath,relPath,action,term)
        _ -> Nothing
      edit' =
        let go (path,old,new) model = case Term.at path model.term of
              Nothing -> norequest model
              Just old' ->
                if old == old'
                then case Term.set path model.term new of
                       Just term -> let m2 = { model | term <- term }
                                    in norequest (refreshPanel (Just searchbox) origin m2)
                       Nothing -> norequest model
                else norequest model
        in Signal.map edit reqs
           |> JR.send (Node.editTerm host `JR.to` go) ([],[],Action.Noop,model0.term)
           |> Signal.map withStatus
      metadatas r = case r of
        Metadatas rs -> Just rs
        _ -> Nothing
      metadatas' =
        let go mds model =
              { model | metadata <- Dict.union mds model.metadata }
              |> refreshPanel (Just searchbox) origin
              |> norequest
        in Signal.map metadatas reqs
           |> JR.send (Node.metadatas host `JR.to` go) []
           |> Signal.map withStatus
      noop model = norequest model
  in openEdit' `Signal.merge` metadatas' `Signal.merge` edit'

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
      expr = Term.Lam (Term.Lam (Term.Var 2))
      -- expr = (Term.Lam (Terms.int 42))
      ms = models inputs
                  (search2 (Signal.send inputs.searchbox) origin)
                  { model0 | term <- expr }
      debug model =
        model |> Debug.watchSummary "explorer" .explorer
              |> Debug.watchSummary "status" .status
      ms' = Signal.map debug ms
  in Signal.map view ms'
